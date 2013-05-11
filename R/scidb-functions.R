#/*
#**
#* BEGIN_COPYRIGHT
#*
#* This file is part of SciDB.
#* Copyright (C) 2008-2013 SciDB, Inc.
#*
#* SciDB is free software: you can redistribute it and/or modify
#* it under the terms of the AFFERO GNU General Public License as published by
#* the Free Software Foundation.
#*
#* SciDB is distributed "AS-IS" AND WITHOUT ANY WARRANTY OF ANY KIND,
#* INCLUDING ANY IMPLIED WARRANTY OF MERCHANTABILITY,
#* NON-INFRINGEMENT, OR FITNESS FOR A PARTICULAR PURPOSE. See
#* the AFFERO GNU General Public License for the complete license terms.
#*
#* You should have received a copy of the AFFERO GNU General Public License
#* along with SciDB.  If not, see <http://www.gnu.org/licenses/agpl-3.0.html>
#*
#* END_COPYRIGHT
#*/

# This file defines functions that support a Bigmemory-like SciDB R
# object. See the scidb-class.R file.

# Create a new scidb reference to an existing SciDB array.
# name (character): Name of the backing SciDB array
# attribute (character): Attribute in the backing SciDB array (applies to n-d arrays)
# gc (logical): Remove backing SciDB array when R object is garbage collected?
# data.frame (logical): Return a SciDB data frame object (class scidbdf)
scidb = function(name, attribute, `data.frame`, gc)
{
  if(missing(name)) stop("array name must be specified")
  if(missing(attribute)) attribute=""
  if(missing(gc)) gc=FALSE
  D = .scidbdim(name)
  x = .scidbattributes(name)
  if(missing(`data.frame`)) `data.frame` = ( (dim(D)[1]==1) &&  (length(x$attributes)>1))
  if(dim(D)[1]>1 && `data.frame`) stop("SciDB data frame objects can only be associated with 1-D SciDB arrays")
  TYPES = x$types
  A = x$attributes
  NULLABLE = x$nullable
  if(attribute=="") w = 1
  else w = which(A == attribute)
  if(length(w)<1) stop(paste(attribute,"is not a valid attribute name. The array ",name," contains the attributes:\n\n",paste(A,collapse="\n"),"\nTry selecting one of those.",sep=""))
  attribute = A[w]
  TYPE = TYPES[w]

  DIM = D$length
  LENGTH = prod(DIM)

  if(`data.frame`)
  {
# Set default column types
    ctypes = c("int64",TYPES)
    cc = rep(NA,length(ctypes))
    cc[ctypes=="datetime"] = "Date"
    cc[ctypes=="float"] = "double"
    cc[ctypes=="double"] = "double"
    cc[ctypes=="bool"] = "logical"
    st = grep("string",ctypes)
    if(length(st>0)) cc[st] = "character"
    obj = new("scidbdf",
            call=match.call(),
            name=name,
            attributes=A,
            types=TYPES,
            nullable=NULLABLE,
            D=D,
            dim=c(DIM,length(A)),
            colClasses=cc,
            gc=new.env(),
            length=length(A)
        )
  } else
  {
    obj = new("scidb",
            call=match.call(),
            name=name,
            attribute=attribute,
            type=TYPE,
            attributes=A,
            types=TYPES,
            nullable=NULLABLE,
            D=D,
            dim=DIM,
            gc=new.env(),
            length=LENGTH
        )
  }
  if(gc){
    obj@gc$name   = name
    obj@gc$remove = TRUE
    reg.finalizer(obj@gc, function(e) if(e$remove) 
                  tryCatch(scidbremove(e$name),error=function(e) invisible()),
                  onexit=TRUE)
  }
  obj
}


# .scidbdim is an internal function that retirieves dimension metadata from a
# scidb array called "name."
.scidbdim = function(name)
{
#  if(!.scidbexists(name)) stop ("not found") 
  d = iquery(paste("dimensions(",name,")"),return=TRUE)
# R is unfortunately interpreting 'i' as an imaginary unit I think.
  if(any(is.na(d))) d[is.na(d)] = "i"
  d
}

# Retrieve list of attributes for a named SciDB array (internal function).
.scidbattributes = function(name)
{
  x = iquery(paste("attributes(",name,")",sep=""),return=TRUE,colClasses=c(NA,"character",NA,NA))
# R is unfortunately interpreting 'i' as an imaginary unit I think.
  if(any(is.na(x))) x[is.na(x)] = "i"
  list(attributes=x[,2],types=x[,3],nullable=(x[,4]=="true"))
}

colnames.scidb = function(x)
{
  if(length(x@D$name)<2) return(NULL)
  if(x@D$type[2] != "string") return(c(x@D$start[2],x@D$start[2]+x@D$length[2]-1))
  if(x@D$length[2] > options("scidb.max.array.elements"))
    stop("Result will be too big. Perhaps try a manual query with an iterative result.")
  Q = sprintf("scan(%s:%s)",x@name,x@D$name[2])
  iquery(Q,return=TRUE,n=x@D$length[2]+1)[,2]
}

rownames.scidb = function(x)
{
  if(x@D$type[1] != "string") return(c(x@D$start[1],x@D$start[1]+x@D$length[1]-1))
  if(x@D$length[1] > options("scidb.max.array.elements"))
    stop("Result will be too big. Perhaps try a manual query with an iterative result.")
  Q = sprintf("scan(%s:%s)",x@name,x@D$name[1])
  iquery(Q,return=TRUE,n=x@D$length[1]+1)[,2]
}

names.scidb = function(x)
{
  if(is.null(dim(x))) rownames(x)
  colnames(x)
}

dimnames.scidb = function(x)
{
  lapply(1:length(x@D$name), function(j)
  {
    if(x@D$type[j] != "string") return(c(x@D$start[j],x@D$start[j]+x@D$length[j]-1))
    if(x@D$length[j] > options("scidb.max.array.elements"))
      stop("Result will be too big. Perhaps try a manual query with an iterative result.")
    Q = sprintf("scan(%s:%s)",x@name,x@D$name[j])
    iquery(Q,return=TRUE,n=Inf)[,2]
  })
}

`dimnames<-.scidb` = function(x, value)
{
  stop("unsupported")
}

summary.scidb = function(x)
{
  warning("Not available.")
  invisible()
}

# XXX this will use insert, write me.
#`[<-.scidb` = function(x,j,k, ..., value,default)
#{
#  stop("Sorry, scidb array objects are read only for now.")
#}

# Flexible array subsetting wrapper.
# x: A Scidb array object
# ...: list of dimensions
# defailt: default fill-in value
# 
# Returns a materialized R array if length(list(...))==0.
# Or, a scidb array object that represents the subarray.
`[.scidb` = function(x, ..., default)
{
  M = match.call()
  drop = ifelse(is.null(M$drop),TRUE,M$drop)
  M = M[3:length(M)]
  if(!is.null(names(M))) M = M[!(names(M) %in% c("drop","default"))]
# Check for user-specified default fill-in value
  if(missing(default)) default = options("scidb.default.value")
# i shall contain a list of requested index values
  E = parent.frame()
  i = lapply(1:length(M), function(j) tryCatch(eval(M[j][[1]],E),error=function(e)c()))
# Adjust the indices from one-indexing to zero-indexing.
  if (options()$scidb.index.at.one) {
    i <- lapply(i, 
      FUN=function(x) {
        if (is.numeric(x)) x-1 else x
      })
  }
# User wants this materialized to R...
  if(all(sapply(i,is.null)))
    return(materialize(x,default=default,drop=drop))
# Not materializing, return a SciDB array
  if(length(i)!=length(dim(x))) stop("Dimension mismatch")
  dimfilter(x,i)
}

`dim.scidb` = function(x)
{
  if(length(x@dim)==0) return(NULL)
  x@dim
}

`dim<-.scidb` = function(x, value)
{
  stop("unsupported")
}


`str.scidb` = function(object, ...)
{
  cat("SciDB array name: ",object@name)
  cat("\tattribute in use: ",object@attribute)
  cat("\nAll attributes: ",object@attributes)
  cat("\nArray dimensions:\n")
  cat(paste(capture.output(print(data.frame(object@D))),collapse="\n"))
  cat("\n")
}

`print.scidb` = function(x, ...)
{
  cat("\nSciDB array ",x@name)
  if(nchar(x@attribute)>0)
    cat("\tattribute: ",x@attribute)
  cat("\n")
  print(head(x))
  if(is.null(x@dim)) j = x@length - 6
  else j = x@dim[1]-6
  if(j>2) cat("and ",j,"more rows not displayed...\n")
  if(length(x@dim)>0) {
    k = x@dim[2] - 6
    if(k>2) cat("and ",k,"more columns not displayed...\n")
  }
}

`ncol.scidb` = function(x) x@dim[2]
`nrow.scidb` = function(x) x@dim[1]
`dim.scidb` = function(x) {if(length(x@dim)>0) return(x@dim); NULL}
`length.scidb` = function(x) x@length

# Vector, matrix, or data.frame only.
as.scidb = function(X,
                    name=ifelse(exists(as.character(match.call()[2])),
                                as.character(match.call()[2]),
                                tmpnam("array")),
                    rowChunkSize=1000L,
                    colChunkSize=1000L,
                    start=c(0L,0L),
                    gc=FALSE, ...)
{
  if(inherits(X,"data.frame"))
    return(df2scidb(X,name=name,chunkSize=rowChunkSize,gc=gc,...))
  X0 = X
  D = dim(X)
  rowOverlap=0L
  colOverlap=0L
  if(length(start)<1) stop ("Invalid starting coordinates")
  if(length(start)>2) start = start[1:2]
  if(length(start)<2) start = c(start, 0)
  start = as.integer(start)
  type = .scidbtypes[[typeof(X)]]
  if(is.null(type)) {
    stop(paste("Unupported data type. The package presently supports: ",
       paste(.scidbtypes,collapse=" "),".",sep=""))
   }
  if(is.null(D)) {
# X is a vector, make into a matrix
    if(!is.vector(X)) stop ("X must be a matrix or a vector")
    X = as.matrix(X)
    schema = sprintf(
      "< val : %s >  [i=%.0f:%.0f,%.0f,%.0f]", type, start[[1]],
      nrow(X)-1+start[[1]], min(nrow(X),rowChunkSize), rowOverlap)
  } else {
    schema = sprintf(
      "< val : %s >  [i=%.0f:%.0f,%.0f,%.0f, j=%.0f:%.0f,%.0f,%.0f]", type, start[[1]],
      nrow(X)-1+start[[1]], min(nrow(X),rowChunkSize), rowOverlap, start[[2]], ncol(X)-1+start[[2]],
      min(ncol(X),colChunkSize), colOverlap)
  }
  if(!is.matrix(X)) stop ("X must be a matrix or a vector")
  schema1d = sprintf("<i:int64, j:int64, val : %s>[idx=0:*,10000,0]",type)

# Create the array, might error out here if array already exists
  query = sprintf("create_array(%s,%s)",name,schema)
  scidbquery(query,async=FALSE)

# Obtain a session from shim for the upload process
  u = url(paste(URI(),"/new_session",sep=""))
  session = readLines(u, warn=FALSE)[1]
  close(u)

# Upload the data
  f = .m2scidb(X, session,start)

# Load query
#  query = sprintf("input(%s,'%s', 0, '(int64,int64,%s)')",tmparray,f,type)
  query = sprintf("input(%s,'%s', 0, '(int64,int64,%s)')",schema1d,f,type)
  query = sprintf("redimension_store(%s, %s)",query, name)
  tryCatch( scidbquery(query, async=FALSE, release=1, session=session),
    error = function(e) {
      stop(e)
    })
  unlink(f)

  ans = scidb(name,gc=gc)

# Check for NIDs
#  dn = dimnames(X0)
#  if(is.null(dn)) dn = list(x=names(X0))
#  if(any(!unlist(lapply(dn,is.null))))
#    ans=addnids(ans, dn)

  ans
}


# Transpose array
t.scidb = function(x)
{
  tmp = tmpnam("array")
  query = paste("store(transpose(",x@name,"),",tmp,")",sep="")
  scidbquery(query)
  scidb(tmp,gc=TRUE)
}
