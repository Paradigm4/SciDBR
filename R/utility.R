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

# This file contains general utility routines not related to the
# scidb array class.

# An environment to hold connection state
.scidbenv = new.env()

# store the connection information and obtain a unique ID
scidbconnect = function(host='localhost', port=8080L)
{
# check connectivity XXX
  assign("host",host, envir=.scidbenv)
  assign("port",port, envir=.scidbenv)
# Use the query ID from a bogus query as a unique ID for automated
# array name generation.
  x = scidbquery(query="list()",release=1,resp=TRUE)
  id = gsub(".*\\r\\n","",x$response)
  assign("uid",id,envir=.scidbenv)
  invisible()
}

scidbdisconnect = function()
{
  rm("host", envir=.scidbenv)
  rm("port", envir=.scidbenv)
  gc()
}

make.names_ = function(x)
{
  gsub("\\.","_",make.names(x, unique=TRUE))
}

# Make a name from a prefix and a unique SciDB identifier.
tmpnam = function(prefix="_")
{
  salt = basename(tempfile(pattern=prefix))
  if(!exists("uid",envir=.scidbenv)) stop("Not connected...try scidbconnect")
  paste(salt,get("uid",envir=.scidbenv),sep="")
}


URI = function(q="")
{
  if(!exists("host",envir=.scidbenv)) stop("Not connected...try scidbconnect")
  ans  = paste("http://",get("host",envir=.scidbenv),":",get("port",envir=.scidbenv),sep="")
  if(nchar(q)>0) ans = paste(ans,q,sep="/")
  ans
}

# Send an HTTP GET message
GET = function(uri, async=TRUE)
{
  ans = invisible()
  if(!exists("host",envir=.scidbenv)) stop("Not connected...try scidbconnect")
  host = get("host",envir=.scidbenv)
  port = get("port",envir=.scidbenv)
  uri = URLencode(uri)
  uri = gsub("\\+","%2B",uri)
  msg = sprintf("GET %s HTTP/1.0\r\nHost: %s\r\nPragma: no-cache\r\nUser-Agent: R\r\n\r\n", uri, host)
  s = .SOCK_CONNECT(host,port)
  .SOCK_SEND(s, msg)
  if(!async) ans = .SOCK_RECV(s)
  .SOCK_CLOSE(s)
  ans
}

# HTTP POST synchronous upload data of size bytes
# uri: SciDB web API uri
# size: Number of bytes of data object
# transmit: A function f(fd) that writes the bytes to an open file
#           descriptor fd supplied by this function.
POST = function(uri, size, transmit)
{
  if(!exists("host",envir=.scidbenv)) stop("Not connected...try scidbconnect")
  host = get("host",envir=.scidbenv)
  port = get("port",envir=.scidbenv)
  boundary = paste("----------------------------",basename(tempfile(pattern="")),sep="")

  m2 = paste("\r\n",boundary,"\r\nContent-Disposition: form-data; name=\"file\"; filename=\"upload.rdata\"\r\nContent-Type: application/octet-stream\r\n\r\n",sep="")
  m3 = paste("\r\n",boundary,"--",sep="")
  l = size + nchar(m2) + nchar(m3)
  msg = sprintf("POST %s HTTP/1.0\r\nHost: %s\r\nAccept: */*\r\nContent-Length: %.0f\r\nExpect: 100-continue\r\nContent-Type: multipart/form-data; boundary=%s\r\n\r\n", uri, host, l, boundary)
  s = .SOCK_CONNECT(host,port)
  .SOCK_SEND(s, msg)
  .SOCK_SEND(s, m2)
  transmit(s)
  .SOCK_SEND(s, m3)
  ans = .SOCK_RECV(s)

  .SOCK_CLOSE(s)

  ans = rawToChar(ans)
  err = 200
  if(nchar(ans)>9)
    err = as.integer(strsplit(substr(ans,1,20)," ")[[1]][[2]])
  if(err>399)
    {
      ans = paste(strsplit(ans,"\r\n\r\n")[[1]][-1],collapse="\n")
      stop(ans)
    }
  ans = gsub(".*text/html\r\n\r\n","",ans)
  ans = gsub(".*text/plain\r\n\r\n","",ans)
  ans = gsub("\r\n","",ans)
  ans
}

# Check if array exists
.scidbexists = function(name)
{
  Q = scidblist()
  return(name %in% Q)
}

# scidblist: A convenience wrapper for list().
# Input:
# type (character), one of the indicated list types
# verbose (boolean), include attribute and dimension data when type="arrays"
# n: maximum lines of output to return
# Output:
# A list.
scidblist = function(pattern,
type= c("arrays","operators","functions","types","aggregates","instances","queries"),
              verbose=FALSE, n=Inf)
{
  type = match.arg(type)
  Q = iquery(paste("list('",type,"')",sep=""), return=TRUE, n=n)

  if(dim(Q)[1]==0) return(NULL)
  z=Q[,-1,drop=FALSE]
  if(type=="arrays" && !verbose) {
    z=z[,1]
    if(!missing(pattern))
      z = grep(z,pattern=pattern,value=TRUE)
  }
  if(type=="arrays" && verbose) z=z[,3]
  z
}
scidbls = function(...) scidblist(...)

# Basic low-level query. Returns query id.
# query: a character query string
# afl: TRUE indicates use AFL, FALSE AQL
# async: TRUE=Ignore return value and return immediately, FALSE=wait for return
# save: Save format query string or NULL. If async=FALSE, save is ignored.
# release: Set to zero preserve web session until manually calling release_session
# session: if you already have a SciDB http session, set this to it, otherwise NULL
# resp(logical): return http response
# Example values of save:
# save="&save='dcsv'"
# save="&save='lcsv+'"
#
# Returns the HTTP session in each case
scidbquery = function(query, afl=TRUE, async=FALSE, save=NULL, release=1, session=NULL, resp=FALSE)
{
  DEBUG = FALSE
  if(!is.null(options("scidb.debug")[[1]]) && TRUE==options("scidb.debug")[[1]]) DEBUG=TRUE
  if(is.null(session))
  {
# Obtain a session from shim
    u = url(paste(URI(),"/new_session",sep=""))
    session = tryCatch(readLines(u, warn=FALSE),
                error=function(e) NULL,
                warning=function(e) NULL)
    close(u)
  }
  if(is.null(save)) save=""
  if(length(session)<1) stop("SciDB http session error; are you connecting to a valid SciDB host?")
  bail = paste(URI(),"/release_session?id=",session,sep="")
  if(DEBUG)
  {
    cat(query, "\n")
    t1=proc.time()
  }
  if(async)
  {
    ans =tryCatch(
      GET(sprintf("/execute_query?id=%s&release=%d&query=%s",session[1],release,query),async=TRUE),
      error=function(e) {
        bail = url(bail)
        readLines(bail)
        close(bail)
        stop("HTTP/1.0 500 ERROR")
      })
  } else
  {
    u = paste("/execute_query?id=",session[1],"&release=",release,save,"&query=",query,"&afl=",as.integer(afl),sep="")
    ans = tryCatch(
      GET(u,async=FALSE),
      error=function(e) {
        bail = url(bail)
        readLines(bail)
        close(bail)
        "HTTP/1.0 500 ERROR"
      })
    w = rawToChar(ans)
    err = 200
    if(nchar(w)>9)
      err = as.integer(strsplit(substr(w,1,20)," ")[[1]][[2]])
    if(err>399)
    {
      w = paste(strsplit(w,"\r\n\r\n")[[1]][-1],collapse="\n")
      stop(w)
    }
  }
  if(DEBUG) print(proc.time()-t1)
  if(resp) return(list(session=session, response=rawToChar(ans)))
  session
}

# scidbremove: Convenience function to remove one or more scidb arrays
# Input:
# x (character): a vector or single character string listing array names
# error (function): error handler. Use stop or warn, for example.
# Output:
# null
scidbremove = function(x, error=warning)
{
  if(inherits(x,"scidb")) x = x@name
  if(!inherits(x,"character")) stop("Invalid argument. Perhaps you meant to quote the variable name(s)?")
  for(y in x) {
    tryCatch( scidbquery(paste("remove(",y,")",sep=""),async=FALSE, release=1),
              error=function(e) error(e))
  }
  invisible()
}
scidbrm = function(x,error=warning) scidbremove(x,error)

# df2scidb: User function to send a data frame to SciDB
# Returns a scidbdf object
df2scidb = function(X,
                    name=ifelse(exists(as.character(match.call()[2])),
                                 as.character(match.call()[2]),"X"),
                    dimlabel="row",
                    chunkSize,
                    rowOverlap=0L,
                    types=NULL,
                    nullable=FALSE,
                    gc)
{
  if(!is.data.frame(X)) stop("X must be a data frame")
  if(missing(gc)) gc=FALSE
  if(length(nullable)==1) nullable = rep(nullable, ncol(X))
  if(length(nullable)!=ncol(X)) stop ("nullable must be either of length 1 or ncol(X)")
  if(!is.null(types) && length(types)!=ncol(X)) stop("types must match the number of columns of X")
  n1 = nullable
  nullable = rep("",ncol(X))
  if(any(n1)) nullable[n1] = "NULL"
  anames = make.names(names(X),unique=TRUE)
  anames = gsub("\\.","_",anames)
  if(length(anames)!=ncol(X)) anames=make.names(1:ncol(X))
  if(!all(anames==names(X))) warning("Attribute names have been changed")
# Check for attribute/dimension name conflict
  old_dimlabel = dimlabel
  dimlabel = tail(make.names(c(anames,dimlabel),unique=TRUE),n=1)
  dimlabel = gsub("\\.","_",dimlabel)
  if(dimlabel!=old_dimlabel) warning("Dimension name has been changed")
  if(missing(chunkSize)) {
    chunkSize = min(nrow(X),10000)
  }
  m = ceiling(nrow(X) / chunkSize)

# Default type is string
  typ = rep(paste("string",nullable),ncol(X))
  args = "<"
  if(!is.null(types)) {
    for(j in 1:ncol(X)) typ[j]=paste(types[j],nullable[j])
  } else {
    for(j in 1:ncol(X)) {
      if(class(X[,j])=="numeric") 
        typ[j] = paste("double",nullable[j])
      else if(class(X[,j])=="integer") 
        typ[j] = paste("int32",nullable[j])
      else if(class(X[,j])=="logical") 
        typ[j] = paste("bool",nullable[j])
      else if(class(X[,j]) %in% c("character","factor")) 
        typ[j] = paste("string",nullable[j])
    }  
  }
  for(j in 1:ncol(X)) {
    args = paste(args,anames[j],":",typ[j],sep="")
    if(j<ncol(X)) args=paste(args,",",sep="")
    else args=paste(args,">")
  }

  SCHEMA = paste(args,"[",dimlabel,"=1:",sprintf("%.0f",nrow(X)),",",sprintf("%.0f",chunkSize),",", rowOverlap,"]",sep="")

# Obtain a session from the SciDB http service for the upload process
  u = url(paste(URI(),"/new_session",sep=""))
  session = readLines(u, warn=FALSE)[1]
  close(u)

# Create SciDB input string from the data frame
  scidbInput = .df2scidb(X,chunkSize)

# Post the input string to the SciDB http service
  tmp = tryCatch(
          POST(paste("/upload_file?id=",session,sep=""),
               nchar(scidbInput,type="bytes"),
               function(fd) scidb:::.SOCK_SEND(fd, scidbInput)),
          error=function(e)
           {
             GET(paste("/release_session?id=",session,sep=""),async=FALSE)
             stop(e)
           })

# Load query
  query = sprintf("store(input(%s, '%s'),%s)",SCHEMA, tmp, name)
  scidbquery(query, async=FALSE, release=1, session=session)
  scidb(name,`data.frame`=TRUE,gc=gc)
}


# Calling wrapper for matrix 2 scidb array function
# A: an R matrix object
# session: An active SciDB http session
.m2scidb = function (A,session,start=c(0L,0L))
{
# Compute the size of the upload data:
  n = length(A)
  u = switch(typeof(A),
        double = 8,
        integer = 4,
        character = 1,
        logical = 1)  
  l = n*u + 2*8*n;
  
# Define a transmit function for the POST routine
  f = function(fd)
   {
    .Call('m2scidb', A, as.integer(fd),as.integer(start), PACKAGE='scidb')
   }

# DEBUG
#  session=0
#
  if(length(session)<1) stop("SciDB http session error")
  tmp = tryCatch(POST(paste("/upload_file?id=",session,sep=""),l,f),
          error=function(e) 
           {
             GET(paste("/release_session?id=",session,sep=""),async=FALSE) 
             stop(e)
           })
# Note! session is not released here...
  tmp
}


iquery = function(query, `return`=FALSE,
                  afl=TRUE, iterative=FALSE,
                  n=1000, excludecol, ...)
{
  if(!afl && `return`) stop("return=TRUE may only be used with AFL statements")
  if(iterative && !`return`) stop("Iterative result requires return=TRUE")
  if(missing(excludecol)) excludecol=NA
  if(iterative)
  {
    sessionid = scidbquery(query,afl,async=FALSE,save="&save=lcsv+",release=0)
    return(iqiter(con=sessionid,n=n,excludecol=excludecol,...))
  }
  qsplit = strsplit(query,";")[[1]]
  m = 1
  if(n==Inf) n = -1    # Indicate to shim that we want all the lines of output
  for(query in qsplit)
  {
    if(`return` && m==length(qsplit))
    {
      ans = tryCatch(
       {
        sessionid = scidbquery(query,afl,async=FALSE,save="&save=lcsv+",release=0)
        host = get("host",envir=.scidbenv)
        port = get("port",envir=.scidbenv)
        u = sprintf("http://%s:%d/read_lines?id=%s&n=%.0f",host,port,sessionid,n+1)
        u=url(u)
        ret=read.table(u,sep=",",stringsAsFactors=FALSE,header=TRUE,...)
        GET(paste("/release_session?id=",sessionid,sep=""),async=FALSE)
        ret
       }, error = function(e)
           {
             stop(e)
           })
    } else ans = scidbquery(query,afl)
    m = m + 1
  }
  if(!(`return`)) return(invisible())
  ans
}

iqiter = function (con, n = 1, excludecol, ...)
{
  if(missing(excludecol)) excludecol=NA
  dostop = function(s=TRUE)
  {
    GET(paste("/release_session?id=",con,sep=""),async=FALSE)
    if(s) stop("StopIteration",call.=FALSE)
  }
  cls = function(u)
  {
    tryCatch(close(u), error=function(e) invisible())
  }
  if (!is.numeric(n) || length(n) != 1 || n < 1)
    stop("n must be a numeric value >= 1")
  init = TRUE
  header = c()
  nextEl = function() {
    if (is.null(con)) dostop()
    host = get("host",envir=.scidbenv)
    port = get("port",envir=.scidbenv)
    u = sprintf("http://%s:%d/read_lines?id=%s&n=%.0f",host,port,con,n)
    u = url(u)
    if(init) {
      ans = tryCatch(
       {
        read.table(u, sep=",",stringsAsFactors=FALSE,header=TRUE,nrows=n,...)
       }, error = function(e) {cls(u);dostop()},
          warning = function(w) {dostop()}
      )
      header <<- colnames(ans)
      init <<- FALSE
    } else {
      ans = tryCatch(
       {
        read.table(u, sep=",",stringsAsFactors=FALSE,header=FALSE,nrows=n,...)
       }, error = function(e) {cls(u);dostop()},
          warning = function(w) {dostop()}
      )
      colnames(ans) = header
    }
    cls(u)
    if(!is.na(excludecol) && excludecol<=ncol(ans))
    {
      rownames(ans) = ans[,excludecol]
      ans=ans[,-excludecol, drop=FALSE]
    }
    ans
  }
  it = list(nextElem = nextEl, gc=new.env())
  class(it) = c("abstractiter", "iter")
  it$gc$remove = TRUE
  reg.finalizer(it$gc, function(e) if(e$remove) 
                  tryCatch(dostop(FALSE),error=function(e) stop(e)),
                  onexit=TRUE)
  it
}


# Utility csv2scidb function
.df2scidb = function(X, chunksize, start=1)
{
  if(missing(chunksize)) chunksize = min(nrow(X),10000)
  scipen = options("scipen")
  options(scipen=20)
  buf = capture.output(
         write.table(X, file=stdout(), sep=",",
                     row.names=FALSE,col.names=FALSE,quote=FALSE)
        )
  options(scipen=scipen)
  x = sapply(1:length(buf), function(j)
    {
      chunk = floor(j/chunksize)
      if(j==length(buf))
      {
        if((j-1) %% chunksize == 0)
          tmp = sprintf("{%.0f}[\n(%s)];",start + chunk*chunksize, buf[j])
        else
          tmp = sprintf("(%s)];",buf[j])
      } else if((j-1) %% chunksize==0)
      {
        tmp = sprintf("{%.0f}[\n(%s),",start + chunk*chunksize, buf[j])
      } else if((j) %% chunksize == 0)
      {
        tmp = sprintf("(%s)];",buf[j])
      } else
      {
        tmp = sprintf("(%s),",buf[j])
      }
      tmp
    }
  )
  paste(x,collapse="")
}

# Return a SciDB schema of a scidb object x.
# Explicitly indicate attribute part of schema with remaining arguments
extract_schema = function(x, at=x@attributes, ty=x@types, nu=x@nullable)
{
  if(!(inherits(x,"scidb") || inherits(x,"scidbdf"))) stop("Not a scidb object")
  op = options(scipen=20)
  nullable = rep("",length(nu))
  if(any(nu)) nullable[nu] = " NULL"
  attr = paste(at,ty,sep=":")
  attr = paste(attr, nullable,sep="")
  attr = sprintf("<%s>",paste(attr,collapse=","))
  dims = sprintf("[%s]",paste(paste(paste(paste(paste(paste(x@D$name,"=",sep=""),x@D$start,sep=""),x@D$start+x@D$length-1,sep=":"),x@D$chunk_interval,sep=","),x@D$chunk_overlap,sep=","),collapse=","))
  options(op)
  paste(attr,dims,sep="")
}
