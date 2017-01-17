# Functions for parsing and building SciDB schema strings.
# Earlier versions of SciDB had a schema that delimited some parameters by commas:
# <attribute_1:type_1 NULL DEFAULT VALUE, attribute_2:type_2, ...>
# [dimension_1=start:end,chunksize,overlap, dimension_2=start:end,chunksize,overlap, ...]
# As of version 16.9, SciDB schema changed to use colons to delimit dimensions.  And since
# the chunksize became optional, it became the last item.  The string looks like:
# <attribute_1:type_1 NULL DEFAULT VALUE, attribute_2:type_2, ...>
# [dimension_1=start:end:overlap:chunksize; dimension_2=start:end:overlap:chunksize; ...]


.dimsplitter = function(x)
{
  # Produces a list of character arrays containing:
  # variable name, start, end, overlap and chunksize in that order regardless of version.
  if(is.character(x)) s = x
  else
  {
    if(!(inherits(x, "scidb"))) return(NULL)
    s = schema(x)
  }

  d = strsplit(s, "\\[")[[1]][[2]]	# Strip off the attributes
  d = strsplit(d, "\\]")[[1]][[1]]	# Remove trailing right bracket and aliases
  d = gsub(" ", "", d)      # Remove whitespace
  
  # Items between parens contain colons that need to remain as fully formed paramters
  p = unlist(regmatches(d, gregexpr("\\(.*?\\)", d))) # Pull items from parentheses
  p1 = gsub(':', '^^^^^', p)                          # Replace the colons with something that will never match
  i = 1
  for (item in p){
    d = gsub(item, p1[i], d, fixed=TRUE)              # Put the not matching params temporarily in the string
    i = i + 1
  }

  d = strsplit(d,',|:|=|;')                           # Split the schema string on all the delmiters from all versions
  d = lapply(d, function(x) {gsub('^^^^^', ':', x, fixed=TRUE)})  # Replace the colons
  d = matrix(d[[1]], nrow=length(d[[1]])/5, ncol=5, byrow=TRUE)   # Unflatten the demensions so they are each on a row
  d = apply(d, 1, function(x) list(x))                # Create a list of array vectors
  if (as.numeric(options("scidb.version")[[1]]) <= 15.12) { # Switch overlap and chunksize positions if before 15.12
    # d = lapply(d, function(x) {y = x[[1]][4]; x[[1]][4] = x[[1]][5]; x[[1]][5] = y})
    d = lapply(d, function(x) {y = x[[1]][4]; x[[1]][4] = x[[1]][5]; x[[1]][5] = y; x})
  }  
  d = lapply(d, function(x) unlist(x))
  d
}

.dimsplit = function(x)
{
  d = .dimsplitter(x)
  n = unlist(lapply(d, function(x) x[1]))	# Pull out the variables as names
  if (as.numeric(options("scidb.version")[[1]]) > 15.12) { # The parameter delimiters changes in 16.9
    separator = ":"
  } else {
    separator = ","
  }
  d = unlist(lapply(d, function(x) {gsub(" ", "", paste(x[1], "=", x[2], ":", x[3], separator, x[4], separator, x[5]))}))
  names(d)<-n	       # Apply names
  d
}

.attsplitter = function(x)
{
  if(is.character(x)) s = x
  else
  {
    if(!(inherits(x, "scidb"))) return(NULL)
    s = schema(x)
  }
  strsplit(strsplit(strsplit(strsplit(s, ">")[[1]][1], "<")[[1]][2], ",")[[1]], ":")
}

.scidb_names = function(x)
{
  c(scidb_attributes(x), dimensions(x))
}

#' SciDB logical plan
#' @param x a \code{\link{scidb}} array object
#' @return a character value containing the SciDB logical plan for the object
#' @export
logical_plan = function(x)
{
  x@meta$logical_plan
}

#' SciDB array attribute names
#' @param x a \code{\link{scidb}} array object
#' @return character vector of SciDB attribute names.
#' @export
scidb_attributes = function(x)
{
  a = .attsplitter(x)
  unlist(lapply(a, function(x) x[[1]]))
}

#' SciDB array attribute types
#' @param x a \code{\link{scidb}} array object
#' @return character vector of SciDB attribute types
#' @export
scidb_types = function(x)
{
  a = .attsplitter(x)
  unlist(lapply(a, function(x) strsplit(x[2]," ")[[1]][1]))
}

#' SciDB array attribute nullability
#' @param x a \code{\link{scidb}} array object
#' @return logical vector of SciDB attribute nullability
#' @export
scidb_nullable = function(x)
{
  # SciDB schema syntax changed in 15.12
  if(compare_versions(options("scidb.version")[[1]],15.12))
  { 
    return (! grepl("NOT NULL", .attsplitter(x)))
  }
  grepl(" NULL", .attsplitter(x))
}

#' SciDB array dimension names
#' @param x a \code{\link{scidb}} array object
#' @return character vector of SciDB dimension names
#' @export
dimensions = function(x)
{
  d = .dimsplit(x)
  n = names(d)
  n
}

#' SciDB array coordinate bounds
#' @param x a \code{\link{scidb}} array object
#' @return list of character-valued vectors of starting and ending coordinate bounds
#' @export
scidb_coordinate_bounds = function(x)
{
  d = .dimsplitter(x)
  start = unlist(lapply(d, function(x) x[2]))
  end = unlist(lapply(d, function(x) x[3]))
  start = gsub("NA","0",start)
  end = gsub("NA","\\*",end)
  s1 = gsub("\\*",.scidb_DIM_MAX,start)
  s2 = gsub("\\*",.scidb_DIM_MAX,end)
  len = as.numeric(s2) - as.numeric(s1) + 1
  i = len >= as.double(.scidb_DIM_MAX)
  len = noE(len) # in particular, len is now character
  if(any(i))
  {
    len[i] = "Inf"
  }
  list(start=noE(start), end=noE(end), length=len)
}


# A between-style string of coordinate bounds
between_coordinate_bounds = function(s)
{
  if((inherits(s,"scidb"))) s = schema(s)
  paste(t(matrix(unlist(lapply(strsplit(gsub("\\].*","",gsub(".*\\[","",s,perl=TRUE),perl=TRUE),"=")[[1]][-1],function(x)strsplit(strsplit(x,",")[[1]][1],":")[[1]])),2,byrow=FALSE)),collapse=",")
}

#' SciDB array coordinate start
#' @param x a \code{\link{scidb}} array object
#' @return character-valued vector of starting coordinate bounds
#' @export
scidb_coordinate_start = function(x)
{
  scidb_coordinate_bounds(x)$start
}

#' SciDB array coordinate end
#' @param x a \code{\link{scidb}} array object
#' @return character-valued vector of end coordinate bounds
#' @export
scidb_coordinate_end = function(x)
{
  scidb_coordinate_bounds(x)$end
}

#' SciDB array coordinate chunksize
#' @param x a \code{\link{scidb}} array object
#' @return character-valued vector of SciDB coordinate chunk sizes
#' @export
scidb_coordinate_chunksize = function(x)
{
  d = .dimsplitter(x)
  unlist(lapply(d ,function(x) x[5]))
}

#' SciDB array coordinate overlap
#' @param x a \code{\link{scidb}} array object
#' @return character-valued vector of SciDB coordinate overlap
#' @export
scidb_coordinate_overlap = function(x)
{
  d = .dimsplitter(x)
  unlist(lapply(d ,function(x) x[4]))
}

#' SciDB array schema
#' @param x a \code{\link{scidb}} array object
#' @return character-valued SciDB array schema
#' @export
schema = function(x)
{
  if(!(inherits(x, "scidb"))) return(NULL)
  gsub(".*<", "<", x@meta$schema)
}

#' SciDB array from a schema string
#' @param s schema character string
#' @param expr a character SciDB array name or array expression
#' @note This function creates a virtual SciDB array object from a schema string and optional expression.
#' It does not contact the database nor does it create the array explicitly in SciDB. The function only
#' creates an R \code{\link{scidb}} array object with the indicated schema and expression.
#' @return \code{\link{scidb}} reference object
#' @export
scidb_from_schemastring = function(s, expr=character())
{
  attributes = scidb_attributes(s)
  dimensions = dimensions(s)

  obj = new("scidb",
              name=expr,
              meta=new.env(),
              gc=new.env())
  obj@meta$schema = gsub("^.*<","<",s,perl=TRUE)
  obj@meta$logical_plan = ""
  obj
}

# Build the attribute part of a SciDB array schema from a scidb, scidb object.
# Set prefix to add a character prefix to all attribute names.
# I: optional vector of dimension indices to use, if missing use all
# newnames: optional vector of new dimension names, must be the same length
#    as I.
# nullable: optional vector of new nullability expressed as FALSE or TRUE,
#    must be the same length as I.
# newtypes: optional vector of new types, must be the same length as I.
build_attr_schema = function(A, prefix="", I, newnames, nullable, newtypes)
{
  if(missing(I) || length(I)==0) I = rep(TRUE,length(scidb_attributes(A)))
  if(is.character(A)) A = scidb_from_schemastring(A)
  if(!(class(A) %in% c("scidb"))) stop("Invalid SciDB object")
  if(is.character(I)) I = which(scidb_attributes(A) %in% I)
  if(is.logical(I)) I = which(I)
  N = rep("", length(scidb_nullable(A)[I]))
  N[scidb_nullable(A)[I]] = " NULL"
  if(!missing(nullable))
  {
    N = rep("", length(I))
    N[nullable] = " NULL"
  }
  if(!missing(newtypes)) N = paste(newtypes,N,sep="")
  else N = paste(scidb_types(A)[I],N,sep="")
  attributes = paste(prefix,scidb_attributes(A)[I],sep="")
  if(!missing(newnames)) attributes = newnames
  S = paste(paste(attributes,N,sep=":"),collapse=",")
  sprintf("<%s>",S)
}

# Build the dimension part of a SciDB array schema from a scidb object
# A: A scidb object
# bracket: if TRUE, enclose dimension expression in square brackets
# I: optional vector of dimension indices to use, if missing use all
# newnames, newstart, newend, newchunk, newoverlap, newlen:
# All optional. Must be same length as I. At most one of newlen,newend
# may be specified.
build_dim_schema = function(A, bracket=TRUE, I,
                            newnames, newlen, newstart,
                            newend, newchunk, newoverlap)
{
  if(is.character(A)) A = scidb_from_schemastring(A)
  if(!(class(A) %in% c("scidb"))) stop("Invalid SciDB object")
  dims = dimensions(A)
  bounds = scidb_coordinate_bounds(A)
  start =  bounds$start
  end = bounds$end
  chunksize = scidb_coordinate_chunksize(A)
  overlap = scidb_coordinate_overlap(A)

  if(!missing(I) && length(I)>0)
  {
    dims      = dims[I]
    start     = start[I]
    end       = end[I]
    chunksize = chunksize[I]
    overlap   = overlap[I]
  }
  if(!missing(newnames))
  {
    dims = newnames
  }
  if(!missing(newstart))
  {
    start = noE(newstart)
  }
  if(!missing(newend))
  {
    if(!missing(newlen)) stop("At most one of newend, newlen may be specified")
    end = newend
  }
  if(!missing(newchunk))
  {
    chunksize = noE(newchunk)
  }
  if(!missing(newoverlap))
  {
    overlap = noE(newoverlap)
  }
  if(!missing(newlen))
  {
    star = grep("\\*",newlen)
    len = gsub("\\*",.scidb_DIM_MAX, newlen)
    end = as.numeric(start) + as.numeric(len) - 1
    i = end >= as.double(.scidb_DIM_MAX)
    end = noE(end)
    if(any(i))
    {
      end[i] = "*"
    }
    if(length(star)>0)
    {
      end[star] = "*"
    }
  }
  R = paste(start,end, sep=":")
  S = paste(dims, R, sep="=")
  S = paste(S, chunksize, sep=",")
  S = paste(S, overlap, sep=",")
  S = paste(S, collapse=",")
  if(bracket) S = sprintf("[%s]",S)
  S
}

# A utility function for operations that require a single attribute
# Throws error if a multi-attribute array is specified.
.get_attribute = function(x)
{
  a = scidb_attributes(x)
  if(length(a) > 1) stop("This function requires a single-attribute array. Consider using project.")
  a
}


# An internal function that compares schema of two scidb objects
# or schema strings.
compare_schema = function(s1, s2,
  s1_attribute_index,
  s1_dimension_index,
  s2_attribute_index,
  s2_dimension_index,
  ignore_dimnames=FALSE,
  ignore_start=FALSE,
  ignore_end=FALSE,
  ignore_chunksize=FALSE,
  ignore_overlap=FALSE,
  ignore_attributes=FALSE,
  ignore_types=FALSE,
  ignore_nullable=FALSE)
{
  if(is.character(s1)) s1 = scidb_from_schemastring(s1)
  if(is.character(s2)) s2 = scidb_from_schemastring(s2)
  if(!(class(s1) %in% c("scidb"))) stop("Invalid SciDB object")
  if(!(class(s2) %in% c("scidb"))) stop("Invalid SciDB object")
  if(missing(s1_attribute_index)) s1_attribute_index=1:length(scidb_attributes(s1))
  if(missing(s2_attribute_index)) s2_attribute_index=1:length(scidb_attributes(s2))
  if(missing(s1_dimension_index)) s1_dimension_index=1:length(dimensions(s1))
  if(missing(s2_dimension_index)) s2_dimension_index=1:length(dimensions(s2))

  dimnames = ignore_dimnames || isTRUE(all.equal(dimensions(s1)[s1_dimension_index],dimensions(s2)[s2_dimension_index]))
  bound_start = ignore_start || isTRUE(all.equal(scidb_coordinate_start(s1)[s1_dimension_index],scidb_coordinate_start(s2)[s2_dimension_index]))
  bound_end = ignore_end || isTRUE(all.equal(scidb_coordinate_end(s1)[s1_dimension_index],scidb_coordinate_end(s2)[s2_dimension_index]))
  chunks = ignore_chunksize || isTRUE(all.equal(scidb_coordinate_chunksize(s1)[s1_dimension_index],scidb_coordinate_chunksize(s2)[s2_dimension_index]))
  overlap = ignore_overlap || isTRUE(all.equal(scidb_coordinate_overlap(s1)[s1_dimension_index],scidb_coordinate_overlap(s2)[s2_dimension_index]))
  attributes = ignore_attributes || isTRUE(all.equal(scidb_attributes(s1)[s1_attribute_index],scidb_attributes(s2)[s1_attribute_index]))
  types = ignore_types || isTRUE(all.equal(scidb_types(s1)[s1_attribute_index],scidb_types(s2)[s2_attribute_index]))
  nullable = ignore_nullable || isTRUE(all.equal(scidb_nullable(s1)[s1_attribute_index],scidb_nullable(s2)[s2_attribute_index]))

  ans = dimnames && bound_start && bound_end && chunks && overlap && attributes && types && nullable

# add a report if FALSE
  if(!ans)
  {
    attr(ans,"equal") = list(dimnames=dimnames,
                   bound_start=bound_start,
                   bound_end=bound_end,
                   chunks=chunks,
                   overlap=overlap,
                   attributes=attributes,
                   types=types,
                   nullable=nullable)
  }
  ans
}

# Internal function compute the difference of two strings, vectorized
strdiff = function(x,y)
{
  if(length(x)==1 && length(y)>1) x = rep(x,length(y))
  if(length(x)!=length(y)) stop("mismatched vector lengths")
  unlist(lapply(seq(1,length(x)), function(i)
         {
           gsub(sprintf("^%s",x[[i]]), "", y[[i]])
         }))
}

# Internal function used to infer aliases in use by comparing the output of
# show and explain_logical. Returns NULL if no aliasing can be determined.
aliases = function(x)
{
  ans = c()
  if(!(inherits(x,"scidb"))) return(ans)
  logical_schema = grep("^>>schema", strsplit(logical_plan(x), "\n")[[1]], value=TRUE)
  if(length(logical_schema) < 1) return(NULL)
  logical_schema = gsub("].*", "]", logical_schema)
  logical_schema = gsub(".*<", "<", logical_schema)
  dl = dimensions(logical_schema)
  ds = dimensions(schema(x))
  if(length(dl) != length(ds)) return(NULL)
  for(j in 1:length(ds))
  {
    d = strsplit(dl[j], "\\|")[[1]]
    p = strdiff(d[j], ds[j])
    d = c(d[1],unlist(lapply(d[-1], function(z) sprintf("%s%s", p, z))))
    ans = c(ans, gsub(" ", "",strdiff(ds[j], d)))
  } 
  unique(ans)
}

# An internal function used to create new alias names that don't conflict
# with existing aliases for scidb objects x and y. Returns a two element
# character vector with the new aliases.
scidb_alias = function(x,y)
{
  make.unique_(c(aliases(x),aliases(y)), c("x","y"))
}
