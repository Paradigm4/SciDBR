check = function(a, b)
{
  print(match.call())
  stopifnot(all.equal(a, b, check.attributes=FALSE, check.names=FALSE))
}

library("scidb")
host = Sys.getenv("SCIDB_TEST_HOST")
if (nchar(host) > 0)
{
  db = scidbconnect(host)

# 1 Data movement tests

# upload data frame
  x = as.scidb(db, iris)
  a =  schema(x, "attributes")$name
# binary download
  check(iris[, 1:4], as.R(x)[, a][, 1:4])
# iquery binary download
  check(iris[, 1:4], iquery(db, x, return=TRUE)[, a][, 1:4])
# iquery CSV download
  check(iris[, 1:4], iquery(db, x, return=TRUE, binary=FALSE)[, a][, 1:4])
# as.R only attributes
  check(iris[, 1],  as.R(x, only_attributes=TRUE)[, 1])

# only attributes and optional skipping of metadata query by supplying schema in full and abbreviated forms
  check(nrow(iris), nrow(as.R(x)))
  check(nrow(iris), nrow(as.R(x, only_attributes=TRUE)))
  a = scidb(db, x@name, schema=schema(x))
  check(nrow(iris), nrow(as.R(a)))
  a = scidb(db, x@name, schema=gsub("\\[.*", "", schema(x)))
  check(nrow(iris), nrow(as.R(a)))
  
# upload vector
  check(1:5, as.R(as.scidb(db, 1:5))[, 2])
# upload matrix
  x = matrix(rnorm(100), 10)
  check(x, matrix(as.R(as.scidb(db, x))[, 3], 10, byrow=TRUE))
# upload csparse matrix
# also check shorthand projection syntax
  x = Matrix::sparseMatrix(i=sample(10, 10), j=sample(10, 10), x=runif(10))
  y = as.R(as.scidb(db, x))
  check(x, Matrix::sparseMatrix(i=y$i + 1, j=y$j + 1, x=y$val))
# issue #126
  df = as.data.frame(matrix(runif(10*100), 10, 100))
  sdf = as.scidb(db, df)
  check(df, as.R(sdf, only_attributes=TRUE))
# issue #130
  df = data.frame(x1 = c("NA", NA), x2 = c(0.13, NA), x3 = c(TRUE, NA), stringsAsFactors=FALSE)
  x = as.scidb(db, df)
  try(check(df, as.R(x, only_attributes=TRUE)))

# upload n-d array
# XXX WRITE ME, not implemented yet

# garbage collection
  gc()


# 2 AFL tests

# Issue #128
 i = 4
 j = 6
 x = db$build("<v:double>[i=1:2,2,0, j=1:3,1,0]", i * j)
 check(as.R(x)$v, c(1, 2, 2, 4, 3, 6))
 x = db$apply(x, w, R(i) * R(j))
 # Need as.integer() for integer64 coversion below
 check(as.integer(as.R(x)$w), rep(24, 6))


# 3 Miscellaneous tests

# issue #156 type checks

# int64 option
 db = scidbconnect(host, int64=TRUE)
 x = db$build("<v:int64>[i=1:2,2,0]", i)
 check(as.R(x, only_attributes = TRUE), as.R(as.scidb(db, as.R(x, TRUE)), only_attributes = TRUE))
 db = scidbconnect(host, int64=FALSE)
 x = db$build("<v:int64>[i=1:2,2,0]", i)
 check(as.R(x, only_attributes = TRUE), as.R(as.scidb(db, as.R(x, TRUE)), only_attributes = TRUE))

# Issue #157
 x = as.R(scidb(db, "build(<v:float>[i=1:5], sin(i))"), binary = FALSE)

# Issue #163
 x = as.scidb(db, serialize(1:5, NULL))
 y = as.R(x)
 check(y$val[[1]], serialize(1:5,NULL))

 iquery(db, "build(<val:binary>[i=1:2,10,0], null)", return=TRUE)

# Test for issue #161
  iquery(db, "op_count(list())", return=TRUE, only_attributes=TRUE,  binary=FALSE)

# Test for issue #158
  x = iquery(db, "join(op_count(build(<val:int32>[i=0:234,100,0],random())),op_count(build(<val:int32>[i=0:1234,100,0],random())))", 
        schema = "<apples:uint64, oranges:uint64>[i=0:1,1,0]", return=TRUE)
  check(names(x), c("i", "apples", "oranges"))

# issue #160 deal with partial schema string
  x = iquery(db, "project(list(), name)", schema="<name:string>[No]", return=TRUE)
  check(names(x), c("No", "name"))
  iquery(db, "build(<val:double>[i=1:3;j=1:3], random())", return=T, schema="<val:double>[i; j]")
  iquery(db, "build(<val:double>[i=1:3;j=1:3], random())", return=T, schema="<val:double>[i=1:3:0:3;j=1:3:0:3]")
  iquery(db, "build(<val:double>[i=1:3;j=1:3], random())", return=T, schema="<val:double>[i=1:3,1,0,j=1:3,1,0]")
  iquery(db, "build(<val:double>[i=1:3;j=1:3], random())", return=T, schema="<val:double>[i=1:3,1,0;j=1:3,1,0]")
  iquery(db, "build(<val:double>[i=1:3;j=1:3], random())", return=T, schema="<val:double>[i=1:3;j=1:3]")
  iquery(db, "build(<val:double>[i=1:3;j=1:3], random())", return=T, schema="<val:double>[i,j]")

# basic types from scalars
  lapply(list(TRUE, "x", 420L, pi), function(x) check(x, as.R(as.scidb(db, x))$val))
# trickier types
  x = Sys.Date()
  check(as.POSIXct(x, tz="UTC"), as.R(as.scidb(db, x))$val)
  x = iris$Species
  check(as.character(x), as.R(as.scidb(db, x))$val)
# type conversion from data frames
  x = data.frame(a=420L, b=pi, c=TRUE, d=factor("yellow"), e="SciDB", f=as.POSIXct(Sys.Date(), tz="UTC"), stringsAsFactors=FALSE)

# issue #164 improper default value parsing
  tryCatch(iquery (db, "remove(x)"), error=invisible)
  iquery(db, "create array x <x:double not null default 1>[i=1:10]")
  as.R(scidb(db, "x"))
  tryCatch(iquery (db, "remove(x)"), error=invisible)

# issue #158 support empty dimension spec []
  iquery(db, "apply(build(<val:double>[i=1:3], random()), x, 'abc')", return=TRUE,
         schema="<val:double,  x:string>[]", only_attributes=TRUE)

# issue #172 (uint16 not supported)
  iquery(db, "list('instances')", return=TRUE, binary=TRUE)

# Test for references and garbage collection in AFL statements
  x = store(db, db$build("<x:double>[i=1:1,1,0]", R(pi)))
  y = db$apply(x, "y", 2)
  rm(x)
  gc()
  as.R(y)
  rm(y)
}
