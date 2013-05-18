\name{scidbremove}
\alias{scidbremove}
\alias{scidbrm}
\title{
Remove an array.
}
\description{
Remove (delete) an array from SciDB.
}
\usage{
scidbremove(x, error = warning)
}
\arguments{
  \item{x}{
The name of the SciDB array to remove (character). This
may also be a vector of names to remove.
}
  \item{error}{Error handling function.}
}
\value{
NULL is invisibly returned. SciDB errors are returned as
normal R errors and may be handled accordingly.
}
\note{
Supply a user-defined error handling function to avoid
stopping on error (for example trying to delete a non-existing
array). For example, use \code{error=warning} to covert errors
to warnings but still report them.
}
\author{
B. W. Lewis<blewis@paradigm4.com>
}

\examples{
\dontrun{
df2scidb(cars)
df2cidb(iris)
scidbremove(c("iris","cars","undefined"), error=warning)

# Remove everything (caution!)
scidbremove(scidblist())

}}
