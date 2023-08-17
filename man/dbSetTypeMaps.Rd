\name{dbSetTypeMaps}
\alias{dbSetTypeMaps}
\title{
  Manage type mapping functions.
}
\description{
  Type mapping functions are functions that perform conversions between
  types as they are received from JDBC to R objects. For any JDBC type
  it is possible to define a function which is called before returning
  results from \code{\link[DBI]{fetch}()}.

  \code{dbSetTypeMaps()} function sets, removes or lists the mapping
  functions.
}
\usage{
dbSetTypeMaps(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
    one or more arguments of the form \code{TYPE=function(x) ...} (set a
    new conversion function) or \code{TYPE=NULL} (remove conversion). If
    empty, no changes are made.
  }
}
\details{
  The type names correspond to the column type names as defined by JDBC
  (see \code{java.sql.Types} Java documentation). For example, if you
  want to convert \code{NUMERIC} columns to numeric values in R
  (regardless of precision)
}
\value{
  Named list of currently registered type mapping functions. If
  \code{\dots} is empty then visibly, otherwise invisibly.
}
%\author{
%}
%\note{
%}
%\seealso{
%}
\examples{
str(dbSetTypeMaps())

## use bit64::as.integer64 (if available) to convert BIGINT to integer64
if (is.environment(bit64 <- tryCatch(getNamespace("bit64"), error=function(e) FALSE)))
    dbSetTypeMaps(BIGINT=bit64$as.integer64)
}
\keyword{interface}
