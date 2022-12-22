\name{dbOption}
\alias{dbOption}
\alias{dbOption,JDBCDriver-method}
\alias{dbOption,JDBCConnection-method}
\alias{dbOption,JDBCResult-method}
\title{
  Retrieves DMBS options.
}
\description{
  \code{dbOption} is the equivalent of \code{\link{getOption}}, but for
  options related JDBC.

  At each level (driver and connection), you can set options that will
  act as defaults for the DBI methods. Options are inherited, to setting
  an option in a call to \code{\link{JDBC}} will automatically propagate
  that option to the \code{JDBCConnection} object created by
  \code{\link{dbConnect}}. You can override an option by specifying it
  downstream, so, setting \code{dbOptions=} parameter in
  \code{\link{dbConnect}} will replace the options from the deriver with
  the same name, but will keep all others.
}
\usage{
dbOption(dbo, name, default)
}
\arguments{
  \item{dbo}{JDBC database object which the options relate to.}
  \item{name}{string, name of the option (must not be an empty string).}
  \item{default}{default value to return in case the option is not set.}
}
%\details{
%}
\value{
  Returns either the value of the option (if set) or the `default` value
  otherwise.
}
\author{
  Simon Urbanek
}
%\note{
%}
\seealso{
\code{\link{JDBC}}, \code{\link{dbConnect}}
}
%\examples{
%}
\keyword{interface}

