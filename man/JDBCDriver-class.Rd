\name{JDBCDriver-class}
\docType{class}
\alias{JDBCDriver-class}

\title{Class "JDBCDriver"}
\description{A DBI driver that uses any JDBC driver to access databases.}
\section{Generators}{
Objects can be created by calls to \code{\link{JDBC}} or \code{\link[DBI]{dbDriver}}.
}
\section{Slots}{
  \describe{
    \item{\code{identifier.quote}:}{Quote character to use for
      identifiers in automatically generated SQL statements or \code{NA}
      if quoted identifiers are not supported by the back-end.}
    \item{\code{jdrv}:}{Java object reference to an instance of the
      driver if the driver can be instantiated by a default
      constructor. This object is only used as a fall-back when the
      driver manager fails to find a driver.}
  }
}
\section{Extends}{
Class \code{"\link[DBI]{DBIDriver-class}"}, directly.
Class \code{"\link[DBI]{DBIObject-class}"}, by class "DBIDriver", distance 2.
}
\section{Methods}{
No methods defined with class "JDBCDriver" in the signature.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }

\seealso{
  \code{\link{JDBC}}
}
%\examples{
%}
\keyword{classes}
