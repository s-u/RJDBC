\name{JDBCResult-class}
\docType{class}
\alias{JDBCResult-class}

\title{Class "JDBCResult"}
\description{Representation of a DBI result set returned from a JDBC connection.}
\section{Generators}{
  Objects can be created by call to \code{\link[DBI]{dbSendQuery}}.
}
\section{Slots}{
  \describe{
    \item{\code{jr}:}{Java reference to the JDBC result set}
    \item{\code{md}:}{Java reference to the JDBC result set meta data}
    \item{\code{env}:}{Environment holding cached objects (currently the
      result helper object used by \code{fetch()})}
    \item{\code{stat}:}{Java reference to the JDBC statement which
      generated this result}
    \item{\code{conn}:}{Connection which created this result set}
  }
}
\section{Extends}{
Class \code{"\link[DBI]{DBIResult-class}"}, directly.
Class \code{"\link[DBI]{DBIObject-class}"}, by class "DBIResult", distance 2.
}
\section{Methods}{
No methods defined with class "JDBCResult" in the signature.
}
\seealso{
  \code{\link{JDBC}}, \code{\link[DBI]{dbSendQuery}}
}
%\examples{
%}
\keyword{classes}
