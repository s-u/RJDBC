\name{JDBCConnection-class}
\docType{class}
\alias{JDBCConnection-class}

\title{Class "JDBCConnection"}
\description{Class representing a (DBI) database connection which uses
  JDBC to connect to a database.}
\section{Generators}{
  Objects can be created by call to \code{\link[DBI]{dbConnect}} of a
  \code{\link{JDBC}} driver.
}
\section{Slots}{
	 \describe{
	   \item{\code{jc}:}{Java object representing the connection.}
	   \item{\code{identifier.quote}:}{Quote character to use for
	     quoting identifiers it automatically generated SQL
	     statements or \code{NA} if the back-end doesn't support
	     quoted identifiers. Usually the value is inherited from the
	     \code{"\linkS4class{JDBCDriver}"}.}
	 }
       }
       
\section{Extends}{
  Class \code{"\link[DBI]{DBIConnection-class}"}, directly.
  Class \code{"\link[DBI]{DBIObject-class}"}, by class "DBIConnection", distance 2.
}
\section{Methods}{
  No methods defined with class "JDBCConnection" in the signature.
}
%\author{ ~~who you are~~ }
\seealso{
  \code{\link{JDBC}}, \code{"\linkS4class{JDBCDriver}"}
}
%\examples{
%}
\keyword{classes}
