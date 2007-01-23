\name{JDBCDriver-methods}
\docType{methods}
\alias{JDBCDriver-methods}
\alias{dbListConnections,JDBCDriver-method}
\alias{dbGetInfo,JDBCDriver-method}
\alias{dbUnloadDriver,JDBCDriver-method}
\alias{dbConnect,JDBCDriver-method}
\title{Methods for the class JDBCDriver in Package `RJDBC'}
\description{
  Methods for the class `JDBCDriver' in Package `RJDBC'.

  Most prominent method is \code{dbConnect}, it creates a new JDBC
  connection using the specified driver. Due to the fact that the actual
  JDBC driver is selected in the URL, the \code{JDBCDriver} object
  itself has little significance and is not used to determine the
  connection type.

  \code{dbListConnections} always return \code{NULL} with a warning,
  because JDBC connecteions are not tracked.

  \code{dbGetInfo} returns very basic information, because the JDBC
  driver is not loaded until a connection is created.

  \code{dbUnloadDriver} is a no-op in the current implementation,
  because drivers are never removed from the JVM.
}
\section{Methods}{
  \describe{
    \item{dbConnect}{\code{signature(drv = "JDBCDriver", ...)}}
    \item{dbListConnections}{\code{signature(drv = "JDBCDriver", ...)}}
    \item{dbGetInfo}{\code{signature(drv = "JDBCDriver", ...)}}
    \item{dbUnloadDriver}{\code{signature(drv = "JDBCDriver", ...)}} 
}}
\keyword{methods}

