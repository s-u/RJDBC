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
  connection using the specified driver. From RJDBC 0.2-9 on the driver
  takes precedence over DriverManager, because DriverManager is static
  and is not capable of finding drivers dynamically. DriverManager is
  now only used if the driver is a NULL-driver, i.e., \code{JDBC(NULL)}.

  There are only two positional properties \code{user=''} and
  \code{password=''} neither of which will be set if empty. All other
  arguments are treated as additional properties passed to the
  connection (except when DriverManager is used).

  Additional arguments to \code{dbConnect()} properties are set

  \code{dbListConnections} always return \code{NULL} with a warning,
  because JDBC connections are not tracked.

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
