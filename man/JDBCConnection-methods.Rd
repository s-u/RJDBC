\name{JDBCConnection-methods}
\docType{methods}
\alias{JDBCConnection-methods}
\alias{dbBegin,JDBCConnection-method}
\alias{dbCommit,JDBCConnection-method}
\alias{dbDataType,JDBCConnection-method}
\alias{dbDisconnect,JDBCConnection-method}
\alias{dbExistsTable,JDBCConnection-method}
\alias{dbExistsTable,JDBCConnection,ANY-method}
\alias{dbGetException,JDBCConnection-method}
\alias{dbGetInfo,JDBCConnection-method}
\alias{dbGetQuery,JDBCConnection,character-method}
\alias{dbListFields,JDBCConnection-method}
\alias{dbListFields,JDBCConnection,ANY-method}
\alias{dbListResults,JDBCConnection-method}
\alias{dbListTables,JDBCConnection-method}
\alias{dbReadTable,JDBCConnection-method}
\alias{dbReadTable,JDBCConnection,ANY-method}
\alias{dbReadTable,JDBCConnection,character-method}
\alias{dbRemoveTable,JDBCConnection-method}
\alias{dbRemoveTable,JDBCConnection,ANY-method}
\alias{dbRollback,JDBCConnection-method}
\alias{dbSendQuery,JDBCConnection,character-method}
\alias{dbSendUpdate}
\alias{dbSendUpdate,JDBCConnection,character-method}
\alias{dbGetTables}
\alias{dbGetTables,JDBCConnection-method}
\alias{dbGetFields}
\alias{dbGetFields,JDBCConnection-method}
\alias{dbWriteTable,JDBCConnection-method}
\alias{dbWriteTable,JDBCConnection,ANY-method}
\title{Methods for the class `JDBCConnect' in Package `RJDBC'}
\description{
  Methods for the class `JDBCConnection' in Package `RJDBC'.

  \code{dbSendQuery} and \code{dbSendUpdate} submit a SQL query to the
  database. The difference between the two is only that
  \code{dbSendUpdate} is used with DBML queries and thus doesn't return
  any result set.

  \code{dbGetTables} and \code{dbGetFields} are similar to
  \code{dbListTables} and \code{dbListFields} but the result is a data
  frame with all available details (whereas the latter return only a
  character vector of the names).
}
\usage{
dbSendUpdate (conn, statement, ...)
dbGetTables (conn, ...)
dbGetFields (conn, ...)
}
\arguments{
  \item{conn}{connection object}
  \item{statement}{SQL statement to execute}
  \item{...}{additional arguments to prepared statement substituted for
    "?"}
}
\section{Methods}{
  \describe{
    \item{dbBegin}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbCommit}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbDataType}{\code{signature(dbObj = "JDBCConnection", obj =
	"ANY", ...)}}
    \item{dbDisconnect}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbExistsTable}{\code{signature(conn = "JDBCConnection", name =
  "character", ...)}}
    \item{dbGetException}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbGetFields}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbGetInfo}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbGetQuery}{\code{signature(conn = "JDBCConnection", statement
	= "character", ...)}}
    \item{dbGetTables}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbListFields}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbListResults}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbListTables}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbReadTable}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbRemoveTable}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbRollback}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbSendQuery}{\code{signature(conn = "JDBCConnection",
	statement = "character", ...)}}
    \item{dbSendUpdate}{\code{signature(conn = "JDBCConnection",
	statement = "character", ...)}}
    \item{dbWriteTable}{\code{signature(conn = "JDBCConnection", ...)}}

}}
\details{
  Some notable enhancements to the DBI API:

  \code{dbSendUpdate} supports vectorized arguments which is far more
  efficient than using scalar updates. Example:
  \code{dbSendUpdate(c, "INSERT INTO myTable VALUES(?, ?)", rnorm(1000), runif(1000))}
  performs a single JDBC \code{batchExecute()} call. Additional
  parameter \code{max.batch=10000L} is an integer that specifies the
  maximum batch size supported by the DBMS.

  \code{dbSendQuery} and \code{dbSendUpdate} accept both \code{...}
  (populated frist) as well as \code{list=} (populated as second).
  Only unnamed arguments are used from \code{...} (assuming that those
  are function arguments and no data) while all elements are used from
  \code{list=}.

  \code{dbGetQuery} is a shorthand for \code{sendQuery} +
  \code{fetch}. Parameters \code{n=-1}, \code{block=2048L} and
  \code{use.label=TRUE} are passed through to \code{fetch()} others to
  \code{dbSendQuery}.

  \code{dbListTables} and \code{dbGetTables} have the arguments
  \code{(conn, pattern="\%", schema=NULL)}. \code{dbExistsTable} is just
  a wrapper for \code{dbGetTables}.

  \code{dbWriteTable} is defined as
  \code{(conn, name, value, overwrite=FALSE, append=FALSE, force=FALSE, ..., max.batch=10000L)}
  and is just a short-hand for the corresponding \code{dbSendUpdate()}
  statements. Since it is only a convenience wrapper, it is strongly
  recommended to use \code{dbSendUpdate()} in any real use-cases
  as you have far more control over the shape and properties of the
  table if you issue the \code{CREATE TABLE} statement according to
  your DBMS' capabilities.

  \code{dbReadTable} is just a shorthand for
  \code{dbGetQuery(c, "SELECT * from <table>")}
}
\keyword{methods}
