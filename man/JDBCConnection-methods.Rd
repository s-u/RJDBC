\name{JDBCConnection-methods}
\docType{methods}
\alias{JDBCConnection-methods}
\alias{dbCommit,JDBCConnection-method}
\alias{dbDataType,JDBCConnection-method}
\alias{dbDisconnect,JDBCConnection-method}
\alias{dbExistsTable,JDBCConnection-method}
\alias{dbGetException,JDBCConnection-method}
\alias{dbGetInfo,JDBCConnection-method}
\alias{dbGetQuery,JDBCConnection,character-method}
\alias{dbListFields,JDBCConnection-method}
\alias{dbListResults,JDBCConnection-method}
\alias{dbListTables,JDBCConnection-method}
\alias{dbReadTable,JDBCConnection-method}
\alias{dbRemoveTable,JDBCConnection-method}
\alias{dbRollback,JDBCConnection-method}
\alias{dbSendQuery,JDBCConnection,character-method}
\alias{dbSendUpdate}
\alias{dbSendUpdate,JDBCConnection,character-method}
\alias{dbWriteTable,JDBCConnection-method}
\title{Methods for the class `JDBCConnect' in Package `RJDBC'}
\description{
  Methods for the class `JDBCConnection' in Package `RJDBC'.

  \code{dbSendQuery} and \code{dbSendUpdate} submit a SQL query to the
  database. The difference between the two is only that
  \code{dbSendUpdate} is used with DBML queries and thus doesn't return
  any result set.
}
\usage{
dbSendUpdate (conn, statement, ...)
}
\arguments{
  \item{conn}{connection object}
  \item{statement}{SQL statement to execute}
  \item{...}{additional arguments to prepared statement substituted for
    "?"}
}
\section{Methods}{
  \describe{
    \item{dbCommit}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbDataType}{\code{signature(dbObj = "JDBCConnection", obj =
	"ANY", ...)}}
    \item{dbDisconnect}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbExistsTable}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbGetException}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbGetInfo}{\code{signature(conn = "JDBCConnection", ...)}}
    \item{dbGetQuery}{\code{signature(conn = "JDBCConnection", statement
	= "character", ...)}}
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
\keyword{methods}

