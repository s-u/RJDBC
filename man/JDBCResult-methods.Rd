\name{JDBCResult-methods}
\docType{methods}
\alias{JDBCResult-methods}
\alias{dbClearResult,JDBCResult-method}
\alias{dbColumnInfo,JDBCResult-method}
\alias{dbGetInfo,JDBCResult-method}
\alias{fetch,JDBCResult,numeric-method}
\title{Methods for the class JDBCResult in Package `RJDBC' ~~}
\description{
  Methods for the class `JDBCResult' in Package `RJDBC'.

  \code{fetch} retrieves the content of the result set in the form of a
  data frame. If \code{n} is -1 then the current implementation fetches
  32k rows first and then (if not sufficient) continues with chunks of
  512k rows, appending them. If the size of the result set is known in
  advance, it is most efficient to set \code{n} to that size.

  \code{dbClearResult} releases the result set.

  \code{dbColumnInfo} returns basic information about the columns
  (fields) in the result set.

  \code{dbGetInfo} returns an empty list.

  \code{dbListResults} returns an empty list and warns that JDBC doesn't
  track results.
  }
\section{Methods}{
  \describe{
    \item{fetch}{\code{signature(res = "JDBCResult", ...)}}
    \item{dbClearResult}{\code{signature(res = "JDBCResult", ...)}}
    \item{dbColumnInfo}{\code{signature(res = "JDBCResult", ...)}}
    \item{dbGetInfo}{\code{signature(res = "JDBCResult", ...)}}
    \item{dbListResults}{\code{signature(res = "JDBCResult", ...)}}
}}
\keyword{methods}
