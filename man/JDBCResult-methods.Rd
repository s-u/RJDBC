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

  Additional argument \code{block} can be used to inform the driver
  that pre-fetching of a certain block of records is desirable
  (see \code{setFetchSize()} in JDBC) leading to possibly faster pulls
  of large queries. The default is to pre-fetch 2048 records. Note
  that some databases (like Oracle) don't support a fetch size of more
  than 32767. If set to \code{NA} or anything less than 1 then the
  fetch size is not changed. This is only a hint, drivers are free to
  ignore it.

  Finally, \code{use.label} logical argument determines whether
  column lables are used for naming (\code{TRUE}, default) or column
  names should be used (\code{FALSE}) since some database drivers do not
  implement labels correctly.

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
