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
  data frame. NOTE: the current implementation is done purely in R and
  is not very efficient.

  \code{dbClearResult} releases the result set.

  \code{dbColumnInfo} returns basic information about the columns
  (fields) in the result set.

  \code{dbGetInfo} returns an empty list.

  \code{dbListResults} returns an empty list and wanrs that JDBC doesn't
  track results
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

