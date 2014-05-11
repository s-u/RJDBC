\name{RJDBC.options}
\alias{RJDBC.options}
\title{RJDBC Package Options}
\description{
  The \code{RJDBC.options} function allows various options to be set that control
  the execution and output of the RJDBC package.
}
\usage{
RJDBC.options(
  convert.types=FALSE,
  type.map=list(
    'character'=c(
      types$CHAR, types$VARCHAR, types$LONGVARCHAR,
      types$NCHAR, types$NVARCHAR, types$LONGNVARCHAR
    ),
    'double'=c(types$DECIMAL, types$DOUBLE, types$FLOAT, types$NUMERIC, types$REAL),
    'integer'=c(types$BIGINT, types$INTEGER, types$SMALLINT, types$TINYINT),
    'logical'=c(types$BOOLEAN),
    'date'=c(types$DATE, types$TIMESTAMP)
  ),
  conversion.map=list(
    'character'=as.character,
    'double'=as.double,
    'integer'=as.integer,
    'logical'=as.logical,
    'date'=as.POSIXlt
  )
)
}
\arguments{
  \item{convert.types}{
    \code{logical}; If \code{TRUE}, convert types as per the conversion map.
    Otherwise, if \code{FALSE} (default) then perform no conversion.
  }
  \item{type.map}{
    \code{list}; List of Java SQL Type Constants (defined in the \code{types}
    list) grouping data-types into categories for conversion by the functions
    in the conversion map.
  }
  \item{conversion.map}{
    \code{list}; List of functions to apply to each data-type group to convert
    to specific R types.
  }
}
\value{
  A named list with the options and their values, or just a value if only one
  requested. The value of any option that is being set will not be returned.
}

\examples{
\dontrun{
# enable conversion of types for query results
RJDBC.options(convert.types=TRUE)

# get the type-map
RJDBC.options(type.map)
}
}
