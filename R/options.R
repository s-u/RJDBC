# Options for RJDBC
# approach inspired by RMR2

default.conversion.map <- list(
  'character'=as.character,
  'double'=as.double,
  'integer'=as.integer,
  'logical'=as.logical,
  'date'=as.POSIXlt
)

default.type.map <- list(
  'character'=c(
    types$CHAR, types$VARCHAR, types$LONGVARCHAR,
    types$NCHAR, types$NVARCHAR, types$LONGNVARCHAR
  ),
  'double'=c(types$DECIMAL, types$DOUBLE, types$FLOAT, types$NUMERIC, types$REAL),
  'integer'=c(types$BIGINT, types$INTEGER, types$SMALLINT, types$TINYINT),
  'logical'=c(types$BOOLEAN),
  'date'=c(types$DATE, types$TIMESTAMP)
)

# defaults
RJDBC.options.env <- new.env(parent=emptyenv())
RJDBC.options.env$convert.types <- FALSE
RJDBC.options.env$type.map <- default.type.map
RJDBC.options.env$conversion.map <- default.conversion.map

# partial application of arguments
partial <- function(f, ...) {
  args <- list(...)
  function(...) do.call(f, c(args, list(...)))
}

# manage options
RJDBC.options <- function(...,
                          convert.types=FALSE,
                          type.map=default.type.map,
                          conversion.map=default.conversion.map) {
  # helper functions
  args <- as.list(sys.call())[-1]
  is.named.arg <- function(x) is.element(x, names(args))
  opt.assign <- partial(assign, envir=RJDBC.options.env)

  # check the options and store
  if (is.named.arg('convert.types')) {
    opt.assign('convert.types', as.logical(convert.types))
  }
  if (is.named.arg('type.map')) {
    if (
      is.list(type.map) & length(type.map) &
      all(sapply(type.map, is.integer))
    ) {
      opt.assign('type.map', type.map)
    } else {
      warning("Invalid type map: List of integer type constants required. Ignoring.")
    }
  }
  if (is.named.arg('conversion.map')) {
    if (
      is.list(conversion.map) & length(conversion.map) &
      all(sapply(conversion.map, is.function))
    ) {
      opt.assign('conversion.map', conversion.map)
    } else {
      warning("Invalid conversion map: List of functions required. Ignoring.")
    }
  }

  # access args
  read.args <- names(formals(RJDBC.options))[-1]
  match.args <- match.call()[-1]
  if (!is.null(match.args)) {
    # filter out any args that were set
    read.args <- read.args[read.args %in% as.character(match.args)]
  }

  if (length(read.args)) {
    result <- as.list(RJDBC.options.env)[read.args]
    if (length(result) == 1) return(result[[1]]) else return(result)
  }
  else {
    return(invisible(NULL))
  }
}
