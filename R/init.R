.onLoad <- function(libname, pkgname) {
  # set RJDBC options
  options(
    'RJDBC.convert_types'=FALSE,
    'RJDBC.conversion_map'=list(
      'character'=as.character,
      'double'=as.double,
      'integer'=as.integer,
      'logical'=as.logical,
      'date'=as.POSIXlt
    )
  )
}
