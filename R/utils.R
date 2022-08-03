#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Helper to extract values from slots
#   x      <- DBI::Id(schema = "test", table = "my_cars")
#   schema <- id_field(x, "schema")
#   table  <- id_field(x, "table")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
id_field <- function(id, field, default = NULL) {
     if (field %in% names(id@name)) {
          id@name[[field]]
     } else {
          default
     }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Helper to extract values from slots
#   x      <- DBI::Id(catalog = "foo", schema = "test", table = "my_cars")
#   catalog <- extract_catalog(x)         # "foo"
#   catalog <- extract_catalog("my_cars") # NULL
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
extract_catalog <- function(x, default = NULL) {
     if (is.null(x)) {
          return(default)
     } else if (inherits(x, "Id")) {
          return(id_field(id = x, field = "catalog", default = default))
     } else {
          return(default)
     }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Helper to extract values from slots
#   x      <- DBI::Id(schema = "test", table = "my_cars")
#   schema <- extract_schema(x)         # "test"
#   schema <- extract_schema("my_cars") # NULL
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
extract_schema <- function(x, default = NULL) {
     if (is.null(x)) {
          return(default)
     } else if (inherits(x, "Id")) {
          return(id_field(id = x, field = "schema", default = default))
     } else {
          return(default)
     }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Helper to extract values from slots
#   x      <- DBI::Id(schema = "test", table = "my_cars")
#   table <- extract_table(x)         # "my_cars"
#   table <- extract_table("my_cars") # "my_cars
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
extract_table <- function(x, default = x) {
     if (is.null(x)) {
          return(default)
     } else if (inherits(x, "Id")) {
          return(id_field(id = x, field = "table", default = default))
     } else if (is.character(x)) {
          return(x)
     } else {
          return(default)
     }
}

quote_identifier <- function(conn, x) {
     if (is.null(x)) {
          return(x)
     } else if (inherits(x, "Id")) {
          id <- DBI::dbQuoteIdentifier(conn, x)
     } else {
          id <- .sql.qescape(as.character(x),TRUE,conn@identifier.quote)
     }
     return(id)
}

create_id <- function(catalog = NULL, schema = NULL, table = NULL) {
     if (!is.null(catalog) && !is.null(schema) && !is.null(table)) {
          return(DBI::Id(catalog = catalog, schema = schema, table = table))
     } else if (is.null(catalog) && !is.null(schema) && !is.null(table)) {
          return(DBI::Id(schema = schema, table = table))
     } else if (!is.null(catalog) && is.null(schema) && !is.null(table)) {
          return(DBI::Id(catalog = catalog, table = table))
     } else if (is.null(catalog) && is.null(schema) && !is.null(table)) {
          return(table)
     } else {
          stop("Invalid ID Specification")
     }
}
