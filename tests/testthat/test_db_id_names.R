library(testthat)

create_cars_table <- function(conn, name) {
     # Create Car Table in Schema
     RJDBC::dbSendUpdate(conn = conn, statement = sprintf("DROP TABLE IF EXISTS %s CASCADE", name))
     RJDBC::dbSendUpdate(conn = conn, statement = sprintf("CREATE TABLE %s (mpg DOUBLE PRECISION,cyl DOUBLE PRECISION,disp DOUBLE PRECISION,hp DOUBLE PRECISION,drat DOUBLE PRECISION,wt DOUBLE PRECISION,qsec DOUBLE PRECISION,vs DOUBLE PRECISION,am DOUBLE PRECISION,gear DOUBLE PRECISION,carb DOUBLE PRECISION,car_name VARCHAR(255))", name))
}

test_cars_table <- function(conn, name) {
     # Create cars in default schema
     schema <- extract_schema(name)
     table  <- extract_table(name)

     ddl_table_name <- table
     if (!is.null(schema)) {
          ddl_table_name <- sprintf("%s.%s", schema, table)
     }

     create_cars_table(conn = conn, name = ddl_table_name)

     # Verify schema and table exist
     res <- RJDBC::dbGetQuery(conn = conn, statement = sprintf("SELECT table_catalog, table_schema, table_name, table_type, row_count_estimate FROM information_schema.tables where table_name = '%s'", table))
     testthat::expect_equal(object = nrow(res), expected = 1)

     if (is.null(schema)) {
          testthat::expect_equal(object = res$table_schema, expected = "public")
     } else {
          testthat::expect_equal(object = res$table_schema, expected = schema)
     }
     testthat::expect_equal(object = res$table_name, expected = table)

     testthat::expect_true(table %in% RJDBC::dbListTables(conn))
     testthat::expect_equal(
          object = sort(RJDBC::dbListFields(conn, name)),
          expected = sort(c("am", "car_name", "carb", "cyl", "disp", "drat", "gear", "hp", "mpg", "qsec", "vs","wt"))
     )
     testthat::expect_true(RJDBC::dbExistsTable(conn, name))

     # Load table via dbWriteTable
     my_cars <- mtcars
     my_cars$car_name <- row.names(mtcars)
     rownames(my_cars) <- NULL
     RJDBC::dbWriteTable(conn = conn, name = name, value = my_cars, overwrite = FALSE, append = TRUE)

     # Verify table was loaded into the schema
     res <- RJDBC::dbGetQuery(conn = conn, statement = sprintf("select * from %s", ddl_table_name))
     testthat::expect_equal(object = nrow(res), expected = nrow(my_cars))

     res <- RJDBC::dbReadTable(conn, name)
     testthat::expect_equal(object = nrow(res), expected = nrow(my_cars))

     # Cleanup and delete table
     RJDBC::dbRemoveTable(conn = conn, name = name)
     testthat::expect_false(RJDBC::dbExistsTable(conn, name))

}

test_that("R01.00.00: JDBC Connection With Default Schema", {


     conn <- create_h2_in_memory_db_connection()

     # Test without schema
     test_cars_table(conn = conn, name = "default_schema_cars")


     # Test with schema using DBI::Id
     create_empty_schema(conn = conn, schema_name = "my_schema")
     test_cars_table(conn = conn, name = DBI::Id(schema = "my_schema", table = "my_cars"))
})

