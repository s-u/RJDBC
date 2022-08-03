create_h2_in_memory_db_connection <- function() {

     h2_jar_path <- system.file("tests/testthat/java/h2-2.1.214.jar", package = "RJDBC")
     drv         <- RJDBC::JDBC(driverClass = 'org.h2.Driver', identifier.quote="`", classPath = h2_jar_path)
     conn        <- DBI::dbConnect(drv, "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1;MODE=PostgreSQL;DATABASE_TO_LOWER=TRUE;DEFAULT_NULL_ORDERING=HIGH")

     return(conn)
}

create_empty_schema <- function(conn, schema_name) {
     RJDBC::dbSendUpdate(conn = conn, statement = "CREATE USER IF NOT EXISTS sa PASSWORD 'sa'")
     RJDBC::dbSendUpdate(conn = conn, statement = sprintf("DROP SCHEMA IF EXISTS %s CASCADE", schema_name))
     RJDBC::dbSendUpdate(conn = conn, statement = sprintf("CREATE SCHEMA %s AUTHORIZATION sa", schema_name))
}

