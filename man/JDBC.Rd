\name{JDBC}
\alias{JDBC}
\title{
  JDBC engine
}
\description{
  \code{JDBC} creates a new DBI driver that can be used to start JDBC connections.
}
\usage{
  JDBC (driverClass = "", classPath = "", identifier.quote = NA)
}
\arguments{
  \item{driverClass}{name of the Java class of the JDBC driver to
    load. If empty, it is assumed that corresponding JDBC drivers were
    loaded by other means.}
  \item{classPath}{class path that needs to be appended in order to
    load the desired JDBC driver. Usually it is the path to the JAR file
    containing the driver.}
  \item{identifier.quote}{character to use for quoting identifiers in
    automatically generated SQL statements or \code{NA} if the back-end
    doesn't support quoted identifiers. See details section below.}    
}
\value{
  Returns a \code{JDBCDriver} object that can be used in calls to
  \code{\link[DBI]{dbConnect}}.
}
\details{
  \code{JDBC} function has two purposes. One is to initialize the Java VM
  and load a Java JDBC driver (not to be confused with the
  \code{JDBCDriver} R object which is actually a DBI driver). The
  second purpose is to create a proxy R object which can be used to a
  call \code{\link[DBI]{dbConnect}} which actually creates a connection.

  JDBC requires a JDBC driver for a database-backend to be
  loaded. Usually a JDBC driver is supplied in a Java Archive (jar)
  file. The path to such a file can be specified in
  \code{classPath}. The driver itself has a Java class name that is used
  to load the driver (for example the MySQL driver uses
  \code{com.mysql.jdbc.Driver}), this has to be specified in
  \code{driverClass}.

  Due to the fact that JDBC can talk to a wide variety of databases, the
  SQL dialect understood by the database is not known in
  advance. Therefore the RJDBC implementation tries to adhere to the SQL92
  standard, but not all databases are compliant. This affects mainly
  functions such as \code{\link[DBI]{dbWriteTable}} that have to
  automatically generate SQL code. One major ability is the support for
  quoted identifiers. The SQL92 standard uses double-quotes, but many
  database engines either don't support it or use other character. The
  \code{identifier.quote} parameter allows you to set the proper quote
  character for the database used. For example MySQL would require
  \code{identifier.quote="`"}. If set to \code{NA}, the ability to quote
  identifiers is disabled, which poses restrictions on the names that
  can be used for tables and fields. Other functionality is not affected.  

  As of RDJBC 0.2-2 JDBC-specific stored procedure calls starting with
  \code{\{call} and \code{\{?= call} are supported in the statements.
}
\seealso{
  \code{\link[DBI]{dbConnect}}
}
\examples{
\dontrun{
drv <- JDBC("com.mysql.jdbc.Driver",
  "/etc/jdbc/mysql-connector-java-3.1.14-bin.jar", "`")
conn <- dbConnect(drv, "jdbc:mysql://localhost/test")
dbListTables(conn)
data(iris)
dbWriteTable(conn, "iris", iris)
dbGetQuery(conn, "select count(*) from iris")
d <- dbReadTable(conn, "iris")
}
}
\keyword{interface}
