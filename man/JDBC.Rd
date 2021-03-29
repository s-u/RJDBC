\name{JDBC}
\alias{JDBC}
\alias{findDrivers}
\title{
  JDBC engine
}
\description{
  \code{JDBC} creates a new DBI driver that can be used to start JDBC
  connections.

  \code{findDrivers} attempts to find and load all JDBC 4 drivers on the
  class path.
}
\usage{
  JDBC (driverClass = "", classPath = "", identifier.quote = NA)
  findDrivers(classPath = "", service = "java.sql.Driver", loader = NULL)
}
\arguments{
  \item{driverClass}{name of the Java class of the JDBC driver to
    load or a one of Java driver instances from \code{findDrivers}.}
  \item{classPath}{character vector, class path that needs to be
    appended in order to load the desired JDBC driver. Usually it is the
    path to the JAR file containing the driver and all necessary
    dependencies. It can be a vector and all paths are expanded.}
  \item{identifier.quote}{character to use for quoting identifiers in
    automatically generated SQL statements or \code{NA} if the back-end
    doesn't support quoted identifiers. See details section below.}
  \item{service}{string, name of the services class (for JDBC always
    "java.sql.Driver")}
  \item{loader}{Java class loader to use during the look-up or
    \code{NULL} for the default one}  
}
\value{
  \code{JDBC} returns a \code{JDBCDriver} object that can be used in
  calls to \code{\link[DBI]{dbConnect}}.

  \code{findDrivers} returns a list of Java object references to
  instances of JDBC drivers that were found. The list can be empty if no
  drivers were found. Elements can be used as the \code{driverClass} in
  calls to \code{JDBC}.
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

  Modern drivers (those supporting JDBC 4) may use Java Service Provider
  interface for discovery and those can be found using the
  \code{findDrivers()} function which returns a list of drivers. You
  can pass any of the returned elements as \code{classDriver}. Note that
  the discovery is dynamic, so you can use
  \code{rJava::.jaddClassPath(...)} to add new locations in which Java
  will look for driver JAR files. However, only drivers providing JSP
  metadata in their JAR files can be found. JSP was introduced in Java
  1.6 so \code{findDrivers()} only works on Java 1.6 or higher.

  There are currently three different ways to specify drivers:
  \enumerate{
    \item \code{dbConnect(JDBC("my.Driver.Class", "driver.jar"), ...)}
    is the most explicit way where the specified driver class is used
    and expected to be found on the class path. This always works, but
    the user has to know the full name of the driver class.
    
    \item \code{dbConnect(JDBC(, "driver.jar"), ...)} omits the driver
    class which means JDBC will try to find the driver using the
    \code{DriverManager}. This \emph{only} works if the JVM has been
    loaded with the driver when initialized, so this method is
    discouraged as it is in general very unreliable. The
    \code{DriverManager} never updates the list of drivers, so once your
    driver is not found, there is nothing you can do about it.
    
    \item \code{dbConnect(JDBC(findDrivers("driver.jar")[[1]]), ...)} uses
    \code{findDrivers()} (see details above) to find all available
    drivers and then passes the needed driver (in this example the first
    one) to \code{JDBC()}. You don't need to repeat teh class path in
    this case as it is already set by \code{findDrivers()}. It is best
    to look at the output to see which drivers have been found, but if
    you pass the list, the first driver is used. Note that if you print
    the driver you will see the class name so you can also use this
    information in the first method above instead.
  }

  If you have issues loading your driver (e.g., you get ClassNotFound
  errors), make sure you specify \emph{all} dependencies of your driver,
  not just the main JAR file. They \emph{all} must be listed on the
  class path. Also make sure your JVM is supported by the driver, trying
  to load drivers with too old JVM versions also leads to ClassNotFound
  errors (as the loader will ignore calsses it cannot load). You can
  always enable debugging information in the rJava class loader using
  \code{.jclassLoader()$setDebug(1L)} for more verbose output that may
  help in your troubleshooting.  

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
