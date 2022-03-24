#!/bin/sh

## Poor man's test suite -- if you want to run it,
## you'll need MySQL on the local machine and
## adjust the paths/names below as needed

DRIVER=mariadb-connector-java.jar
DRVCLASS=org.mariadb.jdbc.Driver

## NOTE: if needed, you can use Docker for the test as follows:
## docker run --rm -it -p 3306:3306 -e MARIADB_ROOT_PASSWORD=root mariadb:latest

## We attempt to download the connetor if needed
if [ ! -e $DRIVER ]; then
   curl -LO https://dlm.mariadb.com/1965742/Connectors/java/connector-java-2.7.5/mariadb-java-client-2.7.5.jar
   ln -s mariadb-java-client-2.7.5.jar $DRIVER
fi

##---- cut here ----

if [ ! -e "$DRIVER" ]; then
    echo "Cannot find JDBC driver for MySQL, cannot perform MySQL tests"
    exit 1
fi

echo "library(RJDBC)
## load driver
MySQL<-JDBC('$DRVCLASS','$DRIVER',"'"`")
## connect
c<-tryCatch(dbConnect(MySQL, "jdbc:mysql://localhost/test", user="test", password="test1234"),
 ## this is a fall-back for Docker-based test mariadb without any users and testdb
 error=function(e) {
    cat("test@localhost failed to connect with: ", as.character(e), "\n")
    cat("Re-trying with root/root credentials assuming a Docker instance started with \ndocker run --rm -it -p 3306:3306 -e MARIADB_ROOT_PASSWORD=root mariadb:latest\n\n")
    c0 <- dbConnect(MySQL, "jdbc:mysql://localhost/mysql", user="root", password="root")
    dbSendUpdate(c0, "CREATE DATABASE IF NOT EXISTS test;")
    dbSendUpdate(c0, "CREATE USER IF NOT EXISTS test@\"%\" IDENTIFIED BY \"test1234\";")
    dbSendUpdate(c0, "GRANT ALL ON test.* TO test@\"%\";")
    dbDisconnect(c0)
    rm(c0)
    gc()
    ## retry after the above
    dbConnect(MySQL, "jdbc:mysql://localhost/test", user="test", password="test1234")
 })

## table operations write/read
data(iris)
dbWriteTable(c, "iris", iris, overwrite=TRUE)
dbReadTable(c, "iris")
## simple send query
fetch(r <- dbSendQuery(c, "SELECT count(*) FROM iris"), 1)
## query info - some people need this
dbGetInfo(r)
dbHasCompleted(r)
## you have to read more than what is tehre
fetch(r, 1)
dbGetInfo(r)
dbHasCompleted(r)
## prepared send query
fetch(r <- dbSendQuery(c, "SELECT Species as Kind, count(Species) as Count FROM iris WHERE `Sepal.Width` > ? GROUP BY Species", 3), 1e3, block=1e3)
dbColumnInfo(r)
## append
i2 <- iris
i2$Species <- paste0("New.", as.character(i2$Species))
dbWriteTable(c, "iris", i2, append=TRUE)
## multi-part fetch
r <- dbSendQuery(c, "SELECT * FROM iris")
while (!dbHasCompleted(r)) print(dim(fetch(r, 50)))
dbClearResult(r)
i3 <- iris
i3$Species <- paste0("XXX.", as.character(i3$Species))
## Regression: named singletons were not inserted
dbWriteTable(c, "iris", i3[1,], append=TRUE)
## Regression: merge ... and list=
dbSendUpdate(c, "INSERT INTO iris VALUES (?, ?, ?, ?, ?)", i3[[1]][2], list=i3[2,-1])
## same but batch-insert
dbSendUpdate(c, "INSERT INTO iris VALUES (?, ?, ?, ?, ?)", i3[[1]][3:5], list=i3[3:5,-1])
dbGetQuery(c, "SELECT Species as Kind, count(Species) as Count FROM iris GROUP BY Species", use.label=TRUE)
dbGetQuery(c, "SELECT Species as Kind, count(Species) as Count FROM iris GROUP BY Species", use.label=FALSE)
## simple update
dbSendUpdate(c, "DROP TABLE IF EXISTS foo")
dbSendUpdate(c, "CREATE TABLE foo (alpha VARCHAR(32), beta INT)")
## prepared update
dbSendUpdate(c, "INSERT INTO foo VALUES (?, ?)", "foo", 123)
dbSendUpdate(c, "INSERT INTO foo VALUES (?, ?)", "bar", 456)
## vectorized update
dbSendUpdate(c, "INSERT INTO foo VALUES (?, ?)", c("x","y","z"), 1:3)
fetch(dbSendQuery(c, "SELECT * FROM foo"), -1)
## list
dbGetTables(c)
dbListTables(c)
## calls
## NOTE: there is a bug in MySQL so DROP PROCEDURE IF EXISTS does NOT work!!
## hence we have to cross our fingers and hope it is not already defined
## If the call fails, it will leave a dirty db
dbSendUpdate(c, "CREATE PROCEDURE bar() BEGIN SELECT * FROM foo; END")
fetch(dbSendQuery(c, "{call bar()}"), -1)
dbSendUpdate(c, "DROP PROCEDURE bar")
## parametrized
dbSendUpdate(c, "CREATE PROCEDURE foobar(IN x INT) BEGIN SELECT * FROM foo WHERE beta >= x; END")
fetch(dbSendQuery(c, "{call foobar(222)}"), -1)
dbSendUpdate(c, "DROP PROCEDURE foobar")
## remove
(dbExistsTable(c, "iris"))
(dbRemoveTable(c, "iris"))
(dbExistsTable(c, "iris"))
(dbRemoveTable(c, "iris", silent=TRUE))
tryCatch(fetch(r <- dbSendQuery(c, "SELECT Species as Kind, count(Species) as Count FROM iris WHERE `Sepal.Width` > ? GROUP BY Species", 3), 1e3, block=1e3), error=function(e) str(e))
dbDisconnect(c)
' | R --vanilla --quiet || echo "*** TEST FAILED ***"

