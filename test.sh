#!/bin/sh

## Poor man's test suite -- if you want to run it,
## you'll need MySQL on the local machine and
## adjust the paths/names below as needed

MYSQL=/usr/local/mysql
DRIVER="$MYSQL/mysql-connector-java.jar"
DRVCLASS=com.mysql.jdbc.Driver


##---- cut here ----

if [ ! -e "$DRIVER" ]; then
    echo "Cannot find JDBC driver for MySQL, cannot perform MySQL tests"
    exit 1
fi

echo "library(RJDBC)
## load driver
MySQL<-JDBC('$DRVCLASS','$DRIVER',"'"`")
## connect
c<-dbConnect(MySQL, "jdbc:mysql://localhost/test")
## table operations write/read
data(iris)
dbWriteTable(c, "iris", iris, overwrite=TRUE)
dbReadTable(c, "iris")
## simple send query
fetch(dbSendQuery(c, "SELECT count(*) FROM iris"), 1)
## prepared send query
fetch(dbSendQuery(c, "SELECT Species, count(Species) FROM iris WHERE `Sepal.Width` > ? GROUP BY Species", 3), 1e3)
## simple update
dbSendUpdate(c, "DROP TABLE IF EXISTS foo")
dbSendUpdate(c, "CREATE TABLE foo (alpha VARCHAR(32), beta INT)")
## prepared update
dbSendUpdate(c, "INSERT INTO foo VALUES (?, ?)", "foo", 123)
dbSendUpdate(c, "INSERT INTO foo VALUES (?, ?)", "bar", 456)
fetch(dbSendQuery(c, "SELECT * FROM foo"), -1)
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

dbDisconnect(c)
' | R --vanilla --quiet || echo "*** TEST FAILED ***"

