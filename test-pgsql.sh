#!/bin/sh

## Poor man's test suite -- if you want to run it,
## you'll need PostgreSQL on the local machine and
## adjust the paths/names below as needed

DRIVER="/opt/jdbc/postgresql-9.4-1204-jdbc4.jar"
DRVCLASS="org.postgresql.Driver"

##---- cut here ----

if [ ! -e "$DRIVER" ]; then
    echo "Cannot find JDBC driver for PostgreSQL, cannot perform tests"
    exit 1
fi

echo "library(RJDBC)

## load driver
drv<-JDBC('$DRVCLASS', '$DRIVER', '\"')

## connect
c<-dbConnect(drv, 'jdbc:postgresql://localhost/postgres', 'postgres', 'postgres')

## table operations write/read
data(iris)
dbWriteTable(c, 'iris', iris, overwrite=TRUE)
dbReadTable(c, 'iris')

## simple send query
fetch(r <- dbSendQuery(c, 'SELECT count(*) FROM iris'), 1)

## query info - some people need this
dbGetInfo(r)
dbHasCompleted(r)

## prepared send query
fetch(r <- dbSendQuery(c, 'SELECT Species as Kind, count(Species) as Count FROM iris WHERE \"Sepal.Width\" > ? GROUP BY Species', 3), 1e3, block=1e3)
dbColumnInfo(r)

## simple update
dbSendUpdate(c, 'DROP TABLE IF EXISTS foo')
dbSendUpdate(c, 'CREATE TABLE foo (alpha VARCHAR(32), beta INT)')

## prepared update
dbSendUpdate(c, 'INSERT INTO foo VALUES (?, ?)', 'foo', 123)
dbSendUpdate(c, 'INSERT INTO foo VALUES (?, ?)', 'bar', 456)

## vectorized update
dbSendUpdate(c, 'INSERT INTO foo VALUES (?, ?)', c('x','y','z'), 1:3)
fetch(dbSendQuery(c, 'SELECT * FROM foo'), -1)

## list
dbGetTables(c)
dbListTables(c)

## calls
#dbSendUpdate(c, 'CREATE PROCEDURE bar() BEGIN SELECT * FROM foo; END')
#fetch(dbSendQuery(c, '{call bar()}'), -1)
#dbSendUpdate(c, 'DROP PROCEDURE bar')
## parametrized
#dbSendUpdate(c, 'CREATE PROCEDURE foobar(IN x INT) BEGIN SELECT * FROM foo WHERE beta >= x; END')
#fetch(dbSendQuery(c, '{call foobar(222)}'), -1)
#dbSendUpdate(c, 'DROP PROCEDURE foobar')

dbDisconnect(c)
" > $0.r

cat $0.r | R --vanilla --quiet || echo '*** TEST FAILED ***'

