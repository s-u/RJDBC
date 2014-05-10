# Java SQL Types
# see: http://docs.oracle.com/javase/7/docs/api/constant-values.html#java.sql
types <- list(
  LONGNVARCHAR=-16,
  NCHAR=-15,
  NVARCHAR=-9,
  ROWID=-8,
  BIT=-7,
  TINYINT=-6,
  BIGINT=-5,
  LONGVARBINARY=-4,
  VARBINARY=-3,
  BINARY=-2,
  LONGVARCHAR=-1,
  NULL=0,
  CHAR=1,
  NUMERIC=2,
  DECIMAL=3,
  INTEGER=4,
  SMALLINT=5,
  FLOAT=6,
  REAL=7,
  DOUBLE=8,
  VARCHAR=12,
  BOOLEAN=16,
  DATALINK=70,
  DATE=91,
  TIME=92,
  TIMESTAMP=93,
  OTHER=1111,
  JAVA_OBJECT=2000,
  DISTINCT=2001,
  STRUCT=2002,
  ARRAY=2003,
  BLOB=2004,
  CLOB=2005,
  REF=2006,
  SQLXML=2009,
  NCLOB=2011
)

type_map <- list(
  'character'=c(
    types$CHAR, types$VARCHAR, types$LONGVARCHAR,
    types$NCHAR, types$NVARCHAR, types$LONGNVARCHAR
  ),
  'double'=c(types$DECIMAL, types$DOUBLE, types$FLOAT, types$NUMERIC, types$REAL),
  'integer'=c(types$BIGINT, types$INTEGER, types$SMALLINT, types$TINYINT),
  'logical'=c(types$BOOLEAN),
  'date'=c(types$DATE, types$TIMESTAMP)
)

# find matching type class
.get.type <- function(x) {
  index = Position(function(t) {x %in% t}, type_map)
  if (!is.na(index)) {
    return(names(type_map)[index])
  } else {
    return('character')
  }
}
