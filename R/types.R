# Java SQL Types
# see: http://docs.oracle.com/javase/7/docs/api/constant-values.html#java.sql
types <- list(
  LONGNVARCHAR=-16L,
  NCHAR=-15L,
  NVARCHAR=-9L,
  ROWID=-8L,
  BIT=-7L,
  TINYINT=-6L,
  BIGINT=-5L,
  LONGVARBINARY=-4L,
  VARBINARY=-3L,
  BINARY=-2L,
  LONGVARCHAR=-1L,
  NULL=0L,
  CHAR=1L,
  NUMERIC=2L,
  DECIMAL=3L,
  INTEGER=4L,
  SMALLINT=5L,
  FLOAT=6L,
  REAL=7L,
  DOUBLE=8L,
  VARCHAR=12L,
  BOOLEAN=16L,
  DATALINK=70L,
  DATE=91L,
  TIME=92L,
  TIMESTAMP=93L,
  OTHER=1111L,
  JAVA_OBJECT=2000L,
  DISTINCT=2001L,
  STRUCT=2002L,
  ARRAY=2003L,
  BLOB=2004L,
  CLOB=2005L,
  REF=2006L,
  SQLXML=2009L,
  NCLOB=2011L
)

# find matching type class
.get.type <- function(x, type_map=RJDBC.options(type.map)) {
  index = Position(function(t) {x %in% t}, type_map)
  if (!is.na(index)) {
    return(names(type_map)[index])
  } else {
    return('character')
  }
}
