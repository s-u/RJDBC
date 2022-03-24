##=== JDBCDriver

setClass("JDBCDriver", representation("DBIDriver", identifier.quote="character", jdrv="jobjRef"))

##=== JDBCResult
## jr - result set, md - metadata, stat - statement
## Since the life of a result set depends on the life of the statement, we have to explicitly
## save the later as well (and close both at the end)

setClass("JDBCResult", representation("DBIResult", jr="jobjRef", md="jobjRef", stat="jobjRef", env="environment"))

setClass("JDBCConnection", representation("DBIConnection", jc="jobjRef", identifier.quote="character"))


JDBC <- function(driverClass='', classPath='', identifier.quote=NA) {
    ## we allow the user to supply the class itself in case they got
    ## it through some other means (like findDrivers())
    if (is(driverClass, "jobjRef") || (is.list(driverClass) && length(driverClass) > 0 && is(driverClass[[1L]], "jobjRef"))) {
        if (is.list(driverClass)) driverClass <- driverClass[[1L]]
        if (!isTRUE(.jcall(.jfindClass("java.sql.Driver"), "Z", "isInstance", .jcast(driverClass, "java.lang.Object"))))
            stop("Provided class object is not a subclass of java.sql.Driver")
        return(new("JDBCDriver", identifier.quote=as.character(identifier.quote), jdrv=driverClass))
    }

    ## expand all paths in the classPath
    classPath <- path.expand(unlist(strsplit(classPath, .Platform$path.sep)))
    .jinit(classPath) ## this is benign in that it's equivalent to .jaddClassPath if a JVM is running
    .jaddClassPath(system.file("java", "RJDBC.jar", package="RJDBC"))

    if (is.null(driverClass) || !nzchar(driverClass)) ## NULL driver
        return(new("JDBCDriver", identifier.quote=as.character(identifier.quote), jdrv=.jnull("java.sql.Driver")))

    if (nchar(driverClass) && is.jnull(.jfindClass(as.character(driverClass)[1])))
        stop("Cannot find JDBC driver class ",driverClass)
    jdrv <- .jnew(driverClass, check=FALSE)
    .jcheck(TRUE)
    if (is.jnull(jdrv)) jdrv <- .jnull()
    new("JDBCDriver", identifier.quote=as.character(identifier.quote), jdrv=jdrv)
}

findDrivers <- function(classPath="", service="java.sql.Driver", loader=NULL) {
    ## we have to make sure we have a JVM
    classPath <- path.expand(unlist(strsplit(classPath, .Platform$path.sep)))
    .jinit(classPath) ## this is benign in that it's equivalent to .jaddClassPath if a JVM is running
    .jaddClassPath(system.file("java", "RJDBC.jar", package="RJDBC"))

    l <- if (is.jnull(loader))
             .jcall("java.util.ServiceLoader", "Ljava/util/ServiceLoader;", "load", .jfindClass(service))
         else
             .jcall("java.util.ServiceLoader", "Ljava/util/ServiceLoader;", "load", .jfindClass(service), .jcast(loader, "java.lang.ClassLoader"))
    i <- .jcall(l, "Ljava/util/Iterator;", "iterator")
    d <- list()
    while (.jcall(i, "Z", "hasNext"))
        d <- c(d, list(.jcall(i, "Ljava/lang/Object;", "next")))
    d
}

## construct list of class names all the way down to Object
## It is guarded by tryCatch() so should always return, possibly character()
## NOTE: uses Java calls so exceptions must be cleared prior, it will
## clear exceptions if it fails itself
.classes <- function(x) {
    cl <- character()
    tryCatch(
        if (!is.jnull(x)) {
            c <- .jcall(x, "Ljava/lang/Class;", "getClass")
            while(!is.jnull(c)) {
                cl <- c(cl, .jcall(c, "S", "getName"))
                c <- .jcall(c, "Ljava/lang/Class;", "getSuperclass")
            }
        }, error=function(e) {
            warning("Error while collecting class hierarchy in Java error handing")
            .jcheck(TRUE)
        })
    cl
}

## calls .jgetEx(TRUE) and raises a JDBC.exception condition including
## class hierarchy of the Java exception
## returns TRUE invisibly if there is no exception
.verify.ex <- function (..., statement, class=character(), call=sys.call(-1)) {
    statement.txt <- if (missing(statement)) "" else paste0("\n  Statement: ", statement)
    if (missing(statement)) statement <- NULL
    x <- .jgetEx(TRUE)
    if (!is.jnull(x))
        stop(errorCondition(paste0(..., "\n  JDBC ERROR: ",.jcall(x, "S", "getMessage"), statement.txt), call=call,
                            desc=paste0(...), jex=x, statement=statement, class=c(class, "JDBC.exception", .classes(x))))
    invisible(TRUE)
}

## requires result to be non-NULL or else calls .verify.ex() and raises an exception (even if there is no Java exception)
## The error has always at least JDBC.result.error class
.verify.JDBC.result <- function (result, ..., statement) {
    if (is.jnull(result)) {
        .verify.ex(..., statement=statement, class="JDBC.result.error", call=sys.call(-2))
        ## if we get here then there is no exception - just no result
        statement.txt <- if (missing(statement)) "" else paste0("\n  Statement: ", statement)
        if (missing(statement)) statement <- NULL
        stop(errorCondition(paste0(..., statement.txt), desc=paste0(..., statement.txt), statement=statement, class="JDBC.result.error", call=sys.call(-1)))
    }
    invisible(TRUE)
}

setMethod("dbListConnections", "JDBCDriver", def=function(drv, ...) { warning("JDBC driver maintains no list of active connections."); list() })

setMethod("dbGetInfo", "JDBCDriver", def=function(dbObj, ...)
  list(name="JDBC", driver.version="0.1-1",
       DBI.version="0.1-1",
       client.version=NA,
       max.connections=NA)
          )

setMethod("dbUnloadDriver", "JDBCDriver", def=function(drv, ...) FALSE)

setMethod("dbConnect", "JDBCDriver", def=function(drv, url, user='', password='', ...) {
    ## We used to try DriverManager first, but now we don't use DriverManager if there
    ## is a driver, because DriverManager is NOT dynamic and will fail to locate drivers
    ## that have been added after the JVM has started. Also it ignores ... parameters.
    jc <- if (is.jnull(drv@jdrv))
              .jcall("java/sql/DriverManager","Ljava/sql/Connection;","getConnection", as.character(url)[1], as.character(user)[1], as.character(password)[1], check=FALSE)
          else NULL
    if (is.null(jc) && !is.jnull(drv@jdrv)) {
        oex <- .jgetEx(TRUE)
        p <- .jnew("java/util/Properties")
        if (length(user)==1 && nzchar(user)) .jcall(p, "Ljava/lang/Object;", "setProperty", "user", as.character(user))
        if (length(password)==1 && nzchar(password)) .jcall(p, "Ljava/lang/Object;", "setProperty", "password", as.character(password))
        l <- list(...)
        if (length(names(l))) for (n in names(l)) .jcall(p, "Ljava/lang/Object;", "setProperty", n, as.character(l[[n]]))
        jc <- .jcall(drv@jdrv, "Ljava/sql/Connection;", "connect", as.character(url)[1], p, check=FALSE)
    }
    .verify.JDBC.result(jc, "Unable to connect JDBC to ",url)
    new("JDBCConnection", jc=jc, identifier.quote=drv@identifier.quote)},
    valueClass="JDBCConnection")

### JDBCConnection

setMethod("dbDisconnect", "JDBCConnection", def=function(conn, ...)
          {.jcall(conn@jc, "V", "close"); TRUE})

## new DBI API?
if (!is.null(asNamespace("DBI")$dbIsValid)) {
    setMethod("dbIsValid", "JDBCConnection", def=function(dbObj, timeout=0, ...)
        (!is.jnull(dbObj@jc)) && .jcall(dbObj@jc, "Z", "isValid", as.integer(timeout)[1]))

    setMethod("dbIsValid", "JDBCResult", def=function(dbObj, ...)
        !(is.jnull(dbObj@jr) || .jcall(dbObj@jr, "Z", "isClosed")))
}

## populate query parameters - non-vectorised version only
## ldots are typically list(...) and only unnamed args will
## be used, while all elements from list= will be used
.fillStatementParameters <- function(s, ldots=NULL, list=NULL) {
    ## remove named arguments from ldots as they are assumed to be
    ## additional method arguments and not part of the query
    if (length(ldots) && !is.null(names(ldots)))
        ldots <- ldots[names(ldots) == ""]
    l <- c(ldots, list)
    if (length(l) < 1) return (NULL)
    for (i in 1:length(l)) {
        v <- l[[i]]
        if (length(v) > 1)
            stop("Vectorized parameters are only supported in batch-updates via dbSendUpdate(), not in queries")
        if (is.null(v) || is.na(v)) { # map NAs to NULLs (courtesy of Axel Klenk)
            sqlType <- if (is.integer(v)) 4L else if (is.numeric(v)) 8L else 12L
            .jcall(s, "V", "setNull", i, sqlType)
        } else if (is.integer(v))
            .jcall(s, "V", "setInt", i, v)
        else if (is.numeric(v))
            .jcall(s, "V", "setDouble", i, as.double(v))
        else
            .jcall(s, "V", "setString", i, as.character(v))
    }
}

setMethod("dbSendQuery", signature(conn="JDBCConnection", statement="character"),  def=function(conn, statement, ..., list=NULL) {
  statement <- as.character(statement)[1L]
  ## if the statement starts with {call or {?= call then we use CallableStatement 
  if (isTRUE(as.logical(grepl("^\\{(call|\\?= *call)", statement)))) {
    s <- .jcall(conn@jc, "Ljava/sql/CallableStatement;", "prepareCall", statement, check=FALSE)
    .verify.JDBC.result(s, "Unable to execute JDBC callable statement", statement=statement)
    .fillStatementParameters(s, list(...), list)
    r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
    .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ", statement=statement)
  } else if (length(list(...)) || length(list)) { ## use prepared statements if there are additional arguments
    s <- .jcall(conn@jc, "Ljava/sql/PreparedStatement;", "prepareStatement", statement, check=FALSE)
    .verify.JDBC.result(s, "Unable to execute JDBC prepared statement", statement=statement)
    .fillStatementParameters(s, list(...), list)
    r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
    .verify.JDBC.result(r, "Unable to retrieve JDBC result set", statement=statement)
  } else { ## otherwise use a simple statement some DBs fail with the above)
    s <- .jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
    .verify.JDBC.result(s, "Unable to create simple JDBC statement", statement=statement)
    r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", as.character(statement)[1], check=FALSE)
    .verify.JDBC.result(r, "Unable to retrieve JDBC result set", statement=statement)
  } 
  md <- .jcall(r, "Ljava/sql/ResultSetMetaData;", "getMetaData", check=FALSE)
  .verify.JDBC.result(md, "Unable to retrieve JDBC result set meta data in dbSendQuery", statement=statement)
  new("JDBCResult", jr=r, md=md, stat=s, env=new.env(parent=emptyenv()))
})

if (is.null(getGeneric("dbSendUpdate"))) setGeneric("dbSendUpdate", function(conn, statement, ...) standardGeneric("dbSendUpdate"))

setMethod("dbSendUpdate",  signature(conn="JDBCConnection", statement="character"),  def=function(conn, statement, ..., list=NULL, max.batch=10000L) {
  statement <- as.character(statement)[1L]
  ## if the statement starts with {call or {?= call then we use CallableStatement 
  if (isTRUE(as.logical(grepl("^\\{(call|\\?= *call)", statement)))) {
    s <- .jcall(conn@jc, "Ljava/sql/CallableStatement;", "prepareCall", statement, check=FALSE)
    .verify.JDBC.result(s, "Unable to execute JDBC callable statement", statement=statement)
    on.exit(.jcall(s, "V", "close")) # same as ORA issue below and #4
    .fillStatementParameters(s, list(...), list)
    r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
    .verify.JDBC.result(r, "Unable to retrieve JDBC result set", statement=statement)
  } else if (length(list(...)) || length(list)) { ## use prepared statements if there are additional arguments
    s <- .jcall(conn@jc, "Ljava/sql/PreparedStatement;", "prepareStatement", statement, check=FALSE)
    .verify.JDBC.result(s, "Unable to create JDBC prepared statement", statement=statement)
    on.exit(.jcall(s, "V", "close")) # this will fix issue #4 and http://stackoverflow.com/q/21603660/2161065
    l <- list(...)
    ## strip named arguments from ... but not list
    if (!is.null(names(l))) l <- l[names(l) == ""]
    l <- c(l, list)
    names(l) <- NULL
    if (length(l)) {
      if (length(tl <- table(sapply(l, length))) > 1) stop("all parameters must have the same length")
      if (as.integer(names(tl)) > 1) { ## batch insert
        bx <- .jnew("info/urbanek/Rpackage/RJDBC/JDBCBatchExecute", s, length(l))
        .verify.JDBC.result(bx, "Unable to create batch-insert object")
        for (o in l) {
          if (is.integer(o)) .jcall(bx, "V", "addIntegers", o)
          else if (is.numeric(o)) .jcall(bx, "V", "addDoubles", o)
          else .jcall(bx, "V", "addStrings", as.character(o))
        }
        .jcall(bx, "V", "execute", as.integer(max.batch))
        .verify.JDBC.result(bx, "Unable to execute batch-insert query", statement=statement)
      } else {
        .fillStatementParameters(s, l)
        .jcall(s, "I", "executeUpdate", check=FALSE)
      }
    } else
      .jcall(s, "I", "executeUpdate", check=FALSE)
  } else {
    s <- .jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
    .verify.JDBC.result(s, "Unable to create JDBC statement ", statement=statement)
    on.exit(.jcall(s, "V", "close")) # in theory this is not necesary since 's' will go away and be collected, but appearently it may be too late for Oracle (ORA-01000)
    .jcall(s, "I", "executeUpdate", as.character(statement)[1], check=FALSE)
  }
  .verify.ex("execute JDBC update query failed in dbSendUpdate", statement=statement)
})

setMethod("dbGetQuery", signature(conn="JDBCConnection", statement="character"),  def=function(conn, statement, ..., n=-1, block=2048L, use.label=TRUE) {
  r <- dbSendQuery(conn, statement, ...)
  ## Teradata needs this - closing the statement also closes the result set according to Java docs
  on.exit(.jcall(r@stat, "V", "close"))
  fetch(r, n, block=block, use.label=use.label)
})

setMethod("dbGetException", "JDBCConnection",
          def = function(conn, ...) list()
          , valueClass = "list")

setMethod("dbGetInfo", "JDBCConnection",
          def = function(dbObj, ...) list() )

setMethod("dbListResults", "JDBCConnection",
          def = function(conn, ...) { warning("JDBC maintains no list of active results"); NULL }
          )

.fetch.result <- function(r) {
  md <- .jcall(r, "Ljava/sql/ResultSetMetaData;", "getMetaData", check=FALSE)
  .verify.JDBC.result(md, "Unable to retrieve JDBC result set meta data")
  res <- new("JDBCResult", jr=r, md=md, stat=.jnull(), env=new.env(parent=emptyenv()))
  fetch(res, -1)
}

setMethod("dbListTables", "JDBCConnection", def=function(conn, pattern="%", schema=NULL, ...) {
  md <- .jcall(conn@jc, "Ljava/sql/DatabaseMetaData;", "getMetaData", check=FALSE)
  .verify.JDBC.result(md, "Unable to retrieve JDBC database metadata")
  schema <- if (is.null(schema)) .jnull("java/lang/String") else as.character(schema)[1L]
  r <- .jcall(md, "Ljava/sql/ResultSet;", "getTables", .jnull("java/lang/String"),
              schema, pattern, .jnull("[Ljava/lang/String;"), check=FALSE)
  .verify.JDBC.result(r, "Unable to retrieve JDBC tables list")
  on.exit(.jcall(r, "V", "close"))
  ts <- character()
  while (.jcall(r, "Z", "next"))
    ts <- c(ts, .jcall(r, "S", "getString", "TABLE_NAME"))
  ts
})

if (is.null(getGeneric("dbGetTables"))) setGeneric("dbGetTables", function(conn, ...) standardGeneric("dbGetTables"))

setMethod("dbGetTables", "JDBCConnection", def=function(conn, pattern="%", schema=NULL, ...) {
  md <- .jcall(conn@jc, "Ljava/sql/DatabaseMetaData;", "getMetaData", check=FALSE)
  .verify.JDBC.result(md, "Unable to retrieve JDBC database metadata")
  schema <- if (is.null(schema)) .jnull("java/lang/String") else as.character(schema)[1L]
  r <- .jcall(md, "Ljava/sql/ResultSet;", "getTables", .jnull("java/lang/String"),
              schema, pattern, .jnull("[Ljava/lang/String;"), check=FALSE)
  .verify.JDBC.result(r, "Unable to retrieve JDBC tables list")
  on.exit(.jcall(r, "V", "close"))
  .fetch.result(r)
})

setMethod("dbExistsTable", "JDBCConnection", def=function(conn, name, schema=NULL, ...) length(dbListTables(conn, name, schema)) > 0)

setMethod("dbRemoveTable", "JDBCConnection", def=function(conn, name, silent=FALSE, ...)
    if (silent) tryCatch(dbRemoveTable(conn, name, silent=FALSE), error=function(e) FALSE) else dbSendUpdate(conn, paste("DROP TABLE", name)))

setMethod("dbListFields", "JDBCConnection", def=function(conn, name, pattern="%", full=FALSE, ...) {
  md <- .jcall(conn@jc, "Ljava/sql/DatabaseMetaData;", "getMetaData", check=FALSE)
  .verify.JDBC.result(md, "Unable to retrieve JDBC database metadata")
  r <- .jcall(md, "Ljava/sql/ResultSet;", "getColumns", .jnull("java/lang/String"),
              .jnull("java/lang/String"), name, pattern, check=FALSE)
  .verify.JDBC.result(r, "Unable to retrieve JDBC columns list for ", name)
  on.exit(.jcall(r, "V", "close"))
  ts <- character()
  while (.jcall(r, "Z", "next"))
    ts <- c(ts, .jcall(r, "S", "getString", "COLUMN_NAME"))
  .jcall(r, "V", "close")
  ts
})

if (is.null(getGeneric("dbGetFields"))) setGeneric("dbGetFields", function(conn, ...) standardGeneric("dbGetFields"))

setMethod("dbGetFields", "JDBCConnection", def=function(conn, name, pattern="%", ...) {
  md <- .jcall(conn@jc, "Ljava/sql/DatabaseMetaData;", "getMetaData", check=FALSE)
  .verify.JDBC.result(md, "Unable to retrieve JDBC database metadata")
  r <- .jcall(md, "Ljava/sql/ResultSet;", "getColumns", .jnull("java/lang/String"),
              .jnull("java/lang/String"), name, pattern, check=FALSE)
  .verify.JDBC.result(r, "Unable to retrieve JDBC columns list for ", name)
  on.exit(.jcall(r, "V", "close"))
  .fetch.result(r)
})

## There is a bug in DBI which consturcts invalid SQL in its default method for
## name=character. So we have to make sure it doesn't get picked by making sure
## we also set a character method even if we don't actually require it.
setMethod("dbReadTable", signature(conn="JDBCConnection", name="character"), def=function(conn, name, ...)
    dbGetQuery(conn, paste("SELECT * FROM",.sql.qescape(name,TRUE,conn@identifier.quote)), ...))

## cover all the other cases where the user likely intended a coersion
setMethod("dbReadTable", signature(conn="JDBCConnection", name="ANY"), def=function(conn, name, ...)
    dbGetQuery(conn, paste("SELECT * FROM",.sql.qescape(as.character(name),TRUE,conn@identifier.quote)), ...))


setMethod("dbDataType", signature(dbObj="JDBCConnection", obj = "ANY"),
          def = function(dbObj, obj, ...) {
            if (is.integer(obj)) "INTEGER"
            else if (is.numeric(obj)) "DOUBLE PRECISION"
            else "VARCHAR(255)"
          }, valueClass = "character")

.sql.qescape <- function(s, identifier=FALSE, quote="\"") {
  s <- as.character(s)
  if (identifier) {
    vid <- grep("^[A-Za-z]+([A-Za-z0-9_]*)$",s)
    if (length(s[-vid])) {
      if (is.na(quote)) stop("The JDBC connection doesn't support quoted identifiers, but table/column name contains characters that must be quoted (",paste(s[-vid],collapse=','),")")
      s[-vid] <- .sql.qescape(s[-vid], FALSE, quote)
    }
    return(s)
  }
  if (is.na(quote)) quote <- ''
  s <- gsub("\\\\","\\\\\\\\",s)
  if (nchar(quote)) s <- gsub(paste("\\",quote,sep=''),paste("\\\\\\",quote,sep=''),s,perl=TRUE)
  paste(quote,s,quote,sep='')
}

setMethod("dbWriteTable", "JDBCConnection", def=function(conn, name, value, overwrite=FALSE, append=FALSE, force=FALSE, ..., max.batch=10000L) {
  ac <- .jcall(conn@jc, "Z", "getAutoCommit")
  overwrite <- isTRUE(as.logical(overwrite))
  append <- isTRUE(as.logical(append))
  if (overwrite && append) stop("overwrite=TRUE and append=TRUE are mutually exclusive")
  if (is.vector(value) && !is.list(value)) value <- data.frame(x=value)
  if (length(value)<1) stop("value must have at least one column")
  if (is.null(names(value))) names(value) <- paste("V",1:length(value),sep='')
  if (length(value[[1]])>0) {
    if (!is.data.frame(value)) value <- as.data.frame(value, row.names=1:length(value[[1]]))
  } else {
    if (!is.data.frame(value)) value <- as.data.frame(value)
  }
  fts <- sapply(value, dbDataType, dbObj=conn)
  if (isTRUE(as.logical(force))) {
    if (overwrite) dbRemoveTable(conn, name, silent=TRUE)
  } else if (dbExistsTable(conn, name)) {
    if (overwrite) dbRemoveTable(conn, name)
    else if (!append) stop("Table `",name,"' already exists")
  } else append <- FALSE ## if the table doesn't exist, append has no meaning
  fdef <- paste(.sql.qescape(names(value), TRUE, conn@identifier.quote),fts,collapse=',')
  qname <- .sql.qescape(name, TRUE, conn@identifier.quote)
  if (ac) {
    .jcall(conn@jc, "V", "setAutoCommit", FALSE)
    on.exit(.jcall(conn@jc, "V", "setAutoCommit", ac))
  }
  if (!append) {
    ct <- paste("CREATE TABLE ",qname," (",fdef,")",sep= '')
    dbSendUpdate(conn, ct)
  }
  if (length(value[[1]])) {
    inss <- paste("INSERT INTO ",qname," VALUES(", paste(rep("?",length(value)),collapse=','),")",sep='')
    ## make sure everything is a character other than real/int
    list <- lapply(value, function(o) if (!is.numeric(o)) as.character(o) else o)
    dbSendUpdate(conn, inss, list=list)
  }
  if (ac) dbCommit(conn)
  invisible(TRUE)
})

setMethod("dbCommit", "JDBCConnection", def=function(conn, ...) {
    .jcall(conn@jc, "V", "commit")
    .verify.ex("commit failed")
    .jcall(conn@jc, "V", "setAutoCommit", TRUE)
    .verify.ex("enabling auto-commit failed")
    invisible(TRUE)
})

setMethod("dbRollback", "JDBCConnection", def=function(conn, ...) {
    .jcall(conn@jc, "V", "rollback")
    .verify.ex("rollback failed")
    .jcall(conn@jc, "V", "setAutoCommit", TRUE)
    .verify.ex("enabling auto-commit failed")
    invisible(TRUE)
})

setMethod("dbBegin", "JDBCConnection", def=function(conn, force=FALSE, ...) {
    ac <- .jcall(conn@jc, "Z", "getAutoCommit")
    .verify.ex("cannot determine transaction state")
    if (!force && !isTRUE(ac))
        stop("JDBC connection is already in transaction mode")
    .jcall(conn@jc, "V", "setAutoCommit", FALSE)
    .verify.ex("disabling auto-commit failed")
    invisible(TRUE)
})

## NOTE: if you modify any defaults or add arguments, also check dbGetQuery() which attempts to pass those through!
setMethod("fetch", signature(res="JDBCResult", n="numeric"), def=function(res, n, block=2048L, use.label=TRUE, ...) {
  getColumnLabel <- if(use.label) "getColumnLabel" else "getColumnName"
  cols <- .jcall(res@md, "I", "getColumnCount")
  block <- as.integer(block)
  if (length(block) != 1L) stop("invalid block size")
  if (cols < 1L) return(NULL)
  l <- vector("list", cols)
  cts <- rep(0L, cols) ## colum type (as per JDBC)
  rts <- rep(0L, cols) ## retrieval types (0 = string, 1 = double)
  for (i in 1:cols) {
    cts[i] <- ct <- .jcall(res@md, "I", "getColumnType", i)
    l[[i]] <- character()
    ## NOTE: this is also needed in dbColumnInfo()
    if (ct == -5 | ct ==-6 | (ct >= 2 & ct <= 8)) {
        ## some numeric types may exceed double precision (see #83)
        ## those must be retrieved as strings
        cp <- .jcall(res@md, "I", "getPrecision", i)
        if (cp <= 15 || ct >= 4) { ## 4+ = INTEGER, FLOAT, REAL, DOUBLE... some DBs (e.g., MySQL) report bogus precition for those
            l[[i]] <- numeric()
            rts[i] <- 1L
        }
    }
    names(l)[i] <- .jcall(res@md, "S", getColumnLabel, i)
  }
  rp <- res@env$pull
  if (is.jnull(rp)) {
    rp <- .jnew("info/urbanek/Rpackage/RJDBC/JDBCResultPull", .jcast(res@jr, "java/sql/ResultSet"), .jarray(as.integer(rts)))
    .verify.JDBC.result(rp, "cannot instantiate JDBCResultPull helper class")
    res@env$pull <- rp
  }
  if (n < 0L) { ## infinite pull - collect (using pairlists) & join
    stride <- 32768L  ## start fairly small to support tiny queries and increase later
    while ((nrec <- .jcall(rp, "I", "fetch", stride, block)) > 0L) {
      for (i in seq.int(cols))
        l[[i]] <- pairlist(l[[i]], if (rts[i] == 1L) .jcall(rp, "[D", "getDoubles", i) else .jcall(rp, "[Ljava/lang/String;", "getStrings", i))
      if (nrec < stride) break
      stride <- 524288L # 512k
    }
    for (i in seq.int(cols)) l[[i]] <- unlist(l[[i]], TRUE, FALSE)
  } else {
    nrec <- .jcall(rp, "I", "fetch", as.integer(n), block)
    for (i in seq.int(cols)) l[[i]] <- if (rts[i] == 1L) .jcall(rp, "[D", "getDoubles", i) else .jcall(rp, "[Ljava/lang/String;", "getStrings", i)
  }
  # as.data.frame is expensive - create it on the fly from the list
  attr(l, "row.names") <- c(NA_integer_, length(l[[1]]))
  class(l) <- "data.frame"
  .remap.types(l, cts)
})

setMethod("dbClearResult", "JDBCResult",
          def = function(res, ...) {
              .jcall(res@jr, "V", "close")
              .jcall(res@stat, "V", "close")
              res@env$pull <- .jnull()
              TRUE
          },
          valueClass = "logical")

setMethod("dbGetInfo", "JDBCResult", def=function(dbObj, ...)
    list(
        has.completed=dbHasCompleted(dbObj)
    ), valueClass="list")

setMethod("dbHasCompleted", "JDBCResult", def=function(res, ...) {
    pull <- res@env$pull
    comp <- if (is.jnull(pull)) .jcall(res@jr, "Z", "isClosed") else .jcall(pull, "Z", "completed")
    .jcheck(TRUE)
    comp
},
    valueClass="logical")

setMethod("dbColumnInfo", "JDBCResult", def = function(res, ...) {
  cols <- .jcall(res@md, "I", "getColumnCount")
  l <- list(field.name=character(), field.type=character(), data.type=character(), precision=integer(), type.id=integer())
  if (cols < 1) return(as.data.frame(l))
  for (i in 1:cols) {
    l$name[i] <- .jcall(res@md, "S", "getColumnLabel", i)
    l$field.type[i] <- .jcall(res@md, "S", "getColumnTypeName", i)
    ct <- .jcall(res@md, "I", "getColumnType", i)
    l$data.type[i] <- if (ct == -5 | ct ==-6 | (ct >= 2 & ct <= 8)) "numeric" else "character"
    l$field.name[i] <- .jcall(res@md, "S", "getColumnName", i)
    l$type.id[i] <- ct
    l$precision[i] <- .jcall(res@md, "I", "getPrecision", i)
  }
  as.data.frame(l, row.names=1:cols)
},
          valueClass = "data.frame")

