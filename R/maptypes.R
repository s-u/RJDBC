## default type maps
.type.map <- as.environment(list(
    DATE = function(x) if (is.character(x)) as.Date(x, "%Y-%m-%d") else if (inherits(x, "POSIXct") && !is.null(tz <- attr(x, "tzone", TRUE))) as.Date(x, tz) else as.Date(x),
    ## they are all obsolete since we parse them in Java using Timestamp
    ## but for people that set posix.ct=FALSE
    DATETIME = function(x) as.POSIXct(x, format="%Y-%m-%d %H:%M:%OS"),
    TIMESTAMP = function(x) as.POSIXct(x, format="%Y-%m-%d %H:%M:%OS")
))

.remap.types <- function(d, ct) {
    mn <- names(JDBC.types)[match(ct, JDBC.types)]
    for(i in seq_along(mn))
        if (!is.na(mn[i]) && !is.null(f <- .type.map[[mn[i]]]))
            d[[i]] <- f(d[[i]])
    d
}

dbSetTypeMaps <- function(...) {
    l <- list(...)
    if (length(l) && is.null(names(l)))
        stop("Type maps must be named")
    for (i in names(l)) {
        if (is.null(l[[i]])) {
            if (!is.null(.type.map[[i]]))
                rm(list=i, envir=.type.map)
        } else
            .type.map[[i]] <- l[[i]]
    }
    if (length(l)) invisible(as.list(.type.map)) else as.list(.type.map)
}
