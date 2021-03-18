## default type maps
.type.map <- as.environment(list(
    DATE = function(x) as.Date(x, "%Y-%m-%d"),
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
    for (i in names(l))
        .type.map[[i]] <- l[[i]]
    as.list(.type.map)
}
