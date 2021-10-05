#' Write Seurat Object to TileDB
#'
#' @param base_uri The base directory into which the individual TileDB arrays
#'   will be written.
#' @references
#' https://mojaveazure.github.io/seurat-disk/articles/h5Seurat-load.html
#'
#'
#'

writeTiledbSeurat <- function(x, base_uri) {

  if (!dir.exists(base_uri)) {
    message(sprintf("Creating directory: '%s'", base_uri))
    dir.create(base_uri, showWarnings = FALSE, recursive = TRUE)
  }

  walkIt(x, base_uri, nm = deparse(substitute(x)))
}

walkIt <- function(x, base_uri, nm="", reduce=TRUE) {
    if (nm=="") nm <- deparse(substitute(x))
    print(nm)

    if (isS4(x)) {
        cl <- class(x)
        if (reduce && cl == "dgCMatrix") {
            dm <- dim(x)
            cat(nm, " : S4 (", cl, ") [", dm[1], " x ", dm[2], "]", .sizeTest(x, base_uri, nm), "\n", sep="")
        } else {
            cat(nm, " : S4 (", cl, ") (descending)\n", sep="")
            sn <- slotNames(x)
            for (s in sn) {
                z <- paste0(nm, "@", s)
                walkIt( eval(str2expression(z)), base_uri, z )
            }
        }
    } else if (is.data.frame(x)) {
        dm <- dim(x)
        cat(nm, ": data.frame [", dm[1], " x ", dm[2], "]", .sizeTest(x, base_uri, nm), "\n", sep="")
        #sn <- names(x)
        #for (s in sn) {
        #    z <- paste0(nm, "$", s)
        #    if (!grepl("\\)$", z))      # special case
        #        walkIt( eval(str2expression(z)), z )
        #}
    } else if (is.list(x)) {
        sn <- names(x)
        l <- length(sn)
        if (reduce && (l == 0 || nchar(nm) > 40 || grepl("(commands|reductions|graphs)", nm))) { # TODO char test is arbitrary
            cat(nm, ": list [", l, "] (not descending)\n", sep="")
        } else {
            cat(nm, ": list [", l, "] (descending)\n", sep="")
            for (s in sn) {
                z <- paste0(nm, "$", s)
                if (!grepl("\\)$", z))      # special case
                    walkIt( eval(str2expression(z)), base_uri, z )
            }
        }
    } else if (inherits(x, "matrix")) {
        dm <- dim(x)
        cat(nm, ": matrix [", dm[1], " x ", dm[2], "]", .sizeTest(x, base_uri, nm), "\n", sep="")
    } else {
        cat(nm , ":", class(x), "\n")
    }


}

.newName <- function(x, nm) gsub("[@$.]", "_", nm)

.saveToTileDB <- function(x, base_uri, nm) {
     newnm <- .newName(x, nm)
     uri <- file.path(base_uri, nm)
     # dir.create(uri, showWarnings=FALSE, recursive = TRUE)
     switch(class(x)[1],
            "dgCMatrix"  = fromSparseMatrix(x, uri),
            "data.frame" = fromDataFrame(.flipLogical(x), uri),
            "matrix"     = fromDataFrame(as.data.frame(x), uri))
 }

.sizeTest <- function(x, base_uri, nm) {
    s <- object.size(x)
    d <- dim(x)
    if (s > 1e5 && d[2] > 0) .saveToTileDB(x, base_uri, nm)
    if (s > 1e5 && d[2] > 0) crayon::red(" [", .newName(x, nm), "]", sep="") else ""
}


.flipLogical <- function(x) {
     tps <- sapply(x, class)
     for (i in seq_along(tps)) {
         if (tps[i] == "logical") {
             x[, i] <- as.integer(x[, i])
         }
     }
     x
 }
