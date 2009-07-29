read.C2Model <- function(fName) {
    if (require(RODBC)==FALSE) {
       stop("This function requires package RODBC")
    }
    channel <- odbcConnectExcel(fName)
    tabs <- sqlTables(channel, errors=TRUE)
    tabs <- tabs[tabs$TABLE_TYPE == "TABLE", ]
    ntabs <- nrow(tabs)
    X <- vector("list", length=ntabs)
    names(X) <- tabs$TABLE_NAME
    for (i in 1:ntabs) {
       X1 <- sqlFetch(channel, tabs$TABLE_NAME[i], colnames=FALSE, rownames=FALSE)
       if (tabs$TABLE_NAME[i] == "Summary") {
          X[[i]] <- as.character(X1[!is.na(X1), 1])
       } else {
          rownames(X1) <- X1[, 2]
          X[[i]] <- X1[, -c(1:3), drop=FALSE]
       }
    }
    odbcCloseAll()
    class(X) <- "C2"
    X
}

print.C2 <- function(x, ...) {
   n <- grep(":", x$Summary)
#   n <- min(which(diff(n) > 1)) - 1
   cat(as.character(x$Summary[1:max(n)]), sep="\n")  
   cat("\nModel contains the following components:\n\n")
   cat(names(x), sep="\n")
}

summary.C2 <- function(object, ...) {
   cat(as.character(object$Summary), sep="\n")  
}


