rand.t.test <- function(object, ...) UseMethod("rand.t.test")

rand.t.test.default <- function(object, ...) { 
  stop(paste("No method to rand.t.test an object of class", class(object)[1]))
}

crossval <- function(object, ...) UseMethod("crossval")

crossval.default <- function(object, ...) {
  stop(paste("No method to cross-validate an object of class", class(object)[1]))
}

performance <- function(object, ...) UseMethod("performance")

performance.default <- function(object, ...) {
  stop(paste("No method for calculating performance for object of class", class(object)[1]))
}

performance <- function(object, ...) UseMethod("performance")

performance.default <- function(object, ...) {
  stop(paste("No method for calculating performance for object of class", class(object)[1]))
}

.check.data <- function(y, x) {
  if (any(apply(y, 1, sum) < 1.0E-8))
     stop(paste("Species data have zero abundaces for the following rows:", which(apply(y, 1, sum) < 1.0E-8)))
  if (any(apply(y, 2, sum) < 1.0E-8))
     stop(paste("Species data have zero abundaces for the following columns:", which(apply(y, 2, sum) < 1.0E-8)))
}

.print.crossval <- function(object) {
  if (object$cv.summary$cv.method == "none" | object$cv.summary$cv.method == "loo")
    cat(paste("Cross val.   :", object$cv.summary$cv.method, "\n\n"))
  else if (object$cv.summary$cv.method == "lgo")
    cat(paste("Cross val.   :", object$cv.summary$cv.method, ": no. groups = ", object$cv.summary$ngroups , "\n\n"))
  else if (object$cv.summary$cv.method == "bootstrap")
    cat(paste("Cross val.   :", object$cv.summary$cv.method, ": no. boot cycles = ", object$cv.summary$nboot , "\n\n"))
}

.print.performance <- function(object, ...) {
   perf <- performance(object)
   results <- perf$object
   if (object$cv.summary$cv.method != "none") {
      results <- rbind(results, perf$crossval)
      n <- nrow(results)
      rownames(results)[(n/2+1):n] <- paste(rownames(results)[(n/2+1):n], "_XVal", sep="")
   }
   print.default(round(results, 4), print.gap = 2, ...)
}

.performance <- function(object, ...) {
  RMSE <- apply(residuals(object), 2, .rmse)
  R2 <- apply(object$fitted.values, 2, .r2, x=object$x)
  Avg.Bias <- apply(residuals(object), 2, mean, na.rm=TRUE)
  Max.Bias <- apply(residuals(object), 2, .max.bias, x=object$x)
  res <- cbind(RMSE, R2, Avg.Bias, Max.Bias)
  result <- list(object=res)
  if (object$cv.summary$cv.method != "none") {
    if (object$cv.summary$cv.method == "bootstrap")
      RMSE <- object$cv.summary$RMSE.boot
    else
      RMSE <- apply(object$x - object$predicted, 2, .rmse)
    R2 <- apply(object$predicted, 2, .r2, x=object$x)
    Avg.Bias <- apply(object$x - object$predicted, 2, mean, na.rm=TRUE)
    Max.Bias <- apply(object$x - object$predicted, 2, .max.bias, x=object$x)
    result.cv <- cbind(RMSE, R2, Avg.Bias, Max.Bias)
    result$crossval <- result.cv
  }
  result
}

.predict <- function(object, newdata=NULL, sse=FALSE, nboot=100, match.data=TRUE, verbose=TRUE, ...) {
  if (is.null(newdata)) {
     return (object$fitted.values)
  }
  if (is.null(newdata))
     return(object$fitted.values)
  if (match.data) {
    nms <- rownames(coef(object))
    mt <- match(colnames(newdata), nms)
    mt1 <- na.omit(mt)
    if (length(mt1) == 0)
       stop("No species in common between model and newdata")
    d <- matrix(0, nrow=nrow(newdata), ncol=length(nms))
    d[, mt1] <- as.matrix(newdata)[, !is.na(mt)]
    rownames(d) <- rownames(newdata)
    newdata <- d
  } else {
    if (ncol(object$y) != ncol(newdata)) 
       stop("Number of taxa does not match between datasets")
    if (any(colnames(object$y) != colnames(newdata)))
       stop("Taxon names do not match between datasets")
  }
  feedback <- ifelse(is.logical(verbose), 50, as.integer(verbose))
#  nm.mod <- rownames(coef(object))
#  nm.new <- colnames(newdata)
#  mt <- match(nm.new, nm.mod)
#  mt1 <- na.omit(mt)
#  if (length(mt1) == 0 )
#    stop("Cannot predict: no taxa in common between object and newdata")
#  newdata2 <- matrix(0, ncol=nrow(coef(object)), nrow=nrow(newdata))
#  mt3 <- which(!is.na(mt))
#  newdata2[, mt1] <- as.matrix(newdata)[, mt3]
  predict.func <- paste("predict.internal", class(object)[1], sep=".")
  xHat.new <- do.call(predict.func, args=list(object=quote(object), y=quote(newdata), lean=FALSE, ...))
  rownames(xHat.new) <- rownames(newdata)
  colnames(xHat.new) <- colnames(object$fitted.values)
  if (sse) {
    nsam <- nrow(object$y)
    nsam.new <- nrow(newdata)
    nest <- ncol(object$fitted.values)
    res2 <- array(dim=c(nsam, nest, nboot))
    res2.new <- array(dim=c(nsam.new, nest, nboot))
    call <- as.call(object$call.fit)
#    .set.rand.seed(100)
    for (i in 1:nboot) {
      o <- sample(nsam, replace=TRUE)
#      o <- apply(data.frame(rep(nsam, nsam)), 1, .get.rand) + 1
      out <- (1:nsam)[-unique(o)]
      y <- object$y[o, ]
      x <- object$x[o]
      y.test <- object$y[out, , drop=FALSE]
      mod <- eval(call)
      res2[out, , i] <- do.call(predict.func, args=list(object=quote(mod), y=quote(y.test), lean=TRUE, ...))
      res2.new[, , i] <- do.call(predict.func, args=list(object=quote(mod), y=quote(newdata), lean=TRUE, ...))
      if (verbose) {
          if (i %% feedback == 0) {
            cat (paste("Bootstrap sample", i, "\n"))
            flush.console()
          }
      }
    }
    xHat <- object$fitted.values
    xHat.boot <- apply(res2, c(1,2), mean, na.rm=TRUE)
#    colnames(xHat.boot) <- colnames(xHat)
#    rownames(xHat.boot) <- rownames(xHat)
    xHat.new.boot <- apply(res2.new, c(1,2), mean, na.rm=TRUE)
    colnames(xHat.new.boot) <- colnames(xHat)
    rownames(xHat.new.boot) <- rownames(newdata)
#    SEP.boot <- apply(object$x-res2, c(1,2), .rmse)
#    colnames(SEP.boot) <- colnames(xHat)
#    rownames(SEP.boot) <- rownames(xHat)
    v1.boot <- apply(res2.new, c(1,2), sd, na.rm=TRUE)
    v2.boot <- apply(object$x-xHat.boot, 2, .rmse)
    colnames(v1.boot) <- colnames(xHat)
    rownames(v1.boot) <- rownames(newdata)
    SEP.boot <- sqrt(sweep(v1.boot^2, 2, v2.boot^2, "+"))
    colnames(SEP.boot) <- colnames(xHat)
    rownames(SEP.boot) <- rownames(newdata)
    results <- list(fit=xHat.new, fit.boot=xHat.new.boot, v1.boot=v1.boot, v2.boot=v2.boot, SEP.boot=SEP.boot)
  } else {
    results <- list(fit=xHat.new)
  }
  results
}

.crossval <- function(object, cv.method="loo", ngroups=10, nboot=100, verbose=TRUE, ...) 
{
  if (!("y" %in% names(object)))
    stop("Object does not contain species data, refit object using option lean=FALSE")
  METHODS <- c("loo", "lgo", "bootstrap")
  cv.method <- pmatch(cv.method, METHODS)
  if (is.na(cv.method))
     stop("Unknown cross-validation method")
  nsam <- length(object$x)
  nres <- ncol(object$fitted.values)
  func <- object$call.fit[1]
  call <- as.call(object$call.fit)
  predict.func <- paste("predict.internal", class(object)[1], sep=".")
  result <- matrix(nrow=nsam, ncol=nres)
  object$cv.summary$cv.method=METHODS[cv.method]
  feedback <- ifelse(is.logical(verbose), 50, as.integer(verbose))
  if (cv.method == 1) {
    for (i in 1:nsam) {
      y <- object$y[-i, ]
      x <- object$x[-i]
      y.test <- object$y[i, , drop=FALSE]
      mod <- eval(call)
      xHat <- do.call(predict.func, args=list(object=quote(mod), y=quote(y.test), lean=TRUE, ...))
#      xHat <- do.call(predict, args=list(object=quote(mod), y=quote(y.test), lean=TRUE))
      result[i, ] <- xHat
      if (verbose) {
          if (i %% feedback == 0) {
            cat (paste("LOO sample", i, "\n"))
            flush.console()
          }
      }
    }
  } 
  if (cv.method == 2) {
    if (length(ngroups) > 1) {
       grps <- ngroups
       ngroups <- length(unique(ngroups))
       o <- 1:nsam
    }
    else {
      o <- sample(nsam)
      grps <- rep(1:ngroups, length.out=nsam) 
    }
    for (i in 1:ngroups) {
      out <- o[grps==i]
      y <- object$y[-out, ]
      x <- object$x[-out]
      y.test <- object$y[out, , drop=FALSE]
      mod <- eval(call)
      xHat <- do.call(predict.func, args=list(object=quote(mod), y=quote(y.test), lean=TRUE))
      result[out, ] <- xHat
      if (verbose) {
            cat (paste("Leavout group", i, "\n"))
            flush.console()
      }
    }
    object$cv.summary$ngroups=ngroups
  } 
  if (cv.method == 3) {
    nest <- ncol(object$fitted.values)
    res2 <- array(dim=c(nsam, nest, nboot))
#    .set.rand.seed(100)
    for (i in 1:nboot) {
#      o <- apply(data.frame(rep(nsam, nsam)), 1, .get.rand) + 1
      o <- sample(nsam, replace=TRUE)
      out <- (1:nsam)[-unique(o)]
      y <- object$y[o, ]
      x <- object$x[o]
      y.test <- object$y[out, , drop=FALSE]
      mod <- eval(call)
      xHat <- do.call(predict.func, args=list(object=quote(mod), y=quote(y.test), lean=TRUE))
      res2[out, , i] <- xHat
      if (verbose) {
          if (i %% feedback == 0) {
            cat (paste("Bootstrap sample", i, "\n"))
            flush.console()
          }
      }
    }
    result <- apply(res2, c(1,2), mean, na.rm=TRUE)
    MS <- apply((object$x-res2)^2, c(1,2), mean, na.rm=TRUE)
    RMSE.boot <- sqrt(apply(MS, 2, mean, na.rm=TRUE))
    object$cv.summary$nboot=nboot
    object$cv.summary$RMSE.boot <- RMSE.boot
  } 
  colnames(result) <- colnames(object$fitted.values)
  rownames(result) <- rownames(object$fitted.values)
  object$predicted <- result
  object$residuals.cv <- object$x - result
  object
}
