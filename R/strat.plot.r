strat.plot <- function (d, yvar = NULL, scale.percent = FALSE, scale.minmax = TRUE,
    xLeft = 0.07, xRight = 1, yBottom = 0.07, yTop = 0.8, title = "", cex.title=1.8, y.axis=TRUE,
    min.width = 5, ylim = NULL, y.rev = FALSE, y.tks=NULL, ylabel = "", cex.ylabel=1, cex.yaxis=1, 
    xSpace = 0.01, wa.order = "none", plot.line = TRUE, col.line = "black", lwd.line = 1,
    plot.bar = TRUE, lwd.bar = 1, col.bar = "grey", sep.bar = FALSE, 
    cex.xlabel = 1.1, mgp=c(3,.5, 0), cex.axis=.8, clust = NULL, clust.width=0.1, orig.fig=NULL, add=FALSE, ...)
{
    if (!is.null(clust)) {
     if (class(clust)[1]!="chclust") 
        stop("clust must be a chclust object")
    }
    if (!is.null(clust)) 
        xRight = xRight - clust.width
    if (is.null(yvar)) {
        yvar <- 1:nrow(d)
        if (is.null(ylim)) {
           ylim=c(0.5, nrow(d)+0.5)
        }
    }
    if (is.null(ylim)) 
        ylim = c(min(yvar, na.rm=TRUE), max(yvar, na.rm=TRUE))
    oldfig = par("fig")
    oldmai <- par("mai")
    if (wa.order == "topleft" || wa.order == "bottomleft") {
        colsum <- colSums(d)
        opt <- (t(d) %*% yvar)/colsum
        if ((wa.order == "topleft" & !y.rev) | (wa.order == "bottomleft" & y.rev))
            opt.order <- rev(order(opt))
        else opt.order <- order(opt)
        d <- d[, opt.order]
    }
    if (is.null(orig.fig)) {
       orig.fig = par("fig")
    }
    par(mai = c(0, 0, 0, 0))
    nsp <- ncol(d)
    nsam <- nrow(d)

    inc <- 0.002
    if (scale.percent) {
        colM <- apply(d, 2, max)
        colM <- floor((colM + 5)/5) * 5
        colM[colM < min.width] <- min.width
        colM.sum <- sum(colM)
#        xLen <- 1 - xLeft - xRight
        xLen <- xRight - xLeft
        xInc <- xLen - ((nsp + 1) * xSpace)
        inc <- xInc/colM.sum
    }
    else {
#        xLen <- 1 - xLeft - xRight
        xLen <- xRight - xLeft
        xInc <- xLen - ((nsp + 1) * xSpace)
        inc <- xInc/(nsp)
    }
    if (inc < 0.0)
       stop("Too many variables, curves will be too small. Reduce xSpace.")
    x1 <- xLeft
#    par(fig = c(x1, x1+0.4, yStart, yTop))
    par(fig = figCnvt(orig.fig, c(x1, min(x1+0.4, .9), yBottom, yTop)), new=add)
    if (y.rev) {
        tmp <- ylim[1]
        ylim[1] <- ylim[2]
        ylim[2] <- tmp
    }
    plot(0, 0, cex = 0.5, xlim = c(0, 1), axes = FALSE, type = "n", yaxs = "r", ylim = ylim, ...)
    usr1 <- par("usr")
    if (y.axis) {
      if (is.null(y.tks))
         y.tks <- axTicks(2)
      ax <- axis(side = 2, las = 1, at = y.tks, labels = as.character(y.tks), cex.axis=cex.yaxis, xpd=NA)
      x1 <- x1 + xSpace
      mtext(title, adj = 0, lin = 5, cex = cex.title)
      mtext(ylabel, side = 2, line = 2.5, cex=cex.ylabel)
    }
    ty <- ifelse(plot.line, "l", "n")
    for (i in 1:nsp) {
        par(new = TRUE)
        par(lend = "butt")
        if (scale.percent) {
            inc2 <- inc * colM[i]
#            par(fig = c(x1, x1 + inc2, yStart, yTop))
            par(fig = figCnvt(orig.fig, c(x1, x1 + inc2, yBottom, yTop)))
            plot(0, 0, cex = 0.5, xlim = c(0, colM[i]), axes = FALSE, 
                xaxs = "i", type = "n", yaxs = "r", ylim = ylim, ...)
            if (plot.bar) {
               if (length(col.bar) > 1) {
                  if (!sep.bar) {
                      segments(rep(0, nsam), yvar, d[, i], yvar, lwd = lwd.bar, col = col.bar)
                  } else {
                      segments(rep(0, nsam), yvar, d[, i], yvar, lwd = lwd.bar, col = col.bar[i])
                  }
                } else {
                  segments(rep(0, nsam), yvar, d[, i], yvar, lwd = lwd.bar, col = col.bar)
                }
            }
            lines(c(0, 0), c(min(yvar, na.rm=TRUE), max(yvar, na.rm=TRUE)), ...)
            if (ty == "l") 
                lines(d[, i], yvar, col = col.line, lwd = lwd.line)
            axis(side = 1, at = seq(0, colM[i], by = 10), labels = FALSE)
            x1 <- x1 + inc2 + xSpace
        }
        else {
#            par(fig = c(x1, x1 + inc, yStart, yTop))
            par(fig = figCnvt(orig.fig, c(x1, min(1, x1 + inc), yBottom, yTop)))
            plot(d[, i], yvar, cex = 0.5, axes = FALSE, xaxs = "i", 
                type = "n", yaxs = "r", ylim = ylim, ...)
            tks <- axTicks(1)
            us <- par("usr")
            tks[1] <- us[1]
            if (plot.bar) {
               if (length(col.bar) > 1) {
                  if (!sep.bar) {
                     segments(rep(tks[1], nsam), yvar, d[, i], yvar, lwd = lwd.bar, col = col.bar)
                  } else {
                     segments(rep(tks[1], nsam), yvar, d[, i], yvar, lwd = lwd.bar, col = col.bar[i])
                  }
               } else {
                  segments(rep(tks[1], nsam), yvar, d[, i], yvar, lwd = lwd.bar, col = col.bar)
               }
            }
            lines(c(tks[1], tks[1]), c(min(yvar, na.rm=TRUE), max(yvar, na.rm=TRUE)), 
                ...)
            if (ty == "l") 
                lines(d[, i], yvar, col = col.line, lwd = lwd.line)
            if (scale.minmax) {
                nn <- length(axTicks(1))
                tk <- c(axTicks(1)[1], axTicks(1)[nn])
                axis(side = 1, at = tk, labels = as.character(tk), 
                  las = 2, cex.axis=cex.axis, mgp=c(3,.8,0), ...)
            }
            else {
                axis(side = 1, las = 2, cex.axis=cex.axis, mgp=c(3,.8,0), ...)
            }
            x1 <- x1 + inc + xSpace
        }
        tks1 <- axTicks(1)
        r <- (usr1[4] - usr1[3]) * 0.01
        pos <- usr1[4]+r
        if (y.rev)
           pos <- usr1[4]-r
        text(tks1[1], pos, labels=colnames(d)[i], adj = c(0, 1), srt=90, cex = cex.xlabel, xpd=NA)
    }
    if (!is.null(clust)) {
#        par(fig = c(x1, 1, yStart, yTop))
        par(fig = figCnvt(orig.fig, c(x1, xRight+clust.width, yBottom, yTop)))
        par(mar=c(0,0,0,0))
        par(new = TRUE)
#        plot(clust, horiz = TRUE, xaxt.rev=yaxt.rev, leaflab = "none", cex.axis = 0.5, yaxt.rev=TRUE)
#        if(y.rev)
#           clust <- rev(clust)

        plot(clust, xvar=yvar, horiz=TRUE, x.rev=y.rev, labels=rep("", length(yvar)), hang=-1, mgp=mgp, cex.axis=cex.axis, ...)

#        plot(clust, horiz = TRUE, leaflab = "none", cex.axis = 0.5, yaxt.rev=TRUE)
    }
    par(mai = oldmai)
    par(fig = oldfig)
    ll <- list(box=c(xLeft=xLeft, xRight=xRight, yBottom=yBottom, yTop=yTop), usr = usr1, yvar=yvar, ylim=ylim, y.rev=y.rev)
    invisible(ll)
    
}

addZone <- function(x, upper, lower=NULL, ...) {
   oldpar <- par(c("fig", "mar", "usr"))
   par(fig=x$box)
   par(mar=c(0,0,0,0))
   par(usr=c(0, 1, x$usr[3], x$usr[4]))
   if (is.null(lower))
      segments(0, upper, 1, upper, xpd=NA, ...)
   else
      rect(0, lower, 1, upper, ...)
   par(oldpar)
}

addClustZone <- function(x, clust, nZone, ...) {
   oldpar <- par(c("fig", "mar", "usr"))
   par(fig=x$box)
   par(mar=c(0,0,0,0))
   par(usr=c(0, 1, x$usr[3], x$usr[4]))
   cc <- cutree(clust, k=nZone)
   zn <- which(diff(cc)>0)
#   if (x$yaxt.rev)
#      x$yvar <- rev(x$yvar)
   zone <- (x$yvar[zn] + x$yvar[zn+1]) / 2
   segments(0, zone, 1, zone, xpd=NA, ...)
   par(oldpar)
}

strat.plot.simple <- function(y1, x1, y2=NULL, x2=NULL, col=c("blue", "red"), sort.vars=c("original","wa", "alphabetical"), ylim=range(x1), y.rev=FALSE, type=c("b", "l"), subset=c(1:ncol(y1)), ...) {
   nsp.y1 <- ncol(y1)
   y1 <- y1[, subset]
   if (ncol(y1) > 50)
      stop("You have more than 50 columns in the species data, split the data into smaller subsets")
   sort.vars <- match.arg(sort.vars)
   if (sort.vars == "original") {
      or1 <- order(colnames(y1))
      ord <- order(or1)
   } else {
      if (sort.vars == "wa") {
          or1 <- order(colnames(y1))
          wa.sc <- apply(y1[, or1], 2, function(x, env) { sum(x*env, na.rm=TRUE) / sum(x, na.rm=TRUE) }, env=x1)
          ord <- order(wa.sc)
      }
      else {
         ord <- 1:ncol(y1)
      }
   }
   require(lattice)
   s <- stack(y1)
   s$x <- rep(x1, times=ncol(y1))
   s$set <- 1
   if (!is.null(y2) | !is.null(x2)) {
     if (nsp.y1 != ncol(y2))
        stop("Number of columns different in y1 and y2")
     y2 <- y2[, subset] 
     s2 <- stack(data.frame(y2))
     s2$x <- rep(x2, times=ncol(y2))
     s2$set <- 2
     s <- rbind(s, s2)
   }
   if (y.rev)
      ylim <- rev(ylim)
   xyplot(x ~ values| ind, data = s, type=type, distribute.type=TRUE, groups = s$set, col=c("blue", "red"), ylim=ylim, index.cond=list(ord), ylab="", xlab="", ...)
}
