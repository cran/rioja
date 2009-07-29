\name{strat.plot}
\alias{strat.plot}
\alias{addZone}
\alias{addClustZone}

\title{ Plot a stratigraphic diagram }
\description{
Plots a diagram of multiple biological, physical or chemical parameters agains depth or time, as used in geology & palaeoecology.
}
\usage{
strat.plot (d, yvar = NULL, scale.percent = FALSE,
    scale.minmax = TRUE, xLeft = 0.07, xRight = 1,
    yBottom = 0.07, yTop = 0.8, title = "", cex.title=1.8,
    y.axis=TRUE, min.width = 5, ylim = NULL, y.rev = FALSE,
    y.tks=NULL, ylabel = "", cex.ylabel=1, cex.yaxis=1,
    xSpace = 0.01, wa.order = "none", plot.line = TRUE,
    col.line = "black", lwd.line = 1, plot.bar = TRUE,
    lwd.bar = 1, col.bar = "grey", sep.bar = FALSE,
    cex.xlabel = 1.1, mgp=c(3,.5, 0), cex.axis=.8, clust = NULL,
    clust.width=0.1, orig.fig=NULL, add=FALSE, ...)
    
addZone (x, upper, lower=NULL, ...)

addClustZone(x, clust, nZone, ...)
}
\arguments{
  \item{d}{ a matrix or data frame of variables to plot. }
  \item{yvar}{ a vector of depths or ages to use for the y-axis (defaults to sample number). }
  \item{scale.percent}{ logical to scale x-axes for (biological) percentage data. }
  \item{scale.minmax}{ logical to show only min and max values on x-axes (to avoid label crowding). }
  \item{xLeft, xRight, yBottom, yTop }{ x, y position of plot on page, in relative units. }
  \item{title}{ main title for plot. }
  \item{cex.title}{ size of label for title. }
  \item{y.axis}{ logical to control drawing to left-hand y-axis scale. Defaults to TRUE. }
  \item{min.width}{ minimum upper value of x-axis when scaled for percent data. }
  \item{ylim}{ numeric vector of 2 values to control limist of y-axis. Defaults to data range. }
  \item{y.rev}{ logical to reverse y-axis. Defaults to FALSE. }
  \item{y.tks}{ numerical vector listing values of y-axis ticks / labels. }
  \item{ylabel}{ label for y-axis. }
  \item{cex.ylabel}{ text size for y-axis label. }
  \item{cex.yaxis}{ text size for y-axis values. }
  \item{xSpace}{ space between graphs, in relative units. }
  \item{wa.order}{ "none", "topleft" or "bottomleft", to sort variables according to the weighted average with y. }
  \item{plot.line}{ logical flag to plot a line graph. }
  \item{col.line}{ colour of lines in line graph. }
  \item{lwd.line}{ line weight (thickness) in line graph. }
  \item{plot.bar}{ logical flag to plot bars or horizonal lines. }
  \item{lwd.bar}{ bar weight (thickness) bar graph. }
  \item{col.bar}{ colour of bars in a bar graph. Can be a vector to specify colours of individual bars or graphs.}
  \item{sep.bar}{ If true, colours applied to individual bars, otherwise individual graphs. }
  \item{cex.xlabel}{ size of label for variable names. }
  \item{mgp}{ value of mgp for x-axes.  See \code{par} for details. }
  \item{cex.axis}{ text size for x-axis labels.  See \code{par} for details. }
  \item{clust}{ an constrained classification object of class \code{chclust} to add to plot. }
  \item{clust.width}{ width of dendrogram to add to right of plot, in relative units. }
  \item{orig.fig}{ fig values to specify area of window in which to place diagram.  See \code{par} for details. Defaults to whole window. }
  \item{add}{ logical to contol drawing of new page. See \code{par} for details. Defaults to FALSE in which a call to strat.plot will start a new diagram. Set to  TRUE to add a diagram to an existing plot. }
  \item{x}{ a stratigraphic diagram object produced by strat.plot. }
  \item{upper, lower}{ upper and (optional) lower limits of a zone to add to an existing stratigraphic diagram. }
  \item{nZone}{ number of zones to draw. }
  \item{...}{ further graphical arguments. }
}
\details{
\code{strat.plot} plots a series of variables in a stratigraphic diagram. Diagrams can be plotted as line graphs and / or bar charts. Samples are plotted on the y-axis by sample number by default but may be plotted against sample age otr depth by specifying a variable for yvar. Margins of the plotting area can be changed using xLeft, xRight, yBottom and yTop.  A dendrogram produced by \code{chclust} can be added to the right of the diagram.

The function \code{addZone} can be used to add a horizontal line or box to an existing plot, and 
\code{addClustZone} will add a specified number of zones from a dendrogram (see examples).

The function uses fig to split the screen and may be incompatible with \code{par(mfrow)} and 
\code{split.screen}.
}

\value{
   Returns a list containing the following objects:
   \item{box }{ Vector of 4 values giving the coordinates of the left, right, bottom
    and top of the plotting area, in relative units. }
   \item{usr }{ Ranges of the plotting area, in data units. }
   \item{yvar }{ Variable used for the y-axis. }
   \item{ylim }{ Limits of the y-axis. }
}

\author{ 
Steve Juggins
}

\seealso{ 
\code{\link{chclust}}. 
}

\examples{
library(vegan) ## decorana
data(RLGH)
\dontrun{
# create appropriately sized graphics window
windows(width=12, height=7) # quartz() on Mac, X11 on linux
}
# remove less abundant taxa
mx <- apply(RLGH$spec, 2, max)
spec <- RLGH$spec[, mx > 3]
depth <- RLGH$depth$Depth
#basic stratigraphic plot
strat.plot(spec, y.rev=TRUE)
#scale for percentage data
strat.plot(spec, y.rev=TRUE, scale.percent=TRUE)
# plot by sample depth
strat.plot(spec, yvar = depth, y.rev=TRUE, scale.percent=TRUE,
title="Round Loch of Glenhead", ylabel="Depth (cm)")
# add a dendromgram from constrained cluster analysis
diss <- dist(sqrt(RLGH$spec/100)^2)
clust <- chclust(diss, method="coniss")
# broken stick model suggest 3 significant zones
bstick(clust)
x <- strat.plot(spec, yvar = depth, y.rev=TRUE,
scale.percent=TRUE, title="Round Loch of Glenhead", ylabel="Depth (cm)",
clust=clust)
# add zones
addClustZone(x, clust, 3, col="red")
# use fig to contol diagram size and position
x <- strat.plot(spec, xRight = 0.7, yvar = depth, y.rev=TRUE,
scale.percent=TRUE, title="Round Loch of Glenhead", ylabel="Depth (cm)")
# add curves for first two DCA components of diatom data
dca <- decorana(spec, iweigh=1)
sc <- scores(dca, display="sites", choices=1:2)
strat.plot(sc, xLeft = 0.7, yvar = depth, y.rev=TRUE, xRight=0.99,
y.axis=FALSE, clust=clust, clust.width=0.08, add=TRUE)
}

\keyword{ hplot }
