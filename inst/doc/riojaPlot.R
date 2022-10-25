## ----setup, include = FALSE---------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
options(warn=1)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
if (knitr::is_html_output()) {
   knitr::opts_chunk$set(out.width = "98%")
}
  fWidth=10; fHeight=6

## ----c2A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
library(rioja)

# use built-in data from Abernethy Forest
# see ?aber for citation
data(aber)
# extract pollen percentages
aber.poll <- aber$spec
# replace species codes with full taxon names
colnames(aber.poll) <- aber$names$Name 
aber.chron <- aber$ages

# plot on depth scale (depth is the first column in chron)
riojaPlot(aber.poll, aber.chron)

## ----c2B, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
# scale for percentage data 
riojaPlot(aber.poll, aber.chron, 
   scale.percent=TRUE)

## ----c2C, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
# turn of silhouettes and lines, increase bar thickness
riojaPlot(aber.poll, aber.chron, 
   scale.percent=TRUE,
   plot.poly=FALSE,
   plot.line=FALSE,
   lwd.bar=3,
   col.bar="black")

## ----c3A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
# reduce number of taxa and add exaggerations
# make a vector of taxon names with max abundance greater that 2 percent
mx <- apply(aber.poll, 2, max)
aber.sel <- names(mx[mx > 2])

riojaPlot(aber.poll, aber.chron, aber.sel,
   scale.percent=TRUE,
   exag=TRUE)

## ----c4A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
riojaPlot(aber.poll, aber.chron, aber.sel,
   yvar.name="Age (years BP)",
   sec.yvar.name="Depth (cm)", 
   plot.sec.axis=TRUE, 
   ymin=6000,
   ymax=14300,
   yinterval=500,
   scale.percent=TRUE,
   exag=TRUE)

## ----c5A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
# group taxa by type and add cumulative graph
# extract types
aber.types <- aber$names[, -1]
# convert pollen types to a factor
aber.types$Group <- factor(aber.types$Group, levels=c("Trees", "Shrubs", "Herbs"))
ylab <- expression(Age~"("*""^{14}*C~years~BP*")")

riojaPlot(aber.poll, aber.chron, aber.sel, aber.types,
   yvar.name="Age (14C years BP)",
   sec.yvar.name="Depth (cm)", 
   ylabel=ylab,
   plot.sec.axis=TRUE, 
   ymin=5500,
   ymax=12200,
   yinterval=500,
   scale.percent=TRUE,
   plot.groups=TRUE,
   plot.cumul=TRUE,
   exag=TRUE, 
   names.italicise = TRUE,
   srt.xlabel=45, 
   las.axis=2,
   yBottom=0.05)

## ----c6A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
riojaPlot(aber.poll, aber.chron, aber.sel, aber.types,
   sec.yvar.name="Depth (cm)", 
   yvar.name="Age (years BP)",
   ymin=6000,
   ymax=14300,
   yinterval=500,
   plot.sec.axis=TRUE, 
   scale.percent=TRUE,
   plot.groups=TRUE, 
   plot.cumul=TRUE,
   exag=TRUE, 
   do.clust=TRUE,
   clust.data.trans="sqrt",
   plot.clust=TRUE, 
   plot.zones="auto", 
   names.italicise = TRUE)

## ----c6B, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
aber.clust <- chclust(dist(log10(aber.poll+1)))
riojaPlot(aber.poll, aber.chron, aber.sel, aber.types, clust=aber.clust,
   sec.yvar.name="Depth (cm)", 
   yvar.name="Age (years BP)",
   ymin=6000,
   ymax=14300,
   yinterval=500,
   plot.sec.axis=TRUE, 
   scale.percent=TRUE,
   plot.groups=TRUE, 
   plot.cumul=TRUE,
   exag=TRUE, 
   clust.data.trans="sqrt",
   plot.clust=TRUE, 
   plot.zones="auto", 
   names.italicise = TRUE)

## ----c6C, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
aber.clust <- chclust(dist(log10(aber.poll+1)))
rp <- riojaPlot(aber.poll, aber.chron, aber.sel, aber.types, 
   sec.yvar.name="Depth (cm)", 
   yvar.name="Age (years BP)",
   ymin=6000,
   ymax=14300,
   yinterval=500,
   plot.sec.axis=TRUE, 
   scale.percent=TRUE,
   plot.groups=TRUE, 
   plot.cumul=TRUE,
   exag=TRUE, 
   clust.data.trans="sqrt",
   do.clust=TRUE,
   plot.clust=TRUE, 
   plot.zones="auto", 
   names.italicise = TRUE,
   xRight = 0.9)

addRPClust(rp, aber.clust, xLeft=0.91, xRight=0.99)

## ----c7A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
mystyle <- makeStyles(
   plot.sec.axis=TRUE, 
   scale.percent=TRUE,
   plot.groups=TRUE, 
   plot.cumul=TRUE,
   exag=TRUE, 
#   plot.bar="full",
   plot.bar=TRUE,
   bar.back=TRUE,
   lwd.bar=0.5,
   col.bar="lightgrey",
   do.clust=TRUE,
   plot.clust=TRUE, 
   plot.zones="auto",
   srt.xlabel=45)

riojaPlot(aber.poll, aber.chron, aber.sel, aber.types, mystyle,
   sec.yvar.name="Depth (cm)", 
   yvar.name="Age (years BP)",
   ymin=6000,
   ymax=14300,
   yinterval=500)

## ----c8A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
# Diatom data the Round Loch of Glenhead, Galloway, SW Scotland
# Shows recent acidification and small recovery
data(RLGH)
RLGH.names <- RLGH$names
RLGH.diat <- RLGH$spec

# plot only common taxa
mx <- apply(RLGH.diat, 2, max)
RLGH.sel <- colnames(RLGH.diat)[mx > 2]
RLGH.chron <- RLGH$depths
# data has 210Pb age based on years before coring (1980)
# Add new column with Years (CE)
RLGH.chron$Year <- 1980 - RLGH.chron$Age

# do a pH reconstruction using SWAP modern dataset (see ?SWAP)
data(SWAP)
# generate a WA model
SWAP.wa <- WA(SWAP$spec, SWAP$pH)
RLGH.pH <- predict(SWAP.wa, RLGH$spec) 
# convert to data frame with single column
RLGH.pH <- data.frame(`DI-pH (SWAP)`=RLGH.pH$fit[, 1], check.names=FALSE)
rp <- riojaPlot(RLGH.diat, RLGH.chron, 
          yvar.name="Year",
          scale.percent=TRUE,
          y.rev=FALSE,
          selVars=RLGH.sel,
          ymax=1980, 
          x.names=RLGH$names$TaxonName, 
          cex.xlabel=0.7, 
          names.break.n=25,
          xRight=0.9)

riojaPlot2(rp, RLGH.pH, RLGH.chron[, "Year", drop=FALSE],
           scale.minmax=FALSE,
           plot.bar=FALSE, 
           plot.symb=TRUE,
           symb.cex=0.6)

## ----c8B, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
RLGH.clust <- chclust(dist(sqrt(RLGH.diat)))
rp <- riojaPlot(RLGH.diat, RLGH.chron, 
          yvar.name="Year",
          scale.percent=TRUE,
          y.rev=FALSE,
          selVars=RLGH.sel,
          ymax=1980, 
          x.names=RLGH$names$TaxonName, 
          cex.xlabel=0.7, 
          names.break.n=25,
          xRight=0.8)

riojaPlot2(rp, RLGH.pH, RLGH.chron[, "Year", drop=FALSE], xRight=0.9,
           scale.minmax=FALSE,
           plot.bar=FALSE, 
           plot.symb=TRUE,
           symb.cex=0.6)

addRPClust(rp, RLGH.clust, xLeft=0.9)
addRPClustZone(rp, RLGH.clust, xRight=0.9, col="blue")

## ----c8C, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
library(readxl)
library(dplyr)
# Import the data, assign pollen types to groups
fpath <- system.file("extdata/allen1999.xlsx", package="rioja")
pollen <- read_excel(fpath, sheet="Pollen data", skip=2)
pollen.chron <- pollen %>% select(1)
pollen <- pollen %>% select(Pinus:`Other herbaceous taxa`)
types <- data.frame(Name=colnames(pollen), Group="Woody taxa")
types$Group[9:12] <- "Herbs"
types$Group <- factor(types$Group, levels=c("Woody taxa", "Herbs"))
mag <- read_excel(fpath, sheet="Magnetic susceptibility", skip=2)
mag.chron <- mag %>% select(1) 
mag <- mag %>% select(`Mag Susc`) %>% mutate(`Mag Susc` = `Mag Susc` / 1000)
loi <- read_excel(fpath, sheet="Loss on ignition", skip=2)
loi.chron <- loi %>% select(1)
loi <- loi %>% select("LOI (wt %)")
BSi <- read_excel(fpath, sheet="Biogenic silica", skip=2)
BSi.chron <- BSi %>% select(1)
BSi <- BSi %>% select("BSi")

rp <- riojaPlot(pollen, pollen.chron, groups=types, 
          yinterval = 5000,
          ymin = 0, 
          ymax=102000,
          yvar.name = "Age BP",
          scale.percent=TRUE,
          plot.groups=TRUE,
          do.clust = TRUE, 
          plot.zones = "auto",
          plot.clust=TRUE,
          plot.cumul=TRUE,
          srt.xlabel=45,
          xSpace = 0.01,
          plot.bar=FALSE,
          tcl=-0.1,
          cex.yaxis=0.7,
          cex.xlabel=0.8,
          xRight = 0.7,
          plot.line=FALSE
          )

riojaPlot2(rp, mag, mag.chron[, "Age BP", drop=FALSE], xRight=0.8, scale.minmax=FALSE, 
           gap=0.02, plot.bar=FALSE, plot.line=F, plot.symb=TRUE, symb.cex=0.3)
riojaPlot2(rp, loi, loi.chron[, "Age BP", drop=FALSE], xLeft=0.8, xRight=0.9, 
           scale.minmax=FALSE, plot.bar=F, plot.line=F, plot.symb=TRUE, symb.cex=0.3)
riojaPlot2(rp, BSi, BSi.chron[, "Age BP", drop=FALSE], xLeft=0.9, xRight=0.99, 
           scale.minmax=FALSE, plot.bar=FALSE, plot.line=FALSE, plot.symb=TRUE, symb.cex=0.3)


## ----c9A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE, eval=TRUE----
fun.gam <- function(x, y, i, nm) {
  tmp <- data.frame(x=y, y=x)
  gam <- mgcv::gam(y ~ s(x, k=50), data=tmp)
  x2 <- predict(gam, type="response")
  lines(x2, y, col="blue", lwd=1)
}

rp <- riojaPlot(mag, mag.chron[, "Age BP", drop=FALSE], 
          yinterval = 5000,
          ymin = 0, 
          ymax=102000,
          xRight=0.3, 
          scale.minmax=FALSE, 
          plot.bar=FALSE, 
          plot.line=F, 
          plot.symb=TRUE, 
          plot.poly=FALSE, 
          symb.cex=0.3,
          fun2=fun.gam)

riojaPlot2(rp, loi, loi.chron[, "Age BP", drop=FALSE], xLeft=0.3, xRight=0.5, 
           scale.minmax=FALSE, plot.bar=F, plot.line=F, plot.symb=TRUE, 
           symb.cex=0.3, fun2=fun.gam)
riojaPlot2(rp, BSi, BSi.chron[, "Age BP", drop=FALSE], xLeft=0.5, xRight=0.7, 
           scale.minmax=FALSE, plot.bar=FALSE, plot.line=FALSE, plot.symb=TRUE, 
           symb.cex=0.3, fun2=fun.gam)

## ----FAQ1A, eval=FALSE--------------------------------------------------------
#  data.reordered <- data %>% select(var3, var1, var5) # and so on...

## ----FAQ1B, eval=FALSE--------------------------------------------------------
#  myorder <- c("var3", "var1", "var5") # and so on...
#  riojaPlot(spec, chron, selVArs=myorder)

## ----FAQ1C, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
riojaPlot(aber.poll, aber.chron, 
          scale.percent=TRUE,
          wa.order="bottomleft")

## ----FAQ1D, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
# these data only contain 41 most common taxa, so recalculate abundances to sum to 100 percent
RLGH.diat <- RLGH$spec
RLGH.diat <- vegan::decostand(RLGH.diat, method="total") * 100

optima <- SWAP.wa$coefficients %>% 
  data.frame() %>%
  tibble::rownames_to_column(var="CODE") 

optima2 <- data.frame(CODE=colnames(RLGH.diat)) %>% 
  dplyr::left_join(optima, by="CODE") %>%
  mutate(Group=case_when(Optima <= 5.1 ~ "Acidobiont.",
                         Optima > 5.1 & Optima < 5.4 ~ "Acidophil.", 
                         TRUE ~ "Acid intol."), 
         Group=factor(Group, levels=c("Acid intol.", "Acidophil.", "Acidobiont."))) %>%
  mutate(Optima=ifelse(is.na(Optima), 5.5, Optima)) %>%
  arrange(desc(Optima)) %>%
  select(CODE, Group)

RLGH.diat <- RLGH.diat %>% select(optima2$CODE)
mx <- apply(RLGH.diat, 2, max)
RLGH.sel <- colnames(RLGH.diat)[mx > 3]

rp <- riojaPlot(RLGH.diat, RLGH.chron, groups=optima2,
          yvar.name="Year",
          scale.percent=TRUE,
          y.rev=FALSE,
          selVars=RLGH.sel,
          ymax=1980, 
          yinterval=10,
          x.names=RLGH$names$TaxonName, 
          names.italicise=TRUE,
          cex.xlabel=0.7, 
          plot.poly=FALSE,
          plot.line=FALSE,
          lwd.bar=3,
          xRight=0.9,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          group.col1="mediumblue",
          group.col2="darkgrey",
          group.col3="red2",
          cumul.mult=0.3
          )

riojaPlot2(rp, RLGH.pH, RLGH.chron[, "Year", drop=FALSE],
           scale.minmax=FALSE,
           plot.bar=FALSE, 
           plot.symb=TRUE,
           symb.cex=0.6)

## ----FAQ5, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
data(aber)
aber.poll <- aber$spec
colnames(aber.poll) <- aber$names$Name 
aber.chron <- aber$ages

riojaPlot(aber.poll, aber.chron, 
   scale.percent=TRUE,
   min.width.pc=10,
   plot.top.axis=TRUE)

## ----FAQ6, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
aber.poll <- aber$spec
colnames(aber.poll) <- aber$names$Name 
aber.chron <- aber$ages
mx <- apply(aber.poll, 2, max)
exag.sel <- mx < 5

riojaPlot(aber.poll, aber.chron, 
   scale.percent=TRUE,
   exag=exag.sel,
   exag.mult=10,
   min.width.pc=10)

## ----FAQ7A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
fpath <- system.file("extdata/maule2020geochem.txt", package="rioja")
maule <- readr::read_delim(fpath, skip=158, show_col_types = FALSE) 

maule.data <- maule %>% select(-(1:4))
maule.chron <- maule %>% select(1:4)

# How many measurements do we have for each variable:
unlist(lapply(maule.data, function(x) sum(!is.na(x))))

selsymb <- rep(FALSE, 9)
selsymb[c(1, 8:9)] <- TRUE
selbar <- rep(FALSE, 9)
selline <- rep(TRUE, 9)
selline[1] <- FALSE

rp <- riojaPlot(maule.data, maule.chron, 
          yvar.name="age_BP", 
          ymin=-100, 
          ymax=13300,
          yinterval=500,
          plot.bar=FALSE,
          plot.poly=FALSE,
          plot.line=selline,
          plot.symb=selsymb,
          plot.groups=TRUE,
          ylabPos=2,
          symb.cex=0.5,
          )

## ----FAQ7B, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
groups <- data.frame(names=colnames(maule.data), groups=c(1, 2, 2, 2, 2, 2, 2, 3, 3))
riojaPlot(maule.data, maule.chron, groups=groups,
          yvar.name="age_BP", 
          ymin=-100, 
          ymax=13300,
          yinterval=500,
          plot.bar=selbar,
          plot.poly=FALSE,
          plot.line=selline,
          plot.symb=selsymb,
          plot.groups=TRUE,
          ylabPos=2,
          symb.cex=0.5,
          group.col1="red"
          )

## ----FAQ8, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
myticks <- seq(0, 13000, by=500)
riojaPlot(maule.data, maule.chron, groups=groups,
          yvar.name="age_BP", 
          ymin=-100, 
          ymax=13300,
          plot.bar=selbar,
          plot.poly=FALSE,
          plot.line=selline,
          plot.symb=selsymb,
          plot.groups=TRUE,
          ylabPos=2,
          symb.cex=0.5,
          group.col1="red",
          ytks1=myticks
          )

## ----FAQ9, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
myticks <- seq(0, 13000, by=500)
rp <- riojaPlot(maule.data, maule.chron, groups=groups,
          yvar.name="age_BP", 
          ymin=-100, 
          ymax=13300,
          yinterval=500,
          group.col1="red",
          plot.bar=FALSE,
          plot.poly=FALSE,
          plot.line=selline,
          plot.symb=selsymb,
          plot.groups=TRUE,
          ylabPos=2.2,
          symb.cex=0.5,
          ytks1=myticks, 
          las.axis=2,
          yBottom=0.06
          )

## ----FAQ10, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
aber.poll <- aber$spec
colnames(aber.poll) <- aber$names$Name 
aber.chron <- aber$ages
# calculate of max of each column
mx <- apply(aber.poll, 2, max)
#create a logical vector which is TRUE for taxa with max < 5
sel <- mx < 5
# define a custom function to plot symbols 
symb.fun <- function(x, y, i, nm) {
   sel <- x > 0
   if (sum(sel) > 0) {
      points(rep(3, sum(sel)), y[sel], cex=0.4, pch=19)
   }
}

# create a list of functions of length equal to the number of columns in the data
funlist <- lapply(1:ncol(aber.poll), function(x) symb.fun)
# now set the elements of the list where we don;t want to plot symbols to NULL
funlist[!sel] <- list(NULL)

# plot silhouettes and lines for taxa > 5% and apply our function to the others
riojaPlot(aber.poll, aber.chron, 
   scale.percent=TRUE,
   plot.poly=!sel,
   plot.bar = !sel,
   plot.line=FALSE,
   lwd.bar=0.6,
   exag=TRUE,
   fun2=funlist)

## ----FAQ11, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
data(SWAP)
# reorder the rows in decreasing pH
o <- order(SWAP$pH, decreasing=TRUE)
swap.diat <- SWAP$spec[o, ]
swap.pH <- data.frame(pH=SWAP$pH[o], LakeCode=rownames(SWAP$spec)[o]) 
# replace taxon codes with names
colnames(swap.diat) <- SWAP$names[, "TaxonName"]
# remove rare species
mx <- apply(swap.diat, 2, max)
swap.sel <- colnames(swap.diat)[mx > 20]

# this will plot symbols at zero abundances.  
# To stop this we write a custom function to plot the symbols

myfun <- function(x, y, i, nm) {
   x[x > 0.1] <- NA
   points(x, y, cex=0.4, pch=19)
}

riojaPlot(swap.diat, swap.pH, selVars=swap.sel,
          yvar.name="LakeCode",
          sec.yvar.name="pH",
          sec.yinterval = 0.1,
          plot.sec.axis=TRUE,
          scale.percent=TRUE, 
          plot.poly=FALSE,
          plot.line=FALSE,
          plot.bar=FALSE,
          plot.symb=TRUE,
          lwd.bar = 2,
          symb.cex=0.4,
          cex.xlabel=0.6, 
          cex.yaxis=0.3,
          wa.order="topleft", 
          fun1=myfun,
          names.italicise=TRUE,
          las.axis=2, 
          cex.axis=0.5)

## ----FAQ12, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
swap.pH2 <- swap.pH %>%  mutate(bar.cols=case_when(pH < 5 ~ "orange",
                            pH >=5 & pH < 5.5 ~ "red",
                            pH >= 5.5 & pH < 6.0 ~ "blue",
                            TRUE ~ "green")) 

nms <- rep("", ncol(swap.diat))

riojaPlot(swap.diat, swap.pH, selVars=swap.sel,
          yvar.name="LakeCode",
          sec.yvar.name="pH",
          sec.yinterval = 0.1,
          plot.sec.axis=TRUE,
          scale.percent=TRUE, 
          plot.poly=FALSE,
          plot.line=FALSE,
          plot.bar=TRUE,
          sep.bar=TRUE,
          lwd.bar = 2,
          col.sep.bar=swap.pH2$bar.cols,
          symb.cex=0.4,
          cex.xlabel=0.6, 
          cex.yaxis=0.3,
          wa.order="bottomleft", 
          names.italicise=TRUE,
          las.axis=1, 
          cex.axis=0.5)

## ----FAQ13A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
fpath <- system.file("extdata/LochChon.xlsx", package="rioja")
chon <- readxl::read_excel(fpath, sheet="Diatoms")
sig_test <- readxl::read_excel(fpath, sheet="Sig_test")

chon.diat <- chon %>% select(-Year)
chon.year <- chon %>% select(Year)
# data has many rare species, remove these first
mx <- apply(chon.diat, 2, max)
chon.diat <- chon.diat[, mx > 3]

# Create a grouping variable based on significance of trend (increasing, decreasing or not sig.)
chon.groups <- data.frame(TaxonName=colnames(chon.diat)) %>% 
  left_join(sig_test, by="TaxonName") %>%
  mutate(group = case_when(p_unadj < 0.1 & slope>0 ~ "Increasing",
                           p_unadj < 0.1 & slope<0 ~ "Decreasing",
                           TRUE ~ "Not sig"), 
         group=factor(group, levels=c("Increasing", "Not sig", "Decreasing"))) %>%
  select(TaxonName, group)

# Sort variables according to significance
diat.order <- chon.groups %>% arrange(group) %>% pull(TaxonName)

chon.ordered <- chon.diat %>% select(!!diat.order)
riojaPlot(chon.ordered, chon.year, groups=chon.groups,
          scale.percent=TRUE, 
          yinterval=1, 
          plot.poly=TRUE,
          plot.line=FALSE,
          plot.groups=TRUE,
          cex.xlabel=0.6, 
          cex.yaxis=0.5,
          names.italicise=TRUE,
          cex.axis=0.5, 
          group.col3="darkred",
          group.col2="darkgrey")

## ----FAQ13B, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----

# create list of blank names to suppress taxon names

names <- rep("", ncol(chon.ordered))

myfun <- function(x, y, i, nm) {
  usr <- par("usr") # extract the x and y data limits of the plot
  name <- bquote(italic(.(names(nm)))) #  extract name from nm and italicise it
  text(usr[2]-2, usr[4]+0.5, name, adj=c(0, 0), xpd=NA, srt=-90, cex=0.7)
  xval <- seq(0, usr[2], by=10) # create a vector of labels for the axis
  xlab <- rep("", length(xval))
  axis(side=3, at=xval, labels=xlab, tcl=-0.2, cex.axis=0.5, mgp=c(3, 0.1, 0))
  text(xval, usr[4]-0.5, xval, cex=0.5, srt=-90, adj=c(1, 0), xpd=NA) # ad the x-axis vales
}

riojaPlot(chon.ordered, chon.year, groups=chon.groups,
          scale.percent=TRUE, 
          yinterval=1, 
          x.names=names, # replace taxon names with a vector of blank names
          ylabel = " ",  # suppress y-axis label
          plot.poly=TRUE,
          plot.line=FALSE,
          plot.groups=TRUE,
          cex.xlabel=0.6, 
          cex.yaxis=0.5,
          names.italicise=TRUE,
          cex.axis=0.5, 
          group.col3="darkred",
          group.col2="darkgrey", 
          plot.bottom.axis=FALSE,
          min.width.pc=10, # set minimum size of x-axes
          fun1=myfun)

## ----FAQ14A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE----
mx <- apply(aber.poll, 2, max)
aber.sel <- names(mx[mx > 2])

rp <- riojaPlot(aber.poll, aber.chron, aber.sel,
   yvar.name="Age (years BP)",
   ymin=6000,
   ymax=14300,
   yinterval=500,
   scale.percent=TRUE,
   exag=TRUE)

# define zone lines at 11000 and 13000 years BP
myzones <- c(11000, 13000)
addRPZone(rp, myzones, col="red")

# add shaded zone from 6000 to 8000 years BP
addRPZone(rp, 6000, 8000)

# change colour and shading
addRPZone(rp, 13000, 13500, col="blue", alpha = 0.05)

## ----c99, fig.width=8, fig.height=4.5, warning=FALSE, echo=FALSE, ft.align="left", message=FALSE----
if (require(flextable) & require(dplyr)) {
ft <- flextable::flextable(listStyles()) %>% 
  flextable::autofit() %>% 
  flextable::height_all(2, unit="mm") %>%
  flextable::padding(padding=1)
ft
}

