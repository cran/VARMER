## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----Install_Stable, eval = FALSE---------------------------------------------
#  install.packages("VARMER")

## ----Loading_other_pks, eval = TRUE, message=FALSE----------------------------
library(zoo)
library(sf)
library(raster)
library(RFmerge)
library(tictoc)
library(cluster)
#library(ggplot2)
library(parallel)
library(VARMER)

## ----Loading_GroundObservarions, eval = TRUE----------------------------------
data(ValparaisoPPts)    
data(ValparaisoPPgis) 
data(ValparaisoSHP)  

## ----LoadingSatelliteData, eval = TRUE----------------------------------------
chirps.fname   <- system.file("extdata/CHIRPS5km.tif", package="RFmerge")
CHIRPS5km <- brick(chirps.fname)

## ----GivingMeaninfulNamesToLayers, eval = TRUE--------------------------------
ldates                <- seq(from=as.Date("1983-01-01"), to=as.Date("1983-08-31"), by="days")
names(CHIRPS5km)      <- ldates

## ----ExploringMetadata--------------------------------------------------------
head(ValparaisoPPgis)

## ----PlottingPts, fig.width = 7, fig.height = 3.5, fig.align = "center", message=FALSE----
main <- paste("Daily precipitation for the station", ValparaisoPPgis$Code[1])
ylab <- "Precipitation [mm]"
x.ts <- ValparaisoPPts[,1]

plot(x.ts, main=main, ylab= ylab, col="blue")
grid()

## ----PlotingTotalP, fig.width= 4, fig.height = 4, fig.align = "center", message=FALSE----
chirps.total   <- sum(CHIRPS5km, na.rm= FALSE)

plot(chirps.total, main = "CHIRPSv2 [Jan - Aug] ", xlab = "Longitude", ylab = "Latitude")
plot(ValparaisoSHP[1], add=TRUE, col="transparent")

## ----RFmergeWithoutParallelisation, eval = FALSE------------------------------
#  varmer.ts(x=ValparaisoPPts, x.metadata=ValparaisoPPgis,
#            v=CHIRPS5km, lat='lat', lon='lon',
#            drty.out="~/Documentos/dataset_valparaiso/")

## ----SoftwareDetails, echo=FALSE, eval = TRUE---------------------------------
sessionInfo()$platform
sessionInfo()$R.version$version.string 
paste("VARMER", sessionInfo()$otherPkgs$VARMER$Version)

