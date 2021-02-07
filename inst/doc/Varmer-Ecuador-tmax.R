## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----Install_Stable, eval = FALSE---------------------------------------------
#  install.packages("VARMER")

## ----Loading_other_pks, eval = TRUE, message=FALSE----------------------------
library(zoo)
library(sf)
library(raster)
library(tictoc)
library(cluster)
library(parallel)
#library(ggplot2)
library(VARMER)

## ----Loading_GroundObservarions, eval = TRUE----------------------------------
data(ecuador.tmax.zoo)
data(ecuador.tmax.stations.df)

## ----LoadingSatelliteData, eval = TRUE----------------------------------------
data(ecuador.tmax.wrf.out)

## ----ExploringMetadata--------------------------------------------------------
head(ecuador.tmax.stations.df)

## ----PlottingPts, fig.width = 7, fig.height = 3.5, fig.align = "center", message=FALSE----
main <- paste("Daily maximum temperature for the station", ecuador.tmax.stations.df$CODIGO[1])
ylab <- "Maximum temperature [°C]"
x.ts <- ecuador.tmax.zoo[,1]

plot(x.ts, main=main, ylab= ylab, col="blue")
grid()

## ----PlotingTotalP, fig.width= 4, fig.height = 4, fig.align = "center", message=FALSE----
wrfout.total <- mean(ecuador.tmax.wrf.out, na.rm=FALSE)

plot(wrfout.total, main = "WRF output [Jan-2004] ", xlab = "Longitude", ylab = "Latitude")

## ---- eval = FALSE------------------------------------------------------------
#  varmer.ts(x=ecuador.tmax.zoo, x.metadata=ecuador.tmax.stations.df,
#            v=ecuador.tmax.wrf.out, lat='LAT', lon='LON',
#            drty.out="~/Documentos/dataset_ecuador")

## ----SoftwareDetails, echo=FALSE, eval = TRUE---------------------------------
sessionInfo()$platform
sessionInfo()$R.version$version.string 
paste("VARMER", sessionInfo()$otherPkgs$VARMER$Version)

