#' @title Ground-based observations of maximum temperature for Ecuador
#' @description A zoo data frame containing time series for january 2004
#' on 34 meteorological stations located in continental Ecuador.
#' @usage data(ecuador.tmax.zoo)
#' @format A zoo object with 34 columns (one for each station) and 10 rows (one for each day in jan-2004).
"ecuador.tmax.zoo"


#' @title Spatial location of meteorological stations in Ecuador
#' @description Spatial location of the 34 stations with daily maximum temperature for Ecuador (dataset ecuador.tmax.zoo).
#' @usage data(ecuador.tmax.stations.df)
#' @format A data.frame with eight fields:
#' *) CODIGO        : identifier of each station
#' *) NOMBRE        : station name
#' *) CUENCA_INAMHI : basin name
#' *) PROVINCIA     : province name
#' *) CANTON        : canton name
#' *) PARROQUIA     : parish name
#' *) LAT           : northing coordinate of the station, EPSG:4326
#' *) LON           : easting coordinate of the station, EPSG:4326
"ecuador.tmax.stations.df"

#' @title WRF (model-based) output for maximum temperature
#'
#' @description Model-based maximum temperature datasets providing global spatial structure
#' @usage data(ecuador.tmax.wrf.out)
#' @format A RasterBrick frame with 10 layers (1 for each day), and a geographical area of 44x47 for a 10Km spatial resolution
"ecuador.tmax.wrf.out"



