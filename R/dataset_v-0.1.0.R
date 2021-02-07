#' @title coordinates data
#'
#' @description A data containing the nodes coordinates of uhat.raster and v.raster.
#' @usage data(coordinates,"VARMER")
#' @format A data frame with 4340 rows and 3 columns (x y and index).
"coordinates"


#' @title elements data
#'
#' @description A table of elements
#' @usage data(elements,"VARMER")
#' @format A data frame with 8418 rows and 3 columns index1 index2 index3.
"elements"

#' @title U data
#'
#' @description A data frame containing the data to make a corrected raster by the Variational Merging method.
#' @usage data(U,"VARMER")
#' @format A data frame with 4320 rows and 3 columns x y and data
"U"

#' @title uhat data
#'
#' @description A data frame containing the data present in the raster uhat.raster.
#' @usage data(uhat,"VARMER")
#' @format A data frame with 4340 rows and 3 columns x and y are column 1 and 2 and data is the next column.
"uhat"

#' @title uhat.raster data
#'
#' @description A raster layer containing temperature values of interpolated stations
#' @usage data(uhat_raster,"VARMER")
#' @format A raster with 70 rows and 62 columns with UTM 17S as CRS.
"uhat.raster"

#' @title v data
#'
#' @description A data frame containing the data present in the raster v.raster.
#' @usage data(v,"VARMER")
#' @format A data frame with 4340 rows and 3 columns x and y are column 1 and 2 and data is the next column.
"v"

#' @title v.raster data
#'
#' @description A raster layer containing temperature values of a satellite image
#' @usage data(uhat_raster,"VARMER")
#' @format A raster with 70 rows and 62 columns with UTM 17S as CRS.
"v.raster"


