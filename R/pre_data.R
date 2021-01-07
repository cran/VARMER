#' @title Obtain necessary data to the package.
#'
#' @description  Using a variable in the needed format the unique coordinates.
#'
#' The function calculates necessary variables for mesh_3.
#'
#' @param uhat A matrix containing values from the interpolated image with x  y and data and the next column structure
#'
#' x y date1 date2 date3
#'
#' @return A list with vector data to use later with other functions of the package (Lx,Ly,Nx,Ny).
#'
#'@examples
#'data(uhat,"VARMER")
#'invisible(force(uhat))
#'pre_results=pre_data(uhat)
#'@export
pre_data= function(uhat){
coordsx=unique(uhat[,1])
coordsy=unique(uhat[,2])
Lx=tail(coordsx, n=1)-head(coordsx, n=1)
Ly=tail(coordsy, n=1)-head(coordsy, n=1)
Nx=length(coordsx)-1
Ny=length(coordsy)-1
pre_results=list(coords=list(x=coordsx,y=coordsy),Lx=Lx,Ly=Ly,Nx=Nx,Ny=Ny)
}
