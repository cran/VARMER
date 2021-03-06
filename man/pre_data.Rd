% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varmer.R
\name{pre_data}
\alias{pre_data}
\title{Obtain necessary data to the package.}
\usage{
pre_data(uhat)
}
\arguments{
\item{uhat}{A matrix containing values from the interpolated image with x  y and data and the next column structure

x y date1 date2 date3}
}
\value{
A list with vector data to use later with other functions of the package (Lx,Ly,Nx,Ny).
}
\description{
Using a variable in the needed format the unique coordinates.

The function calculates necessary variables for mesh_3.
}
\examples{
data(uhat,"VARMER")
invisible(force(uhat))
pre_results=pre_data(uhat)
}
