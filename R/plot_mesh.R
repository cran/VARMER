#'@title Visualization of the curves
#'
#'@description Using the data of previous functions (mesh_3 and var_merge), three 3D graphs are realized.
#'Each graph represent in order the variation through the space (longitude and latitude) U,uhat and v.
#'
#'@param U A data frame representing resulting images in relative coordinates.
#'
#'@param uhat A matrix containing values from the interpolated image with x  y and data and the next column structure
#'
#' x y date1 date2 date3
#'
#'@param v A matrix containing values from the satellite/model based image.
#'
#'The values of U, uhat and v should correspond to the same pixels in the coordinates matrix.
#'
#'@param color_plot A palette of colors from grDevices.
#'
#'@param eta_0 A scalar representing length scale constant.
#'
#'@return Three 3D plots representing the variation
#'
#'@examples
#'\donttest{
#'data(U,"VARMER")
#'invisible(force(U))
#'data(uhat,"VARMER")
#'invisible(force(uhat))
#'data(v,"VARMER")
#'invisible(force(v))
#'eta_0=0.5
#'plot_mesh(U,uhat,v,topo.colors,eta_0)
#'}
#'@importFrom rgl par3d open3d close3d
#'@importFrom deldir deldir
#'@export
plot_mesh=function(U,uhat,v,color_plot,eta_0){
  #a function is created to have an absolute color scale
  zetas=matrix(c(U[,3],uhat[,3],v[,3]),ncol=3)
  paleta=color_plot(n=nrow(zetas))
  map2color <- function(zeta, paleta, zetas){
    limits = range(zetas)
    paleta[findInterval(zeta, seq(limits[1], limits[2], length.out = length(paleta) + 1),
                        all.inside=TRUE)]
  }

  zetas_col=sapply(1:ncol(zetas),function(j) map2color(zetas[,j],paleta,zetas = zetas))
  oldpar3d=rgl::par3d(no.readonly = TRUE)
  on.exit(rgl::par3d(oldpar3d))
  on.exit(rgl::close3d())
  rgl::par3d(windowRect = c(20, 30, 800, 800))
  rgl::plot3d(deldir::deldir(x =  uhat[,1], y =  uhat[,2], z =  zetas[,1]),col=zetas_col[,1],main=paste("U with eta_0=",eta_0,sep=""))
  rgl::open3d()
  rgl::par3d(windowRect = c(20, 30, 800, 800))
  rgl::plot3d(deldir::deldir(x =  uhat[,1], y =  uhat[,2], z =  zetas[,2]),col=zetas_col[,2],main=paste("uhat with eta_0=",eta_0,sep=""))
  rgl::open3d()
  rgl::par3d(windowRect = c(20, 30, 800, 800))
  rgl::plot3d(deldir::deldir(x =  uhat[,1], y =  uhat[,2], z =  zetas[,3]),col=zetas_col[,3],main=paste("v with eta_0=",eta_0,sep=""))
  rgl::open3d()
  }
