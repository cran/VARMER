#'@title Variational merging
#'
#'@description Apply a function to the interpolated and satellite image (These image are raster, see \link[raster]{raster}).
#'The function is based on many functions, these are applied in the following order:
#'
#'\link[VARMER]{img2varmer} Compare two raster or bricks with different z values(uhat and v images)
#'It check if the images has the same extension, resolution, coordinate reference system, columns and rows.
#'
#'\link[VARMER]{pre_data} Using a variable in the needed format the unique coordinates.
#'The function calculates necessary variables for mesh_3.
#'
#'\link[VARMER]{mesh_3} The function mesh_3 takes as input data the length of the domain in x (Lx), the length of the domain in y (Ly), the number of divisions in x (Nx) and the number of divisions in y (Ny).
#'These values are obtained with the function prev_data, specifically from coordinates  inside the variable uhat (which must be equal to those of v).
#'
#'\link[VARMER]{var_merge} It takes as input the coordinates of the nodes (variable coordinates defined in the mesh_3 function).
#'The table of elements (variable elements defined in the mesh_3 function).
#'The fields uhat and v, and the factor eta_0.
#'
#'\link[VARMER]{plot_mesh} Using the data of previous functions (mesh_3 and var_merge), three 3D graphs are realized.
#'Each graph represent in order the variation through the space (longitude and latitude) U,uhat and v.
#'
#'@param uhat.img The interpolated image as a raster.
#'
#'@param v.img The satellite image as a raster.
#'
#'@param eta_0 A scalar
#'
#'@param color_plot The name of a palette of color from grDevices
#'
#'
#'@return Three 3D plots corresponding to the variation of U, uhat and v in the space.
#'A list containing initial data, preparation data and U.
#'Inside of U there are two files U_varmer is a data frame and U_img a raster.
#'
#'@examples
#'\donttest{
#'library(raster)
#'data(uhat_raster,"VARMER")
#'force(uhat.raster)
#'data(v_raster,"VARMER")
#'force(v.raster)
#'eta_0=0.5
#'color_plot=topo.colors
#'varmer_results=varmer(uhat.raster,v.raster,eta_0,color_plot)
#'}
#'@export

varmer=function(uhat.img,v.img,eta_0,color_plot){
  img_results=img2varmer(uhat.img,v.img)
  uhat=img_results[[1]]
  v=img_results[[2]]
  pre_results=pre_data(uhat)
  coordsx=pre_results[[1]][[1]]
  coordsy=pre_results[[1]][[2]]
  Lx=pre_results[[2]]
  Ly=pre_results[[3]]
  Nx=pre_results[[4]]
  Ny=pre_results[[5]]
  #coordinates y elements
  coordinates=mesh_3(Lx,Ly,Nx,Ny)[[1]]
  elements=mesh_3(Lx,Ly,Nx,Ny)[[2]]
  U=var_merge(coordinates, elements, uhat[,3:ncol(uhat)], v[,3:ncol(v)], eta_0)
  plot_mesh(U,uhat,v,color_plot,eta_0)
  U.img=raster::rasterFromXYZ(U,crs = raster::crs(uhat.img))
  varmer_results=list(initial_data=list(uhat.img=uhat.img,v.img=v.img,uhat=uhat,v=v,eta_0=eta_0,color_plot=color_plot),pre_results=pre_results,coordinates=coordinates,elements=elements,U=list(U=U,U.img=U.img))
}
