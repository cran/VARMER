#'@title Compare two images
#'@description The function compare two raster or bricks with different z values(uhat and v images)
#'It check if the images has the same extension, resolution, coordinate reference system, columns and rows.
#'
#'@param uhat.img The interpolated image as a raster or brick.
#'
#'@param v.img The satellite image as a raster or brick.
#'
#'@return
#'If the images have the same extension, resolution, coordinate reference system, columns and rows.
#'
#'A list containing two data frames (uhat and v) in the necessary format to varmer function (x,y,layer1,layer2).
#'
#'If the images has not the same values an error message is returned.
#'
#'@examples
#'library(raster)
#'data(uhat_raster,"VARMER")
#'force(uhat.raster)
#'data(v_raster,"VARMER")
#'force(v.raster)
#'img2varmer(uhat.raster,v.raster)
#'@importFrom raster compareRaster rasterToPoints
#' @export

img2varmer=function(uhat.img,v.img){

  if(class(uhat.img)[1]=="RasterLayer" & class(uhat.img)[1]=="RasterLayer"){
    comparation=raster::compareRaster(uhat.img,v.img,extent = T,crs=T,rowcol = T,res = T)
    if(comparation==TRUE){
      uhat=data.frame(raster::rasterToPoints(uhat.img))
      uhat= uhat[order(uhat[,2]),]
      colnames(uhat)=NULL
      rownames(uhat)=NULL
      v=data.frame(raster::rasterToPoints(v.img))
      v= v[order(v[,2]),]
      rownames(v)=NULL
      colnames(v)=NULL
      img_results=list(uhat=uhat,v=v)
    }else{
      stop("Extent or CRS or number of rows or number cols are not equal. Please check this values.")
    }
  }else{
    stop("uhat.img or v.img are not rasters layers. Please check this files.")
  }
}
