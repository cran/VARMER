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
#'@param v.img The satellite image as a raster.
#'@param eta_0 A scalar
#'@param color_plot The name of a palette of color from grDevices
#'@param create_plots Flag defining whether plots are to be created
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

varmer=function(uhat.img,v.img,eta_0,
                color_plot=grDevices::heat.colors, create_plots = F){
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
  if(create_plots) plot_mesh(U,uhat,v,color_plot,eta_0)

  U.img=raster::rasterFromXYZ(U,crs = raster::crs(uhat.img))
  raster::extent(U.img) = raster::extent(v.img)
  varmer_results=list(initial_data=list(uhat.img=uhat.img,v.img=v.img,uhat=uhat,v=v,eta_0=eta_0,color_plot=color_plot),pre_results=pre_results,coordinates=coordinates,elements=elements,U=list(U=U,U.img=U.img))
}


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



#'@title Generates triangular finite element mesh.
#'
#'@description The function mesh_3 takes as input data the length of the domain in x (Lx), the length of the domain in y (Ly), the number of divisions in x (Nx) and the number of divisions in y (Ny).
#'
#'These values are obtained with the function prev_data, specifically from coordinates  inside the variable uhat (which must be equal to those of v).
#'
#'@param Lx,Ly,Nx,Ny a group of vectors obtained with the function prev_data.
#'
#' The coordinates of the nodes (coordinates) and the table of elements (elements).
#'
#' Following the next structure in columns
#'
#' coordinates: x, y, index
#'
#' elements: index1, index2, index3
#'
#' A plot with triangles from the coordinates
#'
#'@examples
#'\donttest{
#'data(uhat,"VARMER")
#'invisible(force(uhat))
#'pre_results=pre_data(uhat)
#'coordsx=pre_results[[1]][[1]]
#'coordsx=pre_results[[1]][[2]]
#'Lx=pre_results[[2]]
#'Ly=pre_results[[3]]
#'Nx=pre_results[[4]]
#'Ny=pre_results[[5]]
#'mesh_results=mesh_3(Lx,Ly,Nx,Ny)
#'}
#'@importFrom grDevices recordPlot
#'@importFrom utils head tail
#'@importFrom graphics par plot points polygon
#'@importFrom pracma linspace
#' @export
mesh_3=function(Lx,Ly,Nx,Ny){
  NE=(Nx*Ny*2)
  m=1
  j=c(1:Nx)
  k=pracma::linspace(Nx*2,NE,Ny)
  #elementos, se crea una matriz y se acomodan los valores j
  elements3=matrix(NA,nrow = tail(k,n=1),3)
  for (i in 1:Ny) {
    elements3[seq(m,k[i],2),1]=j #node 1 of 1st element
    elements3[seq(m+1,k[i],2),1]=j #node 1 of 2nd element
    elements3[seq(m,k[i],2),2]=j+1# node 2 of 1st element
    elements3[seq(m+1,k[i],2),2]=j+Nx+2# node 2 of 2nd element
    elements3[seq(m,k[i],2),3]=j+Nx+2 #node 3 of 1st element
    elements3[seq(m+1,k[i],2),3]=j+1+Nx #node 3 of 1st element
    m=k[i]+1
    j=j+Nx+1
  }
  #xy coordenadas
  ax=pracma::linspace(0,Lx,Nx+1) #x coordinates
  by=pracma::linspace(0,Ly,Ny+1) #y coordinates
  X1=character()
  Y1=character()
  by1=character()
  for (i1 in 1:(Ny+1)) {
    #General Nodal Coordinates layer by layer
    by1[1:Nx+1]=as.numeric(by[i1])
    by1=as.numeric(append(by1[1:Nx+1],by[i1],after = length(by1[1:Nx+1])))
    X1=as.numeric(append(X1,ax,after = length(X1)))
    Y1=as.numeric(append(Y1,by1,after = length(Y1)))
  }
  X=character()
  Y=character()
  j=1:3
  for(n in 1:NE){
    X[j]=X1[elements3[n,]]
    Y[j]=Y1[elements3[n,]]
    j=j+3
  }
  X=as.numeric(X)
  Y=as.numeric(Y)
  coordinates=data.frame(X=X,Y=Y) #x and y coordinates
  #z coordinates
  z=1
  #TODO: quitar plots
  #plot(coordinates,type="n")
  for(i in 1:NE){
    #oldpar <- par(no.readonly = TRUE)
    #on.exit(par(oldpar))
    #polygon(coordinates[z:(z+2),],border="green")
    z=z+3
  }
  #ploting=recordPlot()
  #ploting
  #oldpar <- par(no.readonly = TRUE)    # code line i
  #on.exit(par(oldpar))            # code line i + 1
  #points(coordinates[,1],coordinates[,2],col="blue",bg="red",pch=21)
  #ploting_2=recordPlot()
  coordinates[,3]=rep(NA,nrow(coordinates))
  colnames(coordinates)[3]="elements"
  for(i in 1:NE){
    coordinates[(3*i-2):(3*i),3]=elements3[i,]
  }
  coordinates=unique(coordinates)
  coordinates=coordinates[order(coordinates$elements),]
  rownames(coordinates)=NULL
  elements3=data.frame(elements3)
  #resultados, al parecer la ultima linea se exporta
  results=list(coordinates=coordinates,elements=elements3)
}


#'@title Solve the variational problem by the finite element method.
#'
#'@description It takes as input the coordinates of the nodes (variable coordinates defined in the mesh_3 function).
#'The table of elements (variable elements defined in the mesh_3 function).
#'The fields uhat and v, and the factor eta_0
#'
#'Note that columns of uhat and v are arranged as follows:
#'
#'date1 date2 date3
#'
#'And performs the variational merging technique to uhat and v using the finite.
#'
#'uhat and v have to be introduced like in the example, only with data
#'
#'@param coordinates A data frame with Node definitions in relative coordinates.
#'
#'@param elements A data frame containing element definitions and nodes should be ordered counterclockwise.
#'
#'@param uhat A matrix containing values from the interpolated image with x  y and data and the next column structure
#'
#' x y date1 date2 date3
#'
#' @param v A matrix containing values from the satellite/model based image.
#'
#' The values of uhat and v should correspond to the same pixels in the coordinates matrix.
#'
#'@param eta_0 A scalar representing length scale constant.
#'
#'@return A data frame representing resulting images in relative coordinates.
#'
#'@examples
#'\donttest{
#'data(uhat,"VARMER")
#'invisible(force(uhat))
#'data(v,"VARMER")
#'invisible(force(v))
#'data(coordinates,"VARMER")
#'invisible(force(coordinates))
#'data(elements,"VARMER")
#'invisible(force(elements))
#'eta_0=0.5
#'U=var_merge(coordinates, elements, uhat[,3:ncol(uhat)], v[,3:ncol(v)], eta_0)
#'}
#'@export

var_merge=function(coordinates, elements, uhat, v, eta_0){
  #VARMERGE
  #Performs the variational merging technique to uhat and v using the finite
  #element method.
  #   U    = varmerge(coordinates, elements, uhat, v, eta_0)
  #
  #  coordinates      Node definitions in relative coordinates          [x y NodID] (nPixels * 3)
  #  elements         Element definitions                               [n1 n2 n3] (nElem * 3)
  #                   Nodes should be ordered counterclockwise:
  #
  #             n3
  #             | \
  #             |  \
  #             |   \
  #             |    \
  #             n1---n2
  #
  #  uhat             Values from the interpolated image (nPixels * nTimes)
  #  v                Values from the satellite/model based image (nPixels * nTimes)
  #                   The values of uhat and v should correspond to the same
  #                   pixels in the coordinates matrix.
  #  eta_0            Length scale constant
  #  U                Resulting images in relative coordinates (nPixels * nTimes)

  #Jacinto Ulloa
  #2018

  gp=c(1/3,1/3)
  s=gp[1]
  t=gp[2]
  w=1/2
  #Define shape functions for N and B matrices.
  N1=s
  N2=t
  N3=1-s-t
  N=matrix(c(N1,N2,N3),ncol = 3)
  K=matrix(0,nrow = nrow(coordinates), ncol = nrow(coordinates))
  M=matrix(0,nrow = nrow(coordinates), ncol = nrow(coordinates))

  # Assembly
  for(j in 1:nrow(elements)){
    # Define the nodal coordinates.
    vertices=coordinates[as.vector(t(elements[j,])),]
    # Obtain the Jacobian for integrating in natural coordinates.
    J = rbind(c(vertices[1,1]-vertices[3,1], vertices[1,2]-vertices[3,2]),
              c(vertices[2,1]-vertices[3,1], vertices[2,2]-vertices[3,2]))
    # Obtain the N' matrices.
    b1 = vertices[2,2]-vertices[3,2]
    c1 = vertices[3,1]-vertices[2,1]
    b2 = vertices[3,2]-vertices[1,2]
    c2 = vertices[1,1]-vertices[3,1]
    b3 = vertices[1,2]-vertices[2,2]
    c3 = vertices[2,1]-vertices[1,1]
    B = matrix((1/det(J))*rbind(c(b1,b2,b3),c(c1,c2,c3)),ncol = 3)
    # Stiffness matrix.
    # Define the function to integrate.
    Ke =t(B)%*%B
    Ke = Ke*det(J)/2
    g =t(N)%*%N
    # Perform Guass integration to compute the local stiffness matrix.
    Me = w[1]*g
    Me = Me*det(J)
    # Assemble the global stiffness matrix.
    K[as.vector(t(elements[j,])),as.vector(t(elements[j,]))]=K[as.vector(t(elements[j,])),as.vector(t(elements[j,]))]+Ke
    M[as.vector(t(elements[j,])),as.vector(t(elements[j,]))]=M[as.vector(t(elements[j,])),as.vector(t(elements[j,]))]+Me
  }
  fact_v= (sum(diag(M),na.rm = T)/sum(diag(K)))^2
  Ntimes = ncol(v)
  # Initialize precipitation time history matrix.
  U=matrix(0, nrow = length(uhat), ncol = 1)
  b=matrix(0,nrow = nrow(coordinates),ncol=1)
  for(j in 1:nrow(elements)){
    # Define the function to integrate.
    vertices=coordinates[as.vector(t(elements[j,])),]
    # Obtain the Jacobian for integrating in natural coordinates.
    J = rbind(c(vertices[1,1]-vertices[3,1], vertices[1,2]-vertices[3,2]),
              c(vertices[2,1]-vertices[3,1], vertices[2,2]-vertices[3,2]))
    # Obtain the N' matrices.
    b1 = vertices[2,2]-vertices[3,2]
    c1 = vertices[3,1]-vertices[2,1]
    b2 = vertices[3,2]-vertices[1,2]
    c2 = vertices[1,1]-vertices[3,1]
    b3 = vertices[1,2]-vertices[2,2]
    c3 = vertices[2,1]-vertices[1,1]
    B = matrix((1/det(J))*rbind(c(b1,b2,b3),c(c1,c2,c3)),ncol = 3)
    v_e = v[as.vector(t(elements[j,]))]
    uhat_e = uhat[as.vector(t(elements[j,]))]
    g = (((eta_0^2)*fact_v)*(t(B)%*%(B))%*%v_e)+(as.vector(N%*%uhat_e)*t(N))
    S = w[1]*g
    be = S*det(J)
    # Assemble the global force matrix.
    b[as.vector(t(elements[j,]))] = b[as.vector(t(elements[j,]))] + be
  }
  # Solution with vanishing Neumann boundary conditions
  u = solve(((eta_0^2)*fact_v*K+M),b)
  U = u
  U=cbind(coordinates[,1:2],U)
}


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


