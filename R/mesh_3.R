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
  plot(coordinates,type="n")
  for(i in 1:NE){
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    polygon(coordinates[z:(z+2),],border="green")
    z=z+3
  }
  ploting=recordPlot()
  ploting
  oldpar <- par(no.readonly = TRUE)    # code line i
  on.exit(par(oldpar))            # code line i + 1
  points(coordinates[,1],coordinates[,2],col="blue",bg="red",pch=21)
  ploting_2=recordPlot()
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
