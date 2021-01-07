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
