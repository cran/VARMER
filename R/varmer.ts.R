# varmer.ts ====================================================================
# This is a wrapper function to varmer main function, and allows to merge
# a satellite product with ground-based observations
#'@title Variational merging for time series
#'
#'@description It allows merging satellite-based or model-based grided images
#'(in raster format) with ground-based observed
#'time-series (in zoo format). This function is a wrapper for
#'the  function which allows to combine a product-based image with
#'an interpolated image from observations for a single time step.
#'
#'@param x data.frame with the ground-based observation in time series format.
#'Every column must represent one ground-based station and the codes of the stations
#'must be provided as colnames. class(data) must be zoo.
#'
#'@param x.metadata data.frame with the ground-based stations' metadata. At least,
#'it MUST have the following 3 columns:
#'id: This column stores the unique identifier (ID) of each ground observation. Default value is "id".
#'lat: This column stores the latitude of each ground observation. Default value is "lat".
#'lon: This column stores the longitude of each ground observation. Default value is "lon".
#'@param v The satellite-based or model-based gridded images
#'(in raster format, see \link[raster]{raster}). It can be a RasterStack or
#'RasterBrick object since it is supposed to containe multiple layers (time series).
#'@param lat A character string with the name of the column in x.metadata where the latitude of the stations is stored.
#'@param lon A character string with the name of the column in x.metadata where the longitude of the stations is stored.
#'@param drty.out A character string with the full path to the directory where the final merged products will be exported
#'@param verbose A logical which indicates if information messages are to be printed. By default \code{verbose=TRUE}
#'
#'@export
varmer.ts = function(x, x.metadata, v, lat='lat', lon='lon',
                     drty.out=tempdir(),
                     verbose=T){

  # Pre-processing and validations ----
  if(verbose) message("Start: ", Sys.time())

  # Replace null values to avoid problems with IDW and Varmer ***

  #v[is.na(v[])] = 0
  #x = zoo::na.fill(x, fill = 0)
  v[is.na(v[])] = mean(raster::values(v), na.rm = T)
  x = zoo::na.fill(x, fill = mean(x, na.rm=T))

  # Checking that 'x' is a zoo object
  if ( !zoo::is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo'!!")

  # Checking if the 'x.metadata' object provided is a data frame
  if (!is.data.frame(x.metadata) ) {
    stop("Invalid argument: 'x.metadata' must be a data frame!")
  } else if ( !(lat %in% colnames(x.metadata) ) |
              !(lon %in% colnames(x.metadata) )  )
    stop("Invalid argument: 'x.metadata' must have 'lon' (longitude) and 'lat' (latitude) fields!")

  # Checking that 'x' has as many time steps as 'v' has layers
  if ( nrow(x) != raster::nlayers(v) )
    stop("Invalid argument: Check that 'x' and 'v' have the same number of time-steps/layers!!")

  # Creating output directories, if necessary
  if ( !file.exists(drty.out) ) dir.create(drty.out)

  ldates  <- zoo::index(x)

  # K-Means ----
  if(verbose) message("Clustering images")

  X = c() # Design matrix for the clustering method
  for (i in 1:dim(v)[3]){
    v0 <- v[[i]]
    X <- rbind(X, raster::values(v0))
  }

  # Get the best number of centers using the silhouette score method
  silhouette_score <- function(k){
    km <- stats::kmeans(X, centers = k, nstart=25)
    ss <- cluster::silhouette(km$cluster, stats::dist(X))
    mean(ss[,3])
  }

  #TODO: define a better way to provide potential number of centers
  # Range of feasible centers
  ks <- 2:max(min(5,floor( nrow(x)/2 )), 2)

  if(verbose) tictoc::tic("silhouette")
  avg_sil <- sapply(ks, silhouette_score)
  if(verbose) tictoc::toc()

  plot(ks, type='b', avg_sil, xlab='Number of clusters',
       ylab='Average Silhouette Scores', frame=FALSE)

  # Best k
  k = ks[which.max(avg_sil)]

  # Plot center images
  km <- stats::kmeans(X, centers = k, nstart=25)
  for(i in 1:k){
    c0 = v[[1]] # start from a base raster image
    raster::values(c0) = km$centers[i,]
    raster::plot(c0, main=paste("Center:", i))
  }

  clusters.df = data.frame(km$cluster)
  names(clusters.df) = "cluster"

  # Heatmap of clusters
  # ggplot2::ggplot(clusters.df,
  #                 ggplot2::aes_string(x=1:nrow(clusters.df), y=1, fill=clusters.df$cluster)) +
  #   ggplot2::geom_raster()  +
  #   ggplot2::theme_bw()

  if(verbose) message("Clusters: ")
  if(verbose) print(table(clusters.df))

  # Optimize eta for each group identified with kmeans ----
  # Optimize eta for each center image (cluster)
  # Create a uhat image as the average of the corresponding time series per
  # cluster
  varmer.out = list()
  for (i in 1:k){
    message("Training eta for cluster ", i)

    # Center
    c0 = v[[1]] # Start from a baseline raster image
    raster::values(c0) = km$centers[i,]
    #plot(c0, main=paste("Center:", i))

    # Observations: Average the observations belonging to the same cluster
    a = colMeans(x[clusters.df$cluster==i,], na.rm = T)
    #length(a); range(a); range(raster::values(c0))

    # Create a sf object for varmer setting CRS from 'v' object
    stations.sf <- x.metadata
    stations.sf <- sf::st_as_sf(stations.sf, coords = c(lon, lat),
                            crs = raster::crs(v))
    stations.sf["Variable"] = a

    # Train the eta parameter using 10-fold cross validation
    out = fit.varmer(stations.sf, v=c0,
                     #etas = c(10,100,500),
                     factor_agg = 2,
                     apply_varmer = F, drty.out = drty.out)
    #raster::plot(out$u, main=paste("Eta:", out$best_eta))
    varmer.out[[i]] = out
    if(verbose) message("Best eta: ", out$best_eta)
  }

  # Generate merged image for each time step
  message("Generating u images in parallel: ", nrow(clusters.df))
  tictoc::tic("images.generation")
  numCores <- parallel::detectCores() / 2
  cl = parallel::makeCluster(numCores)
  parallel::clusterEvalQ(cl, library(sf))
  parallel::clusterEvalQ(cl, library(zoo))
  parallel::clusterEvalQ(cl, library(raster))
  parallel::clusterEvalQ(cl, library(gstat))
  parallel::clusterEvalQ(cl, library(VARMER))
  parallel::clusterExport(cl, c("clusters.df","x","x.metadata","v","lon","lat",
                      "varmer.out","ldates","drty.out"), envir = environment())

  parallel::parLapply(cl, 1:nrow(clusters.df), .create_merged_image_function, clusters.df,
            x, x.metadata, v, lon, lat, varmer.out, ldates, drty.out)
  parallel::stopCluster(cl)
  tictoc::toc()

  message("End: ", Sys.time())
}



# Parallel function to create merged images ####################################
.create_merged_image_function = function(i, clusters.df, x, x.metadata, v,
                                         lon, lat, varmer.out, ldates, drty.out){
  j = clusters.df[i,]
  #print(paste("Generating image", i, "with eta", varmer.out[[j]]$best_eta))

  # Build sf object
  stations.sf <- x.metadata
  stations.sf <- sf::st_as_sf(stations.sf, coords = c(lon, lat),
                              crs = raster::crs(v))
  stations.sf["Variable"] = as.vector(x[i])
  uhat <- idw.uhat.from.sf(stations.sf, v[[i]], Variable~1)
  v0 = v[[i]]
  #tic("varmer")
  varmer_results=varmer(uhat,v0,varmer.out[[j]]$best_eta)
  #tictoc::toc()

  #plot(varmer_results$U$U.img)

  ldate <- paste0("VARMER_", ldates[i], ".tif")
  fname <- file.path(drty.out, ldate)
  raster::writeRaster(varmer_results$U$U.img, fname, format = "GTiff",
                      overwrite = TRUE)
  return(fname)
}



# Fitting function for training eta parameter ##################################
#'@title Training eta parameter for the varmer function
#'@description Training eta parameter for the varmer function evaluating a vector of
#'etas using Cross-validation. The best eta is the one yielding the highest
#'KGE metric.
#'
#'@param stations.sf data.frame with the observations metadata
#'@param v grided image
#'@param etas (optional) vector of eta values to evaluate in a CV exercise
#'@param idw_formula formula for the idw interpolation
#'@param factor_agg scalar which defines the aggregation factor to apply to the raster images in order to reduce computation requirements for solving varmer
#'@param drty.out (optional) output folder for the CV metrics
#'@param apply_varmer (optional) boolean which determines if a merging image is produced with the best eta
#'
#'@export
fit.varmer = function(stations.sf, v,
                      etas=c(10,100,500,1000,5000),
                      idw_formula=Variable~1, factor_agg=2,
                      drty.out=tempdir(),
                      apply_varmer=T){

  tictoc::tic("fit.varmer")

  set.seed(204)
  folds <- cut(sample(1:nrow(stations.sf)),breaks=10,labels=F)

  message("Running ", 10, "-fold-CV - agg-factor ", factor_agg, " (parallel)")

  numCores <- parallel::detectCores()
  cl = parallel::makeCluster(numCores) # Begins parallel processing
  parallel::clusterEvalQ(cl, library(sf))
  parallel::clusterEvalQ(cl, library(zoo))
  parallel::clusterEvalQ(cl, library(raster))
  parallel::clusterEvalQ(cl, library(gstat))
  parallel::clusterEvalQ(cl, library(Metrics))
  parallel::clusterEvalQ(cl, library(hydroGOF))
  parallel::clusterEvalQ(cl, library(VARMER))
  parallel::clusterExport(cl, c("stations.sf", "v", "idw_formula",
                      "folds", "factor_agg"),
                envir = environment())

  metrics.df = data.frame()
  for(eta_0 in etas){
    #print(paste("eta_0", eta_0, paste(rep("=",50), collapse = "")))
    parallel::clusterExport(cl, "eta_0", envir = environment())
    m = parallel::parLapply(cl, 1:10, .fold_function, stations.sf, folds, v,
                            idw_formula, factor_agg, eta_0)

    metrics <- do.call(rbind,m)
    rownames(metrics) = NULL
    df.eta = data.frame(metrics)
    df.eta$eta = eta_0
    metrics.df = rbind(metrics.df, df.eta)
  }
  parallel::stopCluster(cl) # Ends parallel processing
  t=tictoc::toc(quiet = T)
  cv.runtime = round(t$toc-t$tic)
  print(paste("Duracion CV:",cv.runtime))

  # Treat NA in metrics
  metrics.df$R2[is.na(metrics.df$R2)] <- 0
  metrics.df$KGE[is.na(metrics.df$KGE)] <- 0

  # Best eta
  best.out = get_best_eta(metrics.df)

  # Plot metrics in PDF file
  plot_cv_metrics(n_folds=10, factor_agg, etas, cv.runtime, metrics.df,
                  best.out$metrics.cmb, drty.out)

  # Assemble result / output
  if (apply_varmer){
    print(paste("Generating u from best eta:", best.out$best_eta))
    tictoc::tic("varmer-target")
    uhat <- idw.uhat.from.sf(stations.sf, v, idw_formula)
    varmer_results=varmer(uhat,v,best.out$best_eta)
    tictoc::toc()

    out = list(
      u = varmer_results$U$U.img,
      best_eta = best.out$best_eta,
      metrics = metrics.df
    )
  }else{
    out = list(
      best_eta = best.out$best_eta,
      metrics = metrics.df
    )
  }

  return(out)
}


# Parallel function to perform single fold evaluation ##########################
.fold_function = function(k, stations.sf, folds, v, idw_formula, factor_agg,
                          eta_0){
  #print(paste("fold", k))

  ## Split data
  stations.train.sf <- stations.sf [folds!=k,]
  stations.test.sf  <- stations.sf [folds==k,]

  ## IDW interpolation
  uhat <- idw.uhat.from.sf(stations.train.sf, v, idw_formula)

  # Re-griding (aggregate)
  v2 = raster::aggregate(v, fact=factor_agg)
  uhat2 = raster::aggregate(uhat, fact=factor_agg)

  # varmer
  varmer_results=varmer(uhat2,v2,eta_0)

  u = varmer_results$U$U.img
  #plot(u)

  m <- evaluate.varmer(u, stations.test.sf, idw_formula[[2]])
  #metrics <- rbind(metrics,m)
}








#Interpolation using IDW #######################################################
#Ref: https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/geostatistics/Inverse-Distance-Weighting/IDW-interpolation-of-weather-data/index.html
#'@title IDW interpolation for varmer
#'
#'@description It produces an IDW-interpolated image from observations
#'@param stations.sf data.frame with metadata for the observations
#'@param reference.raster raster image which provides the base structure for the
#'resulting IDW interpolated image
#'@param idw.formula The formula to be passed for the IDW interpolation method
#'@export
idw.uhat.from.sf = function(stations.sf, reference.raster, idw.formula){
  uhat = methods::as(reference.raster, 'SpatialPixels')

  # plot(uhat,
  #      main = paste("Stations in Ecuador\n and Interpolation Grid"),
  #      col = "grey",
  #      cex.main = 0.9)
  # plot(stations.sf[1], add = TRUE, pch = 19, cex = 0.5, col = "blue")

  neighbors = dim(stations.sf)[1]  #length(stations)
  beta = 2

  idw_temp = gstat::gstat(
    formula = idw.formula, # intercept only model (TMAX ~ 1)
    data = stations.sf,
    nmax = neighbors,
    set = list(idp = beta))

  # Same CRS
  raster::crs(uhat) = raster::crs(stations.sf)

  uhat <- stats::predict(object = idw_temp, newdata = uhat)

  #plot(uhat, main = "IDW Temperature (deg C)")
  #plot(stations.sf[1], add = TRUE, pch = 19, cex = 0.5, col = "green")

  # Checks
  #paste("Check extent")
  #extent(reference.raster) == extent(uhat)
  #res(v); res(uhat)

  # Convert Spatial Pixels to raster
  uhat = methods::as(uhat, "RasterLayer")

  return(uhat)
}



# Evaluation function ##########################################################
# Calculate test error metrics between u and observations (sf)
evaluate.varmer = function(u, stations.test.sf, var_name){

  r2 <- function(z, zhat) {
    a <- stats::cor(z,zhat,use="pairwise.complete.obs")^2
  }

  df.tmp = sf::st_drop_geometry(stations.test.sf)
  y = df.tmp[[var_name]]
  yhat = raster::extract(u, stations.test.sf)

  out <- NULL
  out$RMSE <- Metrics::rmse(y, yhat)
  out$BIAS <- Metrics::bias(y, yhat)
  out$MAE  <- Metrics::mae(y, yhat)
  out$MSE  <- Metrics::mse(y, yhat)
  if ( stats::sd(y)!=0 & stats::sd(yhat)!=0 ){
    out$R2   <- r2(y, yhat)
    out$KGE  <- hydroGOF::KGE(sim= yhat, obs= y, method="2012")
  }else{
    out$R2   <- 0
    out$KGE  <- 0
  }
  out <- unlist(out)
  return(out)
}


# Util - Plot CV metrics to PDF ################################################
plot_cv_metrics = function(n_folds, factor_agg, etas, cv.runtime, metrics.df,
                           metrics.cmb, drty.out=tempdir()){

  grDevices::pdf(paste0(drty.out,"/boxplots_folds_",n_folds,"_factor_",
             factor_agg,"_etas_",
             etas[1],"_",tail(etas, n=1),"_runtime_",
             cv.runtime,"s",
             ".pdf"))
  graphics::boxplot(RMSE~eta,data=metrics.df)
  graphics::boxplot(MAE~eta,data=metrics.df)
  graphics::boxplot(MSE~eta,data=metrics.df)
  graphics::boxplot(BIAS~eta,data=metrics.df)
  graphics::boxplot(R2~eta,data=metrics.df)
  graphics::boxplot(KGE~eta,data=metrics.df)
  graphics::barplot(KGE~eta,data=metrics.cmb)
  grDevices::dev.off()
}


# Util - Gets best eta from a data.frame with metrics ##########################
get_best_eta = function(metrics.df){
  metrics.median = stats::aggregate(. ~ eta, metrics.df, stats::median)
  metrics.iqr = stats::aggregate(. ~ eta, metrics.df, stats::IQR)
  median_iqr = metrics.median['KGE']-metrics.iqr['KGE']
  metrics.cmb = data.frame(metrics.median['eta'], median_iqr)

  best_eta = metrics.cmb[which.max(metrics.cmb$KGE),'eta']

  out = list( best_eta=best_eta,
              metrics.cmb=metrics.cmb)
  return(out)
}
