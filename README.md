# varmer
`Varmer` is an R library for merging satellite-based or model-based gridded images with ground-based observations, using a Variational Merging Approach [(Ulloa et al. 2018)](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017JD027982). 

Bugs / comments / questions / collaboration of any kind are very welcomed.

## Installation
Installing the latest stable version from [CRAN](https://CRAN.R-project.org/package=VARMER):
```{r}
install.packages("VARMER")
```

## A simple first application:

Loading required packages:

```{r Loading_other_pks, eval = TRUE, message=FALSE}
library(zoo)
library(sf)
library(raster)
library(tictoc)
library(cluster)
library(parallel)
library(ggplot2)
library(VARMER)
```

Loading times series and metadata of ground observations:

   
```{r Loading_GroundObservarions, eval = TRUE}
data(ecuador.tmax.zoo)
data(ecuador.tmax.stations.df)
```
Loading satellite-based/model-based datasets:
   
```{r LoadingSatelliteData, eval = TRUE}
data(ecuador.tmax.wrf.out)
```

Running `VARMER` 

```{r, eval = FALSE}
varmer.ts(x=ecuador.tmax.zoo, x.metadata=ecuador.tmax.stations.df,
          v=ecuador.tmax.wrf.out, lat='LAT', lon='LON',
          drty.out="~/Documentos/dataset_ecuador")
```



## Reporting bugs, requesting new features

If you find an error in some function, or want to report a typo in the documentation, or to request a new feature (and wish it be implemented :) you can do write to `lenin.gcta@gmail.com`.


## Citation 

To cite `VARMER` in publications use:

> Ulloa, J., Samaniego, E., Campozano, L., & Ballari, D. (2018). A variational merging approach to the spatial description of environmental variables. Journal of Geophysical Research: Atmospheres, 123. https://doi.org/10.1002/2017JD027982.
