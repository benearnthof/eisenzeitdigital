# loading packages ----
packages <- c("tidyverse","broom","sp","geosphere","numbers","doParallel",
              "spdplyr","ggmap","raster","mapview","magrittr","sf","rworldxtra",
              "rgeos","lwgeom","rgdal", "mlr", "parallelMap","hrbrthemes",
              "cowplot", "pROC", "car", "mgcv", "Deducer", "magrittr", "ggplot2",
              "broom", "dplyr", "caret", "arm", "dismo")
packs <- lapply(packages, FUN = function(packages) {
  do.call("require", list(packages)) 
})
packs <- unlist(packs, use.names = F)
instpacks <- packages[!packs]

lapply(instpacks, FUN = function(instpacks){
  do.call("install.packages", list(instpacks))
})

# should return a vector of TRUE entries - one entry for every successfully loaded package
unlist(lapply(packages, FUN = function(packages) {
  do.call("require", list(packages)) 
}))
# mgcv doesnt seem to load through the do.call
require(mgcv)
