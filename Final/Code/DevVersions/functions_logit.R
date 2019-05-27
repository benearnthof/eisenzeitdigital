# functions for sampling process & logistic model ====
# loading packages
source("loadpackages.R")
# loading everything that the functions need as defaultinputs ----

# shapefile that is required for distance to water
shapewater <- shapefile("../../Daten/Raster/RawFiles/DEU_water_lines_dcw.shx")
# raster files that are required for bavSamp and height calculations respectively
ger <- raster::getData(name = "GADM", country = "Germany", level = 1)
ger_alt <- raster::getData(name = "alt", country = "Germany")
# bayern coords is required as default border for bavSamp
bayern_coords <- ger@polygons[[2]]@Polygons[[1]]@coords
# world_subset is required for plot2water
data(wrld_simpl, package = "maptools")
wrld_subset <- wrld_simpl[wrld_simpl@data$ISO2 %in% c("DE"),]
# data for "loess"
loess_shape <- shapefile("../../Daten/Raster/RawFiles/Loess/gk500_haupteinheiten_epsg4258.shx")

# automation of samplingprocess ####
# function to sample random points from bavaria ----
bavSamp <- function(from = ger_alt, within = bayern_coords, size = 1000, maxiter = 100, rlength = 100){
  i <- 0
  iteration <- 1
  # initialize empty 'SpatialPointsDataFrame' to make use of the raster::union function
  returnframe <- new("SpatialPoints",                                                          
                     coords = structure(numeric(0), .Dim = c(0L, 2L),                          
                                        .Dimnames = list(NULL, c("coords.x1", "coords.x2"))),  
                     bbox = structure(c(1, 1, 1, 1), .Dim = c(2L, 2L),                         
                                      .Dimnames = list(c("coords.x1", "coords.x2"),
                                                       c("min", "max"))),
                     proj4string = new("CRS", projargs = NA_character_))
  # initial check upon running the function  
  if (i >= maxiter) {
    warning("Maximum number of iterations reached \n Returning sample.")
    warning("Sampling iterations: ", print(i))
    return(returnframe)    
  }
  while (i < maxiter) {
    print(iteration)
    temp <- sampleRandom(from, size, sp = T)
    checkvector <- point.in.polygon(temp@coords[,1], temp@coords[,2], bayern_coords[,1], bayern_coords[,2])
    checkvector <- as.logical(checkvector)
    temp <- temp[checkvector,]
    returnframe <- union(returnframe, temp)
    iteration <- iteration + 1
    if (nrow(returnframe@coords) >= rlength) {
      returnframe <- returnframe[1:rlength,]
      # set i equal to maxiter to abort sampling process early
      i <- maxiter
    }
    
  }
  return(returnframe)
}
# splitframe function to split data.frames into lists of equal length for parallelization ----

splitframe <- function(df, splits) {
  if (mod(nrow(df), splits) != 0) {
    stop("nrow is not a multiple of splits")
  }
  pointsliste <- list()
  j <- 0
  for (i in 1:splits) {
    pointsliste[[i]] <- df[(j + 1):(i*(nrow(df)/splits)),]
    j <- j + (nrow(df)/splits)
  }
  return(pointsliste)
}

# function to calculate distances in parallel ----
pardist2water <- function(pnts, shapef = shapewater, splits = 10, workers = 4) {
  pnts <- as.data.frame(pnts)
  # split into equal parts
  pointsliste <- splitframe(pnts, splits)
  registerDoParallel(cores = workers)
  tmp <- foreach(i = 1:splits) %dopar% {
    geosphere::dist2Line(p = pointsliste[[i]], line = shapef)
  }
  # unlist tmp to data.frame
  tmp <- do.call(rbind, tmp)
  # bind results with original points
  tmp <- cbind(pnts, tmp)
  return(tmp)
}

# function to plot points and shortest way to rivers as arrows ----
plot2water <- function(df, rows = 100, colorPoints = "red", colorArrows = "green", shp = shapewater) {
  if (rows > nrow(df)) {
    warning("n larger than nrow of data.frame. Setting n equal to nrow(data.frame).")
    rows <- nrow(df)
  }
  temp <- sp::SpatialPoints(coords      = head(df[,c("x","y")], n = rows), # order matters
                            proj4string = wrld_subset@proj4string)
  plot(temp, col = colorPoints)
  plot(shp, add = TRUE)
  # plot arrows to indicate the direction of the great-circle-distance
  for (i in 1:rows) {
    arrows(x0 = df[i,1], 
           y0 = df[i,2], 
           x1 = df[i,4], 
           y1 = df[i,5],
           length = 0.1,
           col = colorArrows)
  }
}

# function to add weather data to output of function dist2water ----
extractWeather <- function(df) {
  weatherdata <- getData("worldclim", var = "bio", res = 0.5, lon = df[1,1], lat = df[1,2])
  # entries 1 and 12 in this rasterstack are mean anual temperature and annual precipitation
  # http://www.worldclim.org/bioclim
  weatherdata <- weatherdata[[c(1, 12)]]
  names(weatherdata) <- c("temp", "rain")
  original <- df
  spatial <- df
  coordinates(spatial) <- c("x","y")
  coords <- SpatialPoints(spatial@coords)
  spatial <- extract(weatherdata, coords)
  spatial <- as.data.frame(spatial)
  temp <- cbind(original, spatial)
  return(temp)
}

# function to add environmental data (height, tpi, aspect) to data.frame ----
extractEnv <- function(df){
  env_data <- terrain(ger_alt, opt = c("slope", "aspect", "tpi"), unit = "degrees")
  spatial <- df
  coordinates(spatial) <- c("x","y")
  points.environment <- raster::extract(env_data, spatial, sp = T)
  points.environment@data$aspect <- ceiling((points.environment@data$aspect + 360/8/2)/(360/8))
  points.environment@data$aspect[points.environment@data$aspect > 8] <- 1
  final <- cbind(df, points.environment@data[,c("tpi", "slope", "aspect")])
  names(final) <- c("lon", "lat", "w.dist", "w.lon", "w.lat", "ID", "temp", "rain", "tpi", "slope", "aspect")
  return(final)
}

# function to add "loess" to data.frame ----
pardist2loess <- function(pnts, shapef = loess_shape, splits = 10, workers = 4) {
  pnts <- as.data.frame(pnts)
  # split into equal parts
  pointsliste <- splitframe(pnts, splits)
  registerDoParallel(cores = workers)
  tmp <- foreach(i = 1:splits) %dopar% {
    geosphere::dist2Line(p = pointsliste[[i]], line = shapef)
  }
  # unlist tmp to data.frame
  tmp <- do.call(rbind, tmp)
  # bind results with original points
  tmp <- cbind(pnts, tmp)
  return(tmp)
}

# function to extract frostdays ----
extractFrosttage <- function(pnts) {
  load("../../Daten/Frosttage.csv")
  final <- round(raster::extract(Frosttage, as.matrix(pnts)))
  return(final)
}

# function to automate sampling process entirely ----
runValidation <- function(numrows, rseed = 1){
  set.seed(rseed)
  tmp.set <- bavSamp(size = 5000, rlength = numrows)
  tmp.points <- tmp.set@coords
  tmp.points <- as.data.frame(tmp.points)
  tmp.points <- pardist2water(tmp.points, splits = 8, workers = 8)
  tmp.points <- extractWeather(tmp.points)
  tmp.points <- extractEnv(tmp.points)
  write.table(tmp.points, file = "../../Daten/negativdaten50k.csv")
  return(tmp.points)
}

# generating datasets to work with ####
fiftyKpoints <- runValidation(50000)
# recalculating values for fender set to eliminate bias introduced through different methods
data3 <- readRDS("../../Daten/Daten2.RDS")
x <- as.numeric(as.vector(data3$lng_wgs84))
y <- as.numeric(as.vector(data3$lat_wgs84))
coords <- cbind(lng, lat)
colnames(coords) <- c("x", "y")
# function to return corrected fender data frame
runValidationFender <- function(df){
  tmp.points <- df
  tmp.points <- as.data.frame(tmp.points)
  tmp.points <- pardist2water(tmp.points, splits = 8, workers = 8)
  tmp.points <- extractWeather(tmp.points)
  tmp.points <- extractEnv(tmp.points)
  write.table(tmp.points, file = "../../Daten/fenderdistance2water.csv")
  return(tmp.points)
}

fenderdistance2water <- runValidationFender(coords)

# write sampling function that respects the spatial proximity of points ----
sitesdf <- read.table(file = "../../Daten/fenderdistance2water.csv")
nonsitesdf <- read.table(file = "../../Daten/negativdaten50k.csv")

# distance function that will be used to calculate distances from point to point
# haversine distance corrects for the curvature of the earth => more accurate than euclidian distance
dist_haversine <- function(lon1, lat1, lon2, lat2, r = 6378.388){
  # r = radius of the sphere the coordinates are located on e.g. the earth
  # calculate distance and return result in km
  temp <- 2*r*asin(sqrt((sin(((lat2 - lat1)*(pi/180))/2))^2 + cos(lat1*(pi/180))*cos(lat2*(pi/180)) * sin((((lon1 - lon2)*(pi/180))/2))^2))
  return(temp)
}

filterbymargin_haversine <- function(samplefrom, checkagainst, margin = 1) {
  df <- samplefrom
  df$temporary <- 0
  for (i in 1:nrow(checkagainst)){
    df$temporary <- mapply(dist_haversine, df$lon, df$lat, checkagainst$lon[i], checkagainst$lat[i])
    df <- df[(df$temporary>margin),]
  }
  df$temporary <- NULL
  return(df)
}

# filtering out nonsites that are close than 1.5 km to sites: ####
negative50kfiltered <- filterbymargin_haversine(samplefrom = nonsitesdf, checkagainst = sitesdf, margin = 1.5)
nrow(negative50kfiltered)
write.table(negative50kfiltered, file = "../../Daten/neg29kfiltered.csv")



