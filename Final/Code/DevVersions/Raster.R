# File for Creating Environmental Raster Data
# DO NOT EXECUTE THIS FILE!


# to do: loess, corine


# LOAD PACKAGES ----

library(dismo)
library(raster)
library(dplyr)
library(maptools)
library(rgdal)
library(stringr)

# RASTER FOR BAVARIA ----

# Extract Bavaria Shape from Spatial Germany ====
ger_file <- getData("GADM", country = "Germany", level = 1)
bay_file <- ger_file[match(toupper("Bayern"),toupper(ger_file$NAME_1)),]

# Create a Random Raster Over the Space ====      
bay_raster <- raster(xmn = 8.975, xmx = 13.84167, ymn = 47.26667, ymx = 50.56667, nrow = 396, ncol = 584)
bay_raster[] <- runif(396*584)

# Create Mask ====
bay_mask <- mask(bay_raster, bay_file)


# DATA ACQUISITION ----

# 1. Height ====

# Load Data
height <- getData(name = "alt", country = "Germany")

# Change Name
names(height) <- "height"

# Cut Bavaria Shape
height <- crop(height, bay_mask)
height <- mask(height, bay_mask)

# Save Raster
# writeRaster(height, "Final/Daten/Raster/height_raster")


# 2. Temperature and Rain ====

# Load Data
weatherdata <- getData("worldclim", var = "bio", res = 0.5, lon =  12.101624, lat = 49.013432)
weatherdata <- weatherdata[[c(1, 12)]]

# Change Name
names(weatherdata) <- c("temp", "rain")

# Cut Bavaria Shape
weatherdata <- crop(weatherdata, bay_mask)
weatherdata <- mask(weatherdata, bay_mask)
weatherdata <- stack(weatherdata[[1]] / 10, weatherdata[[2]])

# Save Raster
# writeRaster(weatherdata[[1]], "Final/Daten/Raster/temp_raster")
# writeRaster(weatherdata[[2]], "Final/Daten/Raster/rain_raster")


# 3. Slope, Aspect, Tpi ====

# Load Data
env_data <- terrain(height, opt = c("slope", "aspect", "tpi"), unit = "degrees")

# Turn into Cathegorial Variable
env_data$aspect <- ceiling((env_data$aspect + 360/8/2)/(360/8))
env_data$aspect[env_data$aspect > 8] <- 1

# Cut Bavaria Shape
env_data <- crop(env_data, bay_mask)
env_data <- mask(env_data, bay_mask)

# Save Raster
# writeRaster(env_data[[1]], "Final/Daten/Raster/tpi_raster")
# writeRaster(env_data[[2]], "Final/Daten/Raster/slope_raster")
# writeRaster(env_data[[3]], "Final/Daten/Raster/aspect_raster")

# 4. Water Distance ====
# (Source: s. Bene)

# Load Shapefiles
river_shape <- shapefile("Daten/DEU_water_lines_dcw.shx")
lake_shape <- shapefile("Daten/DEU_water_areas_dcw.shx")

# Create Water Rasters
river_raster <- mask(bay_raster, river_shape)
lake_raster <- mask(bay_raster, lake_shape)

# Calculate Distance to Rivers and Lakes
distance_river <- distance(river_raster)
distance_lake <- distance(lake_raster)

# Minimum of Distance to Rivers and Distance to Lakes is Distance to Water
distance_water <- min(distance_lake, distance_river)

# Save Raster Files
# writeRaster(distance_river, "Final/Daten/Raster/RawFiles/river")
# writeRaster(distance_lake, "Final/Daten/Raster/RawFiles/lake")
# writeRaster(distance_water, "Final/Daten/Raster/RawFiles/water")

# Change Name
names(distance_water) <- "distance_water"

# Cut Bavaria Shape
distance_water <- mask(distance_water, bay_mask)

# Save Raster
# writeRaster(distance_water, "Final/Daten/Raster/distance_water_raster")


# 5. Loess Distance ====
# (Source: https://www.lfu.bayern.de/umweltdaten/geodatendienste/pretty_downloaddienst.htm?dld=gk500)

# Load Shapefiles
loess_shape <- readOGR(dsn = "Loess", layer = "gk500_haupteinheiten_epsg4258")#, encoding = "UTF-8")
# Mit UTF-8 kommt bei mir kommt bei mir Schmarrn raus...
# Warum Haupteinheiten? Was ist mit Linien, Struktur? Enthält Hauteinheiten alles?

# Subsetting for Loess
loess_shape <- loess_shape[str_detect(loess_shape$kurztext, "Löß"),]

# Create Loess Raster
loess_raster <- mask(bay_raster, loess_shape)

# Calculate Distance to Loess Sediments
distance_loess <- raster::distance(loess_raster)

# Change Name
names(distance_loess) <- "distance_loess"

# Save Raster File
# writeRaster(distance_loess, "Final/Daten/Raster/RawFiles/loess")

# Cut Bavaria Shape
distance_loess <- mask(distance_loess, bay_mask)

# Save Final Raster
# writeRaster(distance_loess, "Final/Daten/Raster/distance_loess_raster")


# 6. Frostdays ====
# (Source: https://maps.dwd.de/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?0)

# Import Data
frostdays <- raster("Final/Daten/Raster/RawFiles/dwd-Frostdays_annual_map_normals_1971_30.tif")

# Change Name
names(frostdays) <- "frostdays"

# Change Coordinates to WGS
frostdays <- projectRaster(frostdays, crs = crs(bay_mask))

# Data in Bavaria Shape
frostdays <- resample(frostdays, bay_mask)
frostdays <- mask(frostdays, bay_mask)

# Save Final Raster
# writeRaster(frostdays, "Final/Daten/Raster/frostdays_raster")

# Load Final Raster
# frostdays <- raster("Final/Daten/Raster/frostdays_raster.grd")


# 7. Sunhours ====
# (Source: https://maps.dwd.de/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?0)

# Import Data 
sunhours <- raster("Final/Daten/Raster/RawFiles/dwd-SDMS_17_1971_30.tif")

# Change Name
names(sunhours) <- "sunhours"

# Change Coordinates to WGS
sunhours <- projectRaster(sunhours, crs = crs(bay_mask))

# Data in Bavaria Shape
sunhours <- resample(sunhours, bay_mask)
sunhours <- mask(sunhours, bay_mask)

# Save Final Raster
# writeRaster(sunhours, "Final/Daten/Raster/sunhours_raster")

# Load Final Raster
# sunhours <- raster("Final/Daten/Raster/sunhours_raster.grd")


# 8. Corine ====
# (Source: https://land.copernicus.eu/pan-european/corine-land-cover/clc2018?tab=download)
# corine <- raster("Final/Daten/Raster/corine_raster.grd")

# Import Data
corine <- raster("Final/Daten/Raster/RawFiles/CLC2012_CLC2006_V2018_20b2.tif")

# Change Name
names(corine) <- "corine"

# Change Coordinates to WGS
corine <- projectRaster(corine, bay_mask)

# Save Raster File
# writeRaster(corine, "Final/Daten/Raster/RawFiles/corine")

# Data in Bavaria Shape
corine <-  mask(corine, bay_mask)

# Save Final Raster
# writeRaster(corine, "Final/Daten/Raster/corine_raster")
