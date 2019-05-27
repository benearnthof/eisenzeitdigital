#################
## DATA CLEANING
#################
set.seed(12345)

library(dplyr)
library(raster)

# READ RASTER DATA ----
predictors <- stack(c(
  "Daten/Raster/height_raster.grd",
  "Daten/Raster/temp_raster.grd",
  "Daten/Raster/rain_raster.grd",
  "Daten/Raster/distance_water_raster.grd",
  "Daten/Raster/distance_loess_raster.grd",
  "Daten/Raster/frostdays_raster.grd",
  "Daten/Raster/sunhours_raster.grd",
  "Daten/Raster/corine_raster.grd",
  "Daten/Raster/tpi_raster.grd",
  "Daten/Raster/aspect_raster.grd",
  "Daten/Raster/slope_raster.grd"))

cols2keep_negative <- c("lon", "lat", "w.dist", "temp", "rain", "slope", "tpi", "aspect")
cols2keep_raw <- c("Typ", "Zeitstellung", "Epoche")
cols2keep <- c(names(predictors), "lon", "lat")


#################
## PRESENCE DATA
#################
raw.data <- readRDS("Daten/Daten_mit_Epoche.RDS")
raw.data <- raw.data[,!colnames(raw.data) %in% c("lng", "lat", "wkb_geom", "wkb_geom_wgs84")]
raw.data$lng_wgs84 <- as.numeric(as.vector(raw.data$lng_wgs84))
raw.data$lat_wgs84 <- as.numeric(as.vector(raw.data$lat_wgs84))
raw.data <- raw.data[order(raw.data$lng_wgs84, raw.data$lat_wgs84),]
#####

presence_coords <- dplyr::select(raw.data, lng_wgs84, lat_wgs84)
presence.df <- raster::extract(predictors, presence_coords)

presence <- na.omit(cbind(presence.df, presence_coords, raw.data[, cols2keep_raw]))
presence$Epoche <- as.factor(presence$Epoche)
saveRDS(presence, "Daten/PresenceData.RDS")
colnames(presence)[colnames(presence) %in% c("lng_wgs84", "lat_wgs84")] <- c("lon", "lat")

# Epoche = Eisenzeit
presence <- presence[presence$Epoche %in% c("Hallstattzeit", "Latènezeit"),]


#################
## NEGATIVE DATA
#################
negative <- read.csv("neg29kfiltered.csv", sep = " ")
negative <- negative[, cols2keep_negative]
negative <- na.omit(negative)
negative$temp <- negative$temp/10
negative$Typ <- NA

row.names(negative) <- seq(1, nrow(negative), 1)
index <- as.numeric(rownames(negative))
index_siedlung <- base::sample(index, 5500)
index <- index[!index %in% index_siedlung]
index_4eckschanze <- base::sample(index, 400)
index <- index[!index %in% index_4eckschanze]
index_ez <- base::sample(index, 8000)
index <- index[!index %in% index_ez]

negative[index_siedlung, "Typ"] <- "Siedlung"
negative[index_4eckschanze, "Typ"] <- "Viereckschanze"
negative[index_ez, "Typ"] <- "Alle"
negative[index, "Typ"] <- "Sonstige"


## Extracting from Raster
# Alle ----
negative_all_coords <- dplyr::select(negative[negative$Typ %in% "Alle",], lon, lat)
negative_all <- raster::extract(predictors, negative_all_coords)

negative_all <- cbind(negative_all, negative_all_coords)

# Siedlung ----
negative_siedlung_coords <- dplyr::select(negative[negative$Typ %in% "Siedlung",], lon, lat)
negative_siedlung <- raster::extract(predictors, negative_siedlung_coords)

negative_siedlung <- cbind(negative_siedlung, negative_siedlung_coords)

# Viereckschanzen ----
negative_viereckschanze_coords <- dplyr::select(negative[negative$Typ %in% "Viereckschanze",], lon, lat)
negative_viereckschanze <- raster::extract(predictors, negative_viereckschanze_coords)

negative_viereckschanze <- cbind(negative_viereckschanze, negative_viereckschanze_coords)


############
## ALL DATA
############
df.all <- rbind(
  data.frame(presence[,cols2keep], site=TRUE),
  data.frame(negative_all, site=FALSE)
)

df.all <- na.omit(df.all)


#########################
## DATA FOR 'Siedlungen'
#########################
df.siedlung <- rbind(
  data.frame(presence[presence$Typ %in% c("Abri", "Abschnittsbefestigung", "Freilandstation", "Herrenhof", "Höhensiedlung", "Höhensiedlung, befestigt", "Siedlung", "Siedlung, befestigt", "Oppidum", "Ringwall", "Kreisgraben"), cols2keep], site=TRUE),
  data.frame(negative_siedlung, site=FALSE)
)

df.siedlung <- na.omit(df.siedlung)


##########################
## DATA FOR '4eckschanze'
##########################
df.viereckschanze <- rbind(
  data.frame(presence[presence$Typ %in% "Viereckschanze", cols2keep], site=TRUE),
  data.frame(negative_viereckschanze, site=FALSE)
)

df.viereckschanze <- na.omit(df.viereckschanze)


################################
## Saving necessary data frames
################################
saveRDS(df.all, "Daten/DataFrames_GGWR/df_all.RDS")
saveRDS(df.siedlung, "Daten/DataFrames_GGWR/df_siedlung.RDS")
saveRDS(df.viereckschanze, "Daten/DataFrames_GGWR/df_viereckschanze.RDS")

