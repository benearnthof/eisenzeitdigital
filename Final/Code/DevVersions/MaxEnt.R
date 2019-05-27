# PACKAGES ----
require(dismo)
require(raster)
require(dplyr)

# LOAD STACK FILES ----
predictors <- stack(c(
  "Final/Daten/Raster/height_raster.grd",
  "Final/Daten/Raster/temp_raster.grd",
  "Final/Daten/Raster/rain_raster.grd",
  "Final/Daten/Raster/distance_water_raster.grd",
  "Final/Daten/Raster/distance_loess_raster.grd",
  "Final/Daten/Raster/frostdays_raster.grd",
  "Final/Daten/Raster/sunhours_raster.grd",
  "Final/Daten/Raster/corine_raster.grd",
  "Final/Daten/Raster/tpi_raster.grd",
  "Final/Daten/Raster/slope_raster.grd",
  "Final/Daten/Raster/aspect_raster.grd"))

predictors_sub <- stack(c(
  "Final/Daten/Raster/height_raster.grd",
  "Final/Daten/Raster/temp_raster.grd",
  "Final/Daten/Raster/rain_raster.grd",
  "Final/Daten/Raster/distance_water_raster.grd",
  "Final/Daten/Raster/distance_loess_raster.grd",
  "Final/Daten/Raster/frostdays_raster.grd",
  "Final/Daten/Raster/sunhours_raster.grd",
  "Final/Daten/Raster/corine_raster.grd",
  "Final/Daten/Raster/slope_raster.grd"))


# CHANGE SKALE OF PREDICTORS PROPOSAL ----

# Height in 100m
predictors[["height"]] <- predictors[["height"]]/100
# Rain in 100mm
predictors[["rain"]] <- predictors[["rain"]]/100
# Distance in 1000m/ km
predictors[["distance_water"]] <- predictors[["distance_water"]]/1000
predictors[["distance_loess"]] <- predictors[["distance_loess"]]/1000


# LOAD PRESENCE DATA ----

# Load Data ====
presence <- readRDS("Final/Daten/Daten_mit_Epoche.RDS")
presence <- filter(presence, Epoche %in% c("Latènezeit", "Hallstattzeit"))
#presence <- filter(presence, Siedlung == TRUE)
presence <- presence[6:7]
presence$lng_wgs84 <- as.numeric(as.character(presence$lng_wgs84))
presence$lat_wgs84 <- as.numeric(as.character(presence$lat_wgs84))

# Split into Training and Testing Set ====
set.seed(12345)
group <- kfold(presence, 5)
presence_train <- presence[group != 1, ]
presence_test <- presence[group == 1, ]

# MAXENT MODEL ----

# Fit Model ====
#maxent_model <- maxent(predictors, presence_train, factors = c('aspect', 'frostdays', 'corine'), nbg = 6591)
maxent_model2 <- maxent(predictors_sub, presence_train, 
                       factors = c('frostdays', 'corine'),
                       nbg = nrow(presence_train), remove.duplicates = T,
                       args = c("responsecurves"))#,
                       #path = "Maxent")

# Variable Importance ====
plot(maxent_model)

# Density ====
density(maxent_model)

# Response Curves ====
response(maxent_model)

# Prediction ====
#r <- predict(maxent_model, predictors)
r <- predict(maxent_model, predictors_sub)
plot(r)
print(r)

# Evaluation ====
#bg <- randomPoints(predictors, 1000)
bg <- randomPoints(predictors_sub, 1000)
#el <- evaluate(maxent_model, p = presence_test, a = bg, x = predictors)
el <- evaluate(maxent_model, p = presence_test, a = bg, x = predictors_sub)
threshold(el)
plot(el, 'ROC')
boxplot(el)
print(el)


# MORE MAXENT ARGUMENTS
# https://groups.google.com/forum/#!topic/maxent/yRBlvZ1_9rQ





'
temperatur_presence_train <- extract(predictors_sub[[2]], presence_train) %>% as.data.frame()
temperatur_presence_train <- na.omit(temperatur_presence_train)
names(temperatur_presence_train) <- "temp"
temperatur_presence_test <- extract(predictors_sub[[2]], presence_test) %>% as.data.frame()
temperatur_presence_test <- na.omit(temperatur_presence_test)
names(temperatur_presence_test) <- "temp"
temperatur <- rbind(temperatur_presence_train, temperatur_presence_test)

temperatur <- rnorm(1000, mean = 8, sd=0.3)
temperatur <- as.data.frame(temperatur)
names(temperatur) <- "temp"

ggplot(temperatur_presence_train, aes(temp)) +
  geom_histogram(aes(y = ..density..), col = "black", fill = "white", binwidth = 0.1) +
  #geom_density(col = "red") +
  geom_density(data = temperatur, col = "blue") +
  theme_bw()
'