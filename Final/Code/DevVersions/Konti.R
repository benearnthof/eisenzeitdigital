# PACKAGES ----
require(dplyr)
require(stringr)
require(readr)
require(geosphere)
require(ggmap)
library(raster)
library(ggplot2)
library(Deducer)
library(broom)
library(tidyr)

# LOAD DATA ----

data <- read_csv(
  "Final/Daten/fender_2017_lk.csv",
  col_types = cols(
    `Corine-Code` = col_character(),
    `Höhe_SRTM1_puffer50m` = col_character(),
    wkb_geom = col_skip(),
    wkb_geom_wgs84 = col_skip(),
    Hangausrichtung_SRTM1_puffer50m = col_character(),
    Umfeldanalyse_km2 = col_character(),
    Temperatur_Jahr = col_character()
  )
)

# chang data to correct arguments

# Höhe
data$Höhe_SRTM1_puffer50m <-
  data$Höhe_SRTM1_puffer50m %>% str_replace_all(",", ".") %>% as.numeric() %>% round(digits = 2)

# Neigung
data$Neigung_SRTM1_puffer50m <-
  data$Neigung_SRTM1_puffer50m %>% str_replace_all(",", ".") %>% as.numeric() %>% round(digits = 2)

# Hangausrichtung
data$Hangausrichtung_SRTM1_puffer50m <-
  data$Hangausrichtung_SRTM1_puffer50m %>% str_replace_all(",", ".") %>% as.numeric() %>% round(digits = 2)

# Loess
data$Loess_1zu500k_puffer50m <-
  data$Loess_1zu500k_puffer50m %>% str_replace_all(",", ".") %>% as.numeric() %>% round(digits = 2)

# Wasser
data$Wasser_puffer50m <-
  data$Wasser_puffer50m %>% str_replace_all(",", ".") %>% as.numeric() %>% round(digits = 2)

# Umfeldanalyse
data$Umfeldanalyse_km2 <-
  data$Umfeldanalyse_km2 %>% str_replace_all(",", ".") %>% as.numeric() %>% round(digits = 2)

# Viewshed
data$Viewshed_km2 <-
  data$Viewshed_km2 %>% str_replace_all(",", ".") %>% as.numeric() %>% round(digits = 2)

# Temperatur
data$Temperatur_Jahr <-
  data$Temperatur_Jahr %>% str_replace_all(",", ".") %>% as.numeric() %>% round(digits = 2)


# Sortingepochs by ages
data <- data %>%
  mutate(Epoche = if_else(
    Zeitstellung %in% c(
      "Frühbronzezeit",
      "Bronzezeit",
      "Spätbronzezeit",
      "Mittelbronzezeit",
      "Urnenfelderzeit"
      ,
      "Urnenfelderzeit, ältere",
      "Urnenfelderzeit, jüngere",
      "Metallzeit"
    ),
    "Bronzezeit",
    if_else(
      Zeitstellung %in% c(
        "Hallstattzeit",
        "Hallstattzeit, spät",
        "Hallstattzeit, früh",
        "Eisenzeit"
      ),
      "Hallstattzeit",
      if_else(
        Zeitstellung %in% c(
          "Latènezeit",
          "Frühlatènezeit",
          "Spätlatènezeit",
          "Mittellatènezeit",
          "jüngere Latènezeit"
        ),
        "Latènezeit",
        if_else(
          Zeitstellung %in% c(
            "Altpaläolithikum",
            "Mittelpaläolithikum",
            "Jungpaläolithikum",
            "Endpaläolithikum",
            "Paläolithikum",
            "Spätpaläolithikum"
          ),
          "Paläolithikum",
          if_else(
            Zeitstellung %in% c(
              "Endneolithikum",
              "Glockenbecherkultur",
              "Großgartacher Kultur",
              "Linearbandkeramik",
              "Michelsberger Kultur"
              ,
              "Mittelneolithikum",
              "Mondsee Gruppe",
              "Mönchshöfener Kultur",
              "Neolithikum",
              "Oberlauterbach",
              "Polling",
              "Rössener Kultur",
              "Schnurkeramik",
              "Schussenrieder Gruppe",
              "SOB",
              "Spätneolithikum",
              "Stichbandkeramik",
              "Altheimer Gruppe",
              "Chamer Kultur",
              "Jungneolithikum"
            )
            ,
            "Neolithikum",
            "Mesolithikum"
          )
        )
      )
    )
  ))


data$Epoche <- factor(
  data$Epoche,
  ordered = TRUE,
  levels = c(
    "Paläolithikum",
    "Mesolithikum",
    "Neolithikum",
    "Bronzezeit",
    "Hallstattzeit",
    "Latènezeit"
  )
)

# FUNCTIONS ----
# functions to calculate continuity
## function to calculate distance and number of settlements 
dis_count <- function(code_epo) {
  epo_mean <- NA
  k <- 0
  data <- data
  for (i in 1:nrow(code_epo)) {
    print(i)
    code_epo[i - 1, 29] <- mean(epo_mean)
    code_epo[i - 1, 30] <- k
    epo_mean <- NA
    k = 0
    for (j in 1:nrow(code_epo)) {
      if (distHaversine(
        c(code_epo$lng_wgs84[i], code_epo$lat_wgs84[i]),
        c(code_epo$lng_wgs84[j], code_epo$lat_wgs84[j])
      ) <= 30000) {
        epo_mean[k] <-
          distHaversine(
            c(code_epo$lng_wgs84[i], code_epo$lat_wgs84[i]),
            c(code_epo$lng_wgs84[j], code_epo$lat_wgs84[j])
          )
        k <- k + 1
      }
    }
  }
  names(code_epo)[29] <- "distanz"
  names(code_epo)[30] <- "anzahl"
  return(code_epo)
}

## coding function
code <- function(code_epo, df_naechste_Epoche) {
  vgl_epo <- dplyr::select(code_epo, lng_wgs84, lat_wgs84)
  vgl_n_epo <-
    dplyr::select(df_naechste_Epoche, lng_wgs84, lat_wgs84)
  for (i in 1:nrow(vgl_epo)) {
    code_epo[i, 31] <- 0
    print(i)
    for (j in 1:nrow(vgl_n_epo)) {
      if (all.equal(vgl_epo[i, ], vgl_n_epo[j, ]) == TRUE) {
        code_epo[i, 31] <- 1
      }
      else{
        
      }
      
    }
  }
  names(code_epo)[31] <- "kodierung"
  return(code_epo)
}

# CALCULATION ----
## Calculation of distance and number of settlements and coding
## total computation time: 5h 2min 52sec
# define settlements
set_typ <- c(
  "Abri",
  "Freilandstation",
  "Siedlung",
  "Abschnittsbefestigung",
  "Höhensiedlung",
  "Höhensiedlung, befestigt",
  "Kreisgraben",
  "Ringwall",
  "Siedlung, befestigt",
  "Höhlenfunde"
)
### Palaeolithikum

# computation time dis_count: ~ 1 min 23 s
code_palae <-
  dplyr::filter(data,
                Epoche %in% "Paläolithikum" &
                  Typ %in% set_typ)
code_palae <- dis_count(code_palae)

Meso <-
  dplyr::filter(data,
                Epoche %in% "Mesolithikum" &
                  Typ %in% set_typ)
# computation time code: ~ 1h 6min
code_palae <- code(code_palae, Meso)
save(code_palae, file = "code_palae.csv")
### Mesolithikum
code_meso <-
  dplyr::filter(data,
                Epoche %in% "Mesolithikum" &
                  Typ %in% set_typ)
# computation time dis_count: ~ 53 s
code_meso <- dis_count(code_meso)
neo <-
  dplyr::filter(
    data,
    Epoche %in% "Neolithikum" &
      Typ %in% set_typ
  )
# computation time code: ~ 1h 
code_meso <- code(code_meso, neo)
save(code_meso, file = "code_neo.csv")
### Neolithikum
code_neo <-
  dplyr::filter(
    data,
    Epoche %in% "Neolithikum" &
      Typ %in% set_typ
  )
# computation time dis_count: ~ 2min 30s
code_neo <- dis_count(code_neo)
bronz <-
  dplyr::filter(
    data,
    Epoche %in% "Bronzezeit" &
      Typ %in% set_typ
  )
# computation time code: ~ 1h 48min
code_neo <- code(code_neo, bronz)
save(code_neo, file = "code_neo.csv")
### Bronzezeit
code_bro <-
  dplyr::filter(
    data,
    Epoche %in% "Bronzezeit" &
      Typ %in% set_typ
  )
# computation time dis_count: ~ 2min 22s
code_bro <- dis_count(code_bro)
system.time(dis_count(code_bro[1:100,]))
hall <-
  dplyr::filter(
    data,
    Epoche %in% "Hallstattzeit" &
      Typ %in% set_typ
  )
# computation time code: ~ 37min
code_bro <- code(code_bro, hall)
save(code_bro, file = "code_bro.csv")
load("code_bro.csv")
### Hallstattzeit
code_hall <-
  dplyr::filter(
    data,
    Epoche %in% "Hallstattzeit" &
      Typ %in% set_typ
  )
# computation time dis_count: ~ 44s
code_hall <- dis_count(code_hall)
lat <-
  dplyr::filter(
    data,
    Epoche %in% "Latènezeit" &
      Typ %in% set_typ
  )
# computation time code: ~ 24min
code_hall <- code(code_hall, lat)

# MERGING ----

conti <-
  dplyr::bind_rows(code_palae, code_meso, code_neo, code_bro, code_hall)

# record whether all the data has been calculated
nrow(filter(
  data,
  Epoche %in% c(
    "Hallstattzeit",
    "Bronzezeit",
    "Neolithikum",
    "Mesolithikum",
    "Paläolithikum"
  ) &
    Typ %in% c(
      "Abri",
      "Freilandstation",
      "Siedlung",
      "Abschnittsbefestigung",
      "Höhensiedlung",
      "Höhensiedlung, befestigt",
      "Kreisgraben",
      "Ringwall",
      "Siedlung, befestigt",
      "Höhlenfunde"
    )
)) - nrow(conti)
# 77 were not recorded



### RASTERDATA ----
extraction <- function(parameter, rasterdata){
  parameter <-
    round(raster::extract(raster[[rasterdata]], base::matrix(
      c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
    )), digits = 2)
  return(parameter)
}

## load raster data
raster <- stack(c(
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

## change skale of predictors proposal
### Height in 100m
raster[["height"]] <- raster[["height"]]/100
### Rain in 100mm
raster[["rain"]] <- raster[["rain"]]/100
### Distance in 1000m/ km
raster[["distance_water"]] <- raster[["distance_water"]]/1000
raster[["distance_loess"]] <- raster[["distance_loess"]]/1000

# Replace certain parameters with raster data
# height
conti$Höhe_SRTM1_puffer50m <-
  round(raster::extract(raster[["height"]], base::matrix(
    c(data$lng_wgs84, data$lat_wgs84), ncol = 2
  )), digits = 2)
names(conti)[9] <- "height"

# temp
conti$Temperatur_Jahr <-
  round(raster::extract(raster[["temp"]], base::matrix(
    c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
  )), digits = 2)
names(conti)[25] <- "temp"

# rain
conti$Niederschlag_Jahr <-
  round(raster::extract(raster[["rain"]], base::matrix(
    c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
  )), digits = 2)
names(conti)[19] <- "rain"

# distance water
conti$Wasser_puffer50m <-
  round(raster::extract(raster[["distance_water"]], base::matrix(
    c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
  )), digits = 2)
names(conti)[13] <- "distance_water" 

# distance loess
conti$Loess_1zu500k_puffer50m <-
  round(raster::extract(raster[["distance_loess"]], base::matrix(
    c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
  )), digits = 2)
names(conti)[12] <- "distance_loess"

# frostdays
conti$Frosttage_Jahr <-
  round(raster::extract(raster[["frostdays"]], base::matrix(
    c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
  )), digits = 2)
names(conti)[18] <- "frostdays"

# sunhours
conti$Sonnenstunden_Jahr <-
  round(raster::extract(raster[["sunhours"]], base::matrix(
    c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
  )), digits = 2)
names(conti)[22] <- "sunhours"

# corine code
conti$`Corine-Code` <-
  round(raster::extract(raster[["corine"]], base::matrix(
    c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
  )))
names(conti)[16] <- "corine"

# correcting the corinecode in a rough way
conti$corine[113 <= conti$corine & conti$corine <= 117] <- 112
conti$corine[118 <= conti$corine & conti$corine <= 120] <- 121
conti$corine[125 <= conti$corine & conti$corine <= 127] <- 124
conti$corine[128 <= conti$corine & conti$corine <= 130] <- 131
conti$corine[134 <= conti$corine & conti$corine <= 137] <- 133
conti$corine[138 <= conti$corine & conti$corine <= 140] <- 141
conti$corine[143 <= conti$corine & conti$corine <= 177] <- 142
conti$corine[178 <= conti$corine & conti$corine <= 210] <- 211
conti$corine[212 <= conti$corine & conti$corine <= 216] <- 211
conti$corine[217 <= conti$corine & conti$corine <= 220] <- 221
conti$corine[223 <= conti$corine & conti$corine <= 227] <- 222
conti$corine[228 <= conti$corine & conti$corine <= 230] <- 231
conti$corine[232 <= conti$corine & conti$corine <= 238] <- 231
conti$corine[239 <= conti$corine & conti$corine <= 241] <- 242
conti$corine[244 <= conti$corine & conti$corine <= 277] <- 243
conti$corine[278 <= conti$corine & conti$corine <= 310] <- 310
conti$corine[314 <= conti$corine & conti$corine <= 317] <- 313
conti$corine[318 <= conti$corine & conti$corine <= 320] <- 321
conti$corine[323 == conti$corine] <- 322
conti$corine[325 <= conti$corine & conti$corine <= 328] <- 324
conti$corine[329 <= conti$corine & conti$corine <= 330] <- 331
conti$corine[336 <= conti$corine & conti$corine <= 373] <- 335
conti$corine[374 <= conti$corine & conti$corine <= 410] <- 411
conti$corine[413 <= conti$corine & conti$corine <= 417] <- 412
conti$corine[418 <= conti$corine & conti$corine <= 420] <- 421
conti$corine[422 == conti$corine] <- 423
conti$corine[424 <= conti$corine & conti$corine <= 467] <- 423
conti$corine[468 <= conti$corine & conti$corine <= 510] <- 511
conti$corine[513 <= conti$corine & conti$corine <= 517] <- 512
conti$corine[518 <= conti$corine & conti$corine <= 520] <- 521
conti$corine[523 <= conti$corine ] <- 523

# slope
conti$Neigung_SRTM1_puffer50m <-
  round(raster::extract(raster[["slope"]], base::matrix(
    c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
  )), digits = 2)
names(conti)[10] <- "slope"
  
# aspect
conti$Hangausrichtung_SRTM1_puffer50m <-
  round(raster::extract(raster[["aspect"]], base::matrix(
    c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
  )), digits = 2)
names(conti)[11] <- "aspect"

# tpi 
conti[,32] <-   round(raster::extract(raster[["tpi"]], base::matrix(
  c(conti$lng_wgs84, conti$lat_wgs84), ncol = 2
)), digits = 2)
names(conti)[32] <- "tpi"

# change the scaling of distance
conti$distanz <- conti$distanz/1000

# epochs as a factor
conti[which(conti$Epoche == "Pal�olithikum"),33] <- 2
conti[which(conti$Epoche == "Mesolithikum"),33] <- 3
conti[which(conti$Epoche == "Neolithikum"),33] <- 4
conti[which(conti$Epoche == "Bronzezeit"),33] <- 5
conti[which(conti$Epoche == "Hallstattzeit"),33] <- 1
names(conti)[33] <- "Epochen_num"

# Saving and loading continuity data
# saveRDS(conti, file = "Final/Daten/Konti/Konti.RDS")
# conti <- readRDS("Final/Daten/Konti/Konti.RDS")

# MODELS ----
# Continuity for all epochs
# Model with all logical interactions 
conti <- conti[sample(nrow(conti)), ]
conti <- na.omit(conti)
conti_glm <-
  glm(
    as.factor(kodierung) ~ (
      height + slope + aspect + distance_loess + distance_water +
        Umfeldanalyse_km2 + Reliefenergie  + Viewshed_km2)^2 + as.factor(corine) +
        (sunhours + temp + frostdays + rain) ^ 2 +
        distanz + anzahl + tpi + as.factor(Epochen_num) +
        Umfeldanalyse_km2:Reliefenergie + frostdays:rain + height:rain
      ,
      data = conti,
      family = binomial
    )
summary(conti_glm)

# Step
conti_glm_step <- step(conti_glm, direction = "both")
summary(conti_glm_step)

# Select the wald test significant parameters
# fitting another glm with the significant parameters
conti_glm <-
  glm(
    as.factor(kodierung) ~ 
      height + aspect + distance_loess + distance_water +
        Umfeldanalyse_km2 + Reliefenergie  + Viewshed_km2 +
      sunhours + temp + frostdays + rain +
      distanz + anzahl + as.factor(Epochen_num) +
      height:Viewshed_km2 + slope:distance_loess + distance_loess:Umfeldanalyse_km2 +
      distance_water:Umfeldanalyse_km2 + Umfeldanalyse_km2:Reliefenergie + sunhours:temp +
      frostdays:rain + height:rain
    ,
    data = conti,
    family = binomial
  )
summary(conti_glm)

# Step
conti_glm_step <- step(conti_glm, direction = "both")
summary(conti_glm_step)

# ... after fitting the model in this way 4 more times:
# last time fitting the model 
conti_glm <-
  glm(
    as.factor(kodierung) ~ height + distance_water + 
      Umfeldanalyse_km2 + Reliefenergie + frostdays + rain + distanz + 
      anzahl + as.factor(Epochen_num) + Umfeldanalyse_km2:Reliefenergie + 
      height:rain,
    family = binomial,
    data = conti
  )
summary(conti_glm)

# Step
conti_glm_step <- step(conti_glm, direction = "both")
summary(conti_glm_step)
# Step function no longer throws out parameters
# cor(conti$Umfeldanalyse_km2,conti$Reliefenergie) = -0.75325
# cor(conti$height, conti$rain) = 0.52783

# KI for the coefficients
confint(conti_glm)

# ROC
par(mar = c(4.5,3.5,3.5,1.5)+0.1)
rocplot(conti_glm)+
  ggtitle("ROC for the continuity of all epochs")+
  xlab("False positive rate")+
  ylab("True positive rate")
dev.copy(png, "../../Output/Konti/conit_ROC.png", res = 200, pointsize = 16, units = "in", width = 14, height = 10)
dev.off()

# Residual
conti_glm <- na.omit(conti_glm)
conti_glm_aug <- augment(conti_glm) %>%
  mutate(index = 1:n()) 
names(conti_glm_aug)[1] <- "Kodierung" 
par(mar = c(4.5,3.5,3.5,1.5)+0.1)
ggplot(conti_glm_aug, aes(index, .std.resid)) +
  geom_point(aes(color = Kodierung), alpha = .5) +
  ylim(-2, 2)+
  guides(colour = guide_legend(reverse=T))
dev.copy(png, "../../Output/Konti/conti_res.png", res = 200, pointsize = 16, units = "in", width = 14, height = 10)
dev.off()

# Probabilities
prob_conti <- predict(conti_glm)
prob_conti <- exp(prob_conti)/(1+ exp(prob_conti))
summary(prob_conti)

### Continuity for Hallstattzeit
hall <- dplyr::filter(conti, Epoche %in% "Hallstattzeit")
hall <- na.omit(hall)
hall <- hall[sample(nrow(hall)), ]
hall_glm <-
  glm(
    as.factor(kodierung) ~ (
      height + slope + aspect + distance_loess + distance_water +
        Umfeldanalyse_km2 + Reliefenergie  + Viewshed_km2)^2 + as.factor(corine) +
      (sunhours + temp + frostdays + rain) ^ 2 +
      distanz + anzahl + tpi + as.factor(Epochen_num) +
      Umfeldanalyse_km2:Reliefenergie + frostdays:rain + height:rain
    ,
    data = conti,
    family = binomial
  )

summary(hall_glm)

# Step
hall_glm_step <- step(na.omit(hall_glm), direction = "both")
summary(hall_glm_step)

# ... after fitting the model as in the upper part 3 more times:
hall_glm <- glm(as.factor(kodierung) ~ height + distance_loess + 
                  distance_water + Umfeldanalyse_km2 + Reliefenergie + frostdays + 
                  rain + distanz + anzahl + Umfeldanalyse_km2:Reliefenergie + 
                  frostdays:rain + height:rain, family = binomial, data = conti)
summary(hall_glm)

# Step
hall_glm_step <- step(na.omit(hall_glm), direction = "both")
summary(hall_glm_step)
# Step function no longer throws out parameters
# cor(hall$Umfeldanalyse_km2, hall$Reliefenergie) = -0.79125
# cor(hall$frostdays, hall$rain) = 0.57090
# cor(hall$height, hall$rain) = 0.66013

# KI for the coefficients
confint(hall_glm)

# ROC
par(mar = c(4.5,3.5,3.5,1.5)+0.1)
rocplot(hall_glm) +
  ggtitle("ROC for the continuity glm of the hallstatt period") +
  xlab("False positive rate") +
  ylab("True positive rate")
dev.copy(png, "../../Output/Konti/hall_ROC.png", res = 200, pointsize = 16, units = "in", width = 14, height = 10)
dev.off()


### Residuals
hall_glm <- na.omit(hall_glm)
hall_glm_aug <- augment(hall_glm) %>%
  mutate(index = 1:n()) 
names(hall_glm_aug)[1] <- "Kodierung"
par(mar = c(4.5,3.5,3.5,1.5)+0.1)
ggplot(hall_glm_aug, aes(index, .resid)) +
  geom_point(aes(color = Kodierung), alpha = .5) +
  theme_bw() +
  ylim(-2, 2)+
  guides(colour = guide_legend(reverse=T))
dev.copy(png, "../../Output/Konti/hall_res.png", res = 200, pointsize = 16, units = "in", width = 14, height = 10)
dev.off()

# Probabilities
prob_hall <- predict(hall_glm)
prob_hall <- exp(prob_hall)/(1+ exp(prob_hall))
summary(prob_hall)
