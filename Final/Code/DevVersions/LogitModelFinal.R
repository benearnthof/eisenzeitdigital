source("loadpackages.R")
setwd("C:/Users/ru25jan/Documents/eisenzeitdigital/Final/Code/DevVersions")
# loading the predictor rasterstack ----
predictors <- stack(c(
  "../../Daten/Raster/height_raster.grd",
  "../../Daten/Raster/temp_raster.grd",
  "../../Daten/Raster/rain_raster.grd",
  "../../Daten/Raster/distance_water_raster.grd",
  "../../Daten/Raster/distance_loess_raster.grd",
  "../../Daten/Raster/frostdays_raster.grd",
  "../../Daten/Raster/sunhours_raster.grd",
  "../../Daten/Raster/corine_raster.grd",
  "../../Daten/Raster/tpi_raster.grd",
  "../../Daten/Raster/slope_raster.grd",
  "../../Daten/Raster/aspect_raster.grd"))

# transforming rasterstack to units that are easy on the eyes
predictors <- stack(c(
  predictors[[1]]/100,
  predictors[[2]],
  predictors[[3]],
  predictors[[4]]/1000,
  predictors[[5]]/1000,
  predictors[[6]],
  predictors[[7]],
  predictors[[8]],
  predictors[[9]],
  predictors[[10]],
  predictors[[11]]
))
# importing predictors 
# plotting predictors to make sure everything works as intended
plot(predictors)

# preparation of data ====
# data contains unequal amount of sites and nonsites 
# thus we reextract the points to fix that
# loading data ----
data_sites <- readRDS("../../Daten/GWGLM/df_all.RDS")
data_settle <- readRDS("../../Daten/GWGLM/df_siedlung.RDS")
data_viereck <- readRDS("../../Daten/GWGLM/df_viereckschanze.RDS")
data_nonsites <- read.table(file = "../../Daten/neg29kfiltered.csv")

data_sites <- filter(data_sites, site == 1)
data_settle <- filter(data_settle, site == 1)
data_viereck <- filter(data_viereck, site == 1)


# function to automate evidence generation from different site and nonsite data ----
generateEvidence <- function(sitesdata, nonsitesdata = data_nonsites, predictorstack = predictors) {
  # selecting site points
  sites_temp <- sitesdata
  sites_temp$lon <- as.numeric(as.vector(sites_temp$lon))
  sites_temp$lat <- as.numeric(as.vector(sites_temp$lat))
  # convert to spatial data in order to extract the predictor values for all points
  coordinates(sites_temp) <- c("lon","lat")
  proj4string(sites_temp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  nonsites_temp <- nonsitesdata
  coordinates(nonsites_temp) <- c("lon", "lat")
  proj4string(nonsites_temp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  # extracting predictor values for sites and nonsites
  sSP <- SpatialPoints(sites_temp@coords)
  nsSP <- SpatialPoints(nonsites_temp@coords)
  values_sites <- raster::extract(predictorstack, sSP)
  values_nonsites <- raster::extract(predictorstack, nsSP)
  # converting back to data.frame for modeling
  coords_sites <- sites_temp@coords
  coords_sites <- as.data.frame(coords_sites)
  coords_nonsites <- nonsites_temp@coords
  coords_nonsites <- as.data.frame(coords_nonsites)
  
  values_sites <- as.data.frame(values_sites)
  values_nonsites <- as.data.frame(values_nonsites)
  values_sites$site <- 1
  values_nonsites$site <- 0
  values_sites$lon <- coords_sites$lon
  values_sites$lat <- coords_sites$lat
  values_nonsites$lon <- coords_nonsites$lon
  values_nonsites$lat <- coords_nonsites$lat
  # selecting unique settlementpoints
  # filtering out coordinate pairs that are equal
  # based on productindex. The Idea being that small differences in coordinate values maka a large difference in 
  # product results. And distinct pairs that yield the same result in products like (9.2, 49.1) and (49.1, 9.2)
  # are impossible to find in the dataset due to the geographic extent of bavaria. 
  # we also tried generating an index with sums which lead to the exact same amount of unique coordinate pairs.
  values_sites$index <- values_sites$lon * values_sites$lat
  values_sites <- values_sites[!duplicated(values_sites$index), ]
  values_sites$index <- NULL
  # combining everything together to a single data.frame for modeling
  evidence <- rbind(values_sites, values_nonsites)
  evidence <- na.omit(evidence)
  return(evidence)
}

# generating evidence data for all three data sets
evidence_sites <- generateEvidence(sitesdata = data_sites)
evidence_settle <- generateEvidence(sitesdata = data_settle)
evidence_viereck <- generateEvidence(sitesdata = data_viereck)

# function to automate building sets with equal counts of site == 1 and site == 0

finalizeEvidence <- function(evd){
  siedl_pts <- filter(evd, site == 1)
  nons_pts <- filter(evd, site == 0)
  sz <- nrow(siedl_pts)
  nons_pts_sub <- sample_n(nons_pts, size = sz)
  temp <- rbind(siedl_pts, nons_pts_sub)
  return(temp)
}

# finalizing datasets ----
set.seed(12345)
evidence_sites <- finalizeEvidence(evd = evidence_sites)
set.seed(12345)
evidence_settle <- finalizeEvidence(evd = evidence_settle)
set.seed(12345)
evidence_viereck <- finalizeEvidence(evd = evidence_viereck)

# checking if everything went as planned
nrow(evidence_sites)
nrow(evidence_settle)
nrow(evidence_viereck)

# visualizing different types of sites

spatial_sites <- data_sites
spatial_settle <- data_settle
spatial_viereck <- data_viereck

spatial_sites$index <- spatial_sites$lon * spatial_sites$lat
spatial_settle$index <- spatial_settle$lon * spatial_settle$lat
spatial_viereck$index <- spatial_viereck$lon * spatial_viereck$lat

# remove settlements and viereckschanzen from sites to enhance visualization
spatial_sites <- spatial_sites[!spatial_sites$index %in% spatial_settle$index,]
spatial_sites <- spatial_sites[!spatial_sites$index %in% spatial_viereck$index,]

# transform to spatial data to enable plotting with mapview()
coordinates(spatial_sites) <- c("lon", "lat")
proj4string(spatial_sites) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
coordinates(spatial_settle) <- c("lon", "lat")
proj4string(spatial_settle) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
coordinates(spatial_viereck) <- c("lon", "lat")
proj4string(spatial_viereck) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
mapview(spatial_sites, alpha.regions = 0.1, color = "purple") +
  mapview(spatial_settle, alpha.regions = 0.1, color = "red") + 
  mapview(spatial_viereck, alpha.regions = 0.1, color = "green")


# fitting models ====
# fitting a full model first to see if model assumptions of logistic regression hold
# or if variable transformation or removal is needed.
fit_sites <- glm(site ~ height + temp + rain + distance_water + distance_loess + frostdays + slope + 
                     sunhours + corine + tpi + aspect, 
                   family = binomial(), 
                   data = evidence_sites)
summary_sites <- summary(fit_sites)
summary_sites
# checking if model assumptions hold ----

probabilities <- predict(fit_sites, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, "pos", "neg")

# Logistic regression diagnostics:
# Linearity assumption
mydata <- evidence_sites %>% 
  dplyr::select_if(is.numeric)
predictors_mydata <- colnames(mydata)
# bind logit and tidy data for plotting
mydata <- mydata %>% 
  mutate(logit = log(probabilities/(1-probabilities))) %>% 
  gather(key = "predictors.mydata", value = "predictor.value", -logit , -lon, -lat, -site)

# creating scatter plots: 
linearityplots <- ggplot(mydata, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) + 
  facet_wrap(~predictors.mydata, scales = "free_y") + 
  ylab("Predictor Wert") +
  xlab("Logits") 
  

# aspect and corine code are categorical variables
# as the summary of a model that introduces aspect as a factor of 8 levels shows, 
# the variables are no longer of significance thus we decided to drop 
# aspect, sunhours and corine code from inputvariables 
# moreover we decided to remove tpi because of lacking linearity 

# let's try to introduce the sunhour variables as a factor to the model
predictors_factors <- predictors
predictors_factors[[7]] <- round(predictors_factors[[7]])

factor_evidence <- generateEvidence(data_sites, nonsitesdata = data_nonsites, predictorstack = predictors_factors)

fit_sites_factor <- glm(site ~ height + temp + rain + distance_water + distance_loess + frostdays + slope + 
                          height * rain + as.factor(layer), 
                        family = binomial(), 
                        data = factor_evidence)
summary_sites_factor <- summary(fit_sites_factor)
summary_sites_factor

# the effects of sunhours are pretty much random (significance levels above 0.9)
# thus we decided to drop sunhours from the model 

# introducing an interaction between height and rain yields interesting results
# shuffling data row wise for better residualplot later on 
nocat_data <- evidence_sites[sample(nrow(evidence_sites)),]
fit_sites_nocat <- glm(site ~ height + temp + rain + distance_water + distance_loess + frostdays + slope + 
                         height * rain, 
                       family = binomial(), 
                       data = evidence_sites)
summary_sites_nocat <- summary(fit_sites_nocat)
summary_sites_nocat

#fitting the other 2 models
fit_settle <- glm(site ~ height + temp + rain + distance_water + distance_loess + frostdays + slope + 
                    height * rain, 
                  family = binomial(), 
                  data = evidence_settle)
summary_settle <- summary(fit_settle)
summary_settle

fit_viereck <- glm(site ~ height + temp + rain + distance_water + distance_loess + frostdays + slope + 
                     height * rain, 
                   family = binomial(), 
                   data = evidence_viereck)
summary_viereck <- summary(fit_viereck)
summary_viereck

fit_sites_nocat2 <- glm(site ~ height + temp + rain + distance_water + distance_loess + frostdays + slope + 
                         height * rain, 
                       family = binomial(), 
                       data = nocat_data)

summary_sites_nocat2 <- summary(fit_sites_nocat2)
all.equal(summary_sites_nocat, summary_sites_nocat2)

# both the interaction and height itself are now of statistical significance

# checking more interactions
fit_sites_interact <- glm(site ~ height + temp + rain + distance_water + distance_loess + frostdays + slope + 
                            temp*rain + temp*frostdays + temp*rain*frostdays + rain*frostdays, 
                          family = binomial(), 
                          data = evidence_sites)
summary(fit_sites_interact)
# this seems to reduce both interpretabilit and significance of the other variables.
# Given that our goal was to produce predictive maps the interpretability of the models
# may not be of utmost importance, but the predictive performance of the model as measured
# by the AUROC only improved by 0.004 for this particular model as compared to the nocat 
# model with only one interaction term. For these reasons, and a lack of distinct domain
# knowledge of potential interactions between archaeological and climate data, we decided
# not to introduce more interaction terms to our models. 

# checking if model assumptions hold for the nocat model ----

probabilities <- predict(fit_sites_nocat, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, "pos", "neg")
pred_cl <-  predicted_classes
reference <- ifelse(evidence_sites$site == TRUE, "pos", "neg")
pred_cl <- as.factor(pred_cl)
reference <- as.factor(reference)

# confusion matrix 
caret::confusionMatrix(pred_cl, reference)

# Logistic regression diagnostics: 
# binned residual plot
arm::binnedplot(fitted(fit_sites_nocat), 
           residuals(fit_sites_nocat, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average Residual", 
           main = "Binned Residual Plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

# Linearity assumption
mydata <- evidence_sites %>% 
  dplyr::select_if(is.numeric)
predictors_mydata <- colnames(mydata)
# bind logit and tidy data for plotting
mydata <- mydata %>% 
  mutate(logit = log(probabilities/(1-probabilities))) %>% 
  gather(key = "predictors.mydata", value = "predictor.value", -logit , -lon, -lat, -site, -aspect, 
         -corine, -sunhours, -tpi)

# creating scatter plots: 
linearityplots_nocat <- ggplot(mydata, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "glm") +
  theme_bw() +
  facet_wrap(~predictors.mydata, scales = "free_y") + 
  ylab("Predictor Wert") +
  xlab("Logits") 
  
linearityplots_nocat

# distance water and distance loess might be better represented through smooth terms
fit_sites_gam <- mgcv::gam(site ~ height + temp + rain + frostdays + slope + 
                       height * rain + s(distance_water) + s(distance_loess),
                     family = binomial(),
                     data = evidence_sites)
summary_gam <- summary(fit_sites_gam)
mgcv::plot.gam(fit_sites_gam, ylim = c(-2, 2), select = 1)
abline(h = 0, col = "red")
abline(v = 1.9, col = "blue")
abline(v = 0.5, col = "green")
abline(h = 0.43, col = "green")
axis(side = 1, at = c(0, 0.5, 1.9, 5, 10, 15), labels = c(0, 0.5, 1.9, 5, 10, 15))
axis(side = 2, at = c(-2,-1,0,0.42,1,2), labels = c(-2,-1,0,0.42,1,2))
# the strength of the effects do not seem to be influenced by the distances themselves
# especially distance_water hovers around 0

# do nonsite points with distance water = 0 exist in the evidence data?
testwdist <- evidence_sites %>% dplyr::filter(site == 0) %>% dplyr::filter(distance_water <= 1.9) %>% nrow()
testwdistsites <- evidence_sites %>% dplyr::filter(site == 1) %>% dplyr::filter(distance_water <= 1.9) %>% nrow()
# positive effect of distances to water larger than 1.9km is not due to a bias in the sampling procedure


plot(fit_sites_nocat)

# Influential values using cook's distance
cookplot <- plot(fit_sites_nocat, which = 4, id.n = 10)
mdl_data <- augment(fit_sites_nocat2) %>% 
  mutate(index = 1:n())
# displaying top 10 largest values
topnoutliers <- mdl_data %>% top_n(10, .cooksd)
# plotting standardized residuals
stdresplot <- ggplot(mdl_data, aes(index, .std.resid)) +
  geom_point(aes(color = site), alpha = 0.5) +
  scale_y_continuous(name="Standardized Residuals", limits=c(-3, 5)) +
  theme_bw()
# filtering potential influential data points 
infpoints <- mdl_data %>% 
  filter(abs(.std.resid) > 3)
# Multicolinearity
vif <- car::vif(fit_sites_nocat)
vif

# calculating area under ROC 
pROC::auc(pROC::roc(evidence_sites$site, fitted(fit_sites_nocat)))
pROC::auc(pROC::roc(evidence_sites$site, fitted(fit_sites_gam)))
plot(roc(evidence_sites$site, fitted(fit_sites_nocat)))

# for this piece of code the libraries rJava and JavaGD are required.
# we also need to set the "JAVA_HOME" to the correct location. 
# if you do not want to install java then skip the next two lines of code.
# Sys.setenv(JAVA_HOME="C:\\\PATH\\\TO\\\JAVA\\\")
# Sys.setenv(JAVA_HOME="C:/Users/benea/Downloads/openjdk-11+28_windows-x64_bin/jdk-11")
library(Deducer)
Deducer::rocplot(fit_sites_nocat)

# predictive mapping ----
env_data_df_full <- as.data.frame(predictors)
pdata <- predict(fit_sites_nocat, newdata = env_data_df_full, type = "response")
pdata_settle <- predict(fit_settle, newdata = env_data_df_full, type = "response")
pdata_veck <- predict(fit_viereck, newdata = env_data_df_full, type = "response")

x_pred <- predictors
x_pred$pred <- pdata
settle_pred <- predictors
settle_pred$pred <- pdata_settle
veck_pred <- predictors
veck_pred$pred <- pdata_veck

# predictive plot for linear model
plot(x_pred$pred)

par(mar = c(4.5,3.5,3.5,1.5)+0.1)
plot(x_pred$pred, xlab = "lon", ylab = "lat")
dev.copy(png, "Sites.png", res = 200, pointsize = 16, units = "in", width = 14, height = 10)
dev.off()
par(mar = c(5,4,4,2) + 0.1)

par(mar = c(4.5,3.5,3.5,1.5)+0.1)
plot(settle_pred$pred, xlab = "lon", ylab = "lat")
dev.copy(png, "Settle.png", res = 200, pointsize = 16, units = "in", width = 14, height = 10)
dev.off()
par(mar = c(5,4,4,2) + 0.1)

par(mar = c(4.5,3.5,3.5,1.5)+0.1)
plot(veck_pred$pred, xlab = "lon", ylab = "lat")
dev.copy(png, "Veck.png", res = 200, pointsize = 16, units = "in", width = 14, height = 10)
dev.off()
par(mar = c(5,4,4,2) + 0.1)

# fitting models and doing the predictive mapping for settlements and viereckschanzen ----


pROC::auc(pROC::roc(evidence_settle$site, fitted(fit_settle)))

arm::binnedplot(fitted(fit_settle), 
                residuals(fit_settle, type = "response"), 
                nclass = 200, 
                xlab = "Expected Values", 
                ylab = "Average Residual", 
                main = "Binned Residual Plot", 
                cex.pts = 0.8, 
                col.pts = 1, 
                col.int = "gray")


pROC::auc(pROC::roc(evidence_viereck$site, fitted(fit_viereck)))

arm::binnedplot(fitted(fit_viereck), 
                residuals(fit_viereck, type = "response"), 
                nclass = 50, 
                xlab = "Expected Values", 
                ylab = "Average Residual", 
                main = "Binned Residual Plot", 
                cex.pts = 0.8, 
                col.pts = 1, 
                col.int = "gray")

# assessing the stability of the model estimators ----
# we have only a certain set of data points, but do have a larger set of 
# nonsite points to sample from.
# so it seems reasonable to inspect the stability of the estimated 
# coefficients. 
# we start off by making a copy of data_sites for further processing
stability_sites <- data_sites
# generating evidence like usual
evidence_stability <- generateEvidence(sitesdata = stability_sites)
# we create an empty data.frame to save the estimated model coefficients into
df_stability <- data.frame(matrix(nrow = 500,ncol = length(fit_sites_nocat$coefficients)))
colnames(df_stability) <- names(fit_sites_nocat$coefficients)
# the following loop redraws a new set of negative datapoints in every iteration.
# then a model mdl gets estimated based on the new dataset.
# the estimated coefficients get saved as the ith row in our data.frame.
# we then print i because my laptop is slow and I want to see the progress.
for (i in 1:nrow(df_stability)) {
  temp <- finalizeEvidence(evidence_stability)
  mdl <- glm(site ~ height + temp + rain + distance_water + distance_loess + frostdays + slope + 
               height * rain, 
             family = binomial(), 
             data = temp)
  df_stability[i,] <- mdl$coefficients
  print(i)
}
head(df_stability)
# we are not interested in the intercept, to keep the boxplots clean we will drop 
# it from the data.frame
df_stability <- df_stability[,2:9]
colnames(df_stability) <- c("height", "temp", "rain", "wdist", "ldist", "frost", "slope", "HR-inter")
require(reshape2)
ggplot(data = melt(df_stability), aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=variable)) + 
  ylab("Coefficient Estimates") +
  xlab("") + 
  theme(legend.position = "none")

# visualizing the interaction of height and rain to help understanding
interdata <- evidence_sites
interdata$height_bin <- ifelse(interdata$height > mean(interdata$height), "above avg","below avg")
interdata$rain_bin <- ifelse(interdata$rain > mean(interdata$rain), "above avg","below avg")
interdata$prob <- fitted(fit_sites_nocat)
interdata$type_num <- interdata$site

# plotting interaction
ggplot(interdata, aes(rain, type_num)) +
  geom_point() +
  stat_smooth(aes(rain, prob, col = height_bin), se = T) +
  labs(y = "Pr(site)", title = "site ~ rain varying by height_bin")

ggplot(interdata, aes(height, type_num)) +
  geom_point() +
  stat_smooth(aes(height, prob, col = rain_bin), se = F) +
  labs(y = "Pr(site)", title = "Diabetes ~ height varying by rain")


# spatially repeated CV only with the full model ----
# also check if model estimators are stable with different negative datasets
coords = evidence_sites[, c("lon", "lat")]
# select response and predictors to use in the modeling
data = dplyr::select(evidence_sites, -lon, -lat)
# create task
data$site <- as.logical(data$site)
task = makeClassifTask(data = data, target = "site",
                       positive = "TRUE", coordinates = coords)

# specify a learner through
lrn = makeLearner(cl = "classif.binomial",
                  link = "logit",
                  predict.type = "prob",
                  fix.factors.prediction = TRUE)
# specifying a resampling method (Spacial repeated Crossvalidation)
perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)
# running the resampling to assess model performance
set.seed(37)
sp_cv = mlr::resample(learner = lrn, task = task,
                      resampling = perf_level, 
                      measures = mlr::auc)
boxplot(sp_cv$measures.test$auc)

perf_level_std <- makeResampleDesc(method = "RepCV", folds = 5, reps = 100)
std_cv <- mlr::resample(learner = lrn, task = task,
                        resampling = perf_level_std,
                        measures = mlr::auc)
# visualization of resampling methods and results ----
boxplot(std_cv$measures.test$auc, sp_cv$measures.test$auc)

# resample visualization 
resplots <- createSpatialResamplingPlots(task = task, resample = std_cv, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", repetitions = 1,
                                         x.axis.breaks = c(8.5, 14.5),
                                         y.axis.breaks = c(47, 51))
resplots[[1]][[1]]

library(mlr)
rdesc = makeResampleDesc("SpRepCV", folds = 5, reps = 4)
resamp = resample(makeLearner("classif.binomial"), task = task, rdesc)
##
plots = createSpatialResamplingPlots(task = task, resamp, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                     repetitions = 2, x.axis.breaks = c(8.5, 14.5),
                                     y.axis.breaks = c(47, 51))
plots[[1]][[3]]

# classic cv 
rdesc = makeResampleDesc("RepCV", folds = 5, reps = 4)
resamp = resample(makeLearner("classif.binomial"), task = task, rdesc)
##
plots.classic = createSpatialResamplingPlots(task = task, resamp, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                             repetitions = 2, x.axis.breaks = c(8.5, 14.5),
                                             y.axis.breaks = c(47, 51))
plots.classic[[1]][[3]]
plots.classic[[1]][[1]]
# plotting them all in a grid: 
require("cowplot")
cowplot::plot_grid(plotlist = plots[["Plots"]], ncol = 3, nrow = 1,
                   labels = plots[["Labels"]])

