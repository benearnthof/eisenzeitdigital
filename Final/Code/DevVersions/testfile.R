# notizen
# https://www.rspatial.org/analysis/index.html
# https://cran.r-project.org/web/packages/rdwd/vignettes/rdwd.html
# https://www.gis-blog.com/r-raster-data-acquisition/
# http://www.worldclim.org/current
# http://datawanderings.com/2018/09/01/r-point-in-polygon-a-mathematical-cookie-cutter/
# https://www.rdocumentation.org/packages/grDevices/versions/3.5.2/topics/chull
# https://gis.stackexchange.com/questions/227585/how-to-use-r-to-extract-data-from-worldclim
# http://mikemeredith.net/blog/1212_GIS_layer_for_Distance_from.htm#R_water

bayern_coords <- ger@polygons[[2]]@Polygons[[1]]@coords
ggplot() + geom_polygon(data = as.data.frame(bayern_coords), aes(x = x, y = y)) + coord_fixed(1.3)
ger_lvl3 <- raster::getData(name = "GADM", country = "Germany", level = 3)


# Climate: https://opendata.dwd.de/climate_environment/CDC/grids_germany/multi_annual/frost_days/
# https://grass.osgeo.org/grass74/manuals/addons/r.stream.distance.html

# logistic regression assumptions
# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/
# ROC explanation
# https://healthcare.ai/model-evaluation-using-roc-curves/

# source for rivershapefile used to generate distance raster and point distances
# https://biogeo.ucdavis.edu/data/diva/wat/DEU_wat.zip
# function documentation defaults
# http://r-pkgs.had.co.nz/man.html

# declustering algorithm for advanced spatial CV
# https://pdfs.semanticscholar.org/e31d/6f36b4ba66546ae41a29e1f6194507fd43d2.pdf

# binnedplot documentation 
# https://www.rdocumentation.org/packages/arm/versions/1.10-1/topics/binnedplot

# todo: 

# gam für regen/temp height               done
# stability of models                     done  
# plot interaction and interpret it       done
# interpret models                        done
# verify everything runs                  done
# folderstructure                         done
# variables in snake_case                 done
# model for viereckschanzen               done
# models for all points                   done
# documentation of functions              done
# pack everything in Rmarkdown            done

# interaction plots & Interpretation      done
# Predictive Mapping methods              done
# Predictive Maps plots                   done
# AUROC                                   done
# Resampling Methods                      done
# Fazit                                   done
# Ausblick                                done
# Zitieren                                done
