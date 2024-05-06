# author: Fernando Aramburu Merlos

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(CAST)
library(sf)
library(terra)


# prepare data -------------

d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")

pts <- vect(d[, cbind(lon, lat)], crs = "+proj=longlat +datum=WGS84") |>
  project("+proj=chamb +lat_1=22 +lon_1=0 +lat_2=22 +lon_2=45 +lat_3=-22 +lon_3=22.5 +datum=WGS84 +type=crs") |>
  st_as_sf()

pol <- vect("data/obs_polygons/obs_polygons_all_data.shp") |>
  project("+proj=chamb +lat_1=22 +lon_1=0 +lat_2=22 +lon_2=45 +lat_3=-22 +lon_3=22.5 +datum=WGS84 +type=crs") |>
  st_as_sf()


# K-NNDM indices -------
kfolds <- knndm(pts, pol)
dir.create("models", F, T)
saveRDS(kfolds, "models/KNNDM_folds.RDS")
