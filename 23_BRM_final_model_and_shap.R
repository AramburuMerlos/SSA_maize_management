# author: Fernando Aramburu Merlos

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(xgboost)
library(treeshap)

# prepare data -------------

d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")


## pred selection ---------
preds <- c(
  # avg season weather
  "avg_season_gdd", "avg_season_tavg", "avg_season_ai",
  # seasonal weather
  "season_prec",
  # topography
  "twi",
  # soil properties
  "soil_rzpawhc", "soil_clay", "soil_pH","soil_orgC", "soil_ECEC",
  # planting management and cultivar
  "plant_date_dev", "pl_m2", "row_spacing",
  # cultivar
  "hybrid", "hyb_mat", "hyb_yor", "hyb_tol_mln", "hyb_tol_msv",
  "hyb_tol_gls", "hyb_tol_nclb", "hyb_tol_rust", "hyb_tol_ear_rot",
  # nutrient management
  "N_kg_ha", "P_kg_ha", "K_kg_ha",
  "compost", "comp_kg_ha", "manure",
  "fert_in_hole",
  # liming
  "lime_kg_ha",
  # crop management
  "weeding", "pesticide",
  # adversities
  "disease", "pest", "striga", "water_excess"
)

ypreds <- c("yield_kg_ha", preds)



# FINAL GBM ---------------------------

tp <- fread("models/gbm/tuning_knndm.csv")

cv_rmse <- min(tp$min_RMSE)
f_varp <- function(x) sum((x - mean(x))^2)/length(x)
cv_r2 <- 1 - (cv_rmse^2/f_varp(d$yield_kg_ha))

dm <- as.matrix(d[, ..preds])

params <- tp[
  which.min(min_RMSE),
  .(eta, max_depth, min_child_weight)
] |>
  as.list()

gbm <- xgboost(
  params = params,
  data = dm,
  label = d$yield_kg_ha,
  nrounds = tp[which.min(min_RMSE), opt_tree],
  verbose = 0
)

saveRDS(gbm, "models/gbm/final_gbm.RDS")

gbmu <- treeshap::xgboost.unify(gbm, dm)

shap <- treeshap(gbmu, dm)
saveRDS(shap, "models/gbm/final_gbm_shap.RDS")

