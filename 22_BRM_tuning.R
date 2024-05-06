# author: Fernando Aramburu Merlos

# Use gradient boosting machine to analyze pooled data to check variance importance across all observations

# Step 1: Tuning

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(xgboost)

dir.create("models/gbm", F, T)

# prepare data -------------

d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")


# K-NNDM indices -------
kfolds <- readRDS("models/KNNDM_folds.RDS")


## pred selection ---------
preds <- c(
  # avg season weather
  "avg_season_gdd", "avg_season_ai", "avg_season_tavg",
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



# TUNE GBM with KNNDM --------------------

tp <- expand.grid(
   eta = c(.005, .01, 0.02, 0.05, 0.1, 0.2, 0.5),
   max_depth = c(3, 5, 10, 15),
   min_child_weight = c(10, 20, 40),
   subsample = c(0.6, 0.8, 1),
   min_RMSE = NA_real_,
   opt_tree = NA_real_
) |> setDT()

dm <- as.matrix(d[, ..preds])
class(dm[1,1])

# if this is not the initial tuning attempt
# if(file.exists("models/gbm/tuning_knndm.csv")) {
#   tp_old <- fread("models/gbm/tuning_knndm.csv")
#   tp[, min_RMSE:= NULL]
#   tp[, opt_tree:= NULL]
#   tp <- tp_old[tp, on = .NATURAL]
#   rm(tp_old)
# }

pb = txtProgressBar(min = 0, max = nrow(tp), initial = 0)

for(i in seq_len(nrow(tp))) {

  if(!is.na(tp[i, min_RMSE])) next

  params <- list(
    eta = tp$eta[i],
    max_depth = tp$max_depth[i],
    min_child_weight = tp$min_child_weight[i],
    objective = "reg:squarederror"
  )

  set.seed(0)
  m_cv <- xgb.cv(
    params = params,
    data = dm,
    label = d$yield_kg_ha,
    nrounds = 1500,
    nfold = 5,
    folds = kfolds$indx_test,
    train_folds = kfolds$indx_train,
    verbose = 0,
    early_stopping_rounds = 20
  )

  tp[i, opt_tree:= which.min(m_cv$evaluation_log$test_rmse_mean)]
  tp[i, min_RMSE:= min(m_cv$evaluation_log$test_rmse_mean)]

  setTxtProgressBar(pb, i)
}


setorder(tp, min_RMSE)
head(tp, 20)
fwrite(tp, "models/gbm/tuning_knndm.csv")

tp[which.min(min_RMSE)]




# TUNE GBM with reg K-fold cV --------------------

tp <- expand.grid(
  eta = c(0.01, 0.1),
  max_depth = c(5, 10),
  min_child_weight = c(20, 40),
  subsample = 0.6,
  min_RMSE = NA_real_,
  opt_tree = NA_real_
) |> setDT()

dm <- as.matrix(d[, ..preds])
class(dm[1,1])

# if this is not the initial tuning attempt
if(file.exists("models/gbm/tuning_kcv.csv")) {
  tp_old <- fread("models/gbm/tuning_kcv.csv")
  tp[, min_RMSE:= NULL]
  tp[, opt_tree:= NULL]
  tp <- tp_old[tp, on = .NATURAL]
  rm(tp_old)
}

pb = txtProgressBar(min = 0, max = nrow(tp), initial = 0)

for(i in seq_len(nrow(tp))) {

  if(!is.na(tp[i, min_RMSE])) next

  params <- list(
    eta = tp$eta[i],
    max_depth = tp$max_depth[i],
    min_child_weight = tp$min_child_weight[i],
    objective = "reg:squarederror"
  )

  set.seed(0)
  m_cv <- xgb.cv(
    params = params,
    data = dm,
    label = d$yield_kg_ha,
    nrounds = 1500,
    nfold = 5,
    verbose = 0,
    early_stopping_rounds = 20
  )

  tp[i, opt_tree:= which.min(m_cv$evaluation_log$test_rmse_mean)]
  tp[i, min_RMSE:= min(m_cv$evaluation_log$test_rmse_mean)]

  setTxtProgressBar(pb, i)
}


setorder(tp, min_RMSE)
head(tp, 20)
fwrite(tp, "models/gbm/tuning_kcv.csv")

tp[which.min(min_RMSE)]


