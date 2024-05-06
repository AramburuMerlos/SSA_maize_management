# author: Fernando Aramburu Merlos
# date: 2022-11-01

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(partykit)

# prepare data -------------

d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")
d <- d[!is.na(strat), ]

d[, country:= stringr::str_to_title(country)]

strats <- d[, unique(strat)]


## pred selection ---------
preds <- c(
  # seasonal weather
  "season_prec", paste0("season_prec_", 1:3),
  # elevation and topography
  "elev", "twi",
  # soil properties
  "soil_rzpawhc", "soil_clay", "soil_pH","soil_orgC", "soil_ECEC",
  # planting management and cultivar
  "plant_date_dev", "pl_m2", "row_spacing",
  # cultivar
  "hybrid", "hyb_mat", "hyb_type", "hyb_yor", "hyb_tol_mln", "hyb_tol_msv",
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

## logical to yes/no -----------------------
for(j in preds[sapply(d[,..preds], is.logical)]) {
  set(d, i = NULL, j, as.character(d[[j]]))
  d[d[[j]] == "TRUE", (j):= "yes"]
  d[d[[j]] == "FALSE", (j):= "no"]
  set(d, i = NULL, j, as.factor(d[[j]]))
}

## change character to factor ----------
for(j in preds[sapply(d[,..preds], is.character)]) {
  set(d, i = NULL, j, as.factor(d[[j]]))
}

# Run trees -----------------------------------

# loop over CZ by country and season. Year considered by its environmental index
lt <- vector("list", length(strats))
names(lt) <- strats

for(i in seq_along(lt)) {

  ii <- d[, .I[strat == strats[i]]]
  di <- d[ii, ..ypreds]

  ## control for conditional inference trees
  ctrl <- ctree_control(
    teststat = "quadratic",
    testtype = "MonteCarlo",
    nresample = 1e4,
    mincriterion = .99,
    minsplit = min(round(di[,.N] * .20), 200),
    minbucket = max(min(round(di[,.N] * .05), 50), 16),
    maxdepth = 10
  )
  if(strats[i] %like% "UGA|NGA") {
    ctrl <- ctree_control(
      teststat = "quadratic",
      testtype = "MonteCarlo",
      nresample = 1e4,
      mincriterion = .90,
      minsplit = 10,
      minbucket = 8,
      maxdepth = 10
    )
  }

  # if there is only one year, remove year_index column
  if(length(unique(di$year_y_avg)) == 1) {
    di[, year_y_avg:= NULL]
  }

  # remove columns with not enough data
  # 25 % is needed for a split. So at least there should be 50% of not NA values
  for(j in preds) {
    j_NA_p <- sum(is.na(di[[j]])) # j_noneNA_p <- sum(!is.na(di[[j]])) ### TO BE FIX !!!!
    if(j_NA_p > ctrl$minsplit) di[, (j):= NULL] # if(j_noneNA_p < ctrl$minsplit * 2) di[, (j):= NULL] ## TO BE FIX!!!!
  }

  # 8% is needed in a terminal node.
  # So at least there should be 8% of incidence of an adversity to be considered.
  for(j in c("disease", "pest", "water_excess")) {
    j_T_p <- sum(di[[j]] == "yes") # note that logic vars were transformed to factor
    if(j_T_p < ctrl$minbucket) di[, (j):= NULL]
  }

  # fit model
  set.seed(0)
  lt[[i]] <- ctree(
    yield_kg_ha ~ .,
    data = di,
    control = ctrl
  )

  # if tree has no branches, inceas alpha and try again
  nnode <- nodeids(lt[[i]]) |> length()

  # some trees have no branches; change alpha
  while(nnode == 1) {
    ctrl$logmincriterion <- log(exp(ctrl$logmincr) - .05)

    lt[[i]] <- ctree(
      yield_kg_ha ~ .,
      data = di,
      control = ctrl
    )
    nnode <- nodeids(lt[[i]]) |> length()
  }
  d[ii, cit_yhat:= predict(lt[[i]])]
}


dir.create("models/ctree/", FALSE, TRUE)
fwrite(d, "models/ctree/data_with_ctree_preds.csv")
saveRDS(lt, "models/ctree/ctrees_by_ctry_cz_season.RDS")

d[, cor(yield_kg_ha, cit_yhat)^2]


