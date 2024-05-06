# author: Fernando Aramburu Merlos
# date: 2023-01-02

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
d <- d[!is.na(strat)]

d[, country:= stringr::str_to_title(country)]


## stratification ------------------------
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

pred_list <- list(
  response = "yield_kg_ha",
  environmental = c("elev", "season_prec", paste0("season_prec_", 1:3), "twi",
                    "soil_rzpawhc", "soil_clay", "soil_sand", "soil_pH","soil_orgC",
                    "soil_ECEC", "soil_Al","soil_rzpawhc"),
  `crop establishment` = c("plant_date_dev", "pl_m2", "row_spacing"),
  cultivar = c("hybrid", "hyb_mat", "hyb_type", "hyb_yor", "hyb_tol_mln", "hyb_tol_msv",
               "hyb_tol_gls", "hyb_tol_nclb", "hyb_tol_rust", "hyb_tol_ear_rot"),
  nutrients = c("N_kg_ha", "P_kg_ha", "K_kg_ha", "fert_in_hole", "compost", "comp_kg_ha", "manure"),
  adversities = c("weeding", "pesticide", "disease", "pest", "striga", "water_excess"),
  liming = c("lime_kg_ha")
)


# # preapere table ----------------
d[, hyb_mat:= ceiling(hyb_mat)]
sapply(d[,..preds], class)

tl <- vector("list", length(preds))
i = 9
for(i in seq_along(preds)) {
  pred <- preds[i]
  v <- d[[pred]]
  fgs <- ifelse(max(v, na.rm = T) > 100, 0, ifelse(max(v, na.rm = T) > 10, 1, 2))
  tl[[i]] <- data.table(
    category = names(pred_list)[sapply(pred_list, function(x) pred %in% x)],
    name = pred,
    type = if(is.logical(v)) "binary" else if(pred == "hyb_mat") "ordinal" else "continuous",
    units = if(pred %like% "kg_ha") "kg/ha" else "",
    desc = "insert desc",
    stats = if(pred == "hyb_mat") {
      paste(d[!is.na(hyb_mat), .N, by = .(hyb_mat)][order(hyb_mat), round(N/sum(N) * 100)], collapse = "-")
    } else if(is.numeric(v)) {
      paste(round(mean(v, na.rm = T), fgs), "±", round(sd(v, na.rm = TRUE), fgs))
    } else if (is.logical(v)) {
      paste0(round(sum(d[[pred]], na.rm = T)/sum(!is.na(d[[pred]])) * 100), "%")
    } else "",
    `missing values` = if(pred == 'hyb_mat'){
      round((1 - sum(!is.na(d[[pred]]))/d[hybrid == TRUE,.N]) * 100)
    } else if(pred == "manure") {
      round((1 - sum(!is.na(d[[pred]]))/d[compost == TRUE,.N]) * 100)
    } else {
      round(sum(is.na(d[[pred]]))/d[,.N] * 100)
    }

  )
}

dt <- rbindlist(tl)
dt
dir.create("tables", F)
fwrite(dt, "tables/var_desc.csv")
