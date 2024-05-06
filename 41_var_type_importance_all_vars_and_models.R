# author: Fernando Aramburu Merlos

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(terra)
library(partykit)
library(stringr)
library(xgboost)
library(treeshap)

# Variable type -----------------------------
pred_list <- list(
  management = c("N_kg_ha", "P_kg_ha", "K_kg_ha", "fert_in_hole", "compost", "comp_kg_ha", "manure",
                 "hybrid", "hyb_mat", "hyb_type", "hyb_yor", "hyb_tol_mln", "hyb_tol_msv",
                 "hyb_tol_gls", "hyb_tol_nclb", "hyb_tol_rust", "hyb_tol_ear_rot",
                 "plant_date_dev", "pl_m2", "row_spacing",
                 "weeding", "pesticide", "disease", "pest", "striga", "lime_kg_ha"),
  climate = c("avg_season_tavg", "avg_season_gdd", "avg_season_ai"),
  rainfall = c("season_prec", paste0("season_prec_", 1:3), "water_excess"),
  elevation = c("elev"),
  topography = c("twi"),
  soil = c("soil_rzpawhc", "soil_clay",  "soil_pH","soil_orgC", "soil_ECEC")
)

# CIT --------------

## PREPARE BARPLOT ----------------
lt <- readRDS("models/ctree/ctrees_by_ctry_cz_season.RDS")
lt <- lt[sort(names(lt))]

getUsefulPredictors <- function(x) {
  varid <- nodeapply(x, ids = nodeids(x),
                     FUN = function(n) split_node(n)$varid)
  varid <- unique(unlist(varid))
  names(data_party(x))[varid]
}

vl <- sapply(lt, getUsefulPredictors)

vi <-  vl |>
  unlist() |>
  table() |>
  sort()

var_type_indx <- sapply(names(vi), function(x) sapply(pred_list, function(y) x %in% y)) |>
  apply(2, which)

all(names(vi) %in% unlist(pred_list))

vtl <- vector("list", length(vl))

for(i in seq_along(vl)) {
  if(length(vl[[i]]) == 0) next

  if(length(vl[[i]]) > 1) {
    vtl[[i]] <- names(pred_list)[
      sapply(pred_list, function(x) vl[[i]] %in% x) |>
        apply(1, which)
    ] |>
      unique()
  } else {
    vtl[[i]] <- names(pred_list)[
      sapply(pred_list, function(x) vl[[i]] %in% x) |>
        which()
    ]
  }
}

vti <- vtl |>
  unlist() |>
  table() |>
  sort()

vti <- vti/length(vtl) * 100

# PLOT -----------------
png("plots/variable_type_importance_CIT.png", width = 4, height = 3, res = 300, units="in")

par(mar = c(4,6.5,1,1), mgp = c(2.5,1,0), xpd = NA)
cols <- RColorBrewer::brewer.pal(7, "Set2")[c(6,7,2,3,1)]
barplot(vti, col = cols,
        horiz = TRUE, las = 1, cex.names = .9,
        xlim = c(0,100), xlab = "Relative importance (%)")
dev.off()

# BRT ----------
gbm <- readRDS("models/gbm/final_gbm.RDS")
d <- xgb.importance(model = gbm)
setDT(d)
d <- d[,.(variable = Feature, importance = Frequency)]
#shap <- readRDS("models/gbm/final_gbm_shap.RDS")
#d <- plot_feature_importance(shap)$data
#setDT()
#d <- d[!variable %like% "hyb_tol"]
#d <- d[!variable %in% c("pest", "water_excess")]
pl <- rbindlist(lapply(pred_list, as.data.frame), idcol = "type")
setnames(pl, names(pl), c("type", "variable"))
pl[type == "rainfall", type:= "climate"]
all(d$variable %in% pl$variable)
d <- d[pl, on = .(variable)]
d <- d[!is.na(importance)]
#dd <- d[, .(importance= mean(importance)), by = .(type)]
dd <- d[, .(importance= sum(importance)), by = .(type)]
dd[, importance:= importance/sum(importance) * 100]
setorder(dd, type)
dd

## plot --------------
png("plots/feature_imp_BRT_var_type_pie.png", width = 4, height = 4, res = 300, units="in")
par(mar = c(3,3,3,3), xpd = NA)
cols <- RColorBrewer::brewer.pal(7, "Set2")[c(3,1,7,6)]
pie(dd$importance, labels = dd$type, col = cols, main = "Relative importance (%)")
dev.off()

# 2 panel plot ---------
dd[, importance:= importance/max(importance) * 100]
setorder(dd, importance)
png("plots/var_type_imp_CIT_BRT.png", width = 6.5, height = 3.25, res = 300, units="in")
par(mfrow = c(1,2), mar = c(3,6,1,1), xpd = NA, oma = c(1,0,0,0))
cols <- RColorBrewer::brewer.pal(7, "Set2")[c(6,7,2,3,1)]
barplot(vti, col = cols,
        horiz = TRUE, las = 1, cex.names = .9,
        xlim = c(0,100))
mtext("A", side = 3, adj = 0.95)
cols <- RColorBrewer::brewer.pal(7, "Set2")[c(1,3,7,6)] |> rev()
barplot(dd$importance, names.arg = dd$type, col = cols,
        horiz = TRUE, las = 1, cex.names = .9,
        xlim = c(0,100))
mtext("B", side = 3, adj = 0.95)
mtext("Relative importance (%)", side = 1, outer  = TRUE, xpd = NA, line = -0.5, adj = 0.6)
dev.off()

# save source data -------------
setnames(dd, "importance", "BRT_importance")
dout <- merge(data.table(type = names(vti), CIT_importance = as.vector(vti)), dd, by = "type", all = TRUE)
setorderv(dout, "CIT_importance", order = -1L)
fwrite(dout, "plots/source_data/Supplementary_Fig_11.csv")
