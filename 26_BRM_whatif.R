# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(xgboost)
library(terra)
library(stringr)

# prepare data -------------
ex <- ext(c(-18, 50, -35, 15))

## SPAM --------
# rainfed and irrigated wheat, maize, and rice
spam <- rast("../GYGA/data/SPAM/harvested_area_global_merged_30sec/MAIZ_R.tif")
spam <- crop(spam,  ex)
spam[spam < 0.001] <- NA

## predictors --------
pred_names <- c(
  # seasonal climate
  "season_gdd", "season_ai", "season_tavg", "season_prec",
  # soil properties
  "soil_rzpawhc", "soil_clay", "soil_pH","soil_orgC", "soil_ECEC"
)

# Climate
season_preds <- list.files("data/seasonal", ".tif", full.names = TRUE) |>
  rast() |>
  subset(str_subset(pred_names, "season"))

# soil
soil_rzpawhc <- rast("data/envir/RZPAWHC_east_africa_1km.tif")
soil_clay    <- geodata::soil_af_isda("clay", 20, path = "data/envir/isda/")
soil_pH      <- geodata::soil_af_isda("pH.h2o", 20, path = "data/envir/isda/")
soil_orgC    <- geodata::soil_af_isda("oc", 20, path = "data/envir/isda/")
soil_ECEC    <- geodata::soil_af_isda("ecec.f", 20, path = "data/envir/isda/")

names(soil_rzpawhc) <- "soil_rzpawhc"
names(soil_clay   ) <- "soil_clay"
names(soil_pH     ) <- "soil_pH"
names(soil_orgC   ) <- "soil_orgC"
names(soil_ECEC   ) <- "soil_ECEC"


## Crop and allign --------
season_preds <- crop(season_preds, ex)

soil_rzpawhc <- project(soil_rzpawhc, spam) |> crop(ex)
soil_clay    <- crop(soil_clay   , ex)
soil_pH      <- crop(soil_pH     , ex)
soil_orgC    <- crop(soil_orgC   , ex)
soil_ECEC    <- crop(soil_ECEC   , ex)

soil_preds <- c(soil_rzpawhc, soil_clay, soil_pH, soil_orgC, soil_ECEC)

preds <- c(season_preds, soil_preds)

## mask predictors -------
preds <- mask(preds, spam)

## Area of applicability --------
r_di <- rast("models/AOA/DI.tiff") |> crop(spam) |> mask(spam)

# final GBM model --------------
gbm <- readRDS("models/gbm/final_gbm.RDS")
shap <- readRDS("models/gbm/final_gbm_shap.RDS")

# Predict Tech Levels ------------
d <- data.table(cell = cells(spam))
d[, names(preds):= extract(preds, cell)]
setnames(d, c("season_tavg", "season_gdd", "season_ai"), c("avg_season_tavg", "avg_season_gdd", "avg_season_ai"))
d[, prop:= extract(spam, cell)]
d[, di:= extract(r_di, cell)]
d <- d[di < 0.4,]

dd <- data.table(
  tech_level = 0:4,
  CV = c("OPV", rep("HYB", 4)),
  NF = c("low", rep("high", 4)),
  PC = c('no', 'no', rep('yes', 3)),
  PD = c(rep('low',3), rep('high',2)),
  SD = c(rep('late',4), 'early')
)

## highest tech -------
d[, pl_m2:= 5]
d[, hybrid:= TRUE]
d[, N_kg_ha:= 60]
d[, P_kg_ha:= 40]
d[, weeding:= TRUE]
d[, plant_date_dev:= -15]
d[, pesticide:= TRUE]
d[, fert_in_hole:= TRUE]
d[, compost:= TRUE]
d[, comp_kg_ha:= 5e4]
d[, (gbm$feature_names[!gbm$feature_names %in% names(d)]):= NA]
d[, T4_yield:= predict(gbm, newdata = as.matrix(cbind(.SD))), .SDcols = gbm$feature_names]
dd[tech_level == 4, yield:= round(d[, sum(T4_yield * prop/sum(prop))]/1e3,1)]
dd[tech_level == 4, se:= d[, sd(T4_yield)/sqrt(.N)]]

## late planting date ---------
d[, plant_date_dev:= 10]
d[, T3_yield:= predict(gbm, newdata = as.matrix(cbind(.SD))), .SDcols = gbm$feature_names]
dd[tech_level == 3, yield:= round(d[, sum(T3_yield * prop/sum(prop))]/1e3,1)]
dd[tech_level == 3, se:= d[, sd(T3_yield)/sqrt(.N)]]

## low plant density ----------
d[, pl_m2:= 2]
d[, T2_yield:= predict(gbm, newdata = as.matrix(cbind(.SD))), .SDcols = gbm$feature_names]
dd[tech_level == 2, yield:= round(d[, sum(T2_yield * prop/sum(prop))]/1e3,1)]
dd[tech_level == 2, se:= d[, sd(T2_yield)/sqrt(.N)]]

## no pest control ------------
d[, weeding:= FALSE]
d[, pesticide:= FALSE]
d[, T1_yield:= predict(gbm, newdata = as.matrix(cbind(.SD))), .SDcols = gbm$feature_names]
dd[tech_level == 1, yield:= round(d[, sum(T1_yield * prop/sum(prop))]/1e3,1)]
dd[tech_level == 1, se:= d[, sd(T1_yield)/sqrt(.N)]]

## no hybrids and no nutrient application -----------
d[, hybrid:= FALSE]
d[, N_kg_ha:= 0]
d[, P_kg_ha:= 0]
d[, fert_in_hole:= FALSE]
d[, compost:= FALSE]
d[, comp_kg_ha:= 0]
d[, T0_yield:= predict(gbm, newdata = as.matrix(cbind(.SD))), .SDcols = gbm$feature_names]
dd[tech_level == 0, yield:= round(d[, sum(T0_yield * prop/sum(prop))]/1e3,1)]
dd[tech_level == 0, se:= d[, sd(T0_yield)/sqrt(.N)]]

dd

## save results ---------
fwrite(dd, "models/gbm/scenario_summary_results.csv")


# Map extremes -------

rht <- rast(spam)
vht <- rep(NA_real_, ncell(spam))
vht[d$cell] <- d$T4_yield/1e3
values(rht) <- vht

rlt <- rast(spam)
vlt <- rep(NA_real_, ncell(spam))
vlt[d$cell] <- d$T0_yield/1e3
values(rlt) <- vlt

w <- geodata::world(path = "data/gadm") |> project(spam)
w <- w[w$GID_0 != "SHN"]
#col_yield <- paletteer::paletteer_c("ggthemes::Red-Green-Gold Diverging", 256)
col_yield <- viridis::viridis(256)
col_spam <- paletteer::paletteer_c("grDevices::Light Grays", 256) |> rev()


tiff("maps/scenarios.tiff", width = 12, height = 5, units = "in", res = 300)

par(mfrow = c(1,2), omi = c(0,.3,.3,1))
plot(w, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ex,
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(spam, maxcell = ncell(spam), col = col_spam, add = TRUE,
     type = "continuous", legend = FALSE, range = c(0,1))
plot(rlt, maxcell = ncell(rlt), col = col_yield, add = TRUE,
     type = "continuous", legend = FALSE, range = c(0,6))
plot(w, add = T)
mtext(bquote(Baseline:~.(dd[tech_level == 0, yield])~t~ha^-1), 3, line = 3, font = 2, cex = 1.5)

plot(w, lwd = 0.5, border = "gray40", col = "white", background = "lightblue",
     pax = list(ticks = 0, labels = FALSE), ext = ex,
     mar = c(.1,.1,.1,.1), legend = FALSE)
plot(spam, maxcell = ncell(spam), col = col_spam, add = TRUE,
     type = "continuous", legend = FALSE, range = c(0,1))
plot(rht, maxcell = ncell(rht), col = col_yield, add = TRUE,
     type = "continuous", range = c(0,6))
plot(w, add = T)
mtext(bquote(Intensified:~.(dd[tech_level == 4, yield])~t~ha^-1), 3, line = 3, cex = 1.5)
dev.off()

# save source data ----------
rout <- c(
  aggregate(rlt, 10, fun = "mean", na.rm = TRUE),
  aggregate(rht, 10, fun = "mean", na.rm = TRUE)
)
names(rout) <- c("baseline_management", "intensified_management")
writeRaster(rout, "plots/source_data/Supplementary_Fig_5.tiff", overwrite = TRUE,
            wopt = list(names =  c("baseline_management", "intensified_management"), filetype = "GTiff",
                          gdal=c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
)

