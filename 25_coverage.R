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
library(treeshap)
library(caret)
library(CAST)
library(stringr)

dir.create("plots/source_data/", F, T)

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

## survey data ------
d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")

#d <- d[, .SD, .SDcols = c("yield_kg_ha", "lon", "lat", pred_names)]
d <- d[, .(yield_kg_ha, lon, lat)]
d[, names(preds):= extract(preds, cbind(lon, lat))]

# deal with non-NA points
br <- 0
my_mean <- function(x) if(all(is.na(x))) NA else mean(x, na.rm = T)

while(any(is.na(d))) {
  br <- br + 1e3
  na_pts <- d[!complete.cases(d)] |> vect(crs = "+proj=longlat +datum=WGS84")
  na_buf <- buffer(na_pts, br)
  d[!complete.cases(d), names(preds):= extract(preds, na_buf, my_mean)[,-1]]
  if(br == 2e4) break()
}


## mask predictors -------
preds <- mask(preds, spam)
preds_df <- data.table(cell = cells(spam))
preds_df[, names(preds):= extract(preds, cell)]
preds_df[, prop:= extract(spam, cell)]
preds_df <- preds_df[prop > 0.01,]


# COMPUTE AOA --------------------------------

# Using BRT shap relative importance as weights
kfolds <- readRDS("models/KNNDM_folds.RDS")
shap <- readRDS("models/gbm/final_gbm_shap.RDS")
vi <- plot_feature_importance(shap)$data
setDT(vi)
vi[, variable:= str_remove(variable, "avg_")]
vi[, rimp:= round((importance/max(importance) * 100))]
all(pred_names %in% vi$variable)
vi <- vi[variable %in% pred_names]
dvi <- data.frame(t(vi[,2]))
names(dvi) <- vi$variable

aoa <- CAST::aoa(
  newdata = preds, train = d, weight = dvi, variables = pred_names,
  CVtest = kfolds$indx_test,
  CVtrain = kfolds$indx_train
)

dir.create("models/AOA/", F, T)
writeRaster(aoa$DI, filename = "models/AOA/DI.tiff", overwrite = TRUE)
writeRaster(aoa$AOA, filename = "models/AOA/AOA.tiff", overwrite = TRUE)
saveRDS(aoa, "models/AOA/aoa_object.RDS")


# PLOT DI ---------------
rdi <- rast("models/AOA/DI.tiff")
## save source data of DI map --------
# lower resolution to save space
terra::aggregate(rdi, 10, fun = "mean", na.rm = TRUE) |>
  writeRaster("plots/source_data/Supplementary_Fig_4.tiff", overwrite = TRUE,
            wopt = list(names = "Dissimilarity_Index", filetype = "GTiff",
                          gdal=c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6")))
#cols <- paletteer::paletteer_c("ggthemes::Red-Green-Gold Diverging", 13)
#cols <- rev(cols[c(1,seq(5,13,2))])
cols <- hcl.colors(6, "Red-Green") |> rev()
w <- geodata::world(path = "data/gadm")
w <- w[w$GID_0 != "SHN"]

spam_ha <- spam * cellSize(spam, unit = "ha")
p1 <- mask(spam_ha, rdi < 0.2, maskvalues = 0) |> global(sum, na.rm = T)
p2 <- mask(spam_ha, rdi > 0.2 & rdi < 0.4, maskvalues = 0) |> global(sum, na.rm = T)
p3 <- mask(spam_ha, rdi > 0.4 & rdi < 0.6, maskvalues = 0) |> global(sum, na.rm = T)
p4 <- mask(spam_ha, rdi > 0.6 & rdi < 0.8, maskvalues = 0) |> global(sum, na.rm = T)
p5 <- mask(spam_ha, rdi > 0.8 & rdi < 1, maskvalues = 0) |> global(sum, na.rm = T)
p6 <- mask(spam_ha, rdi > 1, maskvalues = 0) |> global(sum, na.rm = T)

tot <- global(spam_ha, sum, na.rm = T)


leg_txt <- paste0(
  c(paste(seq(0,.8,.2), "-", seq(.2,1,0.2)), "> 1"), " (",
  round(c(p1[1,1],p2[1,1],p3[1,1],p4[1,1],p5[1,1],p6[1,1])/tot[1,1] * 100),
  "%)"
)
leg_ttl <- "DI (% of area)"

tiff("maps/dissimilarity_index.tiff", width = 6, height = 5, units = "in", res = 300)
plot(w, ext = ext(rdi), background = "lightblue", col = "gray98", pax = list(las = 1))
plot(rdi, breaks = c(seq(0, 1, .2), 5), col = cols, add = T, maxcell= ncell(rdi),
     plg = list(legend=leg_txt, x="bottomleft", cex=1.2, bty="o", title = leg_ttl))
plot(w, add = T, lwd = 1.5)
dev.off()



# PLOT distributions  -------------
setorderv(vi, "importance", -1L)
pred_names <- vi$variable
pred_names <- pred_names[!pred_names %in% c("season_tavg", "soil_clay")]
lp <- vector("list", length(pred_names) + 1)
names(lp) <- pred_names
d[, prop:= 1]

ranges <- preds_df[, lapply(.SD, range, na.rm = T)]
ranges[2, soil_orgC:= 40]
ranges[2, soil_ECEC:= 40]
ranges[2, soil_ai:= 3]
ranges[2, season_prec:= 2000]

cols <- ggthemes::calc_pal()(2)


# legend
pleg <- ggplot() +
  xlim(0,2) +
  ylim(0,2) +
  geom_rect(aes(xmin = 0.1, xmax = 0.3, ymin = 0.8, ymax = 1.2),
            fill = cols[1], alpha = 0.5, color = "black") +
  geom_rect(aes(xmin = 0.1, xmax = 0.3, ymin = 1.6, ymax = 2.0),
            fill = cols[2], alpha = 0.5, color = "black") +
  annotate("text", x = 0.4, y = 1, label = "SSA", hjust = 0) +
  annotate("text", x = 0.4, y = 1.8, label = "Fields", hjust = 0) +
  theme_void() +
  theme(legend.position = "none")


for(i in pred_names) {
  ii <- rlang::sym(i)

  xlab <- str_replace(i, "_", " ") |>
    str_replace("gdd", "GDD") |>
    str_replace("ai", "AI") |>
    str_replace("rzpawhc", "RZPAWHC") |>
    str_replace("tavg", "Tavg") |>
    str_replace("srad", "Solar Rad")

  ggp <- ggplot(data = preds_df, aes(x = !!ii, weight = prop)) +
    geom_density(fill = cols[1], alpha = 0.5) +
    xlim(ranges[[i]][1], ranges[[i]][2]) +
    geom_density(data = d, fill = cols[2], alpha = 0.5) +
    xlab(xlab) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y = element_blank()) +
    annotate("text", label = paste0("RI = ", vi[variable == i, rimp], "%"),
             x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4)

#  if(which(i == pred_names) == 1) {
#    ggp <- ggp +
#      annotation_custom(ggplotGrob(pleg), xmin = 5, xmax = 30, ymin = 0.00, ymax = 0.3)
#  }
  lp[[i]] <- ggp
}
lp[[length(lp)]] <- pleg
ggsave(
  "plots/envir_var_1AF_SSA_comp.png",
  gridExtra::grid.arrange(grobs = lp),
  width = 6, height = 5, units = "in"
) |> suppressWarnings()

# save source data ----------------
ld <- vector("list", length(pred_names))
names(ld) <- pred_names
tot_ssa_prop <- sum(preds_df$prop)

for(j in pred_names) {
  precis = ifelse(ranges[[j]][2] > 1000, 0, ifelse(ranges[[j]][2] > 100, 1, 2))
  dt <- data.table(
    variable = j,
    value = seq(floor(ranges[[j]][1]), ceiling(ranges[[j]][2]), 10^(-precis))
  )
  d[, value:= round(get(j), precis)]
  dt <- d[, .(fields_prop = .N/nrow(d)), by = .(value)][dt, on = .(value)]
  preds_df[, value:= round(get(j), precis)]
  dt <- preds_df[, .(SSA_prop = sum(prop)/tot_ssa_prop), by = .(value)][dt, on = .(value)]
  dt[is.na(SSA_prop), SSA_prop:= 0]
  dt[is.na(fields_prop), fields_prop:= 0]
  ld[[j]] <- dt[fields_prop > 0 | SSA_prop > 0]
}
dt <- rbindlist(ld)
setcolorder(dt, "variable")
fwrite(dt, "plots/source_data/Supplementary_Fig_3.csv")
