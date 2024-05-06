# author: Fernando Aramburu Merlos

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(corrplot)
library(ltm)

# load data and trees -------------

d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")
d <- d[!is.na(strat)]

d[country %in% c("Rwanda", "Burundi"), country:= "Rwanda - Burundi"]

# pred selection ---------
preds <- c(
  # seasonal weather
  paste0("season_prec_", 1:3),
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


dt <- d[, ..preds]

# Continuous (numeric) variables -----------------

ncols <- names(dt)[sapply(dt, is.numeric)]
M <- cor(dt[, ..ncols], use = "pairwise.complete.obs") |> suppressWarnings()

# matrix of the p-value of the correlation
p.mat <- cor.mtest(dt[, ..ncols])$p |> suppressWarnings()

# colour scale
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# correlation matrix plot
png(file= "plots/corrplot_num_vars.png", w=10,h=10, res=600, units="in")
corrplot(
  M, method = "color", col = col(200),
  type = "upper", order = "original", number.cex = 1,
  addCoef.col = "black",# na.label = "square",  na.label.col = "orange", # Add coefficient of correlation
  tl.col = "black", tl.srt = 90, # Text label color and rotation
  # Combine with significance
  p.mat = p.mat, sig.level = 0.01, insig = "blank",
  # hide correlation coefficient on the principal diagonal
  diag = FALSE)
dev.off()



# Dichotomous (logical) variables ------------------------------------------------
lcols <- names(dt)[sapply(dt, is.logical)]
Ml <- cor(dt[, ..lcols], use = "pairwise.complete.obs") |> suppressWarnings()

for(j in lcols) dt[, (j):= as.factor(get(j))]

# matrix of the p-value of the correlation
pml <- matrix(nrow = length(lcols), ncol = length(lcols))
rownames(pml) <- colnames(pml) <- lcols

for(i in seq_along(lcols)) {
  for(j in seq_along(lcols)) {
    dtt <- dt[!is.na(get(lcols[i])) & !is.na(get(lcols[j])),]
    dtt <- droplevels(dtt)
    if(nlevels(dtt[, get(lcols[i])]) < 2 | nlevels(dtt[, get(lcols[j])]) < 2) {
      pml[i,j] <- NA
    } else {
      pml[i,j] <- chisq.test(x = dtt[, get(lcols[i])], y = dtt[, get(lcols[j])])$p.value |>
        suppressWarnings()
    }
    rm(dtt)
  }
}

Ml[is.na(Ml)] <- 0
pml[is.na(pml)] <- 1


# correlation matrix plot
png(file= "plots/corrplot_logic_vars.png", w=10,h=10, res=600, units="in")
corrplot(
  Ml, method = "color", col = col(200),
  type = "upper", order = "original", number.cex = 1,
  addCoef.col = "black",# na.label = "square",  na.label.col = "orange", # Add coefficient of correlation
  tl.col = "black", tl.srt = 90, # Text label color and rotation
  # Combine with significance
  p.mat = pml, sig.level = 0.01, insig = "blank",
  # hide correlation coefficient on the principal diagonal
  diag = FALSE)
dev.off()


# Dichotomous vs conitnous -------------------------
m2 <- pm2 <- matrix(nrow = length(ncols), ncol = length(lcols))
rownames(m2) <- rownames(pm2) <- ncols
colnames(m2) <- colnames(pm2) <- lcols

for(i in seq_along(ncols)) {
  for(j in seq_along(lcols)) {
    dtt <- dt[!is.na(get(ncols[i])) & !is.na(get(lcols[j])),]
    dtt <- droplevels(dtt)
    if(nlevels(dtt[, get(lcols[j])]) < 2) {
      m2[i,j] <- 0
      pm2[i,j] <- 1
    } else {
    m2[i,j] <- -biserial.cor(dtt[, get(ncols[i])], dtt[, get(lcols[j])])
    pm2[i,j] <- kruskal.test(dt[, get(ncols[i])], dt[, get(lcols[j])])$p.value
    }
  }
}

# correlation matrix plot
png(file= "plots/corrplot_log_vs_num_vars.png", w=10,h=10, res=600, units="in")
corrplot(
  m2, method = "color", col = col(200),
  order = "original", number.cex = 1,
  addCoef.col = "black",# na.label = "square",  na.label.col = "orange", # Add coefficient of correlation
  # Combine with significance
  p.mat = pm2, sig.level = 0.01, insig = "blank",
  tl.col = "black", tl.srt = 90, # Text label color and rotation
)
dev.off()


# Boxplots ------------------------------


dt[hybrid == TRUE, hyb:= "Hybrid"]
dt[hybrid == FALSE, hyb:= "OPV"]
dt[fert_in_hole == TRUE, fert:= "Incorporated"]
dt[fert_in_hole == FALSE, fert:= "Broadcasted"]

rhn <- -biserial.cor(d$N_kg_ha, d$hybrid, use = "complete.obs") |> round(2)
rhs <- -biserial.cor(d$plant_date_dev, d$hybrid, use = "complete.obs") |> round(2)
rhp <- -biserial.cor(d$pl_m2, d$hybrid, use = "complete.obs") |> round(2)

rfn <- -biserial.cor(d$N_kg_ha, d$fert_in_hole, use = "complete.obs") |> round(2)
rfs <- -biserial.cor(d$plant_date_dev, d$fert_in_hole, use = "complete.obs") |> round(2)
rfp <- -biserial.cor(d$pl_m2, d$fert_in_hole, use = "complete.obs") |> round(2)


png(file= "plots/boxplot_hyb_mgmt.png", width = 7, height = 5, unit = "in", res = 300)
par(mfrow = c(2,3), las = 1, mar = c(4,5,1,1))

# Hybrid
boxplot(N_kg_ha ~ hyb, dt, xlab = NA, ylab = expression(N~rate~(kg~ha^-1)))
bquote(italic(r[pb])~"="~.(rhn)) |>
  mtext(line = -1.1, cex = 0.6)

boxplot(plant_date_dev ~ hyb, dt, xlab = NA, ylab = "Sowing date (days from CZ avg.)")
bquote(italic(r[pb])~"="~.(rhs)) |>
  mtext(line = -1.1, cex = 0.6)

boxplot(pl_m2 ~ hyb, dt, xlab = NA, ylab = expression(Plant~density~(plants~m^-2)))
bquote(italic(r[pb])~"="~.(rhp)) |>
  mtext(line = -1.1, cex = 0.6)

# Fertilization method
boxplot(N_kg_ha ~ fert, dt, xlab = NA, ylab = expression(N~rate~(kg~ha^-1)))
bquote(italic(r[pb])~"="~.(rfn)) |>
  mtext(line = -1.1, cex = 0.6)

boxplot(plant_date_dev ~ fert, dt, xlab = NA, ylab = "Sowing date (days from CZ avg.)")
bquote(italic(r[pb])~"="~.(rfs)) |>
  mtext(line = -1.1, cex = 0.6)

boxplot(pl_m2 ~ fert, dt, xlab = NA, ylab = expression(Plant~density~(plants~m^-2)))
bquote(italic(r[pb])~"="~.(rfp)) |>
  mtext(line = -1.1, cex = 0.6)


dev.off()

# save source data --------
write.csv(M, row.names = TRUE, "plots/source_data/Supplementary_Fig_6.csv")
write.csv(Ml, row.names = TRUE, "plots/source_data/Supplementary_Fig_7.csv")
write.csv(m2, row.names = TRUE, "plots/source_data/Supplementary_Fig_8.csv")
dt[, .(N_kg_ha, plant_date_dev, pl_m2, hybrid_seed_use = hyb, fertilization_method = fert)] |>
  fwrite("plots/source_data/Supplementary_Fig_9.csv")


