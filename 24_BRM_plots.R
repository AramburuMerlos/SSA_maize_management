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
library(ggplot2)
library(treeshap)
library(stringr)
library(grid)
library(gridExtra)
library(gridtext)

dir.create("plots/pdf", F, T)

# load model and shap --------
gbm <- readRDS("models/gbm/final_gbm.RDS")

shap <- readRDS("models/gbm/final_gbm_shap.RDS")

vi <- plot_feature_importance(shap)$data
setDT(vi)

# Variable importance -----------
## predictor list ---------
pred_list <- list(
  a.nutrients = c("N_kg_ha", "P_kg_ha", "K_kg_ha", "fert_in_hole", "compost", "comp_kg_ha", "manure"),
  c.cultivar = c("hybrid", "hyb_mat", "hyb_yor", "hyb_tol_mln", "hyb_tol_msv",
               "hyb_tol_gls", "hyb_tol_nclb", "hyb_tol_rust", "hyb_tol_ear_rot"),
  b.establishment = c("plant_date_dev", "pl_m2", "row_spacing"),
  d.pest_mgmt. = c("weeding", "pesticide", "disease", "pest", "striga", "water_excess"),
  e.liming = c("lime_kg_ha"),
  f.seasonal_climate = c("avg_season_gdd", "avg_season_tavg", "avg_season_ai", "season_prec"),
  g.topography = c("twi"),
  h.soil = c("soil_rzpawhc", "soil_clay",  "soil_pH","soil_orgC", "soil_ECEC")
)
pl <- rbindlist(lapply(pred_list, as.data.frame), idcol = "type")
setnames(pl, names(pl), c("type", "variable"))
all(vi$variable %in% unlist(pred_list))
vi <- vi[pl, on = .(variable)]
vi

##  rename variables --------
vi[, var_name:= variable]
vi[variable %like% "^._kg_ha", var_name:= paste(substr(variable, 1, 1), "fertilizer")]

vi$var_name <- vi$var_name |>
  str_replace("comp_kg_ha", "compost quantity") |>
  str_replace_all("_", " ") |>
  str_remove("kg ha") |>
  str_trim("both") |>
  str_replace("hybrid", "hybrid seed use") |>
  str_replace("fert in hole", "fertilizer placement") |>
  str_replace("plant date dev", "sowing date") |>
  str_replace("pest$", "pest damage") |>
  str_replace("pl m2", "  plant density") |>
  str_replace("^compost$", "compost use") |>
  str_replace("pesticide", "pesticide use") |>
  str_remove("avg") |>
  str_replace("^hyb ", "hybrid ") |>
  str_replace(" mat$", " maturity")

vi <- vi[!is.na(importance)]

#vi[, importance:= importance/max(importance)*100]

## plot --------------
leg_txt <- names(pred_list) |>
  str_remove("^..") |>
  str_replace("_", " ")

### All variables ------
setorderv(vi, "importance", order = 1L, na.last = TRUE)


### crop mgnt vars ----------------
vim <- vi[type %in% names(pred_list)[1:5]]
vim <- vim[(.N-8):.N,]

# Variable dependence -------------
dir.create("plots/source_data", F, T)
vim[variable == "fert_in_hole", var_name:= "fertilizer placement"]
vim <- vim[variable != "hyb_mat",]
vim
setorderv(vim, c("type", "importance"), c(1,-1))
i=1
ld <- lp <- lg <- lw <- vector(mode = "list", vim[,.N])

for(i in seq_along(lp)) {
  ii <- vim$variable[i]
  d <- data.table(variable = shap$observations[,ii], shap = shap$shaps[,ii])
  d <- d[complete.cases(d),]
  setorder(d, variable, shap)
  if(ii == "P_kg_ha") d <- d[variable < 51]
  if(ii == "N_kg_ha") d <- d[variable < 200]
  if(ii == "plant_date_dev") d <- d[variable > -50 & variable < 50]

  if(ii %in% c("hybrid", "weeding", "pesticide", "fert_in_hole")) {
    d <- d[variable %in% c(0,1),]
    d[variable == 0, bar:= if(ii == "fert_in_hole") "surface" else "no"]
    d[variable == 1, bar:= if(ii == "fert_in_hole") "in a hole" else "yes"]
    dt <- d[, .(variable_name = ii, variable_value = bar, shap_value = shap)]
    ld[[i]] <- dt

    lp[[i]] <- ggplot(d) +
      aes(x = bar, y = shap) +
      geom_boxplot(fill = "gray80") +
      labs(x = vim$var_name[i], y = NULL) +
      theme_classic() +
      annotate("text", x = Inf, y = Inf, label = letters[i], hjust = 1, vjust = 1, size = 5) +
      theme(plot.margin = unit(c(.2,.3,.2,.2), "cm"))

  } else {
    dt <- d[, .(variable_name = ii, variable_value = variable, shap_value = shap)]
    ld[[i]] <- dt
    if(ii %like% "_kg_ha") {
      xlab <- bquote(.(vim$var_name[i])~(kg~ha^-1))
    } else if(ii == "pl_m2") {
      xlab <- bquote(.(vim$var_name[i])~("#"~m^-2))
    } else if(ii == "plant_date_dev") {
      xlab <- bquote(.(vim$var_name[i])~(days))
    } else {
      xlab <- xlab
    }
    lp[[i]] <- ggplot(d) +
      aes(x = variable, y = shap) +
      geom_point(alpha = 0.05) +
      geom_smooth(level = 0.99, span = 0.8, method = "loess", fill = "blue") +
      labs(x = xlab, y = NULL) +
      annotate("text", x = Inf, y = Inf, label = letters[i], hjust = 1, vjust = 1, size = 5) +
      theme_classic() +
      theme(plot.margin = unit(c(.2,.3,.2,.2), "cm"))
  }
}

rbindlist(ld) |> fwrite("plots/source_data/Fig_4.csv")

for(i in seq_along(lp)) {
  lg[[i]] <- ggplotGrob(lp[[i]])
  lw[[i]] <- lg[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, lw)

for (i in seq_along(lg)){
  lg[[i]]$widths[2:5] <- as.list(maxwidth)
}

ylab <- textGrob(expression(SHAP~value~"for"~maize~yield~(kg~ha^-1)), rot = 90)

ggsave(
  #"plots/BRT_shap_effects.png",
  "plots/pdf/Figure_4.pdf",
  grid.arrange(grobs = lg, left = ylab),
  width = 6, height = 6, units = "in"
) |> suppressWarnings()

#lg <- lg[c(1,2,4,5,3,6:8)]
#ggsave(
#  "plots/BRT_shap_effects.png",
#  grid.arrange(grobs = lg, left = ylab, ncol = 4),
#  width = 8, height = 4, units = "in"
#) |> suppressWarnings()
