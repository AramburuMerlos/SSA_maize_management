# author: Fernando Aramburu Merlos
# date: 2023-03-31

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(terra)
library(stringr)
library(lme4)
library(emmeans)
library(car)

# Farmer tech levels Yield ------------------------

## survey data -------------
d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")
d <- d[!is.na(strat)]

## define technology levels ---------
# define thresholds (terciles)
d[, quantile(N_kg_ha, c(.33,.66))]
d[!is.na(pl_m2), quantile(pl_m2, c(.33,.66))]
d[!is.na(plant_date_dev), quantile(plant_date_dev, c(.33, .66))]

# subset complete data
d <- d[!is.na(hybrid) & !is.na(pl_m2) & !is.na(plant_date_dev) & yield_kg_ha > 0]

d[!hybrid & N_kg_ha < 12 & pl_m2 < 3 & plant_date_dev >=  0, tech_level:= "0_baseline"]
d[!hybrid & N_kg_ha > 40 & pl_m2 < 3 & plant_date_dev >=  0, tech_level:= "1_N"]
d[ hybrid & N_kg_ha < 12 & pl_m2 < 3 & plant_date_dev >=  0, tech_level:= "2_hyb"]
d[ hybrid & N_kg_ha > 40 & pl_m2 < 3 & plant_date_dev >=  0, tech_level:= "3_N+hyb"]
d[ hybrid & N_kg_ha > 40 & pl_m2 > 4 & plant_date_dev >=  0, tech_level:= "4_N+hyb+pl_dens"]
d[ hybrid & N_kg_ha > 40 & pl_m2 > 4 & plant_date_dev < -9, tech_level:= "5_N+hyb+pl_dens+plant_date"]

# remove NA tech level (middle inputs)
d <- d[!is.na(tech_level)]
setorder(d, tech_level)
d[, .N, by = .(tech_level)]

# visualize
d[, .(y = mean(yield_kg_ha), n = .N), by = .(tech_level)]
d[, .N, by = .(country, tech_level)] |> dcast(tech_level~country)

d[, ystrat:= paste0(strat, "_", "year")]

## Forumlas -------------
formulas <- list(
  formula(sqrt(yield_kg_ha) ~ tech_level + (1|strat)),
  formula(sqrt(yield_kg_ha) ~ tech_level + (1|strat) + (1|year)),
  formula(sqrt(yield_kg_ha) ~ tech_level + (1|strat:year)),
  formula(sqrt(yield_kg_ha) ~ tech_level + (1|ystrat))
)


## Select mixed-effects model ----------
lmod <- vector("list", length(formulas))
for(i in seq_along(formulas)){
  print(i)
  lmod[[i]] <- lmer(formulas[[i]], data = d, REML = FALSE)
}
formulas[[which.min(sapply(lmod, BIC))]]
isSingular(lmod[[which.min(sapply(lmod, BIC))]])

mod_ff <- lmod[[which.min(sapply(lmod, BIC))]]
summary(mod_ff)
plot(mod_ff)
qqnorm(resid(mod_ff))
qqline(resid(mod_ff))


# Get the estimated marginal means for the different tech_level levels
(results <- emmeans(mod_ff, "tech_level", type = "response"))
dm <- as.data.table(results)
for(j in c("response", "SE", "lower.CL", "upper.CL")) {
  set(dm, i = NULL, j=j, value = dm[[j]]/1e3)
}
dm <- dm[!tech_level %in% c("1_N", "2_hyb")]
dm$tech_level <- droplevels(dm$tech_level)

equatiomatic::extract_eq(mod_ff) |>
  capture.output() |>
  cat(sep = "\n", file = "models/lmer_formula.Rmd")


# 1AF maize variety Yield -----------

dt <- fread("data/trials/NICHE - 20A-22B Maize Varieties Phase 1 - N Levels.xlsx - OAF trials database.csv")
setnames(dt, names(dt), str_replace_all(names(dt), " ", "_"))
dt[,.N, by = .(date_planting)]
dt[, date_planting:= as.IDate(date_planting, format = "%m/%d/%Y")]
dt[, .N, by = .(environment_class)]
dt[
  management == "High N" & month(date_planting) %in% c(8,9) & plant_density > 5
  , .(mean(yield_dry), .N), by = .(ID_variety)
]
rm_hyb <- c("Pool|RH|CZH|ZM|PAN")
dt <- dt[month(date_planting) %in% c(8,9) & plant_density > 5 & !ID_variety %like% rm_hyb, ]
dt[management == "New N Rate", .(mean(yield_dry), .N), by = .(ID_exp)]
dt[ID_exp == "RW_Congo Nile_21A_Phase1", .(mean(yield_dry), .N), by = .(management)]
dt <- dt[ID_exp != "RW_Congo Nile_21A_Phase1",]
dt[, .(mean(yield_dry), .N), by = .(management, N_Rate_Category)]


mod_mvt <- lmer(yield_dry ~ management + (1|ID_exp) + (1|ID_variety) + (1|ID_exp:ID_variety), data = dt)

summary(mod_mvt)
plot(mod_mvt)
qqnorm(resid(mod_mvt))
qqline(resid(mod_mvt))

(ddt <- emmeans(mod_mvt, "management", type = "response") |> as.data.frame())
setDT(ddt)
#ddt <- ddt[management %in% c("New N Rate", "High N"), ]
Yatt_1AF <- ddt[management %in% c("High N"), emmean]
Yatt_1AF_SE <- ddt[management %in% c("High N"), SE]
setorder(ddt, emmean)


dt[management == "High N", .N, by = .(season)]
dt[management == "High N", .N, by = .(district)]
dt[management == "High N", .N]



# FAOSTAT Ya of EA and SSA -----------------------
df <- fread("../GYGA/data/FAOSTAT/Production_Crops_Livestock_E_All_Data.csv", encoding = "Latin-1")
df <- df[Item == "Maize (corn)"]

### Tidy -------------
yy <- str_subset(names(df), "Y\\d{4}$") |> str_remove("Y") |> as.numeric() |> unique()
cols <- c("Area", "Item", "Element", "Unit", paste0("Y", yy))
df <- df[, ..cols]
df <- melt(df, id.vars = c("Area", "Item", "Element", "Unit"), variable.name = "year")
setnames(df, names(df), tolower(names(df)))
df[, year:= as.numeric(str_remove(year, "Y"))]
df[, .N, by = .(element, unit)]
# yields to ton per ha
df[element == "Yield", value:= value/1e4]
df <- dcast(df, area + item + year ~ element)

setnames(df, c("area", "item", "Area harvested"), c("region", "crop", "area"))
setnames(df, names(df), tolower(names(df)))


### Filter and aggregate ---------
ctries <- unique(d$country)

dfa <- df[
  region %like% "Africa"
  & !region %like% "Republic"
  & region != "South Africa"
  & region != "Africa"
  & region != "Northern Africa"
  & year > 2017 & year < 2021
  , lapply(.SD, mean),
  , by = .(region, crop)
  , .SDcols = c("yield", "area", "production")
]

Ya_ctries_FAO <- df[
  region %in% str_to_title(ctries)
  & year > 2015 & year < 2021
  , sum(yield * area/sum(area)),
]


Ya_East_Africa_FAO <- dfa[region == "Eastern Africa", yield]
Ya_SSA_FAO <- dfa[, sum(yield * area/sum(area))]


# GYGA CZ & country Yw -------------

## CZ ----------
dcz <- fread("../GYGA/data/API/Y_cz.csv")
dcz <- dcz[crop == "Rainfed maize"]

dctry <- fread("../GYGA/data/API/Y_country.csv")
dctry <- dctry[crop == "Rainfed maize"]
dctry[, country:= tolower(country)]

# ctries GYGA
(Ya_ctries_GYGA <- dctry[country %in% ctries, sum(ya * harv_area/sum(harv_area))])
(Yw_ctries_GYGA <- dctry[country %in% ctries, sum(yw * harv_area/sum(harv_area))])


cc <- geodata::country_codes()
setDT(cc)
dcz <- merge(dcz, cc[, .(ISO3, UNREGION1)], all.x = TRUE, all.y = FALSE, by.x = "country_iso3", by.y = "ISO3")
# East Africa GYGA
(Ya_East_Africa_GYGA <- dcz[UNREGION1 == "Eastern Africa", sum(ya * harv_area/sum(harv_area))])
(Yw_East_Africa_GYGA <- dcz[UNREGION1 == "Eastern Africa", sum(yw * harv_area/sum(harv_area))])
# SSA Africa GYGA
(Ya_SSA_GYGA <- dcz[str_detect(UNREGION1, "(?<!Northern )Africa"), sum(ya * harv_area/sum(harv_area))])
(Yw_SSA_GYGA <- dcz[str_detect(UNREGION1, "(?<!Northern )Africa"), sum(yw * harv_area/sum(harv_area))])

dcz <- dcz[UNREGION1 == "Eastern Africa" | country == "Nigeria",]
dcz <- dcz[country != "Ethiopia"]

# Survey CZs
czs <- d[, .(country, cz)] |> unique()
dcz[, country:= tolower(country)]
setnames(dcz, "climatezone", "cz")

dd <- dcz[, .(country, cz, yw, ya)][czs, on = .NATURAL]
dd[country == "rwanda", ya:= df[region == "Rwanda" & year > 2015 & year < 2021, mean(yield)]]
dd[country == "burundi", ya:= df[region == "Burundi" & year > 2015 & year < 2021, mean(yield)]]
dd[country == "kenya", ya:= df[region == "Kenya" & year > 2015 & year < 2021, mean(yield)]]
dd[country == "tanzania", ya:= df[region %like% "Tanzania" & year > 2015 & year < 2021, mean(yield)]]

dd[dcz, `:=`(yw = ifelse(is.na(yw), i.yw, yw), ya = ifelse(is.na(ya), i.ya, ya)), on = "cz"]
dd[dctry, `:=`(yw = ifelse(is.na(yw), i.yw, yw), ya = ifelse(is.na(ya), i.ya, ya)), on = "country"]

#dd[, yw:= ifelse(is.na(yw), mean(yw, na.rm = TRUE), yw)]


(Ya_CZ_GYGA <- dd[complete.cases(dd), mean(ya)])
(Yw_CZ_GYGA <- dd[complete.cases(dd), mean(yw)])


Ya_CZ_GYGA_SE <- dd[complete.cases(dd), sd(ya)/sqrt(.N)]
Yw_CZ_GYGA_SE <- dd[complete.cases(dd), sd(yw)/sqrt(.N)]


# PLOT -----------------

## Linear mixed model --------------------

hght <- c(Ya_CZ_GYGA, dm$response, Yatt_1AF, Yw_CZ_GYGA)
se <- c(Ya_CZ_GYGA_SE, dm$SE, Yatt_1AF_SE, Yw_CZ_GYGA_SE)
xlims <- c(0,12)
ylims <- c(0, length(hght)*1.2+1)
#cols <-  c("#8B0000", rep("#E69F00", nlevels(dm$tech_level)), "#0072B2", "#009E73")
cols <-  palette.colors(8)[c(5, rep(2, nlevels(dm$tech_level)), 4, 6)]
{
#png("plots/tech_levels_yields.png", 5.5, 4, "in", res = 300)
pdf("plots/pdf/Figure_5.pdf", 5.5, 4)

par(mar = c(3,9,1, 1), cex.axis = .8, mgp = c(2,.7,0), xpd = NA)
plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = xlims, ylim = ylims)

mtext(bquote(Yield~(t~ha^-1)), side = 1, line = 1.7)

text(x = xlims[1] - .1, y = length(hght)*1.2-.5+.6, adj = 1, labels = "actual yield")

text(expression(bold(SD)), x = xlims[1] - 1, y = 6*1.2+.4, adj = .5)
text(expression(bold(PD)), x = xlims[1] - 2.5, y = 6*1.2+.4, adj = .5)
text(expression(bold(NF)), x = xlims[1] - 4, y = 6*1.2+.4, adj = .5)
text(expression(bold(CV)), x = xlims[1] - 5.5, y = 6*1.2+.4, adj = .5)

lines(x = c(-6.5,0), y = c(6*1.2+.1, 6*1.2+.1))
lines(x = c(-6.5,0), y = c(6*1.2+.7, 6*1.2+.7))
lines(x = c(-6.5,0), y = c(2*1.2+.1, 2*1.2+.1))
#lines(x = c(-5.8,-5.8), y = c(2*1.2+.1, 6*1.2+.7))


text("late", x = xlims[1] - 1, y = 6*1.2-.5, adj = .5, col = "gray40")
text("late", x = xlims[1] - 1, y = 5*1.2-.5, adj = .5, col = "gray40")
text("late", x = xlims[1] - 1, y = 4*1.2-.5, adj = .5, col = "gray40")
text(expression(bold(early)), x = xlims[1] - 1, y = 3*1.2-.55, adj = .5)

text("low", x = xlims[1] - 2.5, y = 6*1.2-.5, adj = .5, col = "gray40")
text("low", x = xlims[1] - 2.5, y = 5*1.2-.5, adj = .5, col = "gray40")
text(expression(bold(high)), x = xlims[1] - 2.5, y = 4*1.2-.55, adj = .5)
text(expression(bold(high)), x = xlims[1] - 2.5, y = 3*1.2-.55, adj = .5)

text("low", x = xlims[1] - 4, y = 6*1.2-.5, adj = .5, col = "gray40")
text(expression(bold(high)), x = xlims[1] - 4, y = 5*1.2-.55, adj = .5)
text(expression(bold(high)), x = xlims[1] - 4, y = 4*1.2-.55, adj = .5)
text(expression(bold(high)), x = xlims[1] - 4, y = 3*1.2-.55, adj = .5)

text("OPV", x = xlims[1] - 5.5, y = 6*1.2-.5, adj = .5, col = "gray40")
text(expression(bold(HYB)), x = xlims[1] - 5.5, y = 5*1.2-.5, adj = .5)
text(expression(bold(HYB)), x = xlims[1] - 5.5, y = 4*1.2-.5, adj = .5)
text(expression(bold(HYB)), x = xlims[1] - 5.5, y = 3*1.2-.5, adj = .5)


text(xlims[1] - .1, 2*1.2-.5, adj = 1, labels = "on-station trials")
text(xlims[1] - .1, 1*1.2-.5, adj = 1, labels = "potential yield")

axis(side = 1, at = seq(xlims[1], xlims[2], 2), pos = ylims[1], cex.axis = .8, tcl = -0.4)
rug(seq(xlims[1], xlims[2], 1), ticksize = -0.015, pos = ylims[1])
axis(side = 2, at = ylims, pos = xlims[1], cex.axis = .9, lwd.ticks = 0, labels = F)
clip(xlims[1], xlims[2], ylims[1], ylims[2])
abline(v = seq(xlims[1]+1, xlims[2], 1), lty = 3, col = "gray80")

barplot(
  rev(hght),
  space = c(rep(.2, length(hght)-1),0.8),
  col = rev(cols),
  axes = FALSE,
  horiz = TRUE,
  xlim = xlims,
  add = T
)
y0s <- seq_along(hght)*1.2-.5
y0s[length(y0s)] <- y0s[length(y0s)]+.6
arrows(x0 = rev(hght - se), x1 = rev(hght + se), y0 = y0s,
       angle = 90, code = 3, length = 0.1, lwd = 1.5) |> suppressWarnings()

legend(
  xlims[2], ylims[2], xjust = 1, yjust = 1,
  legend = rev(c("GYGA-CZ", "Maize variety trials", "Farmers fields", "GYGA-CZ")),
  fill = c(cols)[c(1,3,length(cols)-1, length(cols))],
  cex = 0.8,# bty = "n"
)


dev.off()
}
### save source data ---------
data.table(
  yield_level = c("actual yield", rep("farmer yield", 4), "on station trials", "potential yield"),
  cultivar = c(NA, "OPV", rep("hybrid", 3), rep(NA,2)),
  nitrogen_fertilizer = c(NA, "low", rep("high", 3), rep(NA,2)),
  plant_density = c(NA, rep("low",2), rep("high", 2), rep(NA,2)),
  sowing_date = c(NA, rep("late", 3), "early", rep(NA,2)),
  yield_t_ha = hght,
  sandard_error = se
) |>
  fwrite("plots/source_data/Fig_5.csv")



# TABLE MATH AND NUMBERS ------------
dm
## Exploitable yield gap closed by the highest technology -----------
(dm$response[nrow(dm)] - dm$response[1])/(Yw_CZ_GYGA*.8 - dm$response[1])

## Yield gap closed by the highest technology -----------
(dm$response[nrow(dm)] - dm$response[1])/(Yw_CZ_GYGA - dm$response[1])

dm$response[2]/dm$response[1]
dm$response[nrow(dm)]/dm$response[1]
dm$response[nrow(dm)]-dm$response[1]

10.2*.8*.29

Yatt_1AF/Yw_CZ_GYGA

## SSA Scenario assessment -------------------------------------

# future maize demand by 2050
fmd2050 <- 184

### Current situation -----------
# SSA maize area in 2020 (M ha)
Ya_SSA_FAO
(Yg_SSA <- 1 - (Ya_SSA_FAO/Yw_SSA_GYGA))
(eYg_SSA <- 1 - (Ya_SSA_FAO/(Yw_SSA_GYGA*.8)))
(area_SSA <- dfa[, sum(area)]/1e6)
(production_SSA <- dfa[, sum(production)]/1e6)



### Same rate of yield increase ------------
# Yield by year 2050 under same yield gain rate
(Y_2050_sr <- Ya_SSA_FAO + (27*27)/1000)
# Yield gap
(Yg_sr <- 1 - Y_2050_sr/Yw_SSA_GYGA)

# Production
(P_2050_sr <- Y_2050_sr * area_SSA)
#SSR
P_2050_sr/fmd2050
# Balance
P_2050_sr-fmd2050
# extra area
-(P_2050_sr-fmd2050)/Y_2050_sr

### Acceleration of yield gain rate ---------------
# Yg of highest technology level
(Yg_ht <- 1 - dm$response[nrow(dm)]/(Yw_CZ_GYGA))

# Yield by year 2050 if SSA close the yield gap up to Yg_ht
(Y_2050_ac <- Yw_SSA_GYGA * (1-Yg_ht))
(Y_2050_ac <- Yw_SSA_GYGA * (1-Yg_ht))
#Yw_SSA_GYGA*.8 * (1-eYg_ht)
(1 - Ya_SSA_FAO/(Yw_SSA_GYGA*.8)) - (1 - Y_2050_ac/(Yw_SSA_GYGA*.8))

# Production
(P_2050_ac <- Y_2050_ac * area_SSA)
#SSR
P_2050_ac/fmd2050
# Balance
P_2050_ac-fmd2050
# extra area
-(P_2050_ac-fmd2050)/Y_2050_ac


### other calculus ---------

# yield gain rate to reach accelated yield gain rate yield
(Y_2050_ac - Ya_SSA_FAO)*1000/27
(Y_2050_ac - Ya_SSA_FAO)*1000/27/27

Yw_SSA_GYGA*.8 - Ya_SSA_FAO
1 - Ya_SSA_FAO/(Yw_SSA_GYGA*.8)

Yw_SSA_GYGA*.8 - Y_2050_ac
1 - Y_2050_ac/(Yw_SSA_GYGA*.8)


## how much the exploitable yield gap was closed ----------
# exploitable Yg of highest technology level
(eYg_ht <- 1 - dm$response[nrow(dm)]/(Yw_CZ_GYGA*.8))
Yw_CZ_GYGA*.8 - dm$response[nrow(dm)]

# exploitable Yg of baseline
(eYg_bl <- 1 - dm$response[1]/(Yw_CZ_GYGA*.8))

# exploitable Yg study region
(eYg_sr <- 1 - Ya_CZ_GYGA/(Yw_CZ_GYGA*.8))
Yw_CZ_GYGA*.8 - Ya_CZ_GYGA

# how much the exploitable yield gap was closed
(1 - eYg_ht) - (1 - eYg_bl)


## how much the  yield gap was closed ----------
(dm$response[nrow(dm)] - dm$response[1])/(Yw_CZ_GYGA - dm$response[1])

# Yg of highest technology level
(Yg_ht <- 1 - dm$response[nrow(dm)]/(Yw_CZ_GYGA))
Yw_CZ_GYGA - dm$response[nrow(dm)]

# Yg of baseline
(Yg_bl <- 1 - dm$response[1]/(Yw_CZ_GYGA))

# Yg study region
(Yg_sr <- 1 - Ya_CZ_GYGA/(Yw_CZ_GYGA))
Yw_CZ_GYGA - Ya_CZ_GYGA

# how much the yield gap was closed
(1 - Yg_ht) - (1 - Yg_bl)




# yield increase compared to baseline
cumsum(diff(dm$response)*1000)

# technology applied in each technology level
d[
  , lapply(.SD, median),
  .SDcols = c("N_kg_ha", "P_kg_ha", "pl_m2", "plant_date_dev"),
  by = .(tech_level)
][dm, on = "tech_level"] |>
  fwrite("models/tech_level_input_rates_and_yield.csv")


