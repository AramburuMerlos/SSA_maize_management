# author: Fernando Aramburu Merlos
# date: 2022-12-20

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(plotrix)


# prepare data -------------
d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")
dn <- d[, .(iso, country = stringr::str_to_title(country))] |> unique()
#d <- d[!is.na(strat)]
setorder(d, iso, season)
d <- d[, .(iso, season, yield_kg_ha, N_kg_ha, pl_m2, P_kg_ha)]

d[season == "first season", ss:= "S1"]
d[season == "second season", ss:= "S2"]
d[, bar:= paste0(iso, "-", ss)]
xb <- unique(d$bar)
xi <- unique(d$iso)


# plot --------
cols <- RColorBrewer::brewer.pal(3, "Set1")[1:2]
cols <- cols[ifelse(xb %like% "S1", 2, 1)]
xlabs <- c("", xi, "")
xat <- c(1:3,3.8,4.2,5:7)
xw <- c(rep(.7,3), .35, .35, rep(.7, 3))

png("plots/var_dsit_desc.png", 6, 5, units = "in", res = 600)

par(
  mfrow = c(2,2), mar = c(2,4,.5,.5), oma = c(3,0,0,0),
  las = 1, xpd = NA, mgp = c(2, .5, 0), tcl = -0.3
)

## yield ----------

d[, y:= yield_kg_ha/1e3]
d[, q:= ecdf(y)(y), by = .(bar)]
d[q < 0.9, max(y)]

boxplot(
  y ~ bar, d[q > 0.1 & q < 0.9],
  las = 1, ann = FALSE , range = 0,
  col = cols,
  outline = FALSE ,
  at = xat,
  width = xw,
  axes = FALSE,
  ylim = c(0,6)
)
axis(1, at = c(.25, seq_along(xi), 7.75), labels = xlabs,
     lwd.ticks = 0, pos = 0, cex.axis = .9, las = 3)
axis(2, at = 0:6, pos = .3)
mtext(expression(Yield~(Mg~ha^-1)), 2, line = 1.5, las = 0, cex = .9)

m <- boxplot(y ~ bar, d[q > 0.1 & q < 0.9], plot = FALSE)$stats
colnames(m) <- c(xlabs[2:4], "RWA_S1", "RWA_S2", xlabs[6:8])
rownames(m) <- c(10, 25, 50, 75, 90)
dt <- as.data.table(m, keep.rownames = "percentile")
dt <- melt(dt, id.vars = "percentile", variable.name = "iso", value.name = "yield_t_ha")
dt[, season:= "first season"]
dt[iso %like% "_S2", season:= "second season"]
dt[, iso:= gsub("_S.", "", iso)]
dt <- dt[dn, on = .(iso)]
dd <- dt[, .(country, season, percentile, yield_t_ha)]

## N ----------
d[, y:= N_kg_ha]
d[, q:= NULL]
d[, q:= ecdf(y)(y), by = .(bar)]
d[q < 0.9, max(y)]

boxplot(
  y ~ bar, d[q > 0.1 & q < 0.9],
  las = 1, ann = FALSE , range = 0,
  col = cols,
  outline = FALSE ,
  at = xat,
  width = xw,
  axes = FALSE,
  ylim = c(0,120)
)
axis(1, at = c(.25, seq_along(xi), 7.75), labels = xlabs,
     lwd.ticks = 0, pos = 0, cex.axis = .9, las = 3)
axis(2, at = seq(0,120,30), pos = .3)
mtext(expression(N~(kg~ha^-1)), 2, line = 1.5, las = 0, cex = .9)

m <- boxplot(y ~ bar, d[q > 0.1 & q < 0.9], plot = FALSE)$stats
colnames(m) <- c(xlabs[2:4], "RWA_S1", "RWA_S2", xlabs[6:8])
rownames(m) <- c(10, 25, 50, 75, 90)
dt <- as.data.table(m, keep.rownames = "percentile")
dt <- melt(dt, id.vars = "percentile", variable.name = "iso", value.name = "nitrogen_kg_ha")
dt[, season:= "first season"]
dt[iso %like% "_S2", season:= "second season"]
dt[, iso:= gsub("_S.", "", iso)]
dt <- dt[dn, on = .(iso)]
dt[, iso:= NULL]
dd <- dt[dd, on = .NATURAL]


## Plant density ----------
d[, y:= pl_m2]
d[y > 12, y:= NA]
d[, q:= NULL]
d[!is.na(y), q:= ecdf(y)(y), by = .(bar)]
d[q < 0.9 & !is.na(y), max(y)]

boxplot(
  y ~ bar, d[q > 0.1 & q < 0.9],
  las = 1, ann = FALSE , range = 0,
  col = cols[-c(1,7)], # 1: BDI, 7: UGA
  outline = FALSE ,
  at = xat[-c(1,7)],
  width = xw[-c(1,7)],
  axes = FALSE,
  ylim = c(0,8),
  xlim = c(0.45,7.5)
)

axis(1, at = c(.25, seq_along(xi), 7.75), labels = xlabs,
     lwd.ticks = 0, pos = 0, cex.axis = .9, las = 3)
axis(2, at = seq(0,8,2), pos = 0.25)


mtext(expression(Plant~density~(pl~m^-2)), 2, line = 1.5, las = 0, cex = .9)

m <- boxplot(y ~ bar, d[q > 0.1 & q < 0.9], plot = FALSE)$stats
colnames(m) <- c(xlabs[3:4], "RWA_S1", "RWA_S2", xlabs[c(6,8)])
rownames(m) <- c(10, 25, 50, 75, 90)
dt <- as.data.table(m, keep.rownames = "percentile")
dt <- melt(dt, id.vars = "percentile", variable.name = "iso", value.name = "plants_per_square_meter")
dt[, season:= "first season"]
dt[iso %like% "_S2", season:= "second season"]
dt[, iso:= gsub("_S.", "", iso)]
dt <- dt[dn, on = .(iso)]
dt[, iso:= NULL]
dd <- dt[dd, on = .NATURAL]


## P ----------

d[, y:= P_kg_ha]
d[, q:= NULL]
d[, q:= ecdf(y)(y), by = .(bar)]
d[q < 0.9, max(y)]

boxplot(
  y ~ bar, d[q > 0.1 & q < 0.9],
  las = 1, ann = FALSE , range = 0,
  col = cols,
  outline = FALSE ,
  at = xat,
  width = xw,
  axes = FALSE,
  ylim = c(0,40)
)
axis(1, at = c(.25, seq_along(xi), 7.75), labels = xlabs,
     lwd.ticks = 0, pos = 0, cex.axis = .9, las = 3)
axis(2, at = seq(0,40,10), pos = 0.3)
mtext(expression(P~(kg~ha^-1)), 2, line = 1.5, las = 0, cex = .9)

legend(
  "bottomleft", inset = c(-.75, -0.4), xpd = NA, horiz = TRUE, cex = 1,
  legend = c("first season", "second season"), fill = unique(cols)
)

dev.off()

m <- boxplot(y ~ bar, d[q > 0.1 & q < 0.9], plot = FALSE)$stats
colnames(m) <- c(xlabs[2:4], "RWA_S1", "RWA_S2", xlabs[6:8])
rownames(m) <- c(10, 25, 50, 75, 90)
dt <- as.data.table(m, keep.rownames = "percentile")
dt <- melt(dt, id.vars = "percentile", variable.name = "iso", value.name = "phosphorus_kg_ha")
dt[, season:= "first season"]
dt[iso %like% "_S2", season:= "second season"]
dt[, iso:= gsub("_S.", "", iso)]
dt <- dt[dn, on = .(iso)]
dt[, iso:= NULL]
dd <- dt[dd, on = .NATURAL]

setcolorder(dd, c("country", "season", "percentile", "yield_t_ha", "nitrogen_kg_ha"))
fwrite(dd, "plots/source_data/Supplementary_Fig_2.csv")
