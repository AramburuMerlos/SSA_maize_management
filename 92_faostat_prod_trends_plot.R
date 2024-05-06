# author: Fernando Aramburu Merlos
# date: 2022-12-26

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)


# other regions #####################################
do <- fread('data/faostat/FAOSTAT_data_en_4-20-2023.csv')

for(j in names(do)[sapply(do, is.character)]) {
  set(do, j = j, value = tolower(do[[j]]))
}
setnames(do, names(do), tolower(names(do)))
setnames(do, "item", "crop")

# note that yield is in hg/ha (not in kg/ha)
do[, .N, by = .(element, unit)]


## rearrange --------

ddo <- dcast(do, crop + year + area ~ element, value.var = "value")
setnames(ddo, c("area"), c("region"))
ddo[, yield:= yield/1e4] #hg to tons

range(ddo$yield, na.rm = T)
setorder(ddo, region, year)



# SSA ##########################################################

d <- fread('data/faostat/FAOSTAT_data_en_3-30-2023.csv')

for(j in names(d)[sapply(d, is.character)]) {
  set(d, j = j, value = tolower(d[[j]]))
}
setnames(d, names(d), tolower(names(d)))
setnames(d, "item", "crop")


# note that yield is in hg/ha (not in kg/ha)
d[, .N, by = .(element, unit)]


## rearrange --------

dd <- dcast(d, crop + year + area ~ element, value.var = "value")
setnames(dd, c("area", "area harvested"), c("country", "area"))
dd <- dd[area > 0,]
dd[, yield:= yield/1e4] #hg to tons

dd[, w:= area/sum(area), by = .(crop, year)]

dd <- dd[, .(area = sum(area), yield = sum(yield * w), prod = sum(production)), by = .(crop, year)]

dd[, area:= area/1e6]
dd[, prod:= prod/1e6]
dd[, y2:= prod/area]

range(dd$yield, na.rm = T)
range(dd$year)

dd[, w:= area/sum(area), by = year]
dd[year == 2020, .(crop, area)]

## plot --------------
### define years -------
syear <- 2000
fyear <- 2020
dd <- dd[year >= syear & year <= fyear]
ddo <- ddo[year >= syear & year <= fyear]

### linear models -------
mod_y_sam <- lm(yield~year, data = ddo[region == 'south america'])
mod_y_sea <- lm(yield~year, data = ddo[region == 'south-eastern asia'])
mod_y_mz <- lm(yield~year, data = dd[crop == "maize (corn)"])

mod_a_mz <- lm(area~year, data = dd[crop == "maize (corn)"])
mod_a_sg <- lm(area~year, data = dd[crop == "sorghum"])
mod_a_ml <- lm(area~year, data = dd[crop == "millet"])



### colors ------
clr <- ggsci::pal_d3("category20")(6)


### limits --------
xmin <- syear
xmax <- fyear


### text pos ---------
up <- .95
med <- .875
low <- .8

dir.create("plots/pdf", F, T)
### par ------
{
  #png("plots/prod_trends_SSA.png", 7, 3, units = "in", res = 300)
  pdf("plots/pdf/Figure_1.pdf", 6, 3)

  par(mfrow = c(1,2), mar = c(4,4,1,1), mgp = c(2,.8,0), cex.axis = 0.8, las = 1, xpd = NA, oma = c(0,0,0,0))

  #### 1st panel --------
  ymin <- 0
  ymax <- 50
  plot(1, type = "n", xlim = c(xmin, xmax), ylim = c(0,ymax), axes = FALSE, xlab = "", ylab = "")

  ##### axes -------
  axis(1, at = seq(xmin,xmax, 5), pos = 0, tcl = -0.4)
  rug(x = seq(xmin, xmax, 1), ticksize = -0.03, side = 1, pos = 0)
  mtext("Year", side = 1, line = 1.8, las = 0)

  axis(2, at = seq(0,ymax,10), pos = xmin, tcl = -0.4)
  rug(x = seq(0,ymax,5), ticksize = -0.03, side = 2, pos = xmin)
  mtext(expression(SSA~crop~area~(M~ha)), side = 2, line = 1.5, las = 0)
  axis(3, at = c(xmin,xmax), pos = ymax, tcl = 0, labels = NA)
  axis(4, at = 0:ymax, pos = xmax, tcl = 0, labels = NA)

  mtext("a", side = 3, adj = .95, line = -1.2, cex = .8, font = 2)

  ##### inset text -------
  mtx <- format(round(coef(mod_a_mz)[2]*1e6), big.mark = ",")

  text(
    x = xmin+4, y = ymax*up, adj = c(0, .5), cex = 0.6,
    labels = bquote(Maize:~.(mtx)~ha~y^-1)#, col = clr[1]
  )
  stx <- format(round(coef(mod_a_sg)[2]*1e6), big.mark = ",")
  text(
    x = xmin+4, y = ymax*med, adj = c(0, .5), cex = 0.6,
    labels = bquote(Sorghum:~.(stx)~ha~y^-1)#, col = clr[2]
  )
  ltx <- format(round(coef(mod_a_ml)[2]*1e6), big.mark = ",")
  text(
    x = xmin+4, y = ymax*low, adj = c(0, .5), cex = 0.6,
    labels = bquote(Millet:~.(ltx)~ha~y^-1)#, col = clr[3]
  )

  lines(xmin + 1:3, rep(ymax*up,3), col = clr[1], lwd = 2)
  lines(xmin + 1:3, rep(ymax*med,3), col = clr[2], lwd = 2)
  lines(xmin + 1:3, rep(ymax*low,3), col = clr[3], lwd = 2)

  points(x = xmin+2, y = ymax*up, pch = 21, bg = clr[1], cex = .8)
  points(x = xmin+2, y = ymax*med, pch = 22, bg = clr[2], cex = .8)
  points(x = xmin+2, y = ymax*low, pch = 23, bg = clr[3], cex = .8)


  ##### points and lines --------

  clip(xmin,xmax,0,ymax)
  abline(mod_a_mz, col = clr[1], lwd = 2)
  abline(mod_a_sg, col = clr[2], lwd = 2)
  abline(mod_a_ml, col = clr[3], lwd = 2)

  points(dd[crop == "maize (corn)", .(year, area)], pch = 21, bg = clr[1],  cex = .8)
  points(dd[crop == "sorghum", .(year, area)], pch = 22, bg = clr[2],  cex = .8)
  points(dd[crop == "millet", .(year, area)], pch = 23, bg = clr[3],  cex = .8)

  #### 2nd panel ------
  ymin <- 0
  ymax <- 7
  plot(1, type = "n", xlim = c(xmin, xmax), ylim = c(ymin,ymax), axes = FALSE, xlab = "", ylab = "")

  #### axes --------
  axis(1, at = seq(xmin, xmax, 5), pos = 0, tcl = -0.4)
  rug(x = seq(xmin, xmax, 1), ticksize = -0.03, side = 1, pos = 0)
  mtext("Year", side = 1, line = 1.8, las = 0)
  axis(2, at = seq(ymin,ymax), pos = xmin, tcl = -0.4)
  rug(x = seq(ymin,ymax,.5), ticksize = -0.03, side = 2, pos = xmin)
  mtext(expression(Maize~yield~(t~ha^-1)), side = 2, line = 1.5, las = 0)
  axis(3, at = c(xmin,xmax), pos = ymax, tcl = 0, labels = NA)
  axis(4, at = c(ymin,ymax), pos = xmax, tcl = 0, labels = NA)

  mtext("b", side = 3, adj = .95, line = -1.2, cex = .8, font = 2)

  #### points and lines ------
  #points(dt[, .(year, yield)], pch = 21, bg = clr[1],  cex = .8)
  clip(syear,fyear,ymin,ymax)
  abline(mod_y_mz, col = clr[4], lwd = 2)
  abline(mod_y_sea, col = clr[5], lwd = 2)
  abline(mod_y_sam, col = clr[6], lwd = 2)

  points(dd[crop == "maize (corn)", .(year, yield)], pch = 21, bg = clr[4],  cex = .8)
  points(ddo[region == "south-eastern asia", .(year, yield)], pch = 24, bg = clr[5], cex = .8)
  points(ddo[region == "south america", .(year, yield)], pch = 25, bg = clr[6], cex = .8)

  #### inset text -------
  text(
    x = xmin+4, y = ymax*low, adj = c(0, .5), cex = 0.6,# col = clr[4],
    labels = bquote(SSA:~.(round(coef(mod_y_mz)[2]*1e3))~kg~ha^-1~y^-1)
  )
  text(
    x = xmin+4, y = ymax*med, adj = c(0, .5), cex = 0.6,# col = clr[5],
    labels = bquote(SEA:~.(round(coef(mod_y_sea)[2]*1e3))~kg~ha^-1~y^-1)
  )
  text(
    x = xmin+4, y = ymax*up, adj = c(0, .5), cex = 0.6,# col = clr[6],
    labels = bquote(SAM:~.(round(coef(mod_y_sam)[2]*1e3))~kg~ha^-1~y^-1)
  )
  lines(xmin + 1:3, rep(ymax*low,3), col = clr[4], lwd = 2)
  lines(xmin + 1:3, rep(ymax*med,3), col = clr[5], lwd = 2)
  lines(xmin + 1:3, rep(ymax*up,3), col = clr[6], lwd = 2)

  points(x = xmin+2, y = ymax*low, pch = 21, bg = clr[4], cex = .9)
  points(x = xmin+2, y = ymax*med, pch = 24, bg = clr[5], cex = .9)
  points(x = xmin+2, y = ymax*up, pch = 25, bg = clr[6], cex = .9)





  dev.off()
}

dd[year == 2020 & crop %like% "maize", ]

ddo[, region:= stringr::str_to_title(region)]
ddo[, crop:= "maize (corn)"]
dd[, region:= "Sub-Saharan Africa"]
dt <- rbind(
  dd[, .(region, crop, year, yield, area_Mha = area)],
  ddo[, .(region,crop, year, yield)],
  use.names = TRUE, fill = TRUE
)
setnames(dt, "yield", "yield_t_ha")

fwrite(dt, "plots/source_data/Fig_1.csv")


# SSA by 2050 quick math --------------------------
dd[year > 2016 & crop %like% "maize", mean(prod)]
dd[year > 2016 & crop %like% "maize", mean(area)]
# prod by 2050
0.027 * 27 * 40 + 80
# maize demand by 2050 = 191
191 - 109 # Future imports
109/191 ## SSR

# NE production 1.6b bushels to Mt
1.6 * 1e9 * .0254 / 1e6


