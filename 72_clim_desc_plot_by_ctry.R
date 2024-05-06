# author: Fernando Aramburu Merlos
# date: 2022-12-11

# Reference weather station data for environment description



# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(terra)
library(data.table)
library(stringr)
#library(mapview)

# polygons ------
# country borders and data polygons
w <- geodata::world(path = "data/gadm", resolution = 1)
ctry <- w[w$GID_0 %in% c("RWA", "BDI", "TZA", "KEN", "UGA", "ZMB", "NGA"),]
pol <- vect("data/obs_polygons/obs_polygons_all_data.shp")
rm(w)

# spam ---------
spam <- rast("data/spam/spam2017V2r1_SSA_H_MAIZ_R.tif") |>
  crop(ctry) |> mask(ctry)

# CHIRPS data ---------
chirps <- fread("data/climate/chirps/daily_prec_data.csv")
chirps[, month:= month(date)]
chirps[, year:= year(date)]

chirps_pts <- vect(unique(chirps[, .(lon, lat)]), crs = "+proj=longlat +datum=WGS84")
chirps_pts <- intersect(chirps_pts, pol)
chirps_pts$area <- extract(spam, chirps_pts)[, 2]
sel_pts <- chirps_pts[chirps_pts$area > 300, ]

xy <- geom(sel_pts)[, c("x", "y")] |> as.data.table()
setnames(xy, c("x", "y"), c("lon", "lat"))
xy[, gid:= sel_pts$gid]

chirps <- chirps[xy, on = .(lon, lat)]
d <- chirps[
  , .(prec = sum(prec))
  , by = .(year, month, lon, lat, gid)
][
  , .(prec = mean(prec)), by = .(month, lon, lat, gid)
][
  , .(prec = mean(prec)), by = .(month, gid)
]

d[, prec:= round(prec,1)]


# Tmax, Tmin, Srad --------------
pts <- spam |>
  mask(pol) |>
  classify(rcl = cbind(-1,300,NA)) |>
  as.points()
pts <- intersect(pts, pol)

for(i in pol$gid) {
  i_pts <- pts[pts$gid == i]

  if(i == "RBI") {
    r_tmax <- merge(
      geodata::worldclim_country("RWA", "tmax", path = "data/envir/worldclim"),
      geodata::worldclim_country("BDI", "tmax", path = "data/envir/worldclim")
    )
    r_tmin <- merge(
      geodata::worldclim_country("RWA", "tmin", path = "data/envir/worldclim"),
      geodata::worldclim_country("BDI", "tmin", path = "data/envir/worldclim")
    )
    r_srad <- merge(
      geodata::worldclim_country("RWA", "srad", path = "data/envir/worldclim"),
      geodata::worldclim_country("BDI", "srad", path = "data/envir/worldclim")
    )
  } else {
    r_tmax <- geodata::worldclim_country(i, "tmax", path = "data/envir/worldclim")
    r_tmin <- geodata::worldclim_country(i, "tmin", path = "data/envir/worldclim")
    r_srad <- geodata::worldclim_country(i, "srad", path = "data/envir/worldclim")
  }

  v_tmax <- extract(r_tmax, i_pts)[,-1] |> sapply(mean, na.rm = T)
  v_tmin <- extract(r_tmin, i_pts)[,-1] |> sapply(mean, na.rm = T)
  v_srad <- extract(r_srad, i_pts)[,-1] |> sapply(mean, na.rm = T)

  d[gid == i, tmax:= v_tmax]
  d[gid == i, tmin:= v_tmin]
  d[gid == i, srad:= v_srad]

}

d[, srad:= srad / 1e3] # kJ m-2 day-1 to MJ m-2 day-1

# OAF survey data --------------
ds <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv")

ds[, harvest_doy:= yday(harvest_date)]
ds[, gid:= iso]
ds[iso %in% c("RWA", "BDI"), gid:= "RBI"]

ds <- ds[
  , lapply(.SD, function(x) as.numeric(fivenum(x)[2:4]))
  , .SDcols = c("plant_doy", "harvest_doy")
  , by = .(gid, season)
]
ds[, quartile:= rep(2:4, .N/3)]

## UGA harvest date & second season ---------
ds[gid == "UGA" & season %like% "first" & is.na(harvest_doy), harvest_doy:= plant_doy + 140]

yday(as.Date("2023-08-18")) # avg second season planting date from GYGA nearby stations

ds <- rbind(
  ds,
  data.table(
    gid = "UGA", season = "second season",
    plant_doy = c(223, 230, 237),
    harvest_doy = c(350, 357, 364),
    quartile = 2:4
  ),
  fill = TRUE
)

## Kenya second season ---------
ds <- rbind(
  ds,
  data.table(
    gid = "KEN", season = "second season",
    plant_doy = ds[gid == "KEN" & season == "first season", harvest_doy] + 7,
    harvest_doy = ds[gid == "KEN" & season == "first season", harvest_doy] + 137,
    quartile = 2:4
  ),
  fill = TRUE
)
ds[harvest_doy > 365, harvest_doy:= harvest_doy - 365]

## Zambia harvest date ------
ds[gid == "ZMB" & is.na(harvest_doy) & plant_doy > 190, harvest_doy:= plant_doy + 175 - 365]
ds[gid == "ZMB" & is.na(harvest_doy) & plant_doy <= 190, harvest_doy:= plant_doy + 175]

ds


# plot ------------------
mth <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
clr <- RColorBrewer::brewer.pal(5, "Set1")[c(1,2,3,5)]

png("plots/envir_desc.png", 5, 5, units = "in", res = 600)

par(
  mfrow = c(3,2), mar = c(2,2,1,2), oma = c(2,4,0,2),
  las = 1, xpd = NA, mgp = c(2, .5, 0), tcl = -0.3
)

for(i in pol$gid) {
  ## temp ----
  di <- d[gid == i]
  plot(1, type = "n", xlim = c(0,13), ylim = c(-10,40), axes = FALSE, xlab = "", ylab = "")
  axis(1, at = 0:13, labels = NA, pos = -10, tcl = 0)
  rug(x = 1:12, ticksize = -0.03, side = 1, pos = -10)
  axis(2, at = seq(-10,40,10), labels = NA, pos = 0, tcl = 0)
  axis(2, at = seq(0,40,10), cex.axis = 0.8, pos = 0, tcl = 0)
  rug(x = seq(0,40,10), ticksize = -0.03, side = 2, pos = 0)
  rug(x = seq(5,35,5), ticksize = -0.015, side = 2, pos = 0)

  if(which(i==pol$gid) %in% 5:6) {
    ev <- seq(1,11,2)
    un <- seq(2,12,2)
    axis(1, at = ev, labels = mth[ev], cex.axis = 0.8, pos = -10, tcl = 0)
    axis(1, at = un, labels = mth[un], cex.axis = 0.8, pos = -10, tcl = 0)
  }

  lines(di[, .(month, tmax)], col = clr[1])
  points(di[, .(month, tmax)], pch = 24, col = clr[1], bg = "white", cex = .8)
  lines(di[, .(month, tmin)], col = clr[2])
  points(di[, .(month, tmin)], pch = 25, col = clr[2], bg = "white", cex = .8)
  lines(di[, .(month, srad)], col = clr[4])
  points(di[, .(month, srad)], pch = 21, col = clr[4], bg = "white", cex = .8)

  # prec ----
  par(new = T)
  plot(
    di[,.(month, prec)],
    type = "h", xlim = c(0,13), ylim = c(0,500),
    axes = FALSE, xlab = "", ylab = "",
    col = clr[3],
    lwd = 2
  )
  axis(4, at = seq(0,500,50), labels = NA, pos = 13, tcl = 0)
  rug(x = seq(0,300,100), ticksize = -0.03, side = 4, pos = 13)
  rug(x = seq(50,250,50), ticksize = -0.015, side = 4, pos = 13)
  axis(4, at = seq(0,300,100), cex.axis = 0.8, pos = 13)

  axis(3, at = 0:13, labels = NA, tcl = 0, pos = 500)
  if(i == "RBI") {
    mtext("Rwanda-Burundi", cex = .8, line = -.2, adj = .95)
  } else {
    mtext(ctry[ctry$GID_0 == i,]$NAME_0, cex = .8, line = -.2, adj = .95)
  }

  # cropping season -----
  par(new = T)
  pdi <- ds[gid == i & season == "first season", plant_doy]
  hdi <- ds[gid == i & season == "first season", harvest_doy]
  plot(1, xlim = c(0, 365), ylim = c(0,1), type = "n", axes = FALSE, xlab = "", ylab = "")
  if(pdi[2] < hdi[2]){
    polygon(x = c(pdi[1:2], hdi[2:1]), y = c(.95,.97,.97,.95), col = "gray80")
  } else {
    polygon(x = c(pdi[1:2], 365, 365), y = c(.95,.97,.97,.95), col = "gray80")
    polygon(x = c(0, 0, hdi[2:1]), y = c(.95,.97,.97,.95), col = "gray80")
  }
  if(i %in% c("KEN", "RBI", "UGA")) {
    pdi <- ds[gid == i & season == "second season", plant_doy]
    hdi <- ds[gid == i & season == "second season", harvest_doy]
    if(pdi[2] < hdi[2]){
      polygon(x = c(pdi[1:2], hdi[2:1]), y = c(.91,.93,.93,.91), col = "gray80")
    } else {
      polygon(x = c(pdi[1:2], 365, 365), y = c(.91,.93,.93,.91), col = "gray80")
      polygon(x = c(0, 0, hdi[2:1]), y = c(.91,.93,.93,.91), col = "gray80")
    }
  }
  rm(pdi, hdi, di)

  ## legend -----
  if(which(i==pol$gid) == 1) {
    legend(
      x = -45, y = 0.5, xjust = 1, yjust = 0.5,
      legend = c("Season", "Tmax", "Tmin", "Srad", "Prec"),
      pch = c(NA, 24,25,21, -0x007CL), lty = c(1,1,1,1,0), lwd = c(4,1,1,1,0),
      col = c("black", clr[c(1,2,4,3)]), pt.bg = "white", pt.cex = c(1,1,1,1,1.3),
      cex = .8,
      bty = "n",
    )
    legend(
      x = -45, y = 0.5, xjust = 1, yjust = 0.5,
      legend = c("Season", "Tmax", "Tmin", "Srad", "Prec"),
      pch = c(NA, 24,25,21, -0x007CL), lty = c(1,1,1,1,0), lwd = c(3,1,1,1,0),
      col = c("gray80", clr[c(1,2,4,3)]), pt.bg = "white", pt.cex = c(1,1,1,1,1.3),
      cex = .8,
      bty = "n",
    )
  }
}

mtext("Month", outer = TRUE, side = 1, line = 0, las = 0, cex = .8)
mtext(expression(Temperature~(degree*C)), outer = TRUE, side = 2, line = 1.5, las = 0, cex = .8)
mtext(
  bquote(Solar~Radiation~(MJ~m^{-2}~d^{-1})),
  outer = TRUE, side = 2, line = 0, las = 0, cex = .7
)

mtext(
  bquote(Precipitation~(mm~month^{-1})),
  outer = TRUE, side = 4, line = 0.2, las = 0, cex = .8
)

dev.off()

# save source data ----------
d[gid == "KEN", country:= "Kenya"]
d[gid == "NGA", country:= "Nigeria"]
d[gid == "TZA", country:= "Tanzania"]
d[gid == "RBI", country:= "Rwanda-Burundi"]
d[gid == "UGA", country:= "Uganda"]
d[gid == "ZMB", country:= "Zambia"]
setcolorder(d, "country")
ds <- ds[unique(d[, .(gid, country)]), on = .(gid)]
d[, gid:= NULL]
setnames(d, c("prec", "tmax", "tmin", "srad"), c(
  "precipitation_mm_per_month",
  "maximum_temperature_celsius",
  "minimum_temperature_celsius",
  "solar_radiation_MJ_per_square_meter_and_day"))

ds[, gid:= NULL]
ds[, harvest_doy:= round(harvest_doy)]
dt <- melt(ds,
           c("country", "season", "quartile"),
           c("plant_doy", "harvest_doy"),
           "stage", "day_of_the_year")
dt[quartile == 2, progress_percent:= 25]
dt[quartile == 3, progress_percent:= 50]
dt[quartile == 4, progress_percent:= 75]
dt[, quartile:= NULL]
dt[stage == "plant_doy", stage:= "planting"]
dt[stage == "harvest_doy", stage:= "harvest"]
setcolorder(dt, c("country", "season", "stage", "progress_percent", "day_of_the_year"))
setorderv(dt, c("country", "season", "stage"), order = c(1,1,1))

rbind(d, dt, use.names = TRUE, fill = TRUE) |>
  fwrite("plots/source_data/Supplementary_Fig_1.csv")
