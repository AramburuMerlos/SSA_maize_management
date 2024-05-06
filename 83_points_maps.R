# author: Fernando Aramburu Merlos
# date: 2022-11-08

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(terra)

dir.create("maps", F)

# data ------
d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")
setorder(d, strat, na.last = TRUE)

# country ------
ctry_codes <- geodata::country_codes()
af_iso3 <- ctry_codes[ctry_codes$continent == "Africa", "ISO3"]
w <- geodata::world(path = "data/gadm", resolution = 1)
af <- w[w$GID_0 %in% af_iso3]
af <- crop(af, c(-18, 50, -35, 37))

ctry <- geodata::gadm(
  c("RWA", "BDI", "TZA", "KEN", "UGA", "ZMB", "NGA"),
  path = "data/gadm",
  level = 0,
)

ctry1 <- geodata::gadm(
  c("RWA", "BDI", "TZA", "KEN", "UGA", "ZMB", "NGA"),
  path = "data/gadm",
  level = 1,
)

ctry1$country <- tolower(ctry1$COUNTRY)

if(host == "LAPTOP-ST129J47") {
  names(ctry)[names(ctry) == "ID_0"] <- "GID_0"
}

# polygons ------
pol <- vect("data/obs_polygons/obs_polygons.shp")
pol$gid[pol$gid == "UGA"] <- "KEN"
pol <- aggregate(pol, by = "gid")

# colors -------
#cols <- RColorBrewer::brewer.pal(8, "Set1")
#cols[6] <- "gold2"
#cols <- c(cols, "cyan3")
cols <- rcartocolor::carto_pal(11, "Safe")[-11]


# plot----------
#tiff("maps/the_map_with_points.tif", width = 10, heigh = 6, res = 300, unit = "in")
pdf("plots/pdf/Figure_2.pdf", width = 10, heigh = 6)


#par(mfrow = c(2,2), omi = c(.1,.1,.1,.1), xpd = FALSE)
layout(matrix(c(1,2,2,2,3,3,3,4,5,5,6,6), ncol = 3), widths = c(4,6,5))


## NGA ---------
countries <- "nigeria"
iso <- "NGA"
all_pts <- vect(d[country %in% countries,], crs = "+proj=longlat +datum=WGS84")
sel_pts <- vect(d[country %in% countries & !is.na(strat),], crs = "+proj=longlat +datum=WGS84")
strats <- unique(sel_pts$strat) |> substr(1,2)
plot(af, mar = c(0,5,.5,.5), ext = ext(pol[pol$gid == iso,]), axes = FALSE, lwd = 1.5, xpd = FALSE, buffer = FALSE)
plot(ctry1[ctry1$country %in% countries], ext = ext(pol[pol$gid == iso,]), buffer = FALSE,
     border = "gray50", add = T, lwd = 0.5)
plot(all_pts, col = "gray70", add = TRUE,
    ext = ext(pol[pol$gid == iso,]), pch = 18, legend = FALSE, cex = 0.8)
plot(sel_pts, "strat", col = cols[seq_along(strats)], add = TRUE,
     ext = ext(pol[pol$gid == iso,]), pch = 18, legend = FALSE, cex = 1.2)

legend(
  x = "left",
  legend = c(strats, "NC"),
  col = c(cols[seq_along(strats)], "gray70"),
  pt.cex = 1.8, cex = 1.5, pch = 18, inset = c(-0.3), bty = "n", y.intersp = 0.3,
  title = "CZ\n", title.adj = 0.5, xpd = NA
)

plot(af, axes = FALSE, lwd = 1.5, add = TRUE, ext = ext(pol[pol$gid == iso,]), buffer = FALSE)
plot(ext(pol[pol$gid == iso,]), axes = FALSE, lwd = 1.5, border = "darkblue", add = TRUE)
sbar(80, ext(pol[pol$gid == iso,])[c(1,3)]-c(0,.2), "bar", 4, "kilometers", xpd = NA)
text(ext(pol[pol$gid == iso,])[1]+.04, ext(pol[pol$gid == iso,])[4]-.04, "NG", font = 2, cex = 2, adj = c(0,1), xpd = NA)


## RWA-BDI -----
countries <- c("rwanda", "burundi")
iso <- "RBI"
all_pts <- vect(d[country %in% countries,], crs = "+proj=longlat +datum=WGS84")
sel_pts <- vect(d[country %in% countries & !is.na(strat),], crs = "+proj=longlat +datum=WGS84")
strats <- unique(sel_pts$strat) |> substr(1,2)
plot(af, mar = c(.1,5,.1,.1), ext = ext(pol[pol$gid == iso,]), axes = FALSE, lwd = 1.5, xpd = FALSE, buffer = FALSE)
plot(ctry1[ctry1$country %in% countries], ext = ext(pol[pol$gid == iso,]), buffer = FALSE,
     border = "gray50", add = T, lwd = 0.5)
plot(all_pts, col = "gray70", add = TRUE,
     ext = ext(pol[pol$gid == iso,]), pch = 15, legend = FALSE, cex = 0.8)
plot(sel_pts, "strat", col = cols[seq_along(strats)], add = TRUE,
     ext = ext(pol[pol$gid == iso,]), pch = 15, legend = FALSE, cex = 0.8)

legend(
  x = "left",
  legend = c(strats, "NC"),
  col = c(cols[seq_along(strats)], "gray70"),
  pt.cex = 1.8, cex = 1.5, pch = 15, inset = -0.3, bty = "n", y.intersp = 0.9,
  title = "CZ", title.adj = 0.5, xpd = NA
)

plot(af, axes = FALSE, lwd = 1.5, add = TRUE, ext = ext(pol[pol$gid == iso,]), buffer = FALSE)
plot(ext(pol[pol$gid == iso,]), axes = FALSE, lwd = 1.5, border = "darkblue", add = TRUE)
sbar(80, ext(pol[pol$gid == iso,])[c(2,3)]-c(.8,.2), "bar", 4, "kilometers")
text(ext(pol[pol$gid == iso,])[1]+.04, ext(pol[pol$gid == iso,])[4]-.04, "RW-BI", font = 2, cex = 2, adj = c(0,1), xpd = NA)

## Africa ----
plot(af, mar = rep(.1,4), axes = FALSE, lwd = 1,  col = "white", border = "gray60")
#plot(af[af$GID_0 %in% unique(d$iso)], axes = FALSE, lwd = 1,  col = "gray90", add = TRUE)
points(d[,.(lon, lat)], pch = 19, cex = 0.5, col = "red")
plot(ext(pol[pol$gid == "NGA",]), add = TRUE, border = "darkblue", lwd = 2)
text(pol[pol$gid == "NGA",], "NG", pos = 3, cex = 1.7, font = 2, offset = 0.8)
plot(ext(pol[pol$gid == "RBI",]), add = TRUE, border = "darkblue", lwd = 2)
text(pol[pol$gid == "RBI",], "RW-BI", pos = 2, cex = 1.7, font = 2, offset = 1)
plot(ext(pol[pol$gid == "ZMB",]), add = TRUE, border = "darkblue", lwd = 2)
text(pol[pol$gid == "ZMB",], "ZM", pos = 2, cex = 1.7, font = 2, offset = .9)

plot(ext(pol[pol$gid == "KEN",]), add = TRUE, border = "darkblue", lwd = 2)
text(pol[pol$gid == "KEN",], "   UG-KE", pos = 3, cex = 1.7, font = 2, offset = .8)
plot(ext(pol[pol$gid == "TZA",]), add = TRUE, border = "darkblue", lwd = 2)
text(pol[pol$gid == "TZA",], "TZ", pos = 3, cex = 1.7, font = 2, offset = 1)
#text(ext(af)[1]+.04, ext(af)[4]-.04, "A", font = 2, cex = 2, adj = c(0,1), xpd = NA)


## ZMB ---------
countries <- "zambia"
iso <- "ZMB"
all_pts <- vect(d[country %in% countries,], crs = "+proj=longlat +datum=WGS84")
sel_pts <- vect(d[country %in% countries & !is.na(strat),], crs = "+proj=longlat +datum=WGS84")
strats <- unique(sel_pts$strat) |> substr(1,2)
plot(af, mar = c(.5,5,.3,.5), ext = ext(pol[pol$gid == iso,]), axes = FALSE, lwd = 1.5, xpd = FALSE, buffer = FALSE)
plot(ctry1[ctry1$country %in% countries], ext = ext(pol[pol$gid == iso,]), buffer = FALSE,
     border = "gray50", add = T, lwd = 0.5)
plot(all_pts, col = "gray70", add = TRUE,
    ext = ext(pol[pol$gid == iso,]), pch = 8, legend = FALSE, cex = 0.8)
plot(sel_pts, "strat", col = cols[seq_along(strats)], add = TRUE,
     ext = ext(pol[pol$gid == iso,]), pch = 8, legend = FALSE, cex = 0.8)

legend(
  x = "left",
  legend = c(strats, "NC"),
  col = c(cols[seq_along(strats)], "gray70"),
  pt.cex = 1.8, cex = 1.5, pch = 8, inset = c(-0.05), bty = "n", y.intersp = 0.3,
  title = "CZ\n", title.adj = 0.5, xpd = NA
)

plot(af, axes = FALSE, lwd = 1.5, add = TRUE, ext = ext(pol[pol$gid == iso,]), buffer = FALSE)
plot(ext(pol[pol$gid == iso,]), axes = FALSE, lwd = 1.5, border = "darkblue", add = TRUE)
sbar(80, ext(pol[pol$gid == iso,])[c(1,4)]+c(.5,-.2), "bar", 4, "kilometers")
text(ext(pol[pol$gid == iso,])[1]+.04, ext(pol[pol$gid == iso,])[4]-.04, "ZM", font = 2, cex = 2, adj = c(0,1), xpd = NA)

## KEN-UGA ----
countries <- c("kenya", "uganda")
iso <- "KEN"
all_pts <- vect(d[country %in% countries,], crs = "+proj=longlat +datum=WGS84")
sel_pts <- vect(d[country %in% countries & !is.na(strat),], crs = "+proj=longlat +datum=WGS84")
strats <- unique(sel_pts$strat) |> substr(1,2)
plot(af, mar = c(.3,.3,.3,5), ext = ext(pol[pol$gid == iso,]), axes = FALSE, lwd = 1.5, xpd = FALSE, buffer = FALSE)
plot(ctry1[ctry1$country %in% countries], ext = ext(pol[pol$gid == iso,]), buffer = FALSE,
     border = "gray50", add = T, lwd = 0.5)
plot(all_pts, col = "gray70", add = TRUE,
     ext = ext(pol[pol$gid == iso,]), pch = 16, legend = FALSE, cex = 0.8)
plot(sel_pts, "strat", col = cols[seq_along(strats)], add = TRUE,
     ext = ext(pol[pol$gid == iso,]), pch = 16, legend = FALSE, cex = 0.8)

legend(
  x = "right",
  legend = c(strats, "NC"),
  col = c(cols[seq_along(strats)], "gray70"),
  pt.cex = 1.8, cex = 1.5, pch = 16, inset = -0.25, bty = "n", y.intersp = 0.5,
  title = "CZ\n", title.adj = 0.5, xpd = NA
)

plot(af, axes = FALSE, lwd = 1.5, add = TRUE, ext = ext(pol[pol$gid == iso,]), buffer = FALSE)
plot(ext(pol[pol$gid == iso,]), axes = FALSE, lwd = 1.5, border = "darkblue", add = TRUE)
sbar(80, ext(pol[pol$gid == iso,])[c(1,3)]+c(0.1,.4), "bar", 4, "kilometers")
text(ext(pol[pol$gid == iso,])[1]+.04, ext(pol[pol$gid == iso,])[4]-.04, "UG-KE", font = 2, cex = 2, adj = c(0,1), xpd = NA)



## TZA -----
countries <- "tanzania"
iso <- "TZA"
all_pts <- vect(d[country %in% countries,], crs = "+proj=longlat +datum=WGS84")
sel_pts <- vect(d[country %in% countries & !is.na(strat),], crs = "+proj=longlat +datum=WGS84")
strats <- unique(sel_pts$strat) |> substr(1,2)
plot(af, mar = c(.5,.1,.3,5), ext = ext(pol[pol$gid == iso,]), axes = FALSE, lwd = 1.5, xpd = FALSE, buffer = FALSE)
plot(ctry1[ctry1$country %in% countries], ext = ext(pol[pol$gid == iso,]), buffer = FALSE,
     border = "gray50", add = T, lwd = 0.5)
plot(all_pts, col = "gray70", add = TRUE,
     ext = ext(pol[pol$gid == iso,]), pch = 17, legend = FALSE, cex = 0.8)
plot(sel_pts, "strat", col = cols[seq_along(strats)], add = TRUE,
     ext = ext(pol[pol$gid == iso,]), pch = 17, legend = FALSE, cex = 0.8)

legend(
  x = "right",
  legend = c(strats, "NC"),
  col = c(cols[seq_along(strats)], "gray70"),
  pt.cex = 1.8, cex = 1.5, pch = 17, inset = c(-0.25), bty = "n", y.intersp = 0.5,
  title = "CZ\n", title.adj = 0.5, xpd = NA
)

plot(af, axes = FALSE, lwd = 1.5, add = TRUE, ext = ext(pol[pol$gid == iso,]), buffer = FALSE)
plot(ext(pol[pol$gid == iso,]), axes = FALSE, lwd = 1.5, border = "darkblue", add = TRUE)
sbar(80, ext(pol[pol$gid == iso,])[c(1,4)]+c(.5,-.3), "bar", 4, "kilometers")
text(ext(pol[pol$gid == iso,])[1]+.04, ext(pol[pol$gid == iso,])[4]-.04, "TZ", font = 2, cex = 2, adj = c(0,1), xpd = NA)




dev.off()

# save source data -------
dt <- d[, .(country, cz = substr(strat, 1, 2), longitude = round(lon,2), latitude = round(lat,2))]
dt[, country:= stringr::str_to_title(country)]
setorder(dt, cz, latitude, country, na.last = TRUE)
fwrite(dt, "plots/source_data/Fig_2.csv")




