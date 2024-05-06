# author: Fernando Aramburu Merlos
# date: 2022-11-09

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
library(plotrix)

# PREPARE BARPLOT ----------------
lt <- readRDS("models/ctree/ctrees_by_ctry_cz_season.RDS")
lt <- lt[sort(names(lt))]

getUsefulPredictors <- function(x) {
  varid <- nodeapply(x, ids = nodeids(x),
                     FUN = function(n) split_node(n)$varid)
  varid <- unique(unlist(varid))
  names(data_party(x))[varid]
}

pred_list <- list(
  nutrients = c("N_kg_ha", "P_kg_ha", "K_kg_ha", "fert_in_hole", "compost", "comp_kg_ha", "manure"),
  cultivar = c("hybrid", "hyb_mat", "hyb_type", "hyb_yor", "hyb_tol_mln", "hyb_tol_msv",
               "hyb_tol_gls", "hyb_tol_nclb", "hyb_tol_rust", "hyb_tol_ear_rot"),
  `crop\nestablishment` = c("plant_date_dev", "pl_m2", "row_spacing"),
  adversities = c("weeding", "pesticide", "disease", "pest", "striga", "water_excess"),
  liming = c("lime_kg_ha"),
  rainfall = c("elev", "season_prec", paste0("season_prec_", 1:3)),
  topography = c("twi"),
  soil = c("soil_rzpawhc", "soil_clay",  "soil_pH","soil_orgC", "soil_ECEC")
)

vl <- sapply(lt, getUsefulPredictors)
vl
# remove environmental variables
vl <- lapply(vl, function(x) x[!x %in% c(pred_list$rainfall, pred_list$topography, pred_list$soil)])

vi <-  vl |>
  unlist() |>
  table() |>
  sort()

vi <- vi/length(lt) * 100
vi <- vi[vi > 5]

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

vti <- vti/length(lt) * 100
vti_col_indx <- match(names(vti), names(pred_list))

cols <- palette.colors(8)[c(6,8,4,2)]


# rename variables
names(vi) <- names(vi) |>
  str_replace_all("_", " ") |>
  str_remove("kg ha") |>
  str_trim("both") |>
  str_replace("N", "N fertilzation rate") |>
  str_replace("hybrid", "hybrid seed use") |>
  str_replace("fert in hole", "fertilizer placement") |>
  str_replace("plant date dev", "sowing date") |>
  str_replace("P", "P fertilization rate") |>
  str_replace("pest$", "pest damage") |>
  str_replace("pl m2", "  plant density") |>
  str_replace("compost", "compost amount") |>
  str_replace("comp$", "compost use") |>
  str_replace("pesticide", "pesticide use") -> names(vi)
vi <- sort(vi)
names(vi)



dir.create("maps", F)


# Modified floating.pie function from plotrix
my.floating.pie <- function (
    xpos = 0, ypos = 0, x, edges = 200, radius = 1, col = NULL,
    startpos = 0, shadow = FALSE, shadow.col = c("#ffffff", "#cccccc"),
    explode = 0, ...
) {
  if (is.null(dev.list))
    plot(0, xlim = c(-1.5, 1.5) * radius + xpos, ylim = c(-1.5,
                                                          1.5) * radius + ypos, type = "n", axes = FALSE, xlab = "",
         ylab = "")
  if (!is.numeric(x))
    stop("floating.pie: x values must be numeric.")
  validx <- which(!is.na(x) & x > 0)
  col <- col[validx]
  x <- c(0, cumsum(x[validx])/sum(x[validx]))
  dx <- diff(x)
  nx <- length(dx)
  if (is.null(col))
    col <- rainbow(nx)
  else if (length(col) < nx)
    col <- rep(col, nx)
  xylim <- par("usr")
  plotdim <- par("pin")
  #yradius <- radius * (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) *
  #  plotdim[1]/plotdim[2]
  yradius <- radius

  bc <- 2 * pi * (x[1:nx] + dx/2) + startpos
  if (shadow && all(explode == 0)) {
    xc <- c(cos(seq(0, 2 * pi, length = edges)) * radius +
              xpos)
    yc <- c(sin(seq(0, 2 * pi, length = edges)) * yradius +
              ypos)
    polygon.shadow(xc, yc, col = shadow.col)
  }
  if (length(explode) < nx)
    explode <- rep(explode, nx)
  for (i in 1:nx) {
    n <- max(2, floor(edges * dx[i]))
    t2p <- 2 * pi * seq(x[i], x[i + 1], length = n) + startpos
    xc <- c(cos(t2p) * radius + xpos, xpos)
    yc <- c(sin(t2p) * yradius + ypos, ypos)
    if (explode[i]) {
      xc <- xc + cos(bc[i]) * explode[i]
      yc <- yc + sin(bc[i]) * explode[i]
    }
    polygon(xc, yc, col = col[i], ...)
    t2p <- 2 * pi * mean(x[i + 0:1]) + startpos
    xc <- cos(t2p) * radius
    yc <- sin(t2p) * radius
  }
  invisible(bc)
}

# data ------
d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")
d <- d[!is.na(strat)]

setorder(d, cz)
n_ctry <- d[, .N, by = .(country)]
n_ctry[, country:= stringr::str_to_title(country)]
setnames(n_ctry, "country", "COUNTRY")

dd <- fread("data/var_type_imp_by_ctry.csv")

## merge KEN and UGA ----
dd[country == "UGA", country:= "KEN"]
dd <- dd[, .(freq = sum(freq)), by = .(country, var_type)]
dd[, perc:= 100 * freq/sum(freq), by = .(country)]


# vectors -----------
ex <- ext(c(-18, 50, -35, 15))

## country ------
ctry_codes <- geodata::country_codes()
af_iso3 <- ctry_codes[ctry_codes$continent == "Africa", "ISO3"]
w <- geodata::world(path = "data/gadm", resolution = 1)
af <- w[w$GID_0 %in% af_iso3] |> crop(ex)


ctry <- geodata::gadm(
  unique(d$iso),
  path = "data/gadm",
  level = 0,
)

if(host == "LAPTOP-ST129J47") {
  names(ctry)[names(ctry) == "ID_0"] <- "GID_0"
}

ctry <- merge(ctry, n_ctry, by = "COUNTRY")
ctry$n_ctry <- paste0(ctry$GID_0, "\nn = ", ctry$N)

## polygons ------
pol <- vect("data/obs_polygons/obs_polygons.shp")

pol$gid[pol$gid == "UGA"] <- "KEN"
pol <- aggregate(pol, by = "gid")
centroids(pol)


## final details ----------
pie_coords <- centroids(pol)

pcol <- list(nutrients = cols[1], cultivar = cols[2], establishment = cols[3],
             pests = cols[4]) |> utils::stack()
names(pcol) <- c("color", "var_type")
setDT(pcol)
dd <- pcol[dd, on = .(var_type)]

iso <- pie_coords$gid
pc <- geom(pie_coords)

ar <- (ex[4]-ex[3])/(ex[2]-ex[1])

# PLOT -----------------
#png("plots/variable_importance_CIT_map.png", width = 8, height = 3.8, res = 300, units="in")
pdf("plots/pdf/Figure_3.pdf", width = 8, height = 3.8)

## barplot -----
par(mfrow = c(1,2), mar = c(4,7.5,0,1), oma = c(0,0,1,0), mgp = c(2.5,1,0), xpd = NA)

barplot(vi, col = cols[var_type_indx],
        horiz = TRUE, las = 1, cex.names = .85,
        xlim = c(0,ceiling(max(vi)/10)*10), xlab = "Relative importance (%)")

mtext("a", adj = 0.45, font = 2, cex = 1.5, outer = T, line = -0.5)
mtext("b", adj = 0.95, font = 2, cex = 1.5, outer = T, line = -0.5)
## map -------
plot(af, border = "grey60", col = "white", background = "lightblue",
     mar = c(3,1,0,1), las = 1, pax = list(mgp = c(2,.5,0)), buffer = FALSE)
#plot(ctry, add = TRUE, col = "gray92")

for(i in seq_along(iso)) {
  my.floating.pie(
    xpos = pc[i,"x"], ypos = pc[i,"y"],
    radius = 3,
    x = dd[country == iso[i], perc],
    col = dd[country == iso[i], color]
  )
  ty = pc[i,"y"] + 4.5
  tx = pc[i,"x"]
  txt = substr(iso[i],1,2)

  if(iso[i] == "TZA") {ty = ty - 10 ; tx = tx + 3}
  if(iso[i] == "ZMB") {tx = tx - 2}
  if(iso[i] == "NGA") {ty = ty - 3; tx = tx + 5}
  if(iso[i] == "KEN") {txt <- "UG-KE" ; tx = tx - 3; ty = ty - 1}
  if(iso[i] == "RBI") {txt <- "RW-BI" ; tx = tx - 7; ty = ty - 3}
  text(x = tx, y = ty, label = txt, font = 2, adj = c(0.5,0))
}
legend(
  "bottomleft", xpd = NA, cex = 0.87,
  legend = c("nutrients", "cultivars", "establishment", "pests"),
  fill = cols[1:4], bg = "white",
  inset = c(0,0.1)
  #  title = " Management\n type", title.adj = 0
)

dev.off()

# Save source data ----------
dir.create("plots/source_data", F, T)
dt <- data.table(agronomic_practice = names(vi), relative_importance = as.vector(vi))
setorderv(dt, "relative_importance", order = -1L)

dd <- dd[, .(country, type_of_practice = var_type, relative_importance = perc)]
dd <- merge(dd, ctry_codes[, c("ISO3", "NAME")], by.x = "country", by.y = "ISO3", all.x = TRUE)
dd[country == "RBI", NAME:= "Rwanda-Burundi"]
dd[, country:= NULL]
setcolorder(dd, "NAME")
setnames(dd, "NAME", "region")
dd[region == "Kenya", region:= "Uganda-Kenya"]
dt[, region:= "Sub-Saharan Africa"]
dt <- rbind(dt, dd, fill = TRUE)
setcolorder(dt, c("region", "type_of_practice", "agronomic_practice", "relative_importance"))
fwrite(dt, "plots/source_data/Fig_3.csv")

