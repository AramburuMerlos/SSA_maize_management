# author: Fernando Aramburu Merlos
# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)

# prepare data -------------

d <- fread("data/One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv", na.strings = "")

soil_vars <- names(d)[names(d) %like% "soil"]
d <- d[, .SD, .SDcols = c("strat", soil_vars)]
d <- d[!is.na(strat)]

df <- data.table()

for(i in seq_along(soil_vars)) {
  fm <- as.formula(paste(soil_vars[i], "~ strat"))
  a <- lm(fm, data = d) |> anova()
  df <- rbind(df, data.table(var = soil_vars[i],
                             sscz = a$`Sum Sq`[1], sse = a$`Sum Sq`[2]))
}
df
df[, pcz:= 100 * sscz/(sscz + sse)]
df[, per:= 100 * sse/(sscz + sse)]

df[, var:= gsub("soil_", "", var)]
df[var == "rzpawhc", var:= toupper(var)]

setorderv(df, "pcz", order = -1L)

cols <- RColorBrewer::brewer.pal(5, "Set1")

# xy plot --------
png("plots/soil_vars_SS_CZ.png", width = 5, height = 4, units = "in", res = 300)
par(mar = c(4,4,1,6), las = 1)
plot(1, type = "n", xlim = c(0,100), ylim = c(0,100), xlab = "", ylab = "", axes = FALSE)
axis(1, at = seq(0,100,20), pos = 0)
axis(2, at = seq(0,100,20), pos = 0)
axis(3, at = c(0,100), pos = 100, labels = FALSE, lwd.ticks = 0)
axis(4, at = c(0,100), pos = 100, labels = FALSE, lwd.ticks = 0)
mtext("SS Error (%)", 1, line = 1.8)
mtext("SS CZ (%)", 2, line = 2, las = 0)
points(df$per, df$pcz, pch = 21:25, bg = cols, cex = 1.8)
clip(0,100,0,100)
abline(0,1)
legend(x = 100, y = 50, xjust = 0, yjust = 0.5, legend = df$var,
       pch = 21:25, pt.bg = cols, xpd = NA, bty = "n", pt.cex = 1.8)
dev.off()


# barplot ---------
cols <- palette.colors(2)
dp <- t(df[, .(pcz, per)])
colnames(dp) <- df$var

png("plots/soil_vars_SS_CZ_barplot.png", width = 5, height = 4, units = "in", res = 300)
par(mar = c(4,4,1,6), las = 1, mgp = c(3, 0.8, 0))
barplot(dp, col = cols, axisnames = FALSE, las = 1)
mtext("Proportion of variance explained (% of SS)", 2, line = 2.5, las = 0)
text(seq(0.6, (ncol(dp)-1)*1.2 + 0.6, by = 1.2), par("usr")[3] - 0.5,
     srt = 60, adj = 1, xpd = TRUE, labels = colnames(dp), cex = 1)
legend(x = par("usr")[2], y = 50, xjust = 0, yjust = 0.5, legend = c("SSE", "SS CZ"),
       fill = rev(cols), xpd = NA)
dev.off()

fwrite(df, "plots/source_data/Supplementary_Fig_10.csv")
