# author: Fernando Aramburu Merlos
# date: 2022-11-02

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(partykit)
library(ggparty)
library(corrplot)
library(stringr)
# external function
devtools::source_url("https://raw.githubusercontent.com/martin-borkovec/ggparty/martin/R/add_splitvar_breaks_index_new.R")


# functions ----------------
# function to convert consequtive sequences to simplified text (for years)
f1 <- function(x) {
  x <- sort(x)
  if(length(x) == 1) return(as.character(x))
  brks <- c(0, which(diff(x) != 1), length(x))
  if(length(brks) > 2) {
    y <- sapply(seq(length(brks) - 1), function(i) x[(brks[i] + 1):brks[i+1]])
    ty <- lapply(y, function(v) ifelse(length(v) > 1, paste(range(v), collapse = "-"), v))
    paste(unlist(ty), collapse = ", ")
  } else {
    paste(range(x), collapse = "-")
  }
}

# load data and trees -------------

d <- fread("models/ctree/data_with_ctree_preds.csv")

lt <- readRDS("models/ctree/ctrees_by_ctry_cz_season.RDS")
lt <- lt[sort(names(lt))]

# observation groups
strats <- names(lt)

d[iso %in% c("RWA", "BDI") & !is.na(strat),
  country:= ifelse(uniqueN(country) > 1, "Rwanda - Burundi", country),
  by = .(strat)
]

# trees stats ------------
# n, R2, rmse
fr2 <- function(x,y) cor(x,y)^2
frmse <- function(x,y) sqrt(mean((x-y)^2))

dd <- d[
  , .(r2 = fr2(yield_kg_ha, cit_yhat),
      rmse = frmse(yield_kg_ha, cit_yhat),
      n = .N)
  , by = .(strat, country, season)
]
dd[, cz:= as.numeric(str_extract(strat, "(?<=^.{6}+)\\d+(?=..$)"))]
dd[, gdd_code:= floor(cz/1000)]
dd[, ai_code:= floor(cz/100) - gdd_code*10]

dgdd <- data.table(gdd_code = 1:10, gdd = c("0-2670","2671-3169","3170-3791","3792-4829","4830-5949","5950-7111","7112-8564","8565-9311","9312-9850",">9851"))

dai <- data.table(ai_code = 0:9, ai = c("0-2695","2696-3893","3894-4791","4792-5689","5690-6588","6589-7785","7786-8685","8686-10181","10182-12876",">12877"))

dd <- dgdd[dd, on = .(gdd_code)]
dd <- dai[dd, on = .(ai_code)]

# order stats to follow tree order
dd[, tree_order:= match(strat, strats)]
setorder(dd, tree_order)


# plot one by one (for loop) ------------

# list of data.tables with trees rules and other info
ldd <- vector("list", length(lt))

# quantile column names
qcols <- c("10%", "25%", "50%", "75%", "90%")

dir.create("plots/ctrees/trees", F, T)
dir.create("plots/ctrees/corrplot", F, T)

i=1
for(i in seq_along(lt)) {

  ## prepare data  -----

  # extract mean, n and quantiles of each terminal id:
  di <- fitted(lt[[i]]) |> as.data.table()
  setnames(di, names(di), c("id", "w", "y"))
  di[, y:= y/1e3]
  ddi <- di[
    , c(.(n = .N, mean = mean(y)), as.list(quantile(y, c(.1, .25, .5, .75, .9))))
    , by = .(id)
  ] |>
    round(2)

  if(max(ddi[, `90%`]) > 7.5) {
    ymx <- 9
  } else if(max(ddi[, `90%`]) > 5.5) {
    ymx <- 8
  } else {
    ymx <- 6
  }

  # extract rules
  rules <- data.frame(rules = partykit:::.list.rules.party(lt[[i]])) |>
    setDT(keep.rownames = "id")
  rules[, id:= as.numeric(id)]
  ddi <- rules[ddi, on = .(id)]

  ldd[[i]] <- ddi # save for later

  # rounded number in labels from some foreign code
  # https://stackoverflow.com/questions/60553985/number-of-decimal-places-on-edges-of-a-decision-tree-plot-with-ggparty
  rounded_labels <- add_splitvar_breaks_index_new(
    party_object = lt[[i]],
    plot_data = ggparty:::get_plot_data(lt[[i]]),
    round_digits = 2
  )

  # correct the whole list of rounded labels
  column_string <- vapply(rounded_labels$breaks_label,
                          function(.x){y <- unlist(.x, use.names = TRUE)
                          paste0(names(y), y, collapse = "*','~")},
                          FUN.VALUE = character(1))


  # tree depth
  tdepth <- capture.output(lt[[i]]) |>
    stringr::str_count("\\|") |>
    max()

  ### plot components ------------
  node_label <- geom_node_label(
    line_list = list(
      aes(label = splitvar),
      aes(label = paste("n =", nodesize)),
      aes(label = paste( "p", if (p.value < 0.0001){"< 0.0001"} else {paste("=", round(p.value, 4))}))),
    line_gpar = list(
      list(size = 14),
      list(size = 11),
      list(size = 11)),
    ids = "inner"
  )

  # node plots
  bxplt <-  geom_boxplot(
    data = ddi[, .SD, .SDcols = c("id", qcols)],
    aes(x = "",
        ymin = `10%`,
        lower = `25%`,
        middle = `50%`,
        upper = `75%`,
        ymax = `90%`,
        group = id),
    stat = "identity",
    alpha = 1,
    fill = "gray90"
  )
  txt <-  geom_text(
      data= ddi[, .SD, .SDcols = c("id", "rules")],
      aes(x = 1, y = ymx,
          group = id,
          label = paste(ddi$mean, '~Mg~ha^-1')),
      size = 4.5,
      vjust = .8, hjust = 0.5, parse = T
    )

  # put node plots together
  node_plot_list <-  list(
    bxplt,
    txt,
    theme_bw(),
    scale_y_continuous(limit = c(0, ymx), breaks = seq(0, ymx, 2)),
    scale_x_discrete(breaks = NULL),
    xlab(""), ylab(bquote("Yield (Mg ha"^-1*")")),
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 13)
    )
  )

  # avoid overlapping of line labels with nodes
  ids <- nodeids(lt[[i]])
  if(i %in% c(2)) {
    # overlapped edge label
    if(i == 2) oel <- 2
    # if(i == ...) oel <-  XXX
    edge_label <- list(
      geom_edge_label(mapping = aes(label = column_string), size = 5, ids = ids[-oel]),
      geom_edge_label(mapping = aes(label = column_string), size = 5, ids = oel, shift = 0.25)
    )
    rm(oel)
  } else {
    edge_label <- geom_edge_label(mapping = aes(label = column_string), size = 5)
  }

  # put tree plot together
  tree_plot <- ggparty(lt[[i]]) +
    geom_edge() +
    edge_label +
    node_label +
    geom_node_plot(gglist = node_plot_list, shared_axis_labels = TRUE) +
    geom_node_label(aes(label = paste0("n = ", nodesize)), size = 4,
                    ids = "terminal", nudge_y = 0.01, nudge_x = 0.01)
  if(tdepth < 3){
    tree_plot <- tree_plot +
      theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"))
  }

  # plot header

  years <- d[dd[i], on = .NATURAL][, unique(year)] |> sort() |> f1()

  hdr <- paste0(
    "Country: ", dd[i, country], "\n",
    "GDD: ", dd[i, gdd], "\n",
    "AI: ", dd[i, ai], "\n",
    "Year", ifelse(length(years) > 1, "s: ", ": "), years, "\n",
    "Season: ", dd[i, ifelse(strat %like% "KEN|RBI|UGA", season, "unique")]
  )

  # create image
  fn <- paste0("plots/ctrees/trees/", strats[i], "_tree.png")

  png(
    fn,
    height = 2 * tdepth + 4,
    width = ddi[,.N] * 2 + 2,
    units = 'in', type="windows", res=400, #compression = "lzw"
  )

  print(tree_plot)

  # add text with header and accuracy metrics
  fz <- 8 + tdepth * 2 # font size as a function of tree size
  grid::grid.text(hdr, x = 0.1, y = 0.95, hjust = 0, vjust = 1, gp=gpar(fontsize=fz))
  grid::grid.text(paste0("CZ #", i), x = 0.5, y = .97,  gp=gpar(fontsize=fz + 1, fontface = "bold"))
  r2 <- round(dd[i, r2], 2)
  rmse <- round(dd[i, rmse]/1e3, 2)
  grid::grid.text(bquote('RMSE' == .(rmse)), x=0.90, y=0.95, hjust = 1, gp=gpar(fontsize=fz))
  grid::grid.text(bquote(italic(R)^2~"="~ .(r2)), x=0.90, y=0.92, hjust = 1, gp=gpar(fontsize=fz))

  dev.off()

  # Pearson coefficient matrix
  dt <- data_party(lt[[i]]) |> setDT()
  cols <- names(dt)[!grepl("\\(", names(dt))]
  for(j in cols) {
    set(dt, i = NULL, j = j, value = as.numeric(dt[[j]]))
  }
  M <- cor(dt[, ..cols], use = "pairwise.complete.obs") |> suppressWarnings()

  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(dt[, ..cols])$p |> suppressWarnings()

  # colour scale
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

  # correlation matrix plot
  png(file= paste0("plots/ctrees/corrplot/", strats[i], "_corrplot.png"),
      w=10,h=10, res=600, units="in")
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
}

names(ldd) <- names(lt)

rbindlist(ldd, use.names = TRUE, idcol = "tree_name") |>
  fwrite("models/ctree/SummaryTable.csv")



