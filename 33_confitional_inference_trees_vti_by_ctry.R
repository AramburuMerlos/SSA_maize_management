# author: Fernando Aramburu Merlos
# date: 2023-04-25

# setup ---------

host <- system("hostname", TRUE)

if (host == "LAPTOP-ST129J47") {
  setwd("C:/Users/ferar/OneDrive - University of Nebraska-Lincoln/OAF")
} else if (host == "LRDAH-DX5B0R3") {
  setwd("C:/Users/faramburumerlos2/OneDrive - University of Nebraska-Lincoln/OAF")
}

library(data.table)
library(partykit)
library(stringr)
library(ggthemes)


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
  soil = c("soil_rzpawhc", "soil_clay", "soil_pH","soil_orgC", "soil_ECEC")
)

vl <- sapply(lt, getUsefulPredictors)
vl
# remove elevation and season prec
vl <- lapply(vl, function(x) x[!x %in% c(pred_list$rainfall, pred_list$topography, pred_list$soil)])

# countries names
names(vl) <- names(vl) |>
  str_extract("(?<=_)[:alpha:]{3}")


# chat gpt magic (I could have figure it out as well)

# Initialize an empty list to store the results
result <- list()

# Loop over the unique names in the list
for(name in unique(names(vl))) {
  # Extract the character vectors with this name
  vectors <- vl[names(vl) == name]

  # Combine the vectors into a single character vector
  combined_vector <- unlist(vectors)

  # Create a table of frequencies for this combined vector
  combined_freq_table <- table(combined_vector)

  # Store the frequency table for this name in the result list
  result[[name]] <- combined_freq_table
}

# Print the overall frequency table and the per-name frequency tables
print(result)

# Convert the result list to a data frame
df <- do.call(rbind, lapply(names(result), function(name) {
  data.frame(country = name, var = names(result[[name]]), freq = as.vector(result[[name]]))
}))

# Print the resulting data frame
print(df)

setDT(df)

setorder(df, country, freq)

# pred types
pred_list <- list(
  nutrients = c("N_kg_ha", "P_kg_ha", "K_kg_ha", "fert_in_hole", "compost", "comp_kg_ha", "manure"),
  cultivar = c("hybrid", "hyb_mat", "hyb_type", "hyb_yor", "hyb_tol_mln", "hyb_tol_msv",
               "hyb_tol_gls", "hyb_tol_nclb", "hyb_tol_rust", "hyb_tol_ear_rot"),
  establishment = c("plant_date_dev", "pl_m2", "row_spacing"),
  pests = c("weeding", "pesticide", "disease", "pest", "striga", "water_excess")
)
pred_df <- stack(pred_list)
setDT(pred_df)
setnames(pred_df, c("values", "ind"), c("var", "var_type"))
pred_df
d <- pred_df[df, on = "var"]
dd <- d[, .(freq = sum(freq)), by = .(country, var_type)]
setorderv(dd, c("country", "freq"), c(1L, -1L))
dd[, perc:= round(100 * freq/sum(freq),2), by = .(country)]
dd

fwrite(dd, "data/var_type_imp_by_ctry.csv")
