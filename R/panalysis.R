## analysis building on costing and data preparation work

## packages
library(data.table)

## read in pre-prepared data
gdp_inc_le_costs <- readRDS(file = "data/gdp_inc_le_costs.rds") |>
  filter(cov_cat == "WUENIC")

## === functions for calculations

## parameter work
