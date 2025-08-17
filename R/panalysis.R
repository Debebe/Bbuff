## analysis building on costing and data preparation work

## === packages
library(here)
library(data.table)
library(ggplot2)

## === data
## read in pre-prepared data
gdp_inc_le_costs <- readRDS(file = here("data/gdp_inc_le_costs.rds")) |>
  filter(cov_cat == "WUENIC")
load(here("data/whokey.Rdata"))

## expand data for PSA
Niter <- 50 # TODO increase ultimately
D <- gdp_inc_le_costs[rep(seq_len(nrow(gdp_inc_le_costs)), each = Niter)]
D[, iter := rep(seq_len(Niter), nrow(gdp_inc_le_costs))]
## TODO sampling for parameters used


## === functions for calculations
source(here("R/modelfunctions.R"))


## === parameter values and samplers
source(here("R/parameters.R"))


## === calculations
## conventions:
## prepend rslt_ for results
## postpend _sq for status quo (current BCG coverage)
## postpend _cf for counterfactual (no BCG)
source(here("R/calculations.R"))


## === aggregations and outputs
CEA <- D[, .(
  ## expected net benefit at WTP=30%GDP
  ENB30 = mean(
    0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
      (rslt_cost_sq - rslt_cost_cf)
  ),
  GDP = mean(GDP),
  ## ICER
  ICER = mean(rslt_cost_sq - rslt_cost_cf) /
    mean(rslt_health_sq - rslt_health_cf),
  ## downslope for exceeding demand
  u = mean(uc_tot_vax_delv_ave)
),
by = iso3
]

summary(CEA)

CEA <- merge(CEA, whokey, by = "iso3")
CEA <- CEA[!is.na(ENB30)]

## ICER plot
CEA$iso3 <- factor(CEA$iso3, levels = CEA[order(ICER)]$iso3, ordered = TRUE)

ggplot(CEA[ICER > 0], aes(iso3, ICER)) +
  geom_point(aes(iso3, GDP * 0.3), shape = 3, col = 2) +
  geom_point(aes(iso3, GDP * 0.5), shape = 3, col = 4) +
  geom_point(aes(iso3, GDP * 1.0), shape = 3, col = 5) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  facet_wrap(~g_whoregion, scales = "free") +
  theme_linedraw()

ggsave(file = here("outputs/cea_ICER_iso3.png"), w = 9, h = 8)


## ENB plot
CEA$iso3 <- factor(CEA$iso3, levels = CEA[order(ENB30)]$iso3, ordered = TRUE)

ggplot(CEA, aes(iso3, ENB30)) +
  geom_hline(yintercept = 0, col = 2) +
  geom_point() +
  coord_flip() +
  facet_wrap(~g_whoregion, scales = "free") +
  theme_linedraw()

ggsave(file = here("outputs/cea_ENB_iso3.png"), w = 9, h = 8)

## TODO buffers
CEA[,summary(ENB30)]
CEA[, summary(u)]
CEA[, summary(ENB30/(ENB30+u))]
CEA[, summary(qnorm(ENB30 / (ENB30 + u)))]


## TODO global and regional total outputs, e.g.:
output_table <- D[
  !is.na(rslt_tb_deaths_sq + rslt_tb_deaths_cf), #TODO
  .(iso3, iter,
    deaths_sq = rslt_tb_deaths_sq * Pop,
    deaths_cf = rslt_tb_deaths_sq * Pop
  )
]

output_table[, deaths_av := deaths_sq - deaths_cf] # TODO BUG!
output_table <- output_table[, .(iso3, iter,
  deaths_sq = sum(deaths_sq),
  deaths_cf = sum(deaths_cf),
  deaths_av = sum(deaths_av)
  ), by = iter]

## TODO mean,hi, lo quantiles







