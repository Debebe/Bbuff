## analysis building on costing and data preparation work

## === packages
library(here)
library(data.table)
library(ggplot2)

## === data
## read in pre-prepared data
gdp_inc_le_costs <- readRDS(file = here("data/gdp_inc_le_costs.rds"))
load(here("data/whokey.Rdata"))
whoz <- c("AFR", "AMR", "EMR", "EUR", "SEA", "WPR")
whozt <- c(
  "Africa", "The Americas",
  "Eastern Mediterranean", "Europe", "South-East Asia",
  "Western Pacific"
)
for (i in seq_along(whoz)) {
  whokey[g_whoregion == whoz[i], region := whozt[i]]
}
whokeyshort <- unique(whokey[, .(g_whoregion, region)])
whokeyshort <- rbind(
  whokeyshort,
  data.table(g_whoregion = "Global", region = "Global")
)

## === utility functions
source("R/utilities/utilities.R")

## === parameter values and samplers
source(here("R/utilities/parameters.R"))


## === expand data for PSA
set.seed(1234)
Niter <- 50 # TODO increase ultimately
D <- as.data.table(gdp_inc_le_costs)
D <- D[cov_cat == "WUENIC"]
N <- nrow(D)
D <- D[rep(seq_len(N), each = Niter)]
D[, iter := rep(seq_len(Niter), N)]
D[iter == 1][iso3 == "AFG"] #check

## TB incidence uncertainty
D[, tbinc := incbest + rnorm(n = nrow(D), (inchi - inclo) / 3.92)]
D[, tbinc := abs(incbest)] # for illustration
D[, bcg_hr :=
  bcg_haz_tb + rnorm(n = nrow(D), bcg_haz_tb / 20)] # uncorrelated by iso3


## TODO sampling for parameters used


## === functions for calculations
source(here("R/utilities/modelfunctions.R"))




## === calculations
## expect D in memory; carries out operations on D using modelfunctions
## conventions:
## prepend rslt_ for results
## postpend _sq for status quo (current BCG coverage)
## postpend _cf for counterfactual (no BCG)
source(here("R/utilities/calculations.R"))


## === aggregations and outputs
CEA <- D[, .(
  ## expected net benefit at WTP=30%GDP
  ENB30 = mean(
    0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
      (rslt_cost_sq - rslt_cost_cf)
  ),
  g30 = mean(
    0.3 * GDP * (rslt_health_sq - rslt_health_cf) / bcg_coverage -
      (rslt_cost_sq - rslt_cost_cf) / bcg_coverage
  ),
  GDP = mean(GDP),
  ## ICER
  ICER = mean(rslt_cost_sq - rslt_cost_cf) /
    mean(rslt_health_sq - rslt_health_cf),
  ## downslope for exceeding demand TODO
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
CEA[, summary(ENB30)]
CEA[, summary(g30)]
CEA[, summary(u)]
CEA[, summary(g30 / (g30 + u))]
CEA[, summary(qnorm(g30 / (g30 + u)))]


## TODO global and regional total outputs, e.g.:

## extract results, compute differences, reformat
keep <- grep("rslt", names(D), value = TRUE)
keep <- c("iso3", "iter", "Pop", unique(keep))
output_table <- D[, ..keep]
output_table <- melt(output_table, id = c("iter", "iso3", "Pop"))
output_table[, type := ifelse(grepl("cf", variable), "cf", "sq")]
output_table[, variable := gsub("rlst_", "", variable)]
output_table[, variable := gsub("_cf|_sq", "", variable)]
output_table[, value := value * Pop]
output_table <- dcast(output_table,
  iter + iso3 + variable ~ type,
  value.var = "value"
)


## averted
output_table[, av := cf - sq]

## global TODO NaNs? <- merge with WHO regions to do local version
output_table <- output_table[is.finite(av), .(
  cf = sum(cf), sq = sum(sq), av = sum(av)
),
by = .(iter, variable)
]

## hi/lo & reshape
eps <- 0.025
output_table <- melt(output_table,
  id = c("iter", "variable")
) # TODO better var names

output_table <- output_table[, .(
  mid = mean(value), lo = quantile(value, eps), hi = quantile(value, 1 - eps)
), by = .(variable, variable.1)]

output_table <- dcast(output_table,
  variable ~ variable.1,
  value.var = c("mid", "lo", "hi")
)

## format numbers, add brackets
output_table[
  ,
  c("sq_txt", "cf_txt", "av_txt") := .(
    brkt(mid_sq, lo_sq, hi_sq),
    brkt(mid_cf, lo_cf, hi_cf),
    brkt(mid_av, lo_av, hi_av)
  )
]
output_table

fwrite(output_table, file = here("outputs/output_table.csv"))


## NOTE ======== TODO
## in calculating total sq ATT: should really use notifications
## may be true for sq TB incidence
## need noise in parameters:
## - TB incidence
## - BCG efficacy
## - costs
## some parms correlated across countries? -> data.table(iter,parmsample)
## placeholder Ltbm to be updated
## update parameter names in calculations.R to use 'noisy' ones
## CDR defined by notification and incidence
## ICER graph - color points by which threshold they are below
## figure out what's going on in RWA or other ICER<0
## regional versions of output table
