## analysis building on costing and data preparation work

rm(list = ls())

## === packages
library(here)
library(data.table)
library(ggplot2)
library(truncnorm)
library(dplyr)

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


## === expand data for PSA
set.seed(1234)
Niter <- 500 # TODO increase ultimately
D <- as.data.table(gdp_inc_le_costs)
D <- D[cov_cat == "WUENIC"]
N <- nrow(D)
D <- D[rep(seq_len(N), each = Niter)]
D[, iter := rep(seq_len(Niter), N)]
D[iter == 1][iso3 == "AFG"] #check

## === parameter values and samplers
source(here("R/utilities/parameters.R"))

## TODO sampling for parameters used = VE, prop_tbm, cost etc

D <- D %>%
  rowwise() %>%
  mutate(
    # vax efficacy
    bcg_haz_tb = 1 - sample_beta(
      bcg_eff_tb.m,
      bcg_eff_tb.lo,
      bcg_eff_tb.hi
    ),
    bcg_haz_tbm = 1 - sample_beta(
      bcg_eff_tbm.m,
      bcg_eff_tbm.lo,
      bcg_eff_tbm.hi
    ),
    # tbm_prop
    prop_tbm = sample_beta(
      prop_tbm,
      prop_tbm.lo,
      prop_tbm.hi
    ),
    # inc
    incbest = sample_truncn(incbest, inclo, inchi),
    # costs
    ucost_dstb.m = sample_gamma(
      mean = ucost_dstb.m,
      sd = ucost_dstb.sd
    ),
    ucost_tbm.m = sample_gamma(mean = ucost_tbm.m, sd = ucost_tbm.sd),
    uc_tot_vax_delv_ave = sample_gamma(
      mean = uc_tot_vax_delv_ave,
      lo = uc_tot_vax_delv_lo,
      hi = uc_tot_vax_delv_hi
    ),
    uc_labor_ave = sample_gamma(
      mean = uc_labor_ave,
      lo = uc_labor_lo,
      hi = uc_labor_hi
    ),
    uc_sc_ave = sample_gamma(
      mean = uc_sc_ave,
      lo = uc_su_lo,
      hi = uc_sc_hi
    ),
    uc_servd_ave = sample_gamma(
      mean = uc_servd_lo,
      lo = uc_servd_lo,
      hi = uc_servd_hi
    ),
    uc_capital_ave = sample_gamma(
      mean = uc_capital_ave,
      lo = uc_capital_lo,
      hi = uc_capital_hi
    )
  ) %>%
  ungroup() %>%
  as.data.table()



# D$prop_tbm <-0

## === functions for calculations
source(here("R/utilities/modelfunctions.R"))


# ## === parameter values and samplers
# source(here("R/utilities/parameters.R"))


## === calculations
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
  GDP = mean(GDP),
  ## g
  bcg_cov = unique(bcg_coverage),
  # g = ENB30 / bcg_cov,
  ## ICER
  ICER = mean(rslt_cost_sq - rslt_cost_cf) /
    mean(rslt_health_sq - rslt_health_cf),
  ## downslope for exceeding demand
  u = mean(0.75 * uc_labor_ave +
    uc_sc_ave +
    uc_capital_ave +
    ucost_proc_bcg) # TODO vax prep and inject is 25% of all labour cost?
),
by = iso3
]


CEA[, g := ENB30 / bcg_cov]
CEA[, bcg_cov := NULL]
summary(CEA)

CEA <- merge(CEA, whokey, by = "iso3")
CEA <- CEA[!is.na(ENB30)]

## ICER plot
CEA$iso3 <- factor(CEA$iso3, levels = CEA[order(ICER)]$iso3, ordered = TRUE)

# ggplot(CEA[ICER > 0], aes(iso3, ICER)) +
#   geom_point(aes(iso3, GDP * 0.3), shape = 3, col = 2) +
#   geom_point(aes(iso3, GDP * 0.5), shape = 3, col = 4) +
#   geom_point(aes(iso3, GDP * 1.0), shape = 3, col = 5) +
#   geom_point() +
#   scale_y_continuous(labels = scales::comma) +
#   coord_flip() +
#   facet_wrap(~g_whoregion, scales = "free") +
#   theme_linedraw()

ggplot(CEA[ICER > 0], aes(iso3, ICER)) +
  geom_point(aes(iso3, GDP * 0.3), shape = 3, col = 2) +
  geom_point(aes(iso3, GDP * 0.5), shape = 3, col = 4) +
  geom_point(aes(iso3, GDP * 1.0), shape = 3, col = 5) +
  geom_point(aes(shape = (ICER < 0.3 * GDP))) +
  scale_shape_manual(values = c(1, 19), guide = "none") +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  facet_wrap(~region, scales = "free") +
  theme_linedraw() +
  xlab("Country ISO3 code") +
  ylab("Incremental cost-effectiveness ratio (USD/DALY)")

  # geom_point(aes(y = GDP * 0.3, shape = "30% GDP", color = "30% GDP")) +
  # geom_point(aes(y = GDP * 0.5, shape = "50% GDP", color = "50% GDP")) +
  # geom_point(aes(y = GDP * 1.0, shape = "100% GDP", color = "100% GDP")) +
  # geom_point() +
  # scale_shape_manual(
  #   name = "WTP",
  #   values = c("30% GDP" = 3, "50% GDP" = 3, "100% GDP" = 3),
  #   breaks = c("30% GDP", "50% GDP", "100% GDP")
  # ) +
  # scale_color_manual(
  #   name = "WTP",
  #   values = c("30% GDP" = 2, "50% GDP" = 4, "100% GDP" = 5),
  #   breaks = c("30% GDP", "50% GDP", "100% GDP")
  # ) +
  # scale_y_continuous(labels = scales::comma) + 
  # coord_flip() +
  # facet_wrap(~g_whoregion, scales = "free") +
  # theme_linedraw() +
  # theme(legend.position = "top",
  #       legend.box.spacing = unit(0, "pt"),   # no gap between legend and plot
  #       legend.margin = margin(0, 0, 0, 0),   # no internal padding in legend
  #       plot.margin = margin(0, 5, 5, 5)
  # )


ggsave(file = here("outputs/cea_ICER_iso3.png"), w = 9, h = 8)


## ENB plot
CEA$iso3 <- factor(CEA$iso3, levels = CEA[order(ENB30)]$iso3, ordered = TRUE)

ggplot(CEA, aes(iso3, ENB30)) +
  geom_hline(yintercept = 0, col = 2) +
  geom_point() +
  coord_flip() +
  facet_wrap(~region, scales = "free") +
  theme_linedraw() +
  xlab("Country ISO3 code") +
  ylab("Expected net benefit (USD)")

ggsave(file = here("outputs/cea_ENB_iso3.png"), w = 9, h = 8)

## TODO buffers
CEA[, summary(ENB30)]
CEA[, summary(u)]
CEA[, summary(ENB30 / (ENB30 + u))]
CEA[, summary(qnorm(ENB30 / (ENB30 + u)))]


CEAA <- CEA %>%
  inner_join(
    gdp_inc_le_costs %>%
      select(Country = country, iso3) %>%
      distinct(iso3, .keep_all = TRUE),
    by = "iso3"
  ) %>%
  # filter(ENB30>0)%>%
  mutate(
    CV1 = 0.05, CV2 = 0.1, CV3 = 0.15,
    z_score = qnorm(g / (g + u), mean = 0, sd = 1)
  ) %>%
  mutate(
    Bf1 = round(100 * z_score * CV1, 1),
    Bf2 = round(100 * z_score * CV2, 1),
    Bf3 = round(100 * z_score * CV3, 1)
  )

ft <- CEAA %>%
  filter(Bf1 > 0, !is.na(Bf1), ENB30 > 0) %>%
  select(
    Region = g_whoregion,
    Country, ENB = ENB30, ICER, `Buffer 1` = Bf1,
    `Buffer 2` = Bf2, `Buffer 3` = Bf3
  ) %>%
  mutate(
    ENB = round(ENB, 1),
    ICER = round(ICER, 0)
  ) %>%
  as.data.frame() %>%
  make_flextable()


## Save to word document TODO missing package:: 
doc <- read_docx() |>
  body_add_flextable(value = ft) |>
  body_add_par(" ", style = "Normal") # optional spacing

print(doc, target = "outputs/buffer_sz.docx")


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

## global TODO NaNs?
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

## change units to millions for cost and health
fac <- 1e6
output_table[
  variable %in% c("rslt_att_cost", "rslt_health"),
  `:=`(
    mid_sq = mid_sq / fac, lo_sq = lo_sq / fac, hi_sq = hi_sq / fac,
    mid_cf = mid_cf / fac, lo_cf = lo_cf / fac, hi_cf = hi_cf / fac,
    mid_av = mid_av / fac, lo_av = lo_av / fac, hi_av = hi_av / fac
  )
]



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
