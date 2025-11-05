## analysis building on costing and data preparation work

rm(list = ls())


## === packages
pacman::p_load(here,data.table, dplyr, tidyr, stringr, truncnorm,
               flextable, officer,kableExtra,ggplot2, ggrepel)

## === data
## read in pre-prepared data
gdp_inc_le_costs <- readRDS(file = here("data/gdp_inc_le_costs.rds"))

tbinc <- fread("data/TB_burden_countries_2025-10-29.csv") %>%
  dplyr::filter(year==2023) %>%
  dplyr::select(iso3, year,inc_all=e_inc_100k,  e_inc_100k_lo, e_inc_100k_hi,
                notif_all=c_newinc_100k,
                c_cdr, c_cdr_lo, c_cdr_hi)%>%as.data.table()

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

save(whokey,file=here("data/whokey.RData"))

load(here("data/LEu5.Rdata")) # single year under five life expectancy
## === utility functions
source("R/utilities/utilities.R")


## === expand data for PSA
set.seed(1234)
Niter <- 10000 # TODO increase ultimately
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
    # bcg_haz_tbm = 1 - sample_beta(
    #   bcg_eff_tbm.m,
    #   bcg_eff_tbm.lo,
    #   bcg_eff_tbm.hi
    # ),
    # 
    bcg_haz_tbm = sample_gamma(
      bcg_eff_tbm.m,
      bcg_eff_tbm.lo,
      bcg_eff_tbm.hi
    ),
    
    # tbm_prop
    prop_tbm = sample_beta(
      prop_tbm.ave,
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

D[,LE:=NULL]

samp <- D

rm(D)
save(samp, file= here("outputs/samp.RData"))

load("outputs/samp.RData")
load(here("data/LEu5.Rdata")) # single year under five life expectancy

## === functions for calculations
source(here("R/utilities/modelfunctions.R"))
source(here("R/utilities/parameters.R"))

## === calculations
## conventions:
## prepend rslt_ for results
## postpend _sq for status quo (current BCG coverage)
## postpend _cf for counterfactual (no BCG)
source(here("R/utilities/calculations.R")) # generates D data.table

# save PSA samples
save(D, file = here("outputs/PSA.RData")) # 

load(here("outputs/PSA.RData"))


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

thresholds <- c(0.3, 0.5, 1.0)  

CEA <- CEA%>%
  crossing(threshold = thresholds)%>%
  mutate(legend_label = paste0(threshold," GDP"))%>% 
  mutate(legend_label= factor(legend_label,
                              levels = c("0.3 GDP", "0.5 GDP", "1 GDP")))|>
  dplyr::mutate(ICER_val = GDP * threshold,
                ICER_Label= ifelse(ICER < 0.3 * GDP,"ICER < 0.3 GDP", "ICER >= 0.3 GDP" ))%>%
  left_join(gdp_inc_le_costs%>%
               filter(cov_cat=="WUENIC")%>%
                            dplyr::select(iso3, inc_u5=incbest,
                                          notif_u5=notif,
                                          BCG=bcg_coverage,CDR=cdr,
                                          ucvax=uc_tot_vax_delv_ave,
                                          uctb=ucost_dstb.m, 
                                          uctbm=ucost_tbm.m),
                          by= "iso3")%>%
  inner_join(tbinc, by="iso3")%>%
  
  mutate(CV1 = 0.05, CV2 = 0.1, CV3 = 0.15,
    z_score = qnorm(g / (g + u), mean = 0, sd = 1)) %>%
  mutate(
    Bf1 = round(100 * z_score * CV1, 1),
    Bf2 = round(100 * z_score * CV2, 1),
    Bf3 = round(100 * z_score * CV3, 1))%>%
as.data.table()
  
save(CEA, file = here("outputs/CEA.RData"))



## TODO buffers
CEA[, summary(ENB30)]
CEA[, summary(u)]
CEA[, summary(ENB30 / (ENB30 + u))]
CEA[, summary(qnorm(ENB30 / (ENB30 + u)))]


## TODO global and regional total outputs, e.g.:


#====data for percapita deaths======
per_cap_deaths <- D%>%
  dplyr::select(iso3, iter,matches("rslt_tb_deaths|rslt_tbn_deaths|rslt_tbm_deaths"))%>%
  pivot_longer(cols=-c("iter", "iso3"), names_to = "variable")%>%
  as.data.table()

per_cap_deaths[, type:= ifelse(str_detect(variable, "cf"), "cf", "sq")]
per_cap_deaths[, variable:= gsub("rlst_", "", variable)]
per_cap_deaths[, variable:= gsub("_cf|_sq", "", variable)]
per_cap_deaths <- dcast(per_cap_deaths,
                        iter + iso3 + variable ~ type,value.var = "value")%>%
  mutate(av=cf-sq)%>% select(-c("sq", "cf"))%>%as.data.table()

deaths_per_cap <- merge(per_cap_deaths, whokey, by = "iso3") %>%
  group_by(g_whoregion, region,iso3,variable)%>%
  summarise(av= mean(av, na.rm=TRUE))%>% filter(!is.na(av))|>
  mutate(type="percapita_death_averted")%>%
  as.data.table()


##=
per_cap_ave_inc <- D%>%
  select(iso3, iter,matches("rslt_inc|rslt_tbminc"))%>%
  pivot_longer(cols=-c("iter", "iso3"), names_to = "variable")%>%
  as.data.table()

per_cap_ave_inc[, type:= ifelse(str_detect(variable, "cf"), "cf", "sq")]
per_cap_ave_inc[, variable:= gsub("rlst_", "", variable)]
per_cap_ave_inc[, variable:= gsub("_cf|_sq", "", variable)]

per_cap_ave <- dcast(per_cap_ave_inc,
                        iter + iso3 + variable ~ type,value.var = "value")%>%
  mutate(av=cf-sq)%>% select(-c("sq", "cf"))%>%
  group_by(variable, iso3)%>%
  summarise(av= mean(av, na.rm=TRUE))%>% filter(!is.na(av))|>
  mutate(type="percapita_inc_averted")%>%
  bind_rows(per_cap_deaths%>%
               group_by(variable, iso3)%>%
               summarise(av= mean(av, na.rm=TRUE),
                         type="percapita_death_averted"))%>%
  as.data.table()
  
save(per_cap_ave, file=here("outputs/per_cap_ave.RData"))

per_cap_ave %>%
  group_by(variable) %>%
  slice_max(av, n = 1, with_ties = FALSE) %>%
  mutate(Descreption=paste0("Max avertion is in ",iso3),
         variable= gsub("rslt_", "", variable))%>%
  mutate(variable= paste0("Percapita avertion in ", variable))%>%
  select(-iso3, -type, value=av)


per_cap_ave %>%
  group_by(variable) %>%
  slice_min(av, n = 1, with_ties = FALSE) %>%
  mutate(Descreption=paste0("Min avertion is in ",iso3),
         variable= gsub("rslt_", "", variable))%>%
  mutate(variable= paste0("Percapita avertion in ", variable))%>%
  select(-iso3, -type,value=av)




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

# deaths pop level
death_absolute <- output_table[is.finite(av), .(av = sum(av, na.rm = TRUE)),
                              by = .(iso3, variable)]%>%
  select(iso3,variable, av)%>%
  filter(grepl("deaths", variable))%>%
  mutate(type= "absolute")%>%
  inner_join(whokey, by = "iso3")%>%
  distinct(iso3, variable, .keep_all=TRUE)%>%
as.data.table()

deaths <- rbind(deaths_per_cap, death_absolute)
save(deaths, file=here("outputs/deaths.RData"))

load(here("outputs/deaths.RData"))
load(here("outputs/CEA.RData"))

## global TODO NaNs?

output_table <- output_table[is.finite(av), .(
  cf = sum(cf), 
  sq = sum(sq), 
  av = sum(av)
),
by = .(iter, variable)
]


## hi/lo & reshape

eps <- 0.025
output_table <- melt(output_table,
  id = c("iter", "variable")
) # TODO better var names

output_table <- output_table[, .(
  mid = mean(value), 
  lo = quantile(value, eps), 
  hi = quantile(value, 1 - eps)
), by = .(variable, variable.1)]

output_table <- dcast(output_table,
  variable ~ variable.1,
  value.var = c("mid", "lo", "hi")
  )

## change units to millions for cost and health, BCG dosese
fac <- 1e6
output_table[
  variable %in% c("rslt_att_cost","rslt_attm_cost", "rslt_cost",
                  "rslt_bcg_cost", "rslt_bcg_doses", "rslt_health","rslt_ly_tb"),
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


#=====Regional - this should come after the output_table=====

keep <- grep("rslt", names(D), value = TRUE)
keep <- c("who_region","iso3", "iter", "Pop", unique(keep))

out_tab_r <- D[, ..keep]
out_tab_r <- melt(out_tab_r, id = c("iter", "iso3","who_region", "Pop"))
out_tab_r[, type := ifelse(grepl("cf", variable), "cf", "sq")]
out_tab_r[, variable := gsub("rlst_", "", variable)]
out_tab_r[, variable := gsub("_cf|_sq", "", variable)]
out_tab_r[, value := value * Pop]
out_tab_r <- dcast(out_tab_r,
                   iter + iso3 + variable + who_region~ type,
                   value.var = "value")


## averted
out_tab_r[, av := cf - sq]

save(out_tab_r, file= here("outputs/out_tab_r.RData"))


out_r_aggr <- out_tab_r[is.finite(av), .(
  cf = sum(cf), sq = sum(sq), av = sum(av)),
  by = .(iter, variable, who_region)]

## hi/lo & reshape
eps <- 0.025
out_r_aggr <- melt(out_r_aggr,
                   id = c("iter","who_region", "variable")
) # TODO better var names

out_r_aggr <- out_r_aggr[, .(
  mid = mean(value), lo = quantile(value, eps), hi = quantile(value, 1 - eps)
), by = .(who_region,variable, variable.1)]

out_r_aggr <- dcast(out_r_aggr,
                    who_region+variable ~ variable.1,
                    value.var = c("mid", "lo", "hi"))

## change units to millions for cost and health, BCG dosese
fac <- 1e6
out_r_aggr[
  variable %in% c("rslt_att_cost","rslt_attm_cost", "rslt_cost",
                  "rslt_bcg_cost", "rslt_bcg_doses", "rslt_health",
                  "rslt_ly_tb"),
  `:=`(
    mid_sq = mid_sq / fac, lo_sq = lo_sq / fac, hi_sq = hi_sq / fac,
    mid_cf = mid_cf / fac, lo_cf = lo_cf / fac, hi_cf = hi_cf / fac,
    mid_av = mid_av / fac, lo_av = lo_av / fac, hi_av = hi_av / fac
  )
]




## format numbers, add brackets
out_r_aggr[
  ,
  c("sq_txt", "cf_txt", "av_txt") := .(
    brkt(mid_sq, lo_sq, hi_sq),
    brkt(mid_cf, lo_cf, hi_cf),
    brkt(mid_av, lo_av, hi_av)
  )
]
out_r_aggr

fwrite(out_r_aggr, file = here("outputs/output_table_who.csv"))




source("R/create_outs4who_regi.R") # replicates the global analysis for who_regions

