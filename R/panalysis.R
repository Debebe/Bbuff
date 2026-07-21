## analysis building on costing and data preparation work

rm(list = ls())


## === packages
pacman::p_load(here,data.table, dplyr, tidyr, stringr, truncnorm,
               flextable, officer,kableExtra,ggplot2, ggrepel)

## === data
## read in pre-prepared data
gdp_inc_le_costs <- readRDS(file = here("outdata/gdp_inc_le_costs.rds"))
tbinc <- fread("indata/TB_burden_countries_2025-10-29.csv") %>%
  dplyr::filter(year==2023) %>%
  dplyr::select(iso3, year,inc_all=e_inc_100k,  e_inc_100k_lo, e_inc_100k_hi,
                notif_all=c_newinc_100k,
                c_cdr, c_cdr_lo, c_cdr_hi)%>%as.data.table()

load(here("indata/whokey.Rdata"))
load(here("indata/LEu5.Rdata")) # single year under five life expectancy
## === utility functions
source("R/utilities/utilities.R")


## === expand data for PSA
set.seed(1234)
Niter <- 10000 # TODO increase ultimately
samp <- as.data.table(gdp_inc_le_costs)
samp <- samp[cov_cat == "WUENIC"]
N <- nrow(samp)
samp <- samp[rep(seq_len(N), each = Niter)]
samp[, iter := rep(seq_len(Niter), N)]
samp[iter == 1][iso3 == "AFG"] #check

## === parameter values and samplers
source(here("R/utilities/parameters.R"))

## TODO sampling for parameters used = VE, prop_tbm, cost etc

samp <- samp %>%
  rowwise() %>%
  mutate(
    # vax efficacy
    bcg_haz_tb = 1 - sample_beta(
      mean=bcg_eff_tb.m,
      l=bcg_eff_tb.l,
      h=bcg_eff_tb.h
    ),

    bcg_haz_tbm = sample_gamma(
      mean=bcg_haz_tbm.m,
      l=bcg_haz_tbm.l,
      h=bcg_haz_tbm.h
    ),
    
    # tbm_prop
    prop_tbm = sample_beta(
      mean= prop_tbm.ave,
      l=prop_tbm.lo,
      h=prop_tbm.hi
    ),
    
    post_tb_mort_hz = sample_LN(
      post_tb_mort_hz.m, 
      post_tb_mort_hz.l, 
      post_tb_mort_hz.h),
    
    post_tbm_mort_hz = sample_LN(
      post_tbm_mort_hz.m, 
      post_tbm_mort_hz.l, 
      post_tbm_mort_hz.h),
    
    cfr_treat = sample_LN(
      cfr_treat_tb.m, 
      cfr_treat_tb.l, 
      cfr_treat_tb.h),
    
    cfr_utreat = sample_LN(
      cfr_utreat_tb.m, 
      cfr_utreat_tb.l, 
      cfr_utreat_tb.h),
    
    cfr_treat_tbm = sample_LN(
      cfr_treat_tbm.m, 
      cfr_treat_tbm.l, 
      cfr_treat_tbm.h),
    
    prop_sev_seq = sample_beta(
      mean=prop_sev_seq.m,
      l=prop_sev_seq.l,
      h=prop_sev_seq.h
    ),
    
    prop_mild_disab = sample_beta(
      mean=prop_mild_disab.m,
      l=prop_mild_disab.l,
      h=prop_mild_disab.h
    ),
    
    # utility= 1-dw
    tbm_hrqol_mil_seq = 1- sample_beta(
      mean=tbm_dis_wt_mild.m,
      l=tbm_dis_wt_mild.l,
      h=tbm_dis_wt_mild.h
    ),
    
    tbm_hrqol_mod_seq = 1- sample_beta(
      mean=tbm_dis_wt_mod.m,
      l=tbm_dis_wt_mod.l,
      h=tbm_dis_wt_mod.h
    ),
    
    tbm_hrqol_sev_seq = 1- sample_beta(
      mean=tbm_dis_wt_sev.m,
      l=tbm_dis_wt_sev.l,
      h=tbm_dis_wt_sev.h
    ),
    
    ##based on Tomney
    post_tb_hrqol_dur3= 1- sample_beta(mean=0.041, sd=0.054),
    post_tb_hrqol_post3= 1- sample_beta(mean=0.025, sd=0.033),
    
    # inc
    incbest = sample_truncn(incbest, inclo, inchi),
    # costs
    ucost_dstb.m = sample_gamma(
      mean = ucost_dstb.m,
      sd = ucost_dstb.sd
    ),
    ucost_tbm.m = sample_gamma(
      mean = ucost_tbm.m, 
      sd = ucost_tbm.sd),
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
  # generate propo with mild and moderate sequelae after sampling sev-sequelae 
  mutate(prop_mild_seq= prop_mild_disab *(1-prop_sev_seq),
         prop_mod_seq= (1-prop_mild_disab)*(1-prop_sev_seq))%>%
  
  ungroup() %>%
  as.data.table()

samp[,LE:=NULL]

if (!file.exists("tmpdata")) dir.create(here("tmpdata"))# To host bigdata

save(samp, file= here("tmpdata/samp.RData"))

load(here("tmpdata/samp.RData"))
load(here("indata/LEu5.Rdata")) # single year under five life expectancy

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
save(D, file = here("tmpdata/PSA.RData")) # 


