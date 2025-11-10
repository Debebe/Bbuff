
rm(list = ls())
pacman::p_load(here,data.table, dplyr, tidyr, stringr, truncnorm,
               flextable, officer,kableExtra,ggplot2, ggrepel, forcats)

source(here("R/utilities/modelfunctions.R"))

gen_outs <- function(TBMincl=0,posttbincl=0){
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
  Niter <- 10000 # TODO increase ultimately
  samp <- as.data.table(gdp_inc_le_costs)
  samp <- samp[cov_cat == "WUENIC"]
  N <- nrow(samp)
  samp <- samp[rep(seq_len(N), each = Niter)]
  samp[, iter := rep(seq_len(Niter), N)]
  samp[iter == 1][iso3 == "AFG"] #check
  
  ## === parameter values and samplers
  source(here("R/utilities/parameters.R"))
  
  samp[, LE:=NULL]
  ## TODO sampling for parameters used = VE, prop_tbm, cost etc
  
  samp <- samp %>%
    rowwise() %>%
    mutate(
      # vax efficacy
      bcg_haz_tb   = 1 - sample_beta(bcg_eff_tb.m,  bcg_eff_tb.lo,  bcg_eff_tb.hi),
      bcg_haz_tbm = sample_gamma(
        bcg_eff_tbm.m,
        bcg_eff_tbm.lo,
        bcg_eff_tbm.hi),
      #tbm_prop
      prop_tbm     = sample_beta(prop_tbm.ave, prop_tbm.lo, prop_tbm.hi),
      #inc
      incbest      = sample_truncn(incbest, inclo, inchi),
      # costs
      ucost_dstb.m = sample_gamma(mean = ucost_dstb.m, sd = ucost_dstb.sd),
      ucost_tbm.m  = sample_gamma(mean = ucost_tbm.m, sd = ucost_tbm.sd),
      uc_tot_vax_delv_ave = sample_gamma(mean = uc_tot_vax_delv_ave, lo = uc_tot_vax_delv_lo, hi= uc_tot_vax_delv_hi),
      uc_labor_ave = sample_gamma(mean = uc_labor_ave,lo = uc_labor_lo,hi = uc_labor_hi),
      uc_sc_ave    = sample_gamma(mean = uc_sc_ave, lo = uc_su_lo, hi = uc_sc_hi),
      uc_servd_ave = sample_gamma(mean = uc_servd_lo,lo = uc_servd_lo, hi = uc_servd_hi),
      uc_capital_ave = sample_gamma(mean = uc_capital_ave, lo = uc_capital_lo, hi = uc_capital_hi)
    ) %>%
    ungroup()%>%as.data.table()
  
  samp[, prop_tbm := ifelse(TBMincl== 0, 0, prop_tbm)]
  
  assign("samp", samp, envir = .GlobalEnv)
  
  if (posttbincl == 0) {
    post_tb_mort_hz <<- 1
    tbm_mort_hz <<- 1
  }
  
  load(here("data/LEu5.Rdata")) # single year under five life expectancy
  
  assign("LEu5", LEu5, envir = .GlobalEnv)
  #source(here("R/utilities/modelfunctions.R"))
  source(here("R/utilities/calculations.R")) # creates D
  
  ## TODO global and regional total outputs, e.g.:
  
  ## extract results, compute differences, reformat
  keep <- grep("rslt", names(D), value = TRUE)
  keep <- c("iso3", "iter", "Pop", unique(keep))

  CEA_cntry<- D %>%
    group_by(who_region, iso3) %>%
    summarise(ICER = mean(rslt_cost_sq - rslt_cost_cf)/mean(rslt_health_sq - rslt_health_cf),
              ENB30= mean(0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
                (rslt_cost_sq - rslt_cost_cf)), .groups = "drop")%>%filter(!is.na(ENB30))%>%as.data.table()

  CEA_r <- CEA_cntry%>%
    group_by(who_region,iso3="XX")%>%
    summarise(mid_ENB30= median(ENB30),
              mid_ICER= median(ICER),
              
              q25_ENB30= quantile(ENB30, 0.25),
              q75_ENB30= quantile(ENB30, 0.75),
              
              q25_ICER= quantile(ICER, 0.25),
              q75_ICER= quantile(ICER, 0.75), .groups = "drop")
  
  CEA_all <- CEA_cntry%>%
    group_by(who_region="Global", iso3="XX")%>%
    summarise(mid_ENB30= median(ENB30),
              mid_ICER= median(ICER),
              
              q25_ENB30= quantile(ENB30, 0.25),
              q75_ENB30= quantile(ENB30, 0.75),
              
              q25_ICER= quantile(ICER, 0.25),
              q75_ICER= quantile(ICER, 0.75), .groups = "drop")

    
  summary_tab <- CEA_cntry%>%
    #group_by(source)%>%
    summarise(ENB30_median= paste0(round(quantile(ENB30, 0.50),1),"(", "IQR:", 
                                     round(quantile(ENB30, 0.25),1), " to ", 
                                     round(quantile(ENB30, 0.75),1),")"), 
              ICER= paste0(round(quantile(ICER, 0.50),1),"(", "IQR:", 
                                   round(quantile(ICER, 0.25),1), " to ", 
                                   round(quantile(ICER, 0.75),1),")"),
              n_cntrs_all= length(unique(iso3)),
              n_cntrs_ce= length(unique(iso3[ENB30>0])),
              n_cntrs_ce_ICER= length(unique(iso3[ENB30>0])))%>%
    mutate(prop=round(100*n_cntrs_ce/n_cntrs_all, 1))
  
  CEA <-rbind(CEA_all,CEA_r)%>%as.data.table()

  return(list(summary_tab=summary_tab, CEA=CEA, CEA_cntry=CEA_cntry))
}

reslts_ntbm_nptb <- gen_outs(TBMincl=0, posttbincl = 0)
reslts_ntbm_ptb <- gen_outs(TBMincl=0, posttbincl = 1)
reslts_tbm_nptb <- gen_outs(TBMincl=1, posttbincl = 0)
reslts_tbm_ptb <- gen_outs(TBMincl=1, posttbincl = 1)

CEA_ntbm_nptb <- reslts_ntbm_nptb$CEA %>% mutate(model="PostTB(N),TBM(N)")
CEA_ntbm_ptb <- reslts_ntbm_ptb$CEA %>%mutate(model="PostTB(Y),TBM(N)")
CEA_tbm_nptb <- reslts_tbm_nptb$CEA %>%mutate(model="PostTB(N),TBM(Y)")
CEA_tbm_ptb <- reslts_tbm_ptb$CEA %>%mutate(model="PostTB(Y),TBM(Y)")

CEA_ntbm_nptb_sum <- reslts_ntbm_nptb$summary_tab %>% mutate(model="PostTB(N),TBM(N)")
CEA_ntbm_ptb_sum <- reslts_ntbm_ptb$summary_tab %>%mutate(model="PostTB(Y),TBM(N)")
CEA_tbm_nptb_sum <- reslts_tbm_nptb$summary_tab %>%mutate(model="PostTB(N),TBM(Y)")
CEA_tbm_ptb_sum <- reslts_tbm_ptb$summary_tab %>%mutate(model="PostTB(Y),TBM(Y)")

CEA_ntbm_nptb_cntr <- reslts_ntbm_nptb$CEA_cntry%>%mutate(model="PostTB(N),TBM(N)")
CEA_ntbm_ptb_cntr <- reslts_ntbm_ptb$CEA_cntry%>%mutate(model="PostTB(Y),TBM(N)")
CEA_tbm_nptb_cntr <- reslts_tbm_nptb$CEA_cntry%>%mutate(model="PostTB(N),TBM(Y)")
CEA_tbm_ptb_cntr <- reslts_tbm_ptb$CEA_cntry%>%mutate(model="PostTB(Y),TBM(Y)")



cost_eff <- bind_rows(CEA_ntbm_nptb_sum,CEA_ntbm_ptb_sum,
                     CEA_tbm_nptb_sum,CEA_tbm_ptb_sum)%>%
  select(model,n_cntrs_all, n_cntrs_ce,prop, ENB30_median, ICER )

fwrite(cost_eff, file = here("outputs/sens.csv"))
save(cost_eff, file = here("outputs/cost_eff.RData"))

CEA_sens <- bind_rows(CEA_tbm_ptb,CEA_ntbm_ptb, 
                     CEA_tbm_nptb,CEA_ntbm_nptb) %>%
  pivot_longer(cols = -c(who_region,model, iso3))%>%
  separate(name, into =c("measure", "metric"), sep = "_", remove = TRUE)%>%
  pivot_wider(id_cols = c("who_region", "model", "metric", "iso3"), names_from = "measure", values_from = "value")

save(CEA_sens, file = here("outputs/CEA_sens.RData"))

CEA_cntr <- bind_rows(CEA_ntbm_nptb_cntr,
                      CEA_ntbm_ptb_cntr,
                      CEA_tbm_nptb_cntr,
                      CEA_tbm_ptb_cntr)%>%
  inner_join(D%>%select(iso3, GDP)%>%
               distinct(iso3, .keep_all = TRUE), by="iso3")

# CEA_cntr <-inner_join(CEA_cntr,D%>%select(iso3, GDP)%>%
#              distinct(iso3, .keep_all = TRUE), by="iso3")

save(CEA_cntr, file = here("outputs/CEA_cntr.RData"))

sumary_sen <-CEA_cntr%>%
  group_by(Region="Global",model)%>%
  summarise(n_cntrs=length(unique(iso3)),
            n_CEicer=length(unique(iso3[ICER<GDP/3])),
            n_CEenb=length(unique(iso3[ENB30>0])),
            p_CEenb=100*round(mean(ENB30>0),3),
            p_CEicer=100*round(mean(ICER<GDP/3),3),
            icer= paste0(round(median(ICER), 1),"(IQR:", 
                         round(quantile(ICER, 0.25),1), " to ",
                         round(quantile(ICER, 0.75),1), ")"), .groups = "drop")%>%
  bind_rows(CEA_cntr%>%
              group_by(Region=who_region,model)%>%
              summarise(n_cntrs=length(unique(iso3)),
                        n_CEicer=length(unique(iso3[ICER<GDP/3])),
                        n_CEenb=length(unique(iso3[ENB30>0])),
                        p_CEenb=100*round(mean(ENB30>0),3),
                        p_CEicer=100*round(mean(ICER<GDP/3),3),
                        icer= paste0(round(median(ICER), 1),"(IQR:", 
                                     round(quantile(ICER, 0.25),1), " to ",
                                     round(quantile(ICER, 0.75),1), ")"), .groups = "drop"))

fwrite(sumary_sen, file = here("outputs/sumary_sen.csv"))




# check the following should be the same
xx1 <-CEA_cntr%>%filter(model=="PostTB(Y),TBM(Y)")
xx2 <-CEA%>%filter(threshold==0.3)
# 
summary(xx1$ICER[xx1$ICER>0])
summary(xx2$ICER)

# enb1 <- CEA_cntr%>%filter(model=="PostTB(Y),TBM(Y)", ENB30>0)
# enb2 <- CEA_cntr%>%filter(model=="PostTB(Y),TBM(N)", ENB30>0)
# enb0 <- CEAAs%>%filter(ENB30>0)
# 
# summary(enb1$ICER)
# summary(enb2$ICER)
# summary(enb0$ICER)
# 
summary(enb0$ENB30)
summary(enb1$ENB30)
summary(enb2$ENB30)

summary(CEAAs$ENB30)
