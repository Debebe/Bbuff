
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
  Niter <- 5000 # TODO increase ultimately
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
  
  D[, prop_tbm := ifelse(TBMincl== 0, 0, prop_tbm)]
  
  assign("D", D, envir = .GlobalEnv)
  
  if (posttbincl == 0) {
    post_tb_mort_hz <<- 1
    tbm_mort_hz <<- 1
  }
  
  source(here("R/utilities/calculations.R"))
  
  ## TODO global and regional total outputs, e.g.:
  
  ## extract results, compute differences, reformat
  keep <- grep("rslt", names(D), value = TRUE)
  keep <- c("iso3", "iter", "Pop", unique(keep))

  CEA_cntry<- D %>%
    group_by(who_region, iso3) %>%
    summarise(ICER = mean(rslt_cost_sq - rslt_cost_cf)/mean(rslt_health_sq - rslt_health_cf),
              ENB30= mean(0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
                (rslt_cost_sq - rslt_cost_cf)))%>%filter(!is.na(ENB30))

  CEA_r <- CEA_cntry%>%
    group_by(who_region)%>%
    summarise(mean_ENB30= round(mean(ENB30),0),
              mean_ICER= round(mean(ICER),0),
              
              mid_ENB30= round(median(ENB30),0),
              mid_ICER= round(median(ICER),0),
              
              q25_ENB30= round(quantile(ENB30, 0.25),0),
              q75_ENB30= round(quantile(ENB30, 0.75),0),
              
              q25_ICER= round(quantile(ICER, 0.25),0),
              q75_ICER= round(quantile(ICER, 0.75),0), .groups = "drop")
  
  CEA_all <- CEA_cntry%>%
    group_by(who_region="Global")%>%
    summarise(mean_ENB30= round(mean(ENB30),0),
              mean_ICER= round(mean(ICER),0),
              
              mid_ENB30= round(median(ENB30),0),
              mid_ICER= round(median(ICER),0),
              
              q25_ENB30= round(quantile(ENB30, 0.25),0),
              q75_ENB30= round(quantile(ENB30, 0.75),0),
              
              q25_ICER= round(quantile(ICER, 0.25),0),
              q75_ICER= round(quantile(ICER, 0.75),0))

    
  summary_tab <- data.table(n_cntrs_all= length(unique(CEA_cntry$iso3)),
                        n_cntrs_ce= length(unique(CEA_cntry$iso3[CEA_cntry$ENB30>0])))%>%
    mutate(prop=round(100*n_cntrs_ce/n_cntrs_all, 1), 
           
           ENB30_median= paste0(round(quantile(CEA_cntry$ENB30, 0.50),1),"(", "IQR:", 
                         round(quantile(CEA_cntry$ENB30, 0.25),1), " to ", 
                         round(quantile(CEA_cntry$ENB30, 0.75),1),")"),
           ICER_median= paste0(round(quantile(CEA_cntry$ICER, 0.50),0),"(", "IQR:", 
                               round(quantile(CEA_cntry$ICER, 0.25),0), " to ", 
                               round(quantile(CEA_cntry$ICER, 0.75),0),")"),
           ENB30_mean= round(mean(CEA_cntry$ENB30),0),
           ICERmean= round(mean(CEA_cntry$ICER),0))
  
  CEA <-rbind(CEA_all,CEA_r)%>%as.data.table()

  return(list(summary_tab=summary_tab, CEA=CEA, CEA_cntry=CEA_cntry))
}

reslts_ntbm_nptb <- gen_outs(TBMincl=0, posttbincl = 0)
reslts_ntbm_ptb <- gen_outs(TBMincl=0, posttbincl = 1)
reslts_tbm_nptb <- gen_outs(TBMincl=1, posttbincl = 0)
reslts_tbm_ptb <- gen_outs(TBMincl=1, posttbincl = 1)

CEA_ntbm_nptb <- reslts_ntbm_nptb$CEA
CEA_tbm_ptb <- reslts_tbm_ptb$CEA
CEA_tbm_nptb <- reslts_tbm_nptb$CEA
CEA_ntbm_ptb <- reslts_ntbm_ptb$CEA


CEA_ntbm_nptb$model <-"PostTB(N),TBM(N)"
CEA_ntbm_ptb$model <- "PostTB(Y),TBM(N)"
CEA_tbm_nptb$model <- "PostTB(N),TBM(Y)"
CEA_tbm_ptb$model  <- "PostTB(Y),TBM(Y)"

CEA_ntbm_nptb_sum <- reslts_ntbm_nptb$summary_tab
CEA_ntbm_ptb_sum <- reslts_ntbm_ptb$summary_tab
CEA_tbm_nptb_sum <- reslts_tbm_nptb$summary_tab
CEA_tbm_ptb_sum <- reslts_tbm_ptb$summary_tab


CEA_ntbm_nptb_sum$model <-"PostTB(N),TBM(N)"
CEA_ntbm_ptb_sum$model <- "PostTB(Y),TBM(N)"
CEA_tbm_nptb_sum$model <- "PostTB(N),TBM(Y)"
CEA_tbm_ptb_sum$model  <- "PostTB(Y),TBM(Y)"

cost_eff <- bind_rows(CEA_ntbm_nptb_sum,CEA_ntbm_ptb_sum,
                     CEA_tbm_nptb_sum,CEA_tbm_ptb_sum)
fwrite(cost_eff, file = here("outputs/sens.csv"))
save(cost_eff, file = here("outputs/cost_eff.RData"))

CEA_sens <- bind_rows(CEA_tbm_ptb,CEA_ntbm_ptb, 
                     CEA_tbm_nptb,CEA_ntbm_nptb)%>%
  pivot_longer(cols = -c(who_region,model))%>%
  separate(name, into =c("measure", "metric"), sep = "_", remove = TRUE)%>%
  pivot_wider(id_cols = c("who_region", "model", "metric"), names_from = "measure", values_from = "value")

save(CEA_sens, file = here("outputs/CEA_sens.RData"))

##
# load(file=here("outputs/CEA_sens.RData"))
# load(file=here("outputs/cost_eff.RData"))
# cost_eff <- fread(here("outputs/sens.csv"))


# bar_mean <- ggplot(CEA_sens %>% filter(who_region == "Global"),
#       # aes(x = reorder(model, mean), y = mean, fill = model)) +
#        aes(x =model, y = mean, fill = model)) +
#   geom_col() +
#   facet_wrap(~variable, scales = "free") +
#   ylab("Estimates (mean)") +
#   theme_bw() +
#   theme(
#     #legend.position = "bottom",
#     legend.position = "none",
#     legend.title = element_blank(),
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     plot.title = element_text(size = 10)
#   )
# 
# bar_mid <- ggplot(CEA_sens %>% filter(who_region == "Global"),
#                    aes(x = model, y = mid, fill = model)) +
#   geom_col() +
#   facet_wrap(~variable, scales = "free") +
#   ylab("Estimates (median)") +
#   theme_bw() +
#   theme(
#     legend.position = "bottom",
#     legend.key.size = unit(0.5, "lines"),
#     legend.title = element_blank(),
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     plot.title = element_text(size = 10)
#   )
# 
# pp <- bar_mean/bar_mid
# 
# ggsave(pp,file = here("outputs/f_sensitivity.png"), w = 6, h = 3.5)
# 
# 
# bar_mid <- ggplot(CEA_sens %>% filter(!who_region =="Global", iso3=="XX"),
#                   aes(x = model, y = mid, fill = model)) +
#   geom_col() +
#   facet_wrap(who_region~variable, scales = "free", 
#              labeller = labeller(.multi_line = FALSE)) +
#   ylab("Estimates (median)") +
#   theme_linedraw() +
#   theme(
#     strip.text = element_text(size = 7),
#     legend.key.size = unit(0.5, "lines"),
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     plot.title = element_text(size = 8)
#   )
# 
# ggsave(bar_mid,file = here("outputs/f_sensitivity_r.png"), w = 6.5, h = 3.5)
# 

          