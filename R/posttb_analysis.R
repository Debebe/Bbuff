
rm(list = ls())

## === packages
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
  Niter <- 3000 # TODO increase ultimately
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
      bcg_haz_tbm = sample_gamma(bcg_eff_tbm.m,bcg_eff_tbm.lo,bcg_eff_tbm.hi),
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
  keep <- c("who_region","iso3", "iter", "Pop", unique(keep))
  
  output_table <- D[, ..keep]
  output_table <- melt(output_table, id = c("who_region","iter", "iso3", "Pop"))
  output_table[, type := ifelse(grepl("cf", variable), "cf", "sq")]
  output_table[, variable := gsub("rlst_", "", variable)]
  output_table[, variable := gsub("_cf|_sq", "", variable)]
  output_table[, value := value * Pop]
  
  output_table <- dcast(output_table,
                        iter + who_region + iso3 + variable ~ type,
                        value.var = "value")
  
  ## averted
  output_table[, av := cf - sq]
  
  ## global TODO NaNs?
  out_glob <- output_table[is.finite(av), .(
    cf = sum(cf), sq = sum(sq), av = sum(av)
  ), by = .(iter, variable)][,who_region:="Global"]
  
  ## hi/lo & reshape
  eps <- 0.025
  out_glob <- melt(out_glob,id = c("iter", "variable", "who_region")) 
  
  out_glob <- out_glob[, .(
    mid = mean(value), lo = quantile(value, eps), 
    hi = quantile(value, 1 - eps)
  ), by = .(who_region,variable, variable.1)]
  
  out_glob <- dcast(out_glob,
                    who_region+variable ~ variable.1,
                        value.var = c("mid", "lo", "hi"))
  
  #========Regional===========
  
  out_region <- output_table[is.finite(av), .(
    cf = sum(cf), sq = sum(sq), av = sum(av)
  ), by = .(who_region,iter, variable)]
  
  eps <- 0.025
  out_region <- melt(out_region,id = c("iter", "variable", "who_region")) # TODO better var names
  
  out_region <- out_region[, .(
    mid = mean(value), lo = quantile(value, eps), 
    hi = quantile(value, 1 - eps)
  ), by = .(who_region,variable, variable.1)]
  
  out_region <- dcast(out_region,
                      who_region+variable ~ variable.1,
                   value.var = c("mid", "lo", "hi"))
  
  # cost-effectiveness

  CEA <- D%>%
    group_by(who_region, iso3)|>
    summarise(ENB30 = mean(0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
                             (rslt_cost_sq - rslt_cost_cf)>0, na.rm=TRUE),
              ICER = mean(rslt_cost_sq - rslt_cost_cf, na.rm=TRUE) /
                mean(rslt_health_sq - rslt_health_cf, na.rm=TRUE))%>%
    filter(!is.na(ENB30))
  
  
  CEAd <- D%>%
    mutate(dd= 0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
             (rslt_cost_sq - rslt_cost_cf))%>%
    select(iso3,who_region,iter, dd)
    
  
  
  ENB_mean <- D%>%
    group_by(who_region, iso3)|>
    summarise(ENB30 = mean(0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
                             (rslt_cost_sq - rslt_cost_cf), na.rm=TRUE))%>%
    filter(!is.na(ENB30))%>%filter(ENB30>0)
  
  ENB_q25 <- D%>%
    #group_by(who_region, iso3)|>
    summarise(ENB30q25 = quantile(0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
                             (rslt_cost_sq - rslt_cost_cf), 0.25,na.rm=TRUE))
  
  
  CEA_g <- D%>%
    group_by(who_region="Global", iso3="NA")|>
    summarise(ENB30 = mean(0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
                             (rslt_cost_sq - rslt_cost_cf), na.rm=TRUE),
              ICER = mean(rslt_cost_sq - rslt_cost_cf, na.rm=TRUE) /
                mean(rslt_health_sq - rslt_health_cf, na.rm=TRUE)) %>%
    filter(!is.na(ENB30))
  
  

   n_cntrs <- length(CEA$iso3)
   CE <- CEA$ENB30[CEA$ENB30>0] # use ENB>0
   n_cntrs_ce <- length(CE)
  
  CEA_summary <- data.table(cntrs = n_cntrs, 
                            cntrs_ce=n_cntrs_ce,
                            ENB30=paste0(round(median(CE),1), "(", 
                                         round(quantile(CE, 0.25),1), " to ", 
                                         round(quantile(CE, 0.75),1),")"),
                            ICER= paste0(round(median(CEA$ICER),1), "(", 
                                         round(quantile(CEA$ICER, 0.25),1), " to ", 
                                         round(quantile(CEA$ICER, 0.75),1),")"))
  
  out_table <- rbind(out_glob, out_region)%>%as.data.table()
  CEA <-bind_rows(CEA_g,CEA, CEAs)%>%as.data.table()
  
  out_table <-out_table[,.(who_region,variable,mid_sq, lo_sq, hi_sq, mid_cf, lo_cf, hi_cf)]
  return(list(outs=out_table, CEA=CEA, CEA_summary=CEA_summary))
}

reslts_ntbm_nptb <- gen_outs(TBMincl=0, posttbincl = 0)
reslts_ntbm_ptb <- gen_outs(TBMincl=0, posttbincl = 1)
reslts_tbm_nptb <- gen_outs(TBMincl=1, posttbincl = 0)
reslts_tbm_ptb <- gen_outs(TBMincl=1, posttbincl = 1)

outs_ntbm_nptb <- reslts_ntbm_nptb$outs
CEA_ntbm_nptb <- reslts_ntbm_nptb$CEA
CEA_ntbm_nptb_sum <- reslts_ntbm_nptb$CEA_summary

outs_ntbm_ptb <- reslts_ntbm_ptb$outs
CEA_ntbm_ptb <- reslts_ntbm_ptb$CEA
CEA_ntbm_ptb_sum <- reslts_ntbm_ptb$CEA_summary

outs_tbm_nptb <- reslts_tbm_nptb$outs
CEA_tbm_nptb <- reslts_tbm_nptb$CEA
CEA_tbm_nptb_sum <- reslts_tbm_nptb$CEA_summary

outs_tbm_ptb <- reslts_tbm_ptb$outs
CEA_tbm_ptb <- reslts_tbm_ptb$CEA
CEA_tbm_ptb_sum <- reslts_tbm_ptb$CEA_summary


outs_ntbm_nptb$model <-"PostTB(N),TBM(N)"
outs_ntbm_ptb$model <- "PostTB(Y),TBM(N)"
outs_tbm_nptb$model <- "PostTB(N),TBM(Y)"
outs_tbm_ptb$model  <- "PostTB(Y),TBM(Y)"

CEA_ntbm_nptb$model <-"PostTB(N),TBM(N)"
CEA_ntbm_ptb$model <- "PostTB(Y),TBM(N)"
CEA_tbm_nptb$model <- "PostTB(N),TBM(Y)"
CEA_tbm_ptb$model  <- "PostTB(Y),TBM(Y)"

CEA_ntbm_nptb_sum$model <- "PostTB(N),TBM(N)"
CEA_ntbm_ptb_sum$model <- "PostTB(Y),TBM(N)"
CEA_tbm_nptb_sum$model <- "PostTB(N),TBM(Y)"
CEA_tbm_ptb_sum$model <- "PostTB(Y),TBM(Y)"



all_outs <- bind_rows(outs_tbm_ptb,outs_ntbm_ptb, 
                 outs_tbm_nptb,outs_ntbm_nptb) %>%
  filter(variable%in% c("rslt_att","rslt_cost","rslt_health","rslt_inc","rslt_ly_tb",  
                        "rslt_tb_deaths")) %>%
  mutate(variable = recode(variable,
                           rslt_att = "ATT",
                           rslt_cost = "Cost",
                           rslt_health = "Health",
                           rslt_inc = "Incidence",
                           rslt_ly_tb = "LY",
                           rslt_tb_deaths = "TB deaths")) %>%
  filter(variable%in% c("Cost","Health","LY","TB deaths"))%>%
  mutate(model = forcats::fct_relevel(model,
                                      "PostTB(N),TBM(N)", 
                                      "PostTB(Y),TBM(N)", 
                                      "PostTB(N),TBM(Y)", 
                                      "PostTB(Y),TBM(Y)"))
all_CEA <- bind_rows(CEA_tbm_ptb,CEA_ntbm_ptb, 
                      CEA_tbm_nptb,CEA_ntbm_nptb) %>%
  pivot_longer(cols = c("ENB30","ICER"))%>%
  rename(variable="name") %>%
  mutate(model = forcats::fct_relevel(model,
                                      "PostTB(N),TBM(N)", 
                                      "PostTB(Y),TBM(N)", 
                                      "PostTB(N),TBM(Y)", 
                                      "PostTB(Y),TBM(Y)"))


pdiff <- ggplot(all_outs%>%filter(who_region=="Global"), aes(model, mid_sq,col=model))+ geom_point()+
  geom_pointrange(aes(ymin = lo_sq, ymax = hi_sq)) + 
  facet_wrap(~variable, scales="free")+ylab("Estimates")+
  theme_bw()+
  theme(legend.position="bottom",legend.title =element_blank(),  
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())+
  ggtitle("Health and cost consequences of inclusion of TBM and postTB in BCG modelling")+
  theme(plot.title = element_text(size = 10))

 ggsave(pdiff,file = here("outputs/TBMpostTBeffect.png"), w = 6, h = 3.5)


 pcea <- ggplot(all_CEA%>%filter(who_region=="Global"), aes(model, value,col=model))+ geom_point()+
   facet_wrap(~variable, scales="free")+ylab("Estimates")+
   theme_bw()+
   theme(legend.position="bottom",legend.title =element_blank(),  
         axis.title.x = element_blank(), 
         axis.text.x = element_blank())+
   ggtitle("Impact of inclusion of TBM and postTB in BCG cost effectiveness modelling")+
   theme(plot.title = element_text(size = 10))
 
 ggsave(pcea,file = here("outputs/TBMpostTB_role_BCG_CE.png"), w = 6, h = 3.0)
 
 
 
 all_sum <- bind_rows(CEA_ntbm_nptb_sum,CEA_ntbm_ptb_sum, 
                      CEA_tbm_nptb_sum,CEA_tbm_ptb_sum) %>%
   pivot_longer(cols = c("ENB30","ICER")) %>%
   rename(variable="name") %>%
   mutate(model = forcats::fct_relevel(model,
                                       "PostTB(N),TBM(N)", 
                                       "PostTB(Y),TBM(N)", 
                                       "PostTB(N),TBM(Y)", 
                                       "PostTB(Y),TBM(Y)"))
 CEA_ntbm_nptb_sum$model <- "PostTB(N),TBM(N)"
 CEA_ntbm_ptb_sum$model <- "PostTB(Y),TBM(N)"
 CEA_tbm_nptb_sum$model <- "PostTB(N),TBM(Y)"
 CEA_tbm_ptb_sum$model <- "PostTB(Y),TBM(Y)"
