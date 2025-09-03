
rm(list = ls())

## === packages
library(here)
library(data.table)
library(ggplot2)
library(truncnorm)
library(dplyr)
library(flextable)
library(officer)
library(forcats)

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
      bcg_haz_tb   = 1 - sample_beta(bcg_eff_tb.m,  bcg_eff_tb.lo,  bcg_eff_tb.hi),
      bcg_haz_tbm  = 1 - sample_beta(bcg_eff_tbm.m, bcg_eff_tbm.lo, bcg_eff_tbm.hi),
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
    mid = mean(value), lo = quantile(value, eps), 
    hi = quantile(value, 1 - eps)
  ), by = .(variable, variable.1)]
  
  output_table <- dcast(output_table,
                        variable ~ variable.1,
                        value.var = c("mid", "lo", "hi")
  )
  
  CEA <- D%>%
    summarise(ENB30 = mean(0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
        (rslt_cost_sq - rslt_cost_cf), na.rm=TRUE),
    ICER = mean(rslt_cost_sq - rslt_cost_cf, na.rm=TRUE) /
      mean(rslt_health_sq - rslt_health_cf, na.rm=TRUE))
  
  

  output_table <-output_table[,.(variable,mid_sq, lo_sq, hi_sq, mid_cf, lo_cf, hi_cf)]
  return(list(outs=output_table, CEA=CEA))
}

reslts_notbm_nptb <- gen_outs(TBMincl=0, posttbincl = 0)
reslts_wztbm_ptb <- gen_outs(TBMincl=1, posttbincl = 1)
reslts_tbm_nptb <- gen_outs(TBMincl=1, posttbincl = 0)
reslts_ntbm_ptb <- gen_outs(TBMincl=0, posttbincl = 0)

outs_notbm_nptb <- reslts_notbm_nptb$outs
CEA_notbm_nptb <- reslts_notbm_nptb$CEA
outs_tbm_ptb <- reslts_wztbm_ptb$outs
CEA_tbm_ptb <- reslts_wztbm_ptb$CEA
outs_tbm_nptb <- reslts_tbm_nptb$outs
CEA_tbm_nptb <- reslts_tbm_nptb$CEA
outs_ntbm_ptb <- reslts_ntbm_ptb$outs
CEA_ntbm_ptb <- reslts_ntbm_ptb$CEA

outs_tbm_ptb$model  <- "Basecase"
outs_ntbm_ptb$model <- "Basecase - TBM"
outs_tbm_nptb$model <- "Basecase - PostTB"
outs_notbm_nptb$model <- "Basecase - both"

CEA_tbm_ptb$model  <- "Basecase"
CEA_ntbm_ptb$model <- "Basecase - TBM"
CEA_tbm_nptb$model <- "Basecase - PostTB"
CEA_notbm_nptb$model <- "Basecase - both"



all_outs <- bind_rows(outs_tbm_ptb,outs_ntbm_ptb, 
                 outs_tbm_nptb,outs_notbm_nptb) %>%
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
                             "Basecase", 
                             "Basecase - PostTB", 
                             "Basecase - TBM", 
                             "Basecase - both"))
all_CEA <- bind_rows(CEA_tbm_ptb,CEA_ntbm_ptb, 
                      CEA_tbm_nptb,CEA_notbm_nptb) %>%
  pivot_longer(cols = c("ENB30","ICER"))%>%
  rename(variable="name") %>%
  mutate(model = forcats::fct_relevel(model,
                                      "Basecase", 
                                      "Basecase - PostTB", 
                                      "Basecase - TBM", 
                                      "Basecase - both"))


pdiff <- ggplot(all, aes(model, mid_sq,col=model))+ geom_point()+
  geom_pointrange(aes(ymin = lo_sq, ymax = hi_sq)) + 
  facet_wrap(~variable, scales="free")+ylab("Estimates")+
  theme_bw()+
  theme(legend.position="bottom",legend.title =element_blank(),  
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())+
  ggtitle("Health and cost consequences of inclusion of TBM and postTB in BCG modelling")+
  theme(plot.title = element_text(size = 10))

 ggsave(pdiff,file = here("outputs/TBMpostTBeffect.png"), w = 6, h = 3.5)


 pcea <- ggplot(all_CEA, aes(model, value,col=model))+ geom_point()+
   facet_wrap(~variable, scales="free")+ylab("Estimates")+
   theme_bw()+
   theme(legend.position="bottom",legend.title =element_blank(),  
         axis.title.x = element_blank(), 
         axis.text.x = element_blank())+
   ggtitle("Impact of inclusion of TBM and postTB in BCG cost effectiveness modelling")+
   theme(plot.title = element_text(size = 10))
 
 ggsave(pcea,file = here("outputs/TBMpostTB_role_BCG_CE.png"), w = 5.5, h = 3.0)
 
 
 
# 
# percentage_change_df <- all %>%
#   rename(mid=mid_sq,lo=lo_sq,hi=hi_sq) %>%
#   pivot_wider(
#     names_from = model,
#     values_from = c(mid, lo, hi)
#   ) %>%
#   mutate(
#     mid_pct_change = 100*(`mid_sq_With TBM and PostTB` - `mid_sq_Without TBM and PostTB`) / `mid_sq_Without TBM and PostTB`,
#     lo_pct_change = 100*(`lo_sq_With TBM and PostTB` - `lo_sq_Without TBM and PostTB`) / `lo_sq_Without TBM and PostTB`,
#     hi_pct_change = 100*(`hi_sq_With TBM and PostTB` - `hi_sq_Without TBM and PostTB`) / `hi_sq_Without TBM and PostTB`
#   ) %>%
#   select(variable, mid_pct_change, lo_pct_change, hi_pct_change)
# 
# pchange <- ggplot(percentage_change_df, aes(x = variable, y = mid_pct_change)) +
#   geom_bar(stat = "identity", aes(fill = mid_pct_change > 0)) +
#   geom_text(aes(label = paste0(round(mid_pct_change, 2), "%")),
#             vjust = ifelse(percentage_change_df$mid_pct_change > 0, -0.5, 1.5),
#             color = "black", size=3) +
#   geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
#   scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), guide = "none") +
#   labs(
#     title = "Percentage change from inclusion of TBM and Post TB",
#     x = "Variable",
#     y = "Percentage Change (%)"
#   ) + xlab("")+
#   theme_minimal()+
#   theme(plot.title = element_text(size = 10))

#ggsave(pchange,file = here("outputs/TBMpostTBeffect.png"), w = 4.0, h = 4.5)


