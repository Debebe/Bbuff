rm(list = ls())
library(data.table)
library(ggrepel)
library(dplyr)

### analyse BCG data
#bcg <- readRDS("~/Documents/BCG/data/bcg.Rds")|>as.data.table()
bcg <- readRDS(here("indata/bcg.Rds"))|>as.data.table()

bcgt <- bcg[, .(CODE, NAME, YEAR,COVERAGE_CATEGORY, TARGET_NUMBER, DOSES)]
cov_wuenic <- bcg[COVERAGE_CATEGORY=="WUENIC", .(CODE, YEAR, COVERAGE)]

bcg_official <- bcgt[COVERAGE_CATEGORY=="ADMIN" & !is.na(TARGET_NUMBER)]
comb <-dplyr::inner_join(bcg_official, cov_wuenic, by= c("CODE", "YEAR"))|>as.data.table()

comb[, demand:=TARGET_NUMBER*COVERAGE/100]

avail_dt <- readRDS(here("outdata/gdp_inc_le_costs.rds"))|>
  filter(cov_cat=="WUENIC",best>0, !is.na(cdr))|>
  dplyr::select(iso3)


comb_avail <-comb|>filter(CODE%in%avail_dt$iso3)|>as.data.table()


df<-comb_avail|>
  group_by(iso3=CODE,NAME)|>
  summarise(M= mean(demand),
            std= sd(demand))|>
  mutate(CV=std/M)|>as.data.table()




df %>%
  arrange(CV) %>%
  mutate(iso3 = factor(iso3, levels = iso3)) %>%
  ggplot(aes(x = iso3, y = CV)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Coefficient of variation",
    title = "Variation in vaccine demand by country"
  ) +
  theme_minimal()


ggplot(df, aes(M, CV)) +
  geom_point(size = 2) +
  labs(
    x = "Mean demand",
    y = "Coefficient of variation"
  ) +
  theme_minimal()


ggplot(df, aes(CV)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue") +
  labs(
    x = "Coefficient of variation",
    y = "Number of countries"
  ) +
  theme_minimal()




ggplot(df, aes(M, CV, label = iso3)) +
  geom_point(size = 2) +
  geom_text_repel(size = 3, max.overlaps = Inf) +
  scale_x_log10() +
  labs(
    x = "Mean annual demand (log scale)",
    y = "Coefficient of variation"
  ) +
  theme_minimal()

rm(list = ls())
##======Latitude effect on BCG efficacy======

### latitude
#p=palmer(0.41)

# if lat_pos <- 90-latitude
# if lat negative-->90+latitude

#VE_martinez <- ((theta/90)*p + (1-theta/90))*VE_polar
## 
# VE_country=((1-abs(LAT)/90)*p + abs(LAT)/90 )*VE_polar
# 
# VE_polar = VE_leo / F
# F = sum(w * ((1-abs(LAT)/90)*p + abs(LAT)/90)) #average lattitude effect in participants in Leos paper
# #w = proportions of participants at each LAT
# lat_mid <- c(5, 15, 25, 35, 65)
# VE_polar <- 0.63/F

df <- data.table::data.table(
  # from martinez
  lat_mid= c(5, 15, 25, 35, 65),
  wt=0.01*c(6.3,69.8,20.4,3.3, 0.2), 
  palmer_value= 0.41)

df[, lat_effect:=wt*((1-abs(lat_mid)/90)*palmer_value + abs(lat_mid)/90)]

## average latitude effect
F <-sum(df$lat_effect)





### let's work with actual data

load("~/Documents/GitHub/Bbuff/indata/LAT.Rdata")

setDT(LAT)
## create population weight in the lat region
# LAT[, wt := fcase(
#   abs(LAT) >= 0  & abs(LAT) < 10, 0.063,
#   abs(LAT) >= 10 & abs(LAT) < 20, 0.698,
#   abs(LAT) >= 20 & abs(LAT) < 30, 0.204,
#   abs(LAT) >= 30 & abs(LAT) < 40, 0.033,
#   abs(LAT) >= 40,                  0.022
# )]


## parameters
palmer_value <- 0.41
## BCG vaccine efficacy from Leo Martinez
VE_pooled_m <- 0.63
VE_pooled_l <- 0.49
VE_pooled_h <- 0.81

## calculate latitude effect
#LAT[, lat_effect:=wt*((1-abs(LAT)/90)*palmer_value + abs(LAT)/90)]

## average latitude effect
F <-sum(df$lat_effect)



VE_polar_m <- VE_pooled_m/F
VE_polar_l <- VE_pooled_l/F
VE_polar_h <- VE_pooled_h/F

## VE in a given country as a function of latitude and palmer constant
LAT[,VE_m:= ((1-abs(LAT)/90)*palmer_value + abs(LAT)/90 )*VE_polar_m]
LAT[,VE_l:= ((1-abs(LAT)/90)*palmer_value + abs(LAT)/90 )*VE_polar_l]
LAT[,VE_h:= ((1-abs(LAT)/90)*palmer_value + abs(LAT)/90 )*VE_polar_h]



## check - VE all countries

ggplot(LAT, aes(x = reorder(iso3, LAT), y = VE_m)) +
  geom_errorbar(aes(ymin = VE_l, ymax = VE_h), width = 0.2) +
  geom_point() +
  labs(
    x = "Country",
    y = "Vaccine efficacy"
  ) + geom_hline(yintercept = 1, colour = "blue", linetype = "dashed")+
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 7, angle = 90, hjust = 1)
  )

ggplot(LAT[iso3%in% unique(CEA$iso3),], aes(x = reorder(iso3, LAT), y = VE_m)) +
  geom_errorbar(aes(ymin = VE_l, ymax = VE_h), width = 0.2) +
  geom_point() +
  labs(
    x = "Country",
    y = "Vaccine effectiveness"
  ) + geom_hline(yintercept = 1, colour = "blue", linetype = "dashed")+
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 7, angle = 90, hjust = 1)
  )

## using latitude
ggplot(
  LAT[iso3 %in% unique(avail_dt$iso3)],
  aes(x = as.factor(round(LAT, 1)), y = VE_m)
) +
  geom_errorbar(
    aes(ymin = VE_l, ymax = VE_h),
    width = 0
  ) +
  geom_point() +
  geom_hline(
    yintercept = 1,
    colour = "blue",
    linetype = "dashed"
  ) + 
  scale_x_discrete(
    labels = function(x) paste0(x, "°") #display degree on Latitude
  ) +
  labs(
    x = "Latitude",
    y = "Vaccine effectiveness"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90)
  )




ggplot(
  LAT[iso3 %in% unique(avail_dt$iso3)],
  aes(x = as.factor(as.integer(LAT)), y = VE_m)
) +
  geom_errorbar(
    aes(ymin = VE_l, ymax = VE_h),
    width = 0
  ) +
  geom_point() +
  geom_hline(
    yintercept = 1,
    colour = "blue",
    linetype = "dashed"
  ) +
  labs(
    x = "Latitude",
    y = "Vaccine effectiveness"
  ) +
  theme_minimal()+theme(axis.text.x = element_text(angle = 90))


## sensitivity analysis with VE varried by altitude



## if Ve>1, penalise and make it one 
lat_dt <-LAT[, `:=`(
  VEc_m = pmin(VE_m, 1),
  VEc_l = pmin(VE_l, 1),
  VEc_h = pmin(VE_h, 1)
)][, .(iso3,LAT,VEc_m, VEc_l,VEc_h)]


#rm(list = ls())


## === packages
pacman::p_load(here,data.table, dplyr, tidyr, stringr, truncnorm,
               flextable, officer,kableExtra,ggplot2, ggrepel)

##======================data===================================
## read in pre-prepared data
gdp_inc_le_costs <- readRDS(file = here("outdata/gdp_inc_le_costs.rds"))

tbinc <- fread("indata/TB_burden_countries_2025-10-29.csv") %>%
  dplyr::filter(year==2023) %>%
  dplyr::select(iso3, year,inc_all=e_inc_100k,
                e_inc_100k_lo, e_inc_100k_hi,
                notif_all=c_newinc_100k,
                c_cdr, c_cdr_lo, c_cdr_hi)%>%
  as.data.table()

load(here("indata/whokey.Rdata"))
load(here("indata/LEu5.Rdata")) # single year under five life expectancy
## === utility functions
source("R/utilities/utilities.R")


## === expand data for PSA
set.seed(1234)
Niter <- 10000 # TODO increase ultimately
samp <- as.data.table(gdp_inc_le_costs)
samp <-samp[best>0 & !is.na(notif),]
samp <- samp[cov_cat == "WUENIC"]
N <- nrow(samp)
samp <- samp[rep(seq_len(N), each = Niter)]
samp[, iter := rep(seq_len(Niter), N)]
samp[iter == 1][iso3 == "AFG"] #check

setdiff(lat_dt$iso3, samp$iso3)
setdiff(samp$iso3,lat_dt$iso3)
## === parameter values and samplers
source(here("R/utilities/parameters.R"))

## TODO sampling for parameters used = VE, prop_tbm, cost etc

samp <- inner_join(samp,lat_dt, by="iso3") # 3 counties in the original data not in Lat dataset

samp <- samp %>%
  rowwise() %>%
  mutate(
    # vax efficacy
    bcg_haz_tb = 1 - sample_beta(
      mean=VEc_m,
      l=VEc_l,
      h=VEc_h
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
    
    
    # prop_sev_seq = sample_beta(
    #   mean=prop_sev_seq.m,
    #   l=prop_sev_seq.l,
    #   h=prop_sev_seq.h
    # ),
    
    prop_sev_seq= rbeta(n=1, shape1 = 4, shape2 = 8),
    
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
    
    # scaled beta so that cfr TbBM is >0.95 always
    #cfr_untreat_tbm= sample_scaled_beta(l=0.95, h = 1),
    cfr_untreat_tbm= rbeta(n = 1, shape1 = 998, shape2 = 2),
    
    
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

#if (!file.exists("tmpdata")) dir.create(here("tmpdata"))# To host bigdata

save(samp, file= here("tmpdata/wzLAT/samp.RData"))

load(here("tmpdata/wzLAT/samp.RData"))
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
save(D, file = here("tmpdata/wzLAT/PSA.RData"))


load(here("tmpdata/wzLAT/PSA.RData"))


## load

CEA <- D[, .(
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
by = .(iso3, region=who_region)
]

thresholds <- c(0.3, 0.5, 1.0)  

CEA <- CEA%>%
  crossing(threshold = thresholds)%>%
  mutate(legend_label = paste0(threshold," GDP"))%>% 
  mutate(legend_label= factor(legend_label,
                              levels = c("0.3 GDP", "0.5 GDP", "1 GDP")))|>
  dplyr::mutate(ICER_val = GDP * threshold,
                ICER_Label= ifelse(ICER < 0.3 * GDP,"ICER < 0.3 GDP", "ICER >= 0.3 GDP" )) |> as.data.table()


CEA$iso3 <- factor(CEA$iso3, levels = unique(CEA[order(ICER)]$iso3), ordered = TRUE)
all_labels <- c("ICER < 0.3 GDP","ICER >= 0.3 GDP", 
                "0.3 GDP", "0.5 GDP","1 GDP")
all_shapes <- c(19, 1, 3, 3, 3) # shape types
all_colors <- c("black", "black", 2, 4, 5) # colors

ggplot(CEA[ICER > 0 ], aes(iso3, ICER)) +
  geom_point(aes(shape = ICER_Label, col = ICER_Label), 
             size = 1.5) +
  geom_point(aes(y = ICER_val,shape = legend_label,
                 col =legend_label),size = 1.5) +
  scale_shape_manual(name = "Threshold",
                     values = setNames(all_shapes, all_labels)) +
  scale_color_manual(name = "Threshold",
                     values = setNames(all_colors, all_labels)) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  facet_wrap(~region, scales = "free") +
  theme_linedraw()+
  theme(legend.position = "top",
        legend.box.spacing = unit(0, "pt"),   # no gap between legend and plot
        legend.margin = margin(0, 0, 0, 0),   # no internal padding in legend
        plot.margin = margin(0, 5, 5, 5))+
  xlab("Country ISO3 code") +
  ylab("Incremental cost-effectiveness ratio (USD/DALY)")


#cea <- na.omit(CEA)
ggsave(file = here("plots/FS19_lat_effect.png"), w = 9, h = 8)



## PSA inputs and outputs

# load(here("tmpdata/PSA.RData"))     # full PSA data with results. 
# gdp_inc_le_costs <- readRDS(here("outdata/gdp_inc_le_costs.rds"))
# load(here("indata/whokey.Rdata"))     # region and cntry iso codes
# 
# CEA <- D%>%
#   dplyr::select(who_region, iso3,iter,
#                 ## epi_inputs
#                 incbest, notif, cdr,bcg_coverage,
#                 ## cost inputs
#                 GDP,
#                 uc_tot_vax_delv_ave, ucost_proc_bcg,
#                 ucost_dstb.m, ucost_dstb.sd,
#                 ucost_tbm.m,ucost_tbm.sd, 
#                 
#                 ## other epi and consequence inputs
#                 
#                 bcg_haz_tb,bcg_haz_tbm,
#                 # post tb
#                 prop_tbm,       
#                 post_tb_mort_hz, post_tbm_mort_hz, 
#                 
#                 cfr_treat, cfr_utreat,cfr_treat_tbm, prop_sev_seq,      
#                 tbm_hrqol_mil_seq,tbm_hrqol_mod_seq,tbm_hrqol_sev_seq, 
#                 prop_mild_seq, prop_mod_seq,
#                 
#                 ## cost and health outcomes
#                 
#                 rslt_health_sq,rslt_health_cf,
#                 rslt_cost_sq,rslt_cost_cf )|>na.omit()|>as.data.table()
# 
# 
# save(CEA, file = here("tmpdata/PSAreduct.RData")) 
# 


