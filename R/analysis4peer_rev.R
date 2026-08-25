
library(data.table)
library(ggrepel)
library(dplyr)


bcg <- readRDS("~/Documents/BCG/data/bcg.Rds")|>as.data.table()

bcgt <- bcg[, .(CODE, NAME, YEAR,COVERAGE_CATEGORY, TARGET_NUMBER, DOSES)]
cov_wuenic <- bcg[COVERAGE_CATEGORY=="WUENIC", .(CODE, YEAR, COVERAGE)]

bcg_official <- bcgt[COVERAGE_CATEGORY=="ADMIN" & !is.na(TARGET_NUMBER)]
comb <-dplyr::inner_join(bcg_official, cov_wuenic, by= c("CODE", "YEAR"))

comb[, demand:=TARGET_NUMBER*COVERAGE/100]

avail_dt <- readRDS("~/Documents/GitHub/Bbuff/outdata/gdp_inc_le_costs.rds")|>
  filter(cov_cat=="WUENIC", !is.na(cdr), )|>
  select(iso3)

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




### PSA inputs and outputs

load(here("tmpdata/PSA.RData"))     # full PSA data with results. 
gdp_inc_le_costs <- readRDS(here("outdata/gdp_inc_le_costs.rds"))
load(here("indata/whokey.Rdata"))     # region and cntry iso codes

CEA <- D%>%
  select(who_region, iso3,iter,
         ## epi_inputs
         incbest, notif, cdr,bcg_coverage,
         ## cost inputs
         GDP,
         uc_tot_vax_delv_ave, ucost_proc_bcg,
         ucost_dstb.m, ucost_dstb.sd,
         ucost_tbm.m,ucost_tbm.sd, 
         
         ## other epi and consequence inputs
         
         bcg_haz_tb,bcg_haz_tbm,
         # post tb
         prop_tbm,       
         post_tb_mort_hz, post_tbm_mort_hz, 
         
         cfr_treat, cfr_utreat,cfr_treat_tbm, prop_sev_seq,      
         tbm_hrqol_mil_seq,tbm_hrqol_mod_seq,tbm_hrqol_sev_seq, 
         prop_mild_seq, prop_mod_seq,

         ## cost and health outcomes
         
         rslt_health_sq,rslt_health_cf,
         rslt_cost_sq,rslt_cost_cf )|>na.omit()|>as.data.table()
         
         
save(CEA, file = here("tmpdata/PSAreduct.RData")) 



# CEAa <- CEA[, 
#   net_benefit:= 0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
#       (rslt_cost_sq - rslt_cost_cf)
# ]


### latitude
#p=palmer(0.41)

# if lat_pos <- 90-latitude
# if lat negative-->90+latitude

VE_martinez <- ((theta/90)*p + (1-theta/90))*VE_polar
## 
VE_country=((1-abs(LAT)/90)*p + abs(LAT)/90 )*VE_polar

VE_polar = VE_leo / F
F = sum(w * ((1-abs(LAT)/90)*p + abs(LAT)/90)) #average lattitude effect in participants in Leos paper
#w = proportions of participants at each LAT
lat_mid <- c(5, 15, 25, 35, 65)
VE_polar <- 0.63/F

df <- data.table::data.table(
  # from martinez
  #lat_mid= c(5, 15, 25, 35, 65),
  lat_mid= c(10, 20, 30, 40, 90),
  wt=0.01*c(6.3,69.8,20.4,3.3, 0.2), 
  p= 0.41)

df[, lat_effect:=wt*((1-abs(lat_mid)/90)*p + abs(lat_mid)/90)]

VE_pooled_m <- 0.63
VE_pooled_l <- 0.49
VE_pooled_h <- 0.81

F <-sum(df$lat_effect)
VE_polar_m <- VE_pooled_m/(1-F)
VE_polar_l <- VE_pooled_l/F
VE_polar_h <- VE_pooled_h/F

# logit <- function(x) metafor::transf.logit(x)
# xx <- logit(rnorm(10, 0.041, 0.054))
# CDRi=function(x) logitnorm::qlogitnorm(x,priors$cdrSp_m, priors$cdrSp_s),


