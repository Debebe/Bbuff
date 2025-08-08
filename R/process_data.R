
library(here)
library(countrycode)  # to get Iso3 codes
library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(readxl)

## This script reads data from different sources -
  # - GDP, LE, TB-burden, UN-population, BCG coverage,


inc <- read_csv(here("data/TB_burden_age_sex_2025-05-15.csv")) %>% 
  filter(sex %in% c("m", "f"), age_group %in% c("0-4","0-14")) %>%
  select(country, iso2, iso3,year,age_group, sex, best, lo, hi)%>%
  group_by(country,Iso2= iso2, Iso3=iso3,Year=year,Age=age_group)%>%
  summarise(
    best = sum(best, na.rm = TRUE),
    lo = sum(lo, na.rm = TRUE),
    hi = sum(hi, na.rm = TRUE),
    .groups = "drop") %>% as.data.table()

unpop <- read_csv(here("data/unpopulation_dataportal_20250515175045.csv"))%>%
  select(country=Location, Iso2,Iso3, Year=Time,Sex, Age, Pop=Value)

notif <- read_csv("data/TB_notifications_2025-05-21.csv") %>%
  filter(year%in%c(2023)) %>%
  select(country, iso3, year,
         newrel_f04,newrel_f014, newrel_m04,newrel_m014)%>%filter(year%in%c(2023:2024))%>%
  mutate(notif04=  newrel_f04 + newrel_m04,
         notif014= newrel_f014 + newrel_m014)%>%
  select(Iso3= iso3,notif04,notif014) %>%
  pivot_longer(cols = c("notif04", "notif014"), names_to = "Age", values_to = "notif") %>%
  mutate(Age= ifelse(Age=="notif04", "0-4", "0-14")) 


who_incidence <- inner_join(unpop,inc, by=c("country","Iso2","Iso3","Year","Age"))%>%
  inner_join(notif, by=c("Iso3", "Age"))%>%
  mutate(incbest=best/Pop,
         inclo=lo/Pop,
         inchi=hi/Pop, 
         notif=notif/Pop)%>%
  mutate(cdr=notif/incbest)

BCG <- read_excel("data/Bacillus Calmette–Guérin (BCG) vaccination coverage 2025-04-03 10-35 UTC.xlsx")%>%
  filter(YEAR%in% c(2023)) %>%
  select(Iso3= CODE,Year=YEAR, cat=COVERAGE_CATEGORY,bcg_coverage= COVERAGE)%>%filter(!is.na(bcg_coverage))%>%
  mutate(bcg_coverage= round(bcg_coverage/100, 3))%>%
  as.data.table()

inc_pop_bcg <- inner_join(who_incidence,BCG, by= c("Iso3","Year"))


GDP <- read_csv(here("data/03baa64d-3ff7-4719-a48a-5c7b5d1690f9_Data.csv"))%>%
  select(Iso3= "Country Code", GDP= "2023 [YR2023]") %>%
  mutate(GDP= as.numeric(GDP))


LE <- read_csv(here("data/unpopulation_dataportal_20250515195649.csv"))%>%
  select(country= Location, Year= Time, Iso3,Age,L=Value) %>%
  # taking weighted mean of LE between 0 and 1-4 years
  mutate(L=case_when(Age=="0" ~ 0.2*L,
                     Age=="1-4" ~ 0.8*L,
                     TRUE~L),
         Age=case_when(Age %in% c("0", "1-4") ~"0-4",
                       TRUE ~Age))%>%
  group_by(country, Year, Iso3,Age)%>%
  summarise(L=sum(L))%>%
  rename(LE=L)%>%
  as.data.table()



uc_vax_delv <- read_excel('/Users/debebeadewo/Library/CloudStorage/GoogleDrive-d.shaweno@sheffield.ac.uk/Shared drives/VAXHUB_WP2_HAR/papers/costing/StandardizedDeliveryUnitCosts4Dec2020-5.xlsx')

uc_vax_delv <- uc_vax_delv%>%slice(27:162) %>%
  select(1:21)%>%
  mutate(across(2:21, as.numeric))%>%
  rename_with(~ c("Country", "uc_tot_vax_delv_ave","uc_tot_vax_delv_med","uc_tot_vax_delv_lo","uc_tot_vax_delv_hi",
                  "uc_labor_ave","uc_labor_med","uc_labor_lo","uc_labor_hi",
                  "uc_sc_ave","uc_sc_med","uc_su_lo","uc_sc_hi",
                  "uc_servd_ave","uc_servd_med","uc_servd_lo","uc_servd_hi",
                  "uc_capital_ave","uc_capital_med","uc_capital_lo","uc_capital_hi"
  ))

#names(uc_vax_delv) <- c("Country", "ucost_delv_aver","ucost_delv_median", "ucost_delv_lower", "ucost_delv_upper")



uc_vax_delv$Iso3 <- countrycode(uc_vax_delv$Country, origin = "country.name", destination = "iso3c")
uc_vax_delv= setDT(uc_vax_delv)
uc_vax_delv[grepl("Centeral African", Country), Iso3:= "CAF"]
uc_vax_delv[grepl("Republic", Country), ]

#gdp_inc_le[grepl("Kor", country), ]

tbtx_unit_costs <- read_csv(here("data/tbtx_unit_costs.csv"))%>%
  #filter(unit_cost=="c_dstb_txO15")%>%
  select(Iso3= iso3, unit_cost, ucost_tb_trt.m=cost.m, ucost_tb_trt.sd=cost.sd)

tbtx_unit_costs <- readRDS(here("data/cost/outdata/tbtx_unit_costs.Rds")) %>%
  select(-country)

gdp_inc_le <- inner_join(inc_pop_bcg, GDP, by= "Iso3")%>%
  inner_join(LE, by= c("Iso3","country", "Age", "Year")) %>%
  inner_join(uc_vax_delv, by="Iso3") %>% 
  inner_join(tbtx_unit_costs,  by="Iso3")%>%
  rename(cov_cat= "cat")%>%
  select(-Country)%>%
  as.data.table()

#=====Prop notif that is TBM=========

# get data from Pete's TBMK package
vec <- scan("~/Documents/TBMK-main/modelling/data/isoz.txt", what = "", sep = "", quiet = TRUE)
load("~/Documents/TBMK-main/modelling/data/POP.Rdata")

U5POP <-POP[acat=="1-4"|acat=="<1",] 

TBMUnd1 <- data.frame(M=0.0446, L=0.0243,U=0.0803)
TMB1to5 <- data.frame(M=0.0312, L=0.0173,U=0.0554)

U5POP[acat=="<1",TBM_M:=pop*TBMUnd1$M]
U5POP[acat=="<1",TBM_L:=pop*TBMUnd1$L]
U5POP[acat=="<1",TBM_U:=pop*TBMUnd1$U]

U5POP[acat=="1-4",TBM_M:=pop*TMB1to5$M]
U5POP[acat=="1-4",TBM_L:=pop*TMB1to5$L]
U5POP[acat=="1-4",TBM_U:=pop*TMB1to5$U]

U5POP_sum <- U5POP[, lapply(.SD, sum), by = iso3, .SDcols = is.numeric]
U5POP_sum[, `:=`(M = TBM_M / pop,L = TBM_L / pop, U = TBM_U / pop)]

head(U5POP_sum)

saveRDS(LE, file="data/LE.rds")
saveRDS(gdp_inc_le, file="data/gdp_inc_le.rds")






