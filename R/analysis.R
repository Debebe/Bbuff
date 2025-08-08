
rm(list = ls())
library(flextable)
library(officer)
library(here)
library(dplyr)
library(data.table)
library(tidyr)

# only if there is a need to process original data

## source("~/Documents/GitHub/Bbuff/R/process_data.R")
## source("~/Documents/GitHub/Bbuff/R/inflation_adjustment.R")

# some parameters
bcg_eff_tb <- 0.50
bcg_eff_tbm <- 0.73 # 0.73 (0.67 - 0.79), 0.69(0.60- 0.76 in Asia see Trunz

bcg_haz_tb <- 1- bcg_eff_tb
bcg_haz_mtb <- 1- bcg_eff_tbm

cfr_treat <- 0.019   #1·9 (0·5–7·1) tb-mortality treated
cfr_utreat <- 0.436  # 43·6 (36·8–50·6) tb-mort untreated
ucost_proc_bcg <- 0.1205 # for serum institute India- 2023
disc_rate <- 0.03


post_tb_dis_wt    <- 0.04  
post_tb_hrqol     <- 0.93  # 0.88- 0.98 from https://pmc.ncbi.nlm.nih.gov/articles/PMC8080025/
post_tb_mort_hz   <- 1.78

tbm_mort_hz   <- 1.79   # TODO guess
#tbm_hrqol     <- 0.60   # TODO  guess
cfr_treat_tbm <- 0.193  # 19·3% (95% CI 14·0–26·1) 
cfr_untreat_tbm <- 1    # 19·3% (95% CI 14·0–26·1) 



gdp_inc_le_costs <- readRDS(file="data/gdp_inc_le_costs.rds")

high_tb_iso3 <- c(
  "AGO", "BGD", "BRA", "CAF", "CHN", "COG", "PRK", "COD", "ETH", "GAB",
  "IND", "IDN", "KEN", "LSO", "LBR", "MNG", "MOZ", "MMR", "NAM", "NGA",
  "PAK", "PNG", "PHL", "SLE", "SOM", "ZAF", "THA", "UGA", "TZA", "ZMB")

#data_all <- gdp_inc_le
thresholds <- c(0.3, 0.5, 1.0)  

#without tbm - using incidence and notifications as inputs
wzout_mtb0 <- gdp_inc_le_costs%>%
  #I0=I/f(1-v)+(1-f)
   # baseline TB risk/inc without BCG vax
  mutate(inc0=incbest/(bcg_coverage*(1-bcg_eff_tb) + (1-bcg_coverage))) %>%
  mutate(LE_disc= (1-exp(-disc_rate*LE))/disc_rate)%>%
  filter(iso3 %in% high_tb_iso3) %>%
  crossing(Threshold = thresholds) %>%  # create all combinations
  mutate(Threshold_label = paste0(Threshold * 100, "%"),
         lambda = GDP * Threshold) %>%
  mutate(IxB_1 =lambda*(notif *(1-cfr_treat) + (incbest-notif)*(1-cfr_utreat)) *LE_disc,
         IxB_2 =lambda*(min(incbest, notif)*(1-cfr_treat) + max(incbest-notif, 0)*(1-cfr_utreat)) *LE_disc)%>%
  mutate(g1= (1-bcg_eff_tb)*IxB_1 - (ucost_proc_bcg + uc_tot_vax_delv_med) - (1-bcg_eff_tb)*notif*ucost_dstb.m,
         g2= (1-bcg_eff_tb)*IxB_2 - (ucost_proc_bcg + uc_tot_vax_delv_med) - (1-bcg_eff_tb)*notif*ucost_dstb.m)%>%
  # cost of wastage is the sum of unit costs of supply chain  + unit costs of capital+purchae)
  # didnt include labour (as labour may mean labor per dose vaccinated)
  mutate(h=uc_sc_ave+uc_capital_ave+ucost_proc_bcg)%>%
  mutate(
    `g/h`=round(g2/h,1),
    `g/(g + h)1`=g2/(g2+h))%>%
 
  # calculate buffer size under different coefficient of variations
  mutate(mean_demand= Pop*bcg_coverage/100)%>%
  mutate(CV1=0.05,CV2=0.1,CV3=0.15)%>%
  mutate(supply1= mean_demand + CV1*mean_demand *qnorm(`g/(g + h)1`),
         supply2= mean_demand + CV2*mean_demand *qnorm(`g/(g + h)1`),
         supply3= mean_demand + CV3*mean_demand *qnorm(`g/(g + h)1`)) %>%
  mutate(BF1= round(100*(supply1-mean_demand)/mean_demand,1),
         BF2= round(100*(supply2-mean_demand)/mean_demand,1),
         BF3= round(100*(supply3-mean_demand)/mean_demand, 1))%>%
  # cost including buffer = cost of buying incl buffer + cost of deliv vacc *demand + cost of trating
  mutate(cost1= supply1*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med + (1-bcg_eff_tb)*mean_demand*notif*ucost_dstb.m,
         cost2= supply2*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med + (1-bcg_eff_tb)*mean_demand*notif*ucost_dstb.m,
         cost3= supply3*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med + (1-bcg_eff_tb)*mean_demand*notif*ucost_dstb.m)%>%
  mutate(costextra1= 100*((supply1*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med)-(mean_demand*uc_tot_vax_delv_med))/(mean_demand*uc_tot_vax_delv_med),
         costextra2= 100*((supply2*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med)-(mean_demand*uc_tot_vax_delv_med))/(mean_demand*uc_tot_vax_delv_med),
         costextra3= 100*((supply3*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med)-(mean_demand*uc_tot_vax_delv_med))/(mean_demand*uc_tot_vax_delv_med))


# create table()
tmp <-wzout_mtb0%>%
  mutate(GDP= round(GDP, 0))%>%
  filter(cov_cat=="OFFICIAL", iso3%in%high_tb_iso3)%>%
  select(Country=country, GDP, `WTP(%GDP)`=Threshold_label, g=g1,h, `g/h`,
         `g/(g + h)`= `g/(g + h)1`)%>%
  mutate(g= round(g,1),
         h=round(h,2),
         `g/(g + h)`= round(`g/(g + h)`,3))
  
ft <- flextable(tmp)%>%
  set_caption("Monetized net benefit as a function of WTP") |>
  autofit() |>
  merge_v(j = c("Country", "GDP")) |>
  align(align = "left", part = "all") |>
  valign(valign = "top", j = c("Country", "GDP")) |>
  fontsize(size = 7, part = "all") |>
  autofit()
doc <- read_docx() |> 
  body_add_flextable(value = ft) |> 
  body_add_par(" ", style = "Normal") # optional spacing

print(doc, target = "outputs/buffer_netbenefit.docx")




buffdata <-wzout_mtb0%>%
  mutate(GDP= round(GDP, 0))%>%
  filter(cov_cat=="OFFICIAL", iso3%in%high_tb_iso3)%>%
  select(country, GDP, Threshold_label, BF1, BF2, BF3)%>%
  rename_with(~c("Country", "GDP", "WTP(%GDP)", "CV=5%", "CV=10%", "CV=15%")) #%>%


ft <- flextable(buffdata) |>
  set_caption("Buffer size as proportion of demand") |>
  autofit() |>
  merge_v(j = c("Country", "GDP")) |>
  align(align = "left", part = "all") |>
  valign(valign = "top", j = c("Country", "GDP")) |>
  fontsize(size = 7, part = "all") |>
  autofit()


# Save to Word document
doc <- read_docx() |> 
  body_add_flextable(value = ft) |> 
  body_add_par(" ", style = "Normal") # optional spacing

print(doc, target = "outputs/buffer_sz.docx")


#===without Meningeal TB
# using parameters as inputs

wzout_tbm1 <- gdp_inc_le_costs%>%
  # baseline TB risk/inc without BCG vax
  mutate(inc0=incbest/(bcg_coverage*(1-bcg_eff_tb) + (1-bcg_coverage))) %>%
  #LE taking into account increased mortality and decreased quality of life
  mutate(LE_disc= (1-exp(-disc_rate*LE))/disc_rate,
         LEtb= post_tb_hrqol*(1-exp(-disc_rate*LE/post_tb_mort_hz))/disc_rate) %>%
  
  filter(iso3 %in% high_tb_iso3) %>%
  crossing(Threshold = thresholds) %>%  # create all combinations
  mutate(Threshold_label = paste0(Threshold * 100, "%"),
         lambda = GDP * Threshold) %>%
  mutate(NMB =inc0*(1-bcg_eff_tb)* (lambda*LE_disc - 
              (cdr*(1-cfr_treat)+ (1-cdr)* (1-cfr_utreat))*lambda*LEtb - cdr*ucost_dstb.m) - #TODO using minus sign before cdr*ucost_dstb.m
           (ucost_proc_bcg + uc_tot_vax_delv_med),
         g=inc0*(1-bcg_eff_tb)* (lambda*LE_disc -(cdr*(1-cfr_treat)+ (1-cdr)* (1-cfr_utreat))*lambda*LEtb),
         h=(ucost_proc_bcg + uc_tot_vax_delv_med+inc0*(1-bcg_eff_tb)*cdr*ucost_dstb.m))%>%
  mutate(
    `g/h`=round(g/h,1),
    `g/(g + h)`=g/(g+h))%>%
  as.data.table()

#**********************************#
#==========with TBM================#
#**********************************#
#*

# additional inputs
prop_tbm <- 0.034 
prop_sev_seq  <- 0.56 # 0.56[0.47-0. 63] meta-analysis
prop_nsev_seq <- 0.44 # 0.44 [0.37, 0.53]
prop_mild_seq  <-  0.70* prop_nsev_seq
prop_mod_seq   <-  0.30* prop_nsev_seq

tbm_hrqol_mil_seq <- 1-0.031 # 0·031 (0·018–0·050) #Global burden of disease, 2013
tbm_hrqol_mod_seq <- 1-0.203 # 0·203 (0·134–0·290)
tbm_hrqol_sev_seq <- 1-0.542 # 0·542 (0·374–0·702)


data_wz_tbm <- gdp_inc_le_costs%>%
  # baseline TB risk/inc without BCG vax
  mutate(inc0 = incbest/(bcg_coverage*(1-bcg_eff_tb) + (1-bcg_coverage))) %>%
  # baseline inc of MTB in unvaccinated
  mutate(incm0 = inc0*prop_tbm*(1-bcg_coverage),
         incm_vac=inc0*prop_tbm*bcg_coverage*(1-bcg_eff_tb)*(1-bcg_eff_tbm)) %>% 
  mutate(tb_avert=  (1-bcg_haz_tb)*inc0,
         prop_tbm_avert=  (1-bcg_haz_mtb)*prop_tbm) %>% #proportion
  #LE taking into account increased mortality and decreased quality of life
  mutate(LE_disc= (1-exp(-disc_rate*LE))/disc_rate,
         #Lmtb= tbm_hrqol*(1-exp(-disc_rate*LE/tbm_mort_hz))/disc_rate,
         Ltb= post_tb_hrqol*(1-exp(-disc_rate*LE/post_tb_mort_hz))/disc_rate
        ) %>%
  # different severity levels 
  mutate(Lmtb_mil= prop_mild_seq*tbm_hrqol_mil_seq*(1-exp(-disc_rate*LE/tbm_mort_hz))/disc_rate,
         Lmtb_mod= prop_mod_seq*tbm_hrqol_mod_seq*(1-exp(-disc_rate*LE/tbm_mort_hz))/disc_rate,
         Lmtb_sev= prop_sev_seq*tbm_hrqol_sev_seq*(1-exp(-disc_rate*LE/tbm_mort_hz))/disc_rate) %>%
  mutate(Lmtb=Lmtb_mil+Lmtb_mod+Lmtb_sev)%>%
  
  mutate(meanH= (1-inc0)*LE_disc + 
                inc0*(1-prop_tbm)*(cdr*(1-cfr_treat) + (1-cdr)*(1-cfr_utreat))*Ltb +
                incm0*(cdr*(1-cfr_treat_tbm) + (1-cdr)*(1-cfr_untreat_tbm)*Lmtb)) %>%
  ##
  crossing(Threshold = thresholds) %>%  # create all combinations
  mutate(Threshold_label = paste0(Threshold*100,"%"),
         lambda = GDP * Threshold)%>%
  
  mutate(first= tb_avert*(lambda*LE_disc - (1-prop_tbm)*(cdr*(1-cfr_treat)*(1-cdr)*(1-cfr_utreat))*lambda*Ltb + (1-prop_tbm)*cdr*ucost_dstb.m),
         second= tb_avert *(prop_tbm*cdr*(1-cfr_treat_tbm) + (1-cdr)*(1-cfr_untreat_tbm)*lambda*Lmtb -prop_tbm*cdr* ucost_tbm.m),
         third= inc0*(prop_tbm_avert*(cdr*(1-cfr_treat)*(1-cdr)*(1-cfr_utreat))*lambda*Ltb - prop_tbm_avert*cdr*ucost_dstb.m),
         fourth= inc0*(prop_tbm_avert*(cdr*(1-cfr_treat_tbm)+ (1-cdr)*(1-cfr_untreat_tbm))*lambda*Lmtb-prop_tbm_avert*cdr*ucost_tbm.m),
         h=(ucost_proc_bcg + uc_tot_vax_delv_med),
         g=first-second +third-fourth,
         NB=g-h,
         # DALYs averted from averting TB cases
         averted_dalys_tb=  tb_avert*( LE_disc - (1 - prop_tbm)*(cdr*(1 - cfr_treat) + (1 - cdr)*(1 - cfr_utreat) ) * Ltb),
         # DALYs averted from TB meningitis cases
         averted_dalys_tbm= inc0 * prop_tbm_avert * (cdr * (1 - cfr_treat_tbm) + (1 - cdr) * (1 - cfr_untreat_tbm) * Lmtb),
         avertd_tb = inc0*(1-bcg_haz_tb),
         averted_mtb= inc0*prop_tbm*(1-bcg_haz_mtb),
         averted_tb_costs= tb_avert * (1 - prop_tbm) * cdr * ucost_dstb.m,
         averted_mtb_costs= inc0*prop_tbm_avert* cdr * ucost_tbm.m)%>%
         
         # Total DALYs averted
         mutate(averted_total_dalys = averted_dalys_tb + averted_dalys_tbm)%>%
        
         mutate(pdemand_met =round(100*(g/(g + h)), 1),
                `g/h`= round(g/h,1))%>%
  mutate(mean_demand= Pop*bcg_coverage/100)%>%
  mutate(CV1=0.05,CV2=0.1,CV3=0.15)%>%
  mutate(z_score= qnorm(g/(g + h), mean = 0, sd = 1))%>%
  mutate(Buffer1= round(100*z_score*CV1,1),
         Buffer2= round(100*z_score*CV2,1),
         Buffer3= round(100*z_score*CV3,1))%>%as.data.table()
  
  # mutate(supply1= mean_demand + CV1*mean_demand *qnorm(g/(g + h)),
  #        supply2= mean_demand + CV2*mean_demand *qnorm(g/(g + h)),
  #        supply3= mean_demand + CV3*mean_demand *qnorm(g/(g + h))) %>%
  # # buffer as a proportion of demand
  # mutate(Buffer1= round(100*(supply1-mean_demand)/mean_demand,1),
  #        Buffer2= round(100*(supply2-mean_demand)/mean_demand,1),
  #        Buffer3= round(100*(supply3-mean_demand)/mean_demand,1))

buffdata <-data_wz_tbm%>%
  filter(incbest>0)%>%
  mutate(GDP= round(GDP, 0))%>%
  filter(cov_cat=="WUENIC")%>%
  select(country,iso3, GDP, Threshold_label,NB,g, h,`g/h`,pdemand_met, Buffer1, Buffer2, Buffer3)%>%
  rename_with(~c("Country", "ISO", "GDP", "WTP(%GDP)","Net benefit","g","h","g/h","g/(g+h)", "CV=5%", "CV=10%", "CV=15%")) %>%
  mutate(`Net benefit`= round(`Net benefit`, 0),
         g= round(g, 0),
         h= round(h, 2))

averted_stuff <- data_wz_tbm%>%
  filter(cov_cat=="WUENIC")%>%
  distinct(country,.keep_all = TRUE)%>%
  transmute(Country=country,ISO=iso3, TB=1e6*avertd_tb, MTB=1e6*averted_mtb,DALYs=1e6*averted_total_dalys,
            `TB costs`=1e6*averted_tb_costs, `MTB costs`=1e6*averted_mtb_costs) %>%
  filter(TB>0, !is.na(DALYs))

expensive_settings <- data_wz_tbm%>%
  filter(cov_cat=="WUENIC", `g/h`<1) %>%
  distinct(country,.keep_all = TRUE)%>%
  select(country,Iso3=iso3, GDP, Threshold_label,NB,g, h,`g/h`,pdemand_met, Buffer1, Buffer2, Buffer3)%>%
  rename_with(~c("Country", "ISO", "GDP", "WTP(%GDP)","Net benefit","g","h","g/h","g/(g+h)", "CV=5%", "CV=10%", "CV=15%")) %>%
  mutate(`Net benefit`= round(`Net benefit`, 0),
         g= round(g, 0),
         h= round(h, 2))

## averted tables

ft <-averted_stuff%>%
  filter(ISO %in% c("MYS", "THA", "VNM", "IDN", "PHL"))%>%
  select(-ISO)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))%>%
  flextable() |>
  set_caption("Resource use and health gains (per million per dose)") |>
  autofit() |>
 # merge_v(j = c("Country", "GDP")) |>
  align(align = "left", part = "all") |>
  #valign(valign = "top", j = c("Country", "GDP")) |>
  fontsize(size = 7, part = "all") |>
  autofit()


# Save to Word document
doc <- read_docx() |> 
  body_add_flextable(value = ft) |> 
  body_add_par(" ", style = "Normal") # optional spacing

print(doc, target = "outputs/resource-use.docx")


#===all countries
ft_avert_acntrs <-averted_stuff%>%
  #filter(ISO %in% c("MYS", "THA", "VNM", "IDN", "PHL"))%>%
  select(-ISO)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))%>%
  flextable() |>
  set_caption("Resource use and health gains (per million per dose)") |>
  autofit() |>
  # merge_v(j = c("Country", "GDP")) |>
  align(align = "left", part = "all") |>
  #valign(valign = "top", j = c("Country", "GDP")) |>
  fontsize(size = 7, part = "all") |>
  autofit()


# Save to Word document
doc <- read_docx() |> 
  body_add_flextable(value = ft_avert_acntrs) |> 
  body_add_par(" ", style = "Normal") 

print(doc, target = "outputs/resource-use_acntrs.docx")


# high burden countries

ftt <- buffdata%>%
  filter(ISO%in%high_tb_iso3)%>%
  select(-ISO)%>%
  flextable() |>
  set_caption("Cost effectiveness and optimal buffer size under different demand uncertainties") |>
  autofit() |>
  merge_v(j = c("Country", "GDP","h")) |>
  align(align = "left", part = "all") |>
  valign(valign = "top", j = c("Country", "GDP", "h")) |>
  fontsize(size = 7, part = "all") |>
  autofit() 
  

# Save to Word document
doc <- read_docx() |> 
  body_add_flextable(value = ftt) |> 
  body_add_par(" ", style = "Normal") # optional spacing

print(doc, target = "outputs/buffersize_ce_hbc.docx")

ftexp <- expensive_settings%>%
  #filter(ISO%in%high_tb_iso3)%>%
  select(-ISO)%>%
  flextable() |>
  set_caption("Cost effectiveness and optimal buffer size under different demand uncertainties") |>
  autofit() |>
  merge_v(j = c("Country", "GDP","h")) |>
  align(align = "left", part = "all") |>
  valign(valign = "top", j = c("Country", "GDP", "h")) |>
  fontsize(size = 7, part = "all") |>
  autofit() 

# Save to Word document
doc <- read_docx() |> 
  body_add_flextable(value = ftexp) |> 
  body_add_par(" ", style = "Normal") # optional spacing

print(doc, target = "outputs/expensive_countries.docx")

# all countries 

ft_all_cntrs <- buffdata%>%
  filter(!is.na(g))%>%
  #filter(ISO%in%high_tb_iso3)%>%
  select(-ISO)%>%
  flextable() |>
  set_caption("Cost effectiveness and optimal buffer size under different demand uncertainties") |>
  autofit() |>
  merge_v(j = c("Country", "GDP","h")) |>
  align(align = "left", part = "all") |>
  valign(valign = "top", j = c("Country", "GDP", "h")) |>
  fontsize(size = 7, part = "all") |>
  autofit() 


# Save to Word document
doc <- read_docx() |> 
  body_add_flextable(value = ft_all_cntrs) |> 
  body_add_par(" ", style = "Normal") # optional spacing

print(doc, target = "outputs/buffersize_ce_all_cntrs.docx")

## asean
ft3 <- buffdata%>%
  filter(Country %in% c("Malaysia", "Thailand", "Vietnam", "Indonesia", "Philippines")) %>%  
  select(-ISO)%>%
  flextable() |>
  set_caption("Cost effectiveness and optimal buffer size under different demand uncertainties") |>
  autofit() |>
  merge_v(j = c("Country", "GDP","h")) |>
  align(align = "left", part = "all") |>
  valign(valign = "top", j = c("Country", "GDP", "h")) |>
  fontsize(size = 7, part = "all") |>
  autofit()


# Save to word document
doc <- read_docx() |> 
  body_add_flextable(value = ft3) |> 
  body_add_par(" ", style = "Normal") # optional spacing

print(doc, target = "outputs/buffersize_CE_asean.docx")


all_settings <- data_wz_tbm%>%
  filter(cov_cat=="WUENIC", !is.na(GDP), !is.na(cdr)) %>%
  distinct(country,.keep_all = TRUE)%>%
  select(country,Iso3=iso3, GDP, Threshold_label,incbest, uc_tot_vax_delv_ave,ucost_dstb.m, NB,g, h,pdemand_met, Buffer1)%>%
  rename_with(~c("Country", "ISO", "GDP", "WTP(%GDP)","Inc", "ucvax","uctb", "Net benefit","g","h","g/(g+h)", "CV=5%")) %>%
  mutate(GDP= round(GDP, 0),
         `Net benefit`= round(`Net benefit`, 0),
         Inc= round(1e5*Inc, 0),
         ucvax= round(ucvax, 2),
         uctb= round(uctb, 2),
         g= round(g, 0),
         h= round(h, 2))

ftall <- all_settings%>%
  #filter(ISO%in%high_tb_iso3)%>%
  select(-ISO)%>%
  flextable() |>
  set_caption("Cost effectiveness and optimal buffer size under different demand uncertainties") |>
  autofit() |>
  merge_v(j = c("Country", "GDP","h")) |>
  align(align = "left", part = "all") |>
  valign(valign = "top", j = c("Country", "GDP", "h")) |>
  fontsize(size = 7, part = "all") |>
  autofit() 

# Save to word document
doc <- read_docx() |> 
  body_add_flextable(value = ftall) |> 
  body_add_par(" ", style = "Normal") # optional spacing

print(doc, target = "outputs/all_cntrs_compren.docx")

buffdata %>%
  mutate(`WTP(%GDP)`=paste0(`WTP(%GDP)`,"GDP"))%>%
  filter(Country %in% c("Malaysia", "Thailand", "Vietnam", "Indonesia", "Philippines")) %>%
  select(Country,`WTP(%GDP)`,`CV=5%`,`CV=10%`, `CV=15%`) %>%
  pivot_longer(cols = c(`CV=5%`,`CV=10%`, `CV=15%`), names_to = "CV",values_to = "Buffer size")%>%
  mutate(`WTP(%GDP)` = factor(`WTP(%GDP)`, levels = c("30%GDP", "50%GDP", "100%GDP")),
         CV = factor(CV, levels = c("CV=5%", "CV=10%", "CV=15%")))%>%

  ggplot(aes(x = reorder(Country,`Buffer size`), y = `Buffer size`, fill= Country)) +
  geom_text(aes(label = `Buffer size`), angle = 90, vjust = 0.5, hjust = 0.0, size = 3) +  # Add vertical alignment above bar
  
  geom_bar(stat = "identity") + ylab("Buffer size(%)")+
  #facet_wrap(`WTP(%GDP)`~CV)+ theme_bw()+
  facet_grid(`WTP(%GDP)`~CV)+ theme_bw()+
  
  theme(axis.title.x =element_blank(), axis.text.x = element_text(angle=90),
        legend.title = element_blank(), legend.position = "none")+ylim(0, 50)

# inspect_supply
 # supply_demand <- data_wz_tbm%>% 
 #  filter(cov_cat=="WUENIC", !is.na(pdemand_met)) %>%select(country, Threshold_label,mean_demand, supply1, supply2, supply3, `g/(g+h)`=pdemand_met)

library(ggrepel)

dd <- data_wz_tbm%>%filter(cov_cat=="WUENIC" & Threshold==0.3, !is.na(GDP), best>0 & !is.na(cdr))

ggplot(dd, aes(GDP,h, label = country) )+
  geom_point(colour="red") +facet_wrap(~who_region, scales='free', ncol=2)+ 
  geom_text_repel(size = 2.5, 
                  min.segment.length = 0.0, 
                  seed = 42, 
                  force=1,
                  nudge_x = 0,
                  box.padding = 0.5,
                  nudge_y = 0,
                  segment.curvature = -0.3,
                  segment.alpha	=0.5,
                  segment.size  = 0.4,
                  segment.ncp = 3,
                  max.overlaps = Inf,
                  segment.angle = 30) +
  labs(title = "Cost of BCG oversupply and GDP") + ylab("Cost of oversupply (h)") + theme_light()

ggplot(dd, aes(GDP,g, label = country) )+
  geom_point(colour="red") +facet_wrap(~who_region, scales="free", ncol=2)+ 
  geom_text_repel(size = 2.8, 
                  min.segment.length = 0, 
                  seed = 42, 
                  nudge_x = 0,
                  nudge_y = 0,
                  box.padding = 0.5,
                  segment.curvature = -0.1,
                  max.overlaps = Inf,
                  segment.alpha	=0.5,
                  segment.size  = 0.4,
                  segment.color = "purple",
                  segment.ncp = 3,
                  segment.angle = 20) +
  labs(title = "Cost of BCG undersupply and GDP") +ylab("Cost of undersupply (g)") +theme_light()



ggplot(dd, aes(GDP,incbest, label = country) )+
  geom_point(colour="red") +facet_wrap(~who_region, scales="free", ncol=2)+ 
  geom_text_repel(size = 2.8, 
                  min.segment.length = 0, 
                  seed = 42, 
                  nudge_x = 0,
                  nudge_y = 0,
                  box.padding = 0.5,
                  segment.curvature = -0.1,
                  max.overlaps = Inf,
                  segment.alpha	=0.5,
                  segment.size  = 0.4,
                  segment.color = "purple",
                  segment.ncp = 3,
                  segment.angle = 20) +
  labs(title = "TB incidence and GDP") +ylab("Incidence") +theme_light()

ggplot(dd[who_region=="AFR",], aes(GDP,incbest, label = country) )+
  geom_point(colour="red") +facet_wrap(~who_region, scales="free", ncol=2)+ 
  geom_text_repel(size = 2.8, 
                  min.segment.length = 0, 
                  seed = 42, 
                  nudge_x = 0,
                  nudge_y = 0,
                  box.padding = 0.5,
                  segment.curvature = -0.1,
                  max.overlaps = Inf,
                  segment.alpha	=0.5,
                  segment.size  = 0.4,
                  segment.color = "purple",
                  segment.ncp = 3,
                  segment.angle = 20) +
  labs(title = "TB incidence and GDP") +ylab("Incidence") +theme_light()


ggplot(dd, aes(GDP,uc_tot_vax_delv_ave, label = country) )+
  geom_point(colour="red") +facet_wrap(~who_region, scales="free", ncol=2)+ 
  geom_text_repel(size = 2.8, 
                  min.segment.length = 0, 
                  seed = 42, 
                  nudge_x = 0,
                  nudge_y = 0,
                  box.padding = 0.5,
                  segment.curvature = -0.1,
                  max.overlaps = Inf,
                  segment.alpha	=0.5,
                  segment.size  = 0.4,
                  segment.color = "purple",
                  segment.ncp = 3,
                  segment.angle = 20) +
  labs(title = "Vaccine delivery unit cost and GDP") + ylab("BCG delivery unit cost") +theme_light()





