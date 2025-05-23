
library(flextable)
library(officer)

# some parameters
bcg_eff_tb <- 0.50
cdr <- 0.86
cfr_treat <- 0.019 #1·9 (0·5–7·1)
cfr_utreat <- 0.436 #43·6 (36·8–50·6)
ucost_treat <- 6667 # https://pubmed.ncbi.nlm.nih.gov/25939501/
ucost_proc_bcg <- 0.1205 # for serum institute india- 2023

data <- readRDS(file="data/gdp_inc_le.rds")

data_all <- data
thresholds <- c(0.3, 0.5, 1.0)  

data_all <- gdp_inc_le%>%
  filter(Iso3 %in% high_tb_iso3) %>%
  crossing(Threshold = thresholds) %>%  # create all combinations
  mutate(Threshold_label = paste0(Threshold * 100, "%"),
         lambda = GDP * Threshold) %>%
  mutate(IxB_1 =lambda*(notif *(1-cfr_treat) + (incbest-notif)*(1-cfr_utreat)) *LE,
         IxB_2 =lambda*(min(incbest, notif)*(1-cfr_treat) + max(incbest-notif, 0)*(1-cfr_utreat)) *LE)%>%
  mutate(g1= (1-bcg_eff_tb)*IxB_1 - (ucost_proc_bcg + uc_tot_vax_delv_med) - (1-bcg_eff_tb)*notif*ucost_treat,
         g2= (1-bcg_eff_tb)*IxB_2 - (ucost_proc_bcg + uc_tot_vax_delv_med) - (1-bcg_eff_tb)*notif*ucost_treat)%>%
  # cost of wastage is the sum of unit costs of supply chain  + unit costs of capital+purchae)
  # didnt include labour (as labour may mean labor per dose vaccinated)
  mutate(h=uc_sc_ave+uc_capital_ave+ucost_proc_bcg)%>%
  mutate(
    `g/h`=round(g1/h,1),
    `g/(g + h)1`=g1/(g1+h) #,
    #`g/(g + h)2`=round(g2/(g2+h), 3)
  )%>%
  mutate(demand_met1 =round(100*pnorm(`g/(g + h)1`), 1)
         #demand_met2 =round(100*pnorm(`g/(g + h)1`), 1)
  )%>%
  # calculate buffer size under different coefficient of variations
  mutate(mean_demand= Pop*coverage/100)%>%
  mutate(CV1=0.05,CV2=0.1,CV3=0.15)%>%
  mutate(supply1= mean_demand + CV1*mean_demand *qnorm(`g/(g + h)1`),
         supply2= mean_demand + CV2*mean_demand *qnorm(`g/(g + h)1`),
         supply3= mean_demand + CV3*mean_demand *qnorm(`g/(g + h)1`))%>%
  mutate(BF1= round(100*(supply1-mean_demand)/mean_demand,1),
         BF2= round(100*(supply2-mean_demand)/mean_demand,1),
         BF3= round(100*(supply3-mean_demand)/mean_demand, 1))%>%
  # cost including buffer = cost of buying incl buffer + cost of deliv vacc *demand + cost of trating
  mutate(cost1= supply1*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med + (1-bcg_eff_tb)*mean_demand*notif*ucost_treat,
         cost2= supply2*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med + (1-bcg_eff_tb)*mean_demand*notif*ucost_treat,
         cost3= supply3*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med + (1-bcg_eff_tb)*mean_demand*notif*ucost_treat)%>%
  mutate(costextra1= 100*((supply1*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med)-(mean_demand*uc_tot_vax_delv_med))/(mean_demand*uc_tot_vax_delv_med),
         costextra2= 100*((supply2*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med)-(mean_demand*uc_tot_vax_delv_med))/(mean_demand*uc_tot_vax_delv_med),
         costextra3= 100*((supply3*ucost_proc_bcg + mean_demand*uc_tot_vax_delv_med)-(mean_demand*uc_tot_vax_delv_med))/(mean_demand*uc_tot_vax_delv_med))


# create table()

# create table()
tmp <-data_all%>%
  mutate(GDP= round(GDP, 0))%>%
  filter(cov_cat=="OFFICIAL", Iso3%in%high_tb_iso3)%>%
  select(Country=country, GDP, `WTP(%GDP)`=Threshold_label, g=g1,h, `g/h`,
         `g/(g + h)`= `g/(g + h)1`,`Met demand`= demand_met1)%>%
  mutate(g= round(g,1),
         h=round(h,2),
         `g/(g + h)`= round(`g/(g + h)`,3)
         )
  
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







buffdata <-data_all%>%
  mutate(GDP= round(GDP, 0))%>%
  filter(cov_cat=="OFFICIAL", Iso3%in%high_tb_iso3)%>%
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

g/(g+h)= 0.3 

#0.3*(g+h)=g-0.3g=0.3h

0.7g= 0.3h
