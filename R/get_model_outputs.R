
rm(list = ls())
 ## README
# This generates relevant final model outputs reported in the manuscript

pacman::p_load(here,data.table, dplyr, tidyr, stringr, truncnorm,
               flextable, officer,kableExtra,ggplot2, ggrepel, patchwork)

load(here("tmpdata/PSA.RData"))     # full PSA data with results. 
gdp_inc_le_costs <- readRDS(here("outdata/gdp_inc_le_costs.rds"))
load(here("indata/whokey.Rdata"))     # region and cntry iso codes


## === aggregations and outputs
CEA <- D[, .(
  ## expected net benefit at WTP=30%GDP
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
by = iso3
]


CEA[, g := ENB30 / bcg_cov]
CEA[, bcg_cov := NULL]
summary(CEA)

summary(CEA[, .(ENB30, ICER)])

CEA <- merge(CEA, whokey, by = "iso3")
CEA <- CEA[!is.na(ENB30)]

## ICER plot

thresholds <- c(0.3, 0.5, 1.0)  

CEA <- CEA%>%
  crossing(threshold = thresholds)%>%
  mutate(legend_label = paste0(threshold," GDP"))%>% 
  mutate(legend_label= factor(legend_label,
                              levels = c("0.3 GDP", "0.5 GDP", "1 GDP")))|>
  dplyr::mutate(ICER_val = GDP * threshold,
                ICER_Label= ifelse(ICER < 0.3 * GDP,"ICER < 0.3 GDP", "ICER >= 0.3 GDP" ))%>%
  left_join(gdp_inc_le_costs%>%
              filter(cov_cat=="WUENIC")%>%
              dplyr::select(iso3, inc_u5=incbest,
                            notif_u5=notif,
                            BCG=bcg_coverage,CDR=cdr,
                            ucvax=uc_tot_vax_delv_ave,
                            uctb=ucost_dstb.m, 
                            uctbm=ucost_tbm.m),
            by= "iso3")%>%
  inner_join(tbinc, by="iso3")%>%
  
  mutate(CV1 = 0.05, CV2 = 0.1, CV3 = 0.15,
         z_score = qnorm(g / (g + u), mean = 0, sd = 1)) %>%
  mutate(
    Bf1 = round(100 * z_score * CV1, 1),
    Bf2 = round(100 * z_score * CV2, 1),
    Bf3 = round(100 * z_score * CV3, 1))%>%
  as.data.table()

save(CEA, file = here("outdata/CEA.RData"))



## TODO buffers
CEA[, summary(ENB30)]
CEA[, summary(u)]
CEA[, summary(ENB30 / (ENB30 + u))]
CEA[, summary(qnorm(ENB30 / (ENB30 + u)))]


## TODO global and regional total outputs, e.g.:


#====data for percapita deaths======
per_cap_deaths <- D%>%
  dplyr::select(iso3, iter,matches("rslt_tb_deaths|rslt_tbn_deaths|rslt_tbm_deaths"))%>%
  pivot_longer(cols=-c("iter", "iso3"), names_to = "variable")%>%
  as.data.table()

per_cap_deaths[, type:= ifelse(str_detect(variable, "cf"), "cf", "sq")]
per_cap_deaths[, variable:= gsub("rlst_", "", variable)]
per_cap_deaths[, variable:= gsub("_cf|_sq", "", variable)]
per_cap_deaths <- dcast(per_cap_deaths,
                        iter + iso3 + variable ~ type,value.var = "value")%>%
  mutate(av=cf-sq)%>% select(-c("sq", "cf"))%>%as.data.table()

deaths_per_cap <- merge(per_cap_deaths, whokey, by = "iso3") %>%
  group_by(g_whoregion, region,iso3,variable)%>%
  summarise(av= mean(av, na.rm=TRUE))%>% filter(!is.na(av))|>
  mutate(type="percapita_death_averted")%>%
  as.data.table()


##=
per_cap_ave_inc <- D%>%
  select(iso3, iter,matches("rslt_inc|rslt_tbminc"))%>%
  pivot_longer(cols=-c("iter", "iso3"), names_to = "variable")%>%
  as.data.table()

per_cap_ave_inc[, type:= ifelse(str_detect(variable, "cf"), "cf", "sq")]
per_cap_ave_inc[, variable:= gsub("rlst_", "", variable)]
per_cap_ave_inc[, variable:= gsub("_cf|_sq", "", variable)]

per_cap_ave <- dcast(per_cap_ave_inc,
                     iter + iso3 + variable ~ type,value.var = "value")%>%
  mutate(av=cf-sq)%>% select(-c("sq", "cf"))%>%
  group_by(variable, iso3)%>%
  summarise(av= mean(av, na.rm=TRUE))%>% filter(!is.na(av))|>
  mutate(type="percapita_inc_averted")%>%
  bind_rows(per_cap_deaths%>%
              group_by(variable, iso3)%>%
              summarise(av= mean(av, na.rm=TRUE),
                        type="percapita_death_averted"))%>%
  as.data.table()

save(per_cap_ave, file=here("outdata/per_cap_ave.RData"))

per_cap_ave %>%
  group_by(variable) %>%
  slice_max(av, n = 1, with_ties = FALSE) %>%
  mutate(Descreption=paste0("Max avertion is in ",iso3),
         variable= gsub("rslt_", "", variable))%>%
  mutate(variable= paste0("Percapita avertion in ", variable))%>%
  select(-iso3, -type, value=av)


per_cap_ave %>%
  group_by(variable) %>%
  slice_min(av, n = 1, with_ties = FALSE) %>%
  mutate(Descreption=paste0("Min avertion is in ",iso3),
         variable= gsub("rslt_", "", variable))%>%
  mutate(variable= paste0("Percapita avertion in ", variable))%>%
  select(-iso3, -type,value=av)




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

# deaths pop level
death_absolute <- output_table[is.finite(av), .(av = sum(av, na.rm = TRUE)),
                               by = .(iso3, variable)]%>%
  select(iso3,variable, av)%>%
  filter(grepl("deaths", variable))%>%
  mutate(type= "absolute")%>%
  inner_join(whokey, by = "iso3")%>%
  distinct(iso3, variable, .keep_all=TRUE)%>%
  as.data.table()

deaths <- rbind(deaths_per_cap, death_absolute)
save(deaths, file=here("outdata/deaths.RData"))

# load(here("outdata/deaths.RData"))
# load(here("outdata/CEA.RData"))

## global TODO NaNs?

output_table <- output_table[is.finite(av), .(
  cf = sum(cf), 
  sq = sum(sq), 
  av = sum(av)
),
by = .(iter, variable)
]


## hi/lo & reshape

eps <- 0.025
output_table <- melt(output_table,
                     id = c("iter", "variable")
) # TODO better var names

output_table <- output_table[, .(
  mid = mean(value), 
  lo = quantile(value, eps), 
  hi = quantile(value, 1 - eps)
), by = .(variable, variable.1)]

output_table <- dcast(output_table,
                      variable ~ variable.1,
                      value.var = c("mid", "lo", "hi")
)

## change units to millions for cost and health, BCG dosese
fac <- 1e6
output_table[
  variable %in% c("rslt_att_cost","rslt_attm_cost", "rslt_cost",
                  "rslt_bcg_cost", "rslt_bcg_doses", "rslt_health",
                  "rslt_ly_tb","rslt_ly_tbm"),
  `:=`(
    mid_sq = mid_sq / fac, lo_sq = lo_sq / fac, hi_sq = hi_sq / fac,
    mid_cf = mid_cf / fac, lo_cf = lo_cf / fac, hi_cf = hi_cf / fac,
    mid_av = mid_av / fac, lo_av = lo_av / fac, hi_av = hi_av / fac
  )
]



## format numbers, add brackets
output_table[
  ,
  c("sq_txt", "cf_txt", "av_txt") := .(
    brkt(mid_sq, lo_sq, hi_sq),
    brkt(mid_cf, lo_cf, hi_cf),
    brkt(mid_av, lo_av, hi_av)
  )
]
output_table

fwrite(output_table, file = here("outdata/output_table.csv"))

#source("R/create_outs4who_regi.R") # replicates the global analysis for who_regions

#************************************************************
#=====Regional - this should come after the output_table=====
#*************************************************************
keep <- grep("rslt", names(D), value = TRUE)
keep <- c("who_region","iso3", "iter", "Pop", unique(keep))

out_tab_r <- D[, ..keep]
out_tab_r <- melt(out_tab_r, id = c("iter", "iso3","who_region", "Pop"))
out_tab_r[, type := ifelse(grepl("cf", variable), "cf", "sq")]
out_tab_r[, variable := gsub("rlst_", "", variable)]
out_tab_r[, variable := gsub("_cf|_sq", "", variable)]
out_tab_r[, value := value * Pop]
out_tab_r <- dcast(out_tab_r,
                   iter + iso3 + variable + who_region~ type,
                   value.var = "value")


## averted
out_tab_r[, av := cf - sq]

save(out_tab_r, file= here("outdata/out_tab_r.RData"))


out_r_aggr <- out_tab_r[is.finite(av), .(
  cf = sum(cf), sq = sum(sq), av = sum(av)),
  by = .(iter, variable, who_region)]

## hi/lo & reshape
eps <- 0.025
out_r_aggr <- melt(out_r_aggr,
                   id = c("iter","who_region", "variable")
) # TODO better var names

out_r_aggr <- out_r_aggr[, .(
  mid = mean(value), lo = quantile(value, eps), hi = quantile(value, 1 - eps)
), by = .(who_region,variable, variable.1)]

out_r_aggr <- dcast(out_r_aggr,
                    who_region+variable ~ variable.1,
                    value.var = c("mid", "lo", "hi"))

## change units to millions for cost and health, BCG dosese
fac <- 1e6
out_r_aggr[
  variable %in% c("rslt_att_cost","rslt_attm_cost", "rslt_cost",
                  "rslt_bcg_cost", "rslt_bcg_doses", "rslt_health",
                  "rslt_ly_tb","rslt_ly_tbm"),
  `:=`(
    mid_sq = mid_sq / fac, lo_sq = lo_sq / fac, hi_sq = hi_sq / fac,
    mid_cf = mid_cf / fac, lo_cf = lo_cf / fac, hi_cf = hi_cf / fac,
    mid_av = mid_av / fac, lo_av = lo_av / fac, hi_av = hi_av / fac
  )
]




## format numbers, add brackets
out_r_aggr[
  ,
  c("sq_txt", "cf_txt", "av_txt") := .(
    brkt(mid_sq, lo_sq, hi_sq),
    brkt(mid_cf, lo_cf, hi_cf),
    brkt(mid_av, lo_av, hi_av)
  )
]
out_r_aggr

fwrite(out_r_aggr, file = here("outdata/output_table_who.csv"))


##=====averted outcomes=====
keep <- grep("rslt", names(D), value = TRUE)
keep <- c("iso3", "iter", "Pop", unique(keep))

avrt_table <- D[, ..keep]
avrt_table <- melt(avrt_table, id = c("iter", "iso3", "Pop"))
avrt_table[, type := ifelse(grepl("cf", variable), "cf", "sq")]
avrt_table[, variable := gsub("rlst_", "", variable)]
avrt_table[, variable := gsub("_cf|_sq", "", variable)]
avrt_table[, value := value * Pop]
avrt_table <- dcast(avrt_table,
                    iter + iso3 + variable ~ type,
                    value.var = "value")

## averted
avrt_table[, av := cf - sq][,c("cf","sq"):=NULL]
# input for CEAC
ceac_input <-avrt_table%>%filter(variable%in%c("rslt_health","rslt_cost"))%>%
  pivot_wider(id_cols = c("iter", "iso3"), names_from = variable, values_from = av)|>
  na.omit()%>%as.data.table()

save(ceac_input, file = here("outdata/ceac_input.RData"))


## === box-and-whiskers CEAC plot
## assuming results are negative
lamz <- exp(seq(from = log(1), to = log(50e3), len = 100)) # log sequence of CET
lamzd <- expand.grid(iter = seq_len(ceac_input[, max(iter)]), lam = lamz)
lamzd <- as.data.table(lamzd) #grid of iter x threshold values
ceac <- merge(ceac_input, lamzd, by = "iter", allow.cartesian = TRUE)
ceac <- ceac[, .(p = mean(-lam * rslt_health > -rslt_cost)), by = .(iso3, lam)]

## check
ggplot(ceac[iso3 == "AFG"], aes(lam, p)) +
  geom_line() +
  scale_x_log10()

## calculate 2.5, 25, 50, 75, 97.5 percentiles
pcnts <- c(2.5, 25, 50, 75, 97.5)

## calculate quantiles of CEAC looped over countries
ceacq <- ceac[,
              {
                print(iso3)
                af <- approxfun(x = lam, y = p)
                ans <- rep(0, 5)
                for (i in 1:5) {
                  if (min(p) > pcnts[i] / 100) {
                    ans[i] <- min(lam)
                  } else if(max(p) < pcnts[i] / 100){
                    ans[i] <- max(lam)
                  } else {
                    tgt <- function(x) pcnts[i] / 100 - af(x)
                    ans[i] <- uniroot(tgt, lower = min(lamz), upper = max(lamz))$root
                  }
                }
                names(ans) <- paste0("q", pcnts)
                as.list(ans)
              },
              by = iso3
]

ceacq <- merge(ceacq, whokey, by = "iso3") #WHO region
ceacq[,iqr2median_ratio:= (q75-q25)/q50]

## order by median
ceacq$iso3 <- factor(
  ceacq$iso3,
  levels = ceacq[order(q50), iso3],
  ordered = TRUE
)


save(ceacq, file=here("outdata/ceacq.RData"))

ceacq[,.(median(iqr2median_ratio))] #overall
ceacq[,.(mid=median(iqr2median_ratio)), by= region] # by region


avert_deaths <- avrt_table%>%
  filter(variable=="rslt_tb_deaths")%>%
  na.omit()


avrt_table <- avrt_table%>%
  filter(variable%in% c("rslt_att","rslt_bcg_doses","rslt_inc",
                        "rslt_tbminc","rslt_tb_deaths","rslt_ly_tb"))%>%
  group_by(iso3, variable)%>%
  mutate(av=ifelse(variable=="rslt_bcg_doses", abs(av),av))%>%
  summarise(av.m= mean(av,na.rm=TRUE),
            av.l=quantile(av,0.025, na.rm=TRUE),
            av.h=quantile(av,0.975, na.rm=TRUE))%>%
  mutate(variable= case_when(variable=="rslt_att" ~"Averted ATT for TB",
                             variable=="rslt_bcg_doses" ~"BCG doses",
                             variable=="rslt_inc" ~"Averted TB incidence",
                             variable=="rslt_tbminc" ~"Averted TBM incidence",
                             variable=="rslt_ly_tb" ~"Averted DALYs from TB",
                             variable=="rslt_tb_deaths" ~"Averted TB deaths"))

save(avrt_table, file= here("outdata/avrt_table.RData"))
save(avert_deaths, file= here("outdata/avert_deaths.RData"))


# select top 10 cntrs by averted death 
top10 <- avrt_table %>%
  filter(variable == "Averted TB deaths") %>%
  arrange(desc(av.m)) %>%
  head(10) 

# subset top ten cntrs with other variables as well
top10_cntrs <-avrt_table %>%
  filter(iso3%in%top10$iso3) %>%
  left_join(
    avrt_table %>%
      filter(variable == "Averted TB deaths") %>%
      select(iso3, death_value = av.m),
    by = "iso3"
  ) %>%
  arrange(desc(death_value))%>%
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
  mutate(Estimates= brkt(av.m,av.l, av.h)) %>%          #TODO brkt function is not giving exact values, rounds
  mutate(Estimates= ifelse(variable=="BCG doses",av.m, Estimates))%>%
  select(Country=iso3, `BCG effect`= variable, Estimates)

fwrite(top10_cntrs, file = here("outdata/top10_cntrs.csv"))






