rm(list = ls())
# load relevant packages
pacman::p_load(here,data.table, dplyr, tidyr, stringr, 
               flextable, officer,kableExtra,ggplot2, ggrepel, patchwork)

load(here("outputs/PSA.RData"))     # full PSA data with results. 
load(here("outputs/CEA.RData"))     # full PSA data with results
gdp_inc_le_costs <- readRDS(here("data/gdp_inc_le_costs.rds"))
load(here("data/whokey.Rdata"))     # region and cntry iso codes


source("R/utilities/utilities.R") # to use brkt function

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

save(ceac_input, file = here("outputs/ceac_input.RData"))


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


save(ceacq, file=here("outputs/ceacq.RData"))



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
  
 save(avrt_table, file= here("outputs/avrt_table.RData"))
 save(avert_deaths, file= here("outputs/avert_deaths.RData"))
 

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

fwrite(top10_cntrs, file = here("outputs/top10_cntrs.csv"))


##=========outputs by who regions==========





##====other scatter plots======


# sens <- inner_join(
#   cntrs_cost_eff%>%select(model,prop_ce),
#   CEA_sens%>%filter(who_region=="Global")%>%
#     select(model, variable, mean, mid, q25,q75), by="model")%>%
#   select(-who_region)%>%
#   mutate("mid(IQR)"= brkt(mid,q25, q75))
# 
# fwrite(sens, file = here("outputs/sens.csv"))


# inc <-gdp_inc_le_costs%>%distinct(iso3, .keep_all = TRUE)
# 
# total_inc <- sum(inc$best)
# prop_tbm <-0.035
# death_risk <- 0.193
# survi_seq <- 0.539
# total_deaths <- total_inc*prop_tbm*death_risk
# surv_neur_seq <- total_inc*prop_tbm*survi_seq
