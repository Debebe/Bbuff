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

load(here("outputs/ceac_input.RData"))


avrt_table <- avrt_table%>%
  filter(variable%in% c("rslt_att","rslt_bcg_doses","rslt_inc",
                        "rslt_tbminc","rslt_tb_deaths","rslt_ly_tb"))%>%
  group_by(iso3, variable)%>%
  mutate(av=ifelse(variable=="rslt_bcg_doses", abs(av),av))%>%
  summarise(av.m= mean(av,na.rm=TRUE),
            av.l=quantile(av,0.025, na.rm=TRUE),
            av.h=quantile(av,0.975, na.rm=TRUE))%>%
  mutate(variable= case_when(variable=="rslt_att" ~"Averted ATT",
                             variable=="rslt_bcg_doses" ~"BCG doses",
                             variable=="rslt_inc" ~"Averted TB incidence",
                             variable=="rslt_tbminc" ~"Averted TBM incidence",
                             variable=="rslt_ly_tb" ~"Averted DALYs",
                             variable=="rslt_tb_deaths" ~"Averted deaths"))
  
 
# select top 10 cntrs by averted death 
top10 <- avrt_table %>%
  filter(variable == "Averted deaths") %>%
  arrange(desc(av.m)) %>%
  head(10) 

# subset top ten cntrs with other variables as well
top10_cntrs <-avrt_table %>%
  filter(iso3%in%top10$iso3) %>%
  left_join(
    avrt_table %>%
      filter(variable == "Averted deaths") %>%
      select(iso3, death_value = av.m),
    by = "iso3"
  ) %>%
  arrange(desc(death_value))%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))%>%
  mutate(Estimates= paste(av.m,"(",av.l ,"to",av.h, ")"))%>%
  mutate(Estimates= ifelse(variable=="BCG doses",av.m, Estimates))%>%
  select(Country=iso3, `BCG effect`= variable, Estimates)

fwrite(top10_cntrs, file = here("outputs/top10_cntrs.csv"))


##=========outputs by who regions==========


keep <- grep("rslt", names(D), value = TRUE)
keep <- c("who_region","iso3", "iter", "Pop", unique(keep))

output_table <- D[, ..keep]
output_table <- melt(output_table, id = c("iter", "iso3","who_region", "Pop"))
output_table[, type := ifelse(grepl("cf", variable), "cf", "sq")]
output_table[, variable := gsub("rlst_", "", variable)]
output_table[, variable := gsub("_cf|_sq", "", variable)]
output_table[, value := value * Pop]
output_table <- dcast(output_table,
                      iter + iso3 + variable + who_region~ type,
                      value.var = "value")


## averted
output_table[, av := cf - sq]

## global TODO NaNs?
output_table <- output_table[is.finite(av), .(
  cf = sum(cf), sq = sum(sq), av = sum(av)),
by = .(iter, variable, who_region)]

## hi/lo & reshape
eps <- 0.025
output_table <- melt(output_table,
                     id = c("iter","who_region", "variable")
) # TODO better var names

output_table <- output_table[, .(
  mid = mean(value), lo = quantile(value, eps), hi = quantile(value, 1 - eps)
), by = .(who_region,variable, variable.1)]

output_table <- dcast(output_table,
                      who_region+variable ~ variable.1,
                      value.var = c("mid", "lo", "hi"))

## change units to millions for cost and health, BCG dosese
fac <- 1e6
output_table[
  variable %in% c("rslt_att_cost", "rslt_cost",
                  "rslt_bcg_doses", "rslt_health"),
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

fwrite(output_table, file = here("outputs/output_table_who.csv"))


###====other scatter plots======

CEAAs <- CEAA%>%
  filter(threshold==0.3)%>%
  inner_join(gdp_inc_le_costs%>%filter(cov_cat=="WUENIC")%>%
               select(iso3, incbest,BCG=bcg_coverage,CDR=cdr,
                      ucvax=uc_tot_vax_delv_ave,
                      uctb=ucost_dstb.m, 
                      uctbm=ucost_tbm.m),
             by= "iso3")

library(ggrepel)
ggplot(CEAAs, aes(GDP,ICER, label = iso3) )+
  geom_point(colour="red") +facet_wrap(~region, scales="free", ncol=2)+ 
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
  labs(title = "Relation between GDP and ICER") + 
  xlab("GDP (USD)") + ylab("ICER") +theme_linedraw()

ggsave(file = here("outputs/f_gdp_icer.png"), w = 9, h = 8)



ggplot(CEAAs, aes(incbest,ICER, label = iso3) )+
  geom_point(colour="red") +facet_wrap(~region, scales="free", ncol=2)+ 
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
  labs(title = "Relationship between TB incidence and ICER") +
  xlab("Per capita TB incidence") + ylab("ICER")+ theme_linedraw()

ggsave(file = here("outputs/f_inc_icer.png"), w = 9, h = 8)


ggplot(CEAAs, aes(GDP,Bf2, label = iso3) )+
  geom_point(colour="red") +facet_wrap(~region, scales="free", ncol=2)+ 
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
  labs(title = "Relationship between GDP and Buffer size") +
  xlab("GDP (USD)") + ylab("Buffer size (10% CV)")+ theme_linedraw()

ggsave(file = here("outputs/f_gdp_buffsz.png"), w = 9, h = 8)


ggplot(CEAAs, aes(incbest,Bf2, label = iso3) )+
  geom_point(colour="red") +facet_wrap(~region, scales="free", ncol=2)+ 
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
  labs(title = "Relationship between TB incidence and Buffer size") +
  xlab("Per capita TB incidence") + ylab("Buffer size (10% CV)")+ theme_linedraw()

ggsave(file = here("outputs/f_inc_buffsz.png"), w = 9, h = 8)




## regression 
tmp <- CEAAs %>%
  mutate(across(c(ENB30,ICER, GDP, incbest,CDR,BCG, ucvax, uctb, uctbm), ~ as.numeric(scale(.))))
# predctors of ENB30
lms <- lm(ENB30 ~ GDP +incbest +CDR+BCG+ ucvax+uctb, tmp)
summary(lms)
coefs <- summary(lms)$coefficients[, c("Estimate", "Pr(>|t|)")]
cis <- confint(lms)

summary(lms)
# Combine into one table
reg_ENB30 <- cbind(coefs, cis)%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(Predictors=rowname,Pvalue= "Pr(>|t|)")%>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))%>%
  mutate(`Estimate (95%CI)`= paste0(Estimate, " (",`2.5 %`, " , ",`97.5 %`, ")" ))%>%
  select(Predictors,`Estimate (95%CI)`,Pvalue)

fwrite(reg_ENB30, file = here("outputs/regression_ENB.csv"))

#predictors of icer
lms <- lm(ICER ~ GDP +incbest +CDR+BCG+ ucvax+uctb, tmp)
coefs <- summary(lms)$coefficients[, c("Estimate", "Pr(>|t|)")]
cis <- confint(lms)

# Combine into one table
reg_icer <- cbind(coefs, cis)%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(Predictors=rowname,Pvalue= "Pr(>|t|)")%>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))%>%
  mutate(`Estimate (95%CI)`= paste0(Estimate, " (",`2.5 %`, " , ",`97.5 %`, ")" ))%>%
  select(Predictors,`Estimate (95%CI)`,Pvalue)

fwrite(reg_icer, file = here("outputs/regression_icer.csv"))


# ==summary statistics===


summary_tab <- data.frame(variable= "Number of countries",
           value= length(unique(CEAAs$iso3)),
           Description = "number of countries")%>%
  bind_rows(CEAAs%>%
              group_by(variable=region)%>%
              summarise(value= length(unique(iso3)))%>%
              mutate(Description = "number of countries"))%>%
  mutate(value= as.character(value))%>%
  bind_rows(CEAAs%>%
              mutate(variable= "ICER-median")%>%
              group_by(variable)%>%
              mutate(ICER=round(ICER, 0))%>%
              summarise(value= paste0(median(ICER), "(IQR =", IQR(ICER), ")")) %>%
              mutate(Description = "Median global ICER with IQR")) %>%
  bind_rows(CEAAs%>%
              mutate(variable= "ICER-mean")%>%
              mutate(ICER=round(ICER, 0))%>%
              group_by(variable)%>%
              summarise(value= paste0(mean(ICER), "( sd=", sd(ICER), ")")) %>%
              mutate(Description = "Mean global ICER with sd")) %>%

  bind_rows(CEAAs%>%
              group_by(variable=region)%>%
              mutate(ICER= round(ICER, 0))%>%
              summarise(value= paste0(median(ICER),"(IQR=", IQR(ICER),")"))%>%
              mutate(Description = "Median regional ICER with IQR")) %>%
 #mutate(value= as.character(value))%>%
  bind_rows(
  CEAAs%>%
  mutate(variable="Buffer")%>%
  group_by(variable)%>%
  summarise(value= paste0(median(Bf2, na.rm = TRUE),
                          "(IQR=", IQR(Bf2, na.rm = TRUE), ")")) %>%
  mutate(Description = "Median global Buffer with IQR")
  )%>%
  bind_rows(
  
CEAAs%>%
  group_by(variable=region)%>%
  group_by(variable)%>%
  summarise(value= paste0(median(Bf2, na.rm = TRUE),
                          "(IQR=", IQR(Bf2, na.rm = TRUE), ")")) %>%
  mutate(Description = "Median regional Buffer size with IQR"))




fwrite(summary_tab, file = here("outputs/statistics.csv"))




