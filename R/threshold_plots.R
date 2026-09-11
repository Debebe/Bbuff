library(here)
library(data.table)
library(dplyr)

load(here("tmpdata/PSA.RData"))
gdp_inc_le_costs <- readRDS(here("outdata/gdp_inc_le_costs.rds"))
load(here("indata/whokey.Rdata"))     # region and cntry iso codes
source("R/utilities/utilities.R")


tbinc <- fread("indata/TB_burden_countries_2025-10-29.csv") %>%
  dplyr::filter(year==2023) %>%
  dplyr::select(iso3, year,inc_all=e_inc_100k,
                e_inc_100k_lo, e_inc_100k_hi,
                notif_all=c_newinc_100k,
                c_cdr, c_cdr_lo, c_cdr_hi)%>%
  as.data.table()


CEA_all <- D[, .(
  ## expected net benefit at WTP=30%GDP
  ENB30 = mean(
    0.3 * GDP * (rslt_health_sq - rslt_health_cf) -
      (rslt_cost_sq - rslt_cost_cf)),
  ENB50 = mean(
    0.5 * GDP * (rslt_health_sq - rslt_health_cf) -
      (rslt_cost_sq - rslt_cost_cf)),
  
  ENB100 = mean(
    1 * GDP * (rslt_health_sq - rslt_health_cf) -
      (rslt_cost_sq - rslt_cost_cf)),
  
  
  GDP = mean(GDP),
  ## g
  bcg_cov = unique(bcg_coverage),
  # g = ENB30 / bcg_cov,
  ## ICER
  ICER = mean(rslt_cost_sq - rslt_cost_cf) /
    mean(rslt_health_sq - rslt_health_cf), 
  u = mean(0.75 * uc_labor_ave +
             uc_sc_ave +
             uc_capital_ave +
             ucost_proc_bcg)
),
by = iso3
]


CEA_all <- na.omit(CEA_all)

thresholds <- c(0.3, 0.5, 1.0)  

CEA_all <- CEA_all%>%
  crossing(threshold = thresholds)%>%
  mutate(legend_label = paste0(threshold," GDP"))%>% 
  mutate(legend_label= factor(legend_label,
                              levels = c("0.3 GDP", "0.5 GDP", "1 GDP")))|>
  dplyr::mutate(ICER_val = GDP * threshold,
                ICER_Label= ifelse(ICER < 0.3 * GDP,"ICER < 0.3 GDP", "ICER >= 0.3 GDP" )) %>%
  left_join(gdp_inc_le_costs%>%
              filter(cov_cat=="WUENIC")%>%
              dplyr::select(iso3, inc_u5=incbest,
                            notif_u5=notif,
                            BCG=bcg_coverage,CDR=cdr,
                            ucvax=uc_tot_vax_delv_ave,
                            uctb=ucost_dstb.m, 
                            uctbm=ucost_tbm.m),
            by= "iso3")%>%
  inner_join(tbinc, by="iso3") %>%
  mutate(inc_u5=1e5*inc_u5,
         notif_u5=1e5*notif_u5)|>
  as.data.table()


CEA_all[, ICER30:= ifelse(ICER<0.3*GDP,"CE", "NCE")]
CEA_all[, ICER50:= ifelse(ICER<0.5*GDP,"CE", "NCE")]
CEA_all[, ICER100:= ifelse(ICER<GDP,"CE", "NCE")]

CEA_unique <- CEA_all%>%distinct(iso3, .keep_all = TRUE)|>as.data.table()

table(CEA_unique$ICER30)
table(CEA_unique$ICER50)
table(CEA_unique$ICER100)

tempa30 <- CEA_all%>%
 
  filter(threshold==0.3)|>
  mutate(CE=ifelse(ENB30>0,"Cost-effective", "Not cost-effective"))

tempa50 <- CEA_all%>%
  filter(threshold==0.5)|>
  mutate(CE=ifelse(ENB50>0,"Cost-effective", "Not cost-effective"))

tempa100 <- CEA_all%>%
  filter(threshold==1)|>
  mutate(CE=ifelse(ENB100>0,"Cost-effective", "Not cost-effective"))



NCE_min <-tempa30 %>%
  filter(CE == "Not cost-effective") %>%
  summarise(value = min(inc_u5, na.rm = TRUE)) %>%
  pull(value)

NCE_max <-  tempa30%>%
  filter(CE=="Not cost-effective")%>%
  summarise(value=max(inc_u5,na.rm = TRUE ))%>% 
  pull(value)

CE_min <- tempa30%>%
  filter(CE=="Cost-effective")%>%
  summarise(value=min(inc_u5,na.rm = TRUE ))%>% 
  pull(value)
CE_max <-  tempa30%>%
  filter(CE=="Cost-effective")%>%
  summarise(value=max(inc_u5,na.rm = TRUE ))%>% 
  pull(value)
labels <- "Incidence (U5) /100,000"

overlap_min <- max(NCE_min,CE_min)
overlap_max <- min(NCE_max,CE_max)

inc_prop <- tempa30%>%
  mutate(inc_cat=case_when(inc_u5<overlap_min ~"low",
                           (inc_u5>=overlap_min & inc_u5<=overlap_max) ~"medium",
                           inc_u5>overlap_max ~"higher"))%>%
  group_by(CE, inc_cat) %>%
  summarise(n_countries = n(), .groups = "drop") %>% ungroup()%>%
  mutate(prop_of_total = (n_countries / sum(n_countries)) * 100) %>%
  ungroup()%>%as.data.table()

values_to_annotate <- paste0(round(inc_prop$prop_of_total,1), "%")

# --- find log scale centers ---
# Non-overlapping NCE bar (NCE_min to CE_min): x-center is the geometric mean
x_center_nce_only <- 10^((log10(NCE_min) + log10(CE_min)) / 2)
# Overlapping bar (CE_min to NCE_max): x-center is the geometric mean
x_center_overlap <- 10^((log10(CE_min) + log10(NCE_max)) / 2)
# non-overlapping CE bar (NCE_max to CE_max): x-center is the geometric mean
x_center_ce_only <- 10^((log10(NCE_max) + log10(CE_max)) / 2)

a <-ggplot() +
  # Not cost-effective range 
  geom_rect(aes(xmin = NCE_min, xmax = NCE_max, 
                ymin = 0.5, ymax = 1.5), fill = "red", alpha = 0.5) +
  # Cost-effective range
  geom_rect(aes(xmin = CE_min, xmax = CE_max, ymin = 1.5, 
                ymax = 2.5), fill = "green", alpha = 0.5) +
  # Overlapping region - CE_min to NCE_max
  geom_rect(aes(xmin = pmax(NCE_min, CE_min), xmax = pmin(NCE_max, CE_max), 
                ymin = 0.5, ymax = 2.5),
            fill = "purple", alpha = 0.3) +
  
  #======annotate labels=====

# 1. NCE bar (1)--red bar
annotate("text", x = x_center_nce_only, y = 1.0, 
         label = values_to_annotate[3], color = "black", size = 2.5, fontface = "plain") + 
  # 2. NCE bar (2)
  annotate("text", x = x_center_overlap, y = 1.0, 
           label = values_to_annotate[4], color = "black", size = 2.5, fontface = "plain") + 
  # 3. CE bar(1)
  annotate("text", x = x_center_overlap, y = 2.0, 
           label = values_to_annotate[2], color = "black", size = 2.5, fontface = "plain") + 
  
  # 4. CE bar(2) 
  annotate("text", x = x_center_ce_only, y = 2.0, 
           label = values_to_annotate[1], color = "black", size = 2.5, fontface = "plain") + 
  
  scale_x_log10() + 
  scale_y_continuous(breaks = c(1, 2), labels = c("Not cost-effective", "Cost-effective")) +
  xlab(labels) +
  ylab("") + 
  
  geom_vline(xintercept = NCE_max, color = "black", linetype = "dashed", linewidth = 0.2) +
  annotate("text", x = NCE_max, y = 2.5, label = round(NCE_max, 1), 
           color = "black", angle = 90, vjust = -0.5, hjust = 0, size=2.5) +
  
  geom_vline(xintercept = CE_min, color = "navy", linetype = "dashed", linewidth = 0.2) +
  annotate("text", x = CE_min, y = 2.5, 
           label = round(CE_min, 1), 
           color = "black", angle = 90, vjust = -0.5, hjust = 0, size=2.5) +
  
  theme_linedraw() + 
  coord_cartesian(ylim = c(0.5, 2.7), expand = FALSE)+
  theme(axis.title = element_text(size = 9),
        axis.text =  element_text(size = 8.5))


b <-tempa30 %>%
  ggplot(aes(x = CE, y = inc_u5, fill = CE)) +
  geom_boxplot(width = 0.6, alpha = 0.5, outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  scale_y_continuous(trans = "log10") +   
  labs(#x = "CE", 
       y = labels) + theme_linedraw()+ 
  theme(legend.position = "bottom",
        axis.title = element_text(size = 9),
        axis.text.y =  element_text(size = 8.5),
        axis.text.x = element_blank())+ #used to be 8.5
  xlab("")

c <-tempa50 %>%
  ggplot(aes(x = CE, y = inc_u5, fill = CE)) +
  geom_boxplot(width = 0.6, alpha = 0.5, outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  scale_y_continuous(trans = "log10") +   
  labs(#x = "CE", 
       y = labels) + theme_linedraw()+ 
  theme(legend.position = "bottom",
        axis.title = element_text(size = 9),
        axis.text.y =  element_text(size = 8.5),
        axis.text.x = element_blank())+ #used to be 8.5
  xlab("")

d <-tempa100 %>%
  ggplot(aes(x = CE, 
             y = inc_u5, fill = CE)) +
  geom_boxplot(width = 0.6, alpha = 0.5, outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  scale_y_continuous(trans = "log10") +   
  labs(#x = "CE", 
       y = labels) + 
  theme_linedraw()+ 
  theme(legend.position = "bottom",
        axis.title = element_text(size = 9),
        axis.text.y =  element_text(size = 8.5),
        axis.text.x = element_blank())+ #used to be 8.5
  xlab("")



e <- a + b + c + d +
  plot_layout(
    ncol = 4,
    guides = "collect"
  ) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 12, face="bold"),
    legend.position = "bottom", legend.title = element_blank()
  )


ggsave(e,file = here("plots/FS11_more_thresholds.png"), w = 8, h = 3.4) # incU5
