## README

# This script creates the plots in the manuscript

rm(list = ls())
pacman::p_load(here,data.table, dplyr, tidyr, stringr, 
               flextable, officer,kableExtra,ggplot2, ggrepel, patchwork)


load(here("outputs/ceacq.RData"))   # CEAC quantile by cntry
load(here("outputs/CEAAs.RData"))   # ENB, ICER, buffer, predictors 
load(here("outputs/CEA.RData"))
## for sensitivity analysis plots
load(here("outputs/CEA_sens.RData"))
load(here("outputs/cost_eff.RData"))
cost_eff <- fread(here("outputs/sens.csv"))

# ====== box-and-whiskers CEAC plot=====
ggplot(ceacq, aes(iso3,
                  ymin = q2.5, lower = q25,
                  middle = q50, upper = q75, ymax = q97.5)) +
  geom_boxplot(stat = "identity") +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  facet_wrap(~g_whoregion, scales = "free") +
  theme_linedraw() +
  theme(
    legend.position = "top",
    legend.box.spacing = unit(0, "pt"), # no gap between legend and plot
    legend.margin = margin(0, 0, 0, 0), # no internal padding in legend
    plot.margin = margin(0, 5, 5, 5)
  ) +
  xlab("Country ISO3 code") +
  ylab("CEAC quantiles (USD/DALY)")

ggsave(file = here("plots/ceac_iso3.png"), w = 9, h = 8)

# ====== scatter plots=====

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

ggsave(file = here("plots/f_gdp_icer.png"), w = 9, h = 8)


ggplot(CEAAs, aes(GDP,ICER/GDP, label = iso3) )+
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
  labs(title = "Relation between GDP and ICER/GDP ratio") + 
  xlab("GDP (USD)") + ylab("ICER/GDP ratio") +theme_linedraw()

ggsave(file = here("plots/f_gdp_vs_icer2gdp_ratio.png"), w = 9, h = 8)

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

ggsave(file = here("plots/f_inc_icer.png"), w = 9, h = 8)


ggplot(CEAAs, aes(incbest,ICER/GDP, label = iso3) )+
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
  labs(title = "Relation between TB incidence and ICER/GDP ratio") +
  xlab("Per capita TB incidence") + ylab("ICER/GDP ratio")+ theme_linedraw()

ggsave(file = here("plots/f_inc_vs_icer2gdp_ratio.png"), w = 9, h = 8)



ggplot(CEAAs%>%filter(z_score>0), aes(GDP,Bf2, label = iso3) )+
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

ggsave(file = here("plots/f_gdp_buffsz.png"), w = 9, h = 8)


ggplot(CEAAs%>%filter(z_score>0), aes(incbest,Bf2, label = iso3) )+
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
  xlab("Per capita TB incidence") + ylab("Buffer size (10% CV)")+ 
  theme_linedraw()

ggsave(file = here("plots/f_inc_buffsz.png"), w = 9, h = 8)


#====== threshold incidence==============

tempa <- CEAAs%>%
  mutate(CF=ifelse(ENB30>0,"Cost-effective", "Not cost-effective"))
# Compute overlap
NCE_min <- tempa%>%filter(CF=="Not cost-effective")%>%summarise(value=min(1e5*incbest))%>% pull(value)
NCE_max <-  tempa%>%filter(CF=="Not cost-effective")%>%summarise(value=max(1e5*incbest))%>% pull(value)

CE_min <- tempa%>%filter(CF=="Cost-effective")%>%summarise(value=min(1e5*incbest))%>% pull(value)
CE_max <-  tempa%>%filter(CF=="Cost-effective")%>%summarise(value=max(1e5*incbest))%>% pull(value)
overlap_min <- max(NCE_min,CE_min)
overlap_max <- min(NCE_max,CE_max)

inc_prop <- tempa%>%
  mutate(incbest= 1e5*incbest)%>%
  mutate(inc_cat=case_when(incbest<overlap_min ~"low",
                           (incbest>=overlap_min & incbest<=overlap_max) ~"medium",
                           incbest>overlap_max ~"higher"))%>%
  group_by(CF, inc_cat) %>%
  summarise(n_countries = n()) %>% ungroup()%>%
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
           label = values_to_annotate[3], color = "black", size = 3, fontface = "plain") + 
  # 2. NCE bar (2)
  annotate("text", x = x_center_overlap, y = 1.0, 
           label = values_to_annotate[4], color = "black", size = 3, fontface = "plain") + 
  # 3. CE bar(1)
  annotate("text", x = x_center_overlap, y = 2.0, 
           label = values_to_annotate[2], color = "black", size = 3, fontface = "plain") + 
  
  # 4. CE bar(2) 
  annotate("text", x = x_center_ce_only, y = 2.0, 
           label = values_to_annotate[1], color = "black", size = 3, fontface = "plain") + 
  
  scale_x_log10() + 
  scale_y_continuous(breaks = c(1, 2), labels = c("Not cost-effective", "Cost-effective")) +
  xlab("TB incidence per 100,000 population") +
  ylab("") + 
  
  geom_vline(xintercept = NCE_max, color = "black", linetype = "dashed", linewidth = 0.2) +
  annotate("text", x = NCE_max, y = 2.5, label = round(NCE_max, 1), 
           color = "black", angle = 90, vjust = -0.5, hjust = 0, size=2.8) +
  
  geom_vline(xintercept = CE_min, color = "navy", linetype = "dashed", linewidth = 0.2) +
  annotate("text", x = CE_min, y = 2.5, 
           label = round(CE_min, 1), 
           color = "black", angle = 90, vjust = -0.5, hjust = 0, size=2.8) +
  
  theme_linedraw() +
  coord_cartesian(ylim = c(0.5, 2.7), expand = FALSE)


b <-tempa %>%
  mutate(log_incbest = 1e5 * incbest) %>%   # scale first
  ggplot(aes(x = CF, y = log_incbest, fill = CF)) +
  geom_boxplot(width = 0.6, alpha = 0.5, outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  scale_y_continuous(trans = "log10") +   
  labs(x = "CF", y = "Incidence per 100,000") + theme_linedraw()+ 
  theme(legend.position = "none")+xlab("")


c <- a+b+plot_annotation(tag_levels = 'a')

ggsave(c,file = here("plots/f_inc_threshold.png"), w = 7, h = 3.2)



##=====sensitivity analysis plots=====

bar_mean <- ggplot(CEA_sens %>% filter(who_region == "Global"),
                   # aes(x = reorder(model, mean), y = mean, fill = model)) +
                   aes(x =model, y = mean, fill = model)) +
  geom_col() +
  facet_wrap(~variable, scales = "free") +
  ylab("Estimates (mean)") +
  theme_bw() +
  theme(
    #legend.position = "bottom",
    legend.position = "none",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 10)
  )

bar_mid <- ggplot(CEA_sens %>% filter(who_region == "Global"),
                  aes(x = model, y = mid, fill = model)) +
  geom_col() +
  facet_wrap(~variable, scales = "free") +
  ylab("Estimates (median)") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.5, "lines"),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 10)
  )

pp <- bar_mean/bar_mid

ggsave(pp,file = here("outputs/f_sensitivity.png"), w = 6, h = 3.5)


bar_mid <- ggplot(CEA_sens %>% filter(!who_region =="Global", iso3=="XX"),
                  aes(x = model, y = mid, fill = model)) +
  geom_col() +
  facet_wrap(who_region~variable, scales = "free", 
             labeller = labeller(.multi_line = FALSE)) +
  ylab("Estimates (median)") +
  theme_linedraw() +
  theme(
    strip.text = element_text(size = 7),
    legend.key.size = unit(0.5, "lines"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 8)
  )

ggsave(bar_mid,file = here("outputs/f_sensitivity_r.png"), w = 6.5, h = 3.5)




