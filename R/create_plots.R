## README

# This script creates the plots in the manuscript

rm(list = ls())
pacman::p_load(here,data.table, dplyr, tidyr, stringr, 
               flextable, officer,kableExtra,ggplot2, 
               ggrepel, patchwork, readxl)

#=====load data=====
load(here("outdata/ceacq.RData"))   # CEAC quantile by cntry
load(here("outdata/CEA.RData"))
## for sensitivity analysis plots
load(here("outdata/CEA_sens.RData"))
load(here("outdata/cost_eff.RData"))
load(here("outdata/CEA_cntr.RData"))
load(here("indata/whokey.RData"))
source("R/utilities/utilities.R")


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


ggsave(file = here("plots/F3.png"), w = 9, h = 8)


## buffer plot

tmp <- CEA[ENB30 > 0 & z_score > 0]
summary(tmp)
tmpm <- melt(tmp[, .(iso3, region, GDP, Bf1, Bf2, Bf3)],
             id = c("iso3", "region", "GDP")
)
tmpm[, var := fcase(
  variable == "Bf1", "CV = 5%",
  variable == "Bf2", "CV = 10%",
  variable == "Bf3", "CV = 15%"
)]
tmpm$var <- factor(tmpm$var,
                   levels = c("CV = 5%", "CV = 10%", "CV = 15%"),
                   ordered = TRUE
)


ggplot(tmpm, aes(reorder(iso3, value), value,color=var)) + # ordering values
  geom_point() + ylim(0, NA) +
  coord_flip() +
  facet_wrap(~region, scales = "free") +
  theme_linedraw() +
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top",
        legend.box.spacing = unit(0, "pt"),   # no gap between legend and plot
        legend.margin = margin(0, 0, 0, 0),   # no internal padding in legend
        plot.margin = margin(0, 5, 5, 5))+
  xlab("Country ISO3 code") +
  ylab("Optimal buffer as proportion of expected demand (%)")+
  theme(legend.position = 'top',legend.title = element_blank()) 

ggsave(file = here("plots/F4.png"), w = 9, h = 8)


# table of buffer size

ft <- CEA %>%
  filter(Bf1 > 0, !is.na(Bf1), ENB30 > 0) %>%
  dplyr::select(
    Region = g_whoregion,
    iso3, ENB = ENB30, ICER, `Buffer 1` = Bf1,
    `Buffer 2` = Bf2, `Buffer 3` = Bf3
  ) %>%
  mutate(
    ENB = round(ENB, 1),
    ICER = round(ICER, 0)
  ) %>%
  as.data.frame() %>%
  make_flextable()

doc <- read_docx() |>
  body_add_flextable(value = ft) |>
  body_add_par(" ", style = "Normal") # optional spacing

print(doc, target = "outdata/Tab_buf_sz.docx")


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

ggsave(file = here("plots/FS10.png"), w = 9, h = 8)


#====== threshold incidence==============

#******************************************#
#===== CE threshold incidence========
#******************************************#

plot_threshold <- function(indicator) {
  
  
  indicator_name <- rlang::as_name(rlang::ensym(indicator))  # convert to string
  
  if (indicator_name == "inc_u5") {
    labels <- "Incidence (U5) /100,000"
  } else if (indicator_name == "inc_all") {
    labels <- "Incidence (overall) /100,000"
  } else if (indicator_name == "notif_u5") {
    labels <- "Notifications (U5) /100,000"
  } else if (indicator_name == "notif_all") {
    labels <- "Notifications (overall) /100,1000"
  } else {
    labels <- metric_name  # fallback
  }
  
  NCE_min <-tempa %>%
    filter(CE == "Not cost-effective") %>%
    summarise(value = min({{indicator}}, na.rm = TRUE)) %>%
    pull(value)
  
  NCE_max <-  tempa%>%
    filter(CE=="Not cost-effective")%>%
    summarise(value=max({{indicator}},na.rm = TRUE ))%>% 
    pull(value)
  
  CE_min <- tempa%>%
    filter(CE=="Cost-effective")%>%
    summarise(value=min({{indicator}},na.rm = TRUE ))%>% 
    pull(value)
  CE_max <-  tempa%>%
    filter(CE=="Cost-effective")%>%
    summarise(value=max({{indicator}},na.rm = TRUE ))%>% 
    pull(value)
  
  overlap_min <- max(NCE_min,CE_min)
  overlap_max <- min(NCE_max,CE_max)
  
  inc_prop <- tempa%>%
    mutate(inc_cat=case_when({{indicator}}<overlap_min ~"low",
                             ({{indicator}}>=overlap_min & {{indicator}}<=overlap_max) ~"medium",
                             {{indicator}}>overlap_max ~"higher"))%>%
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
    xlab(labels) +
    ylab("") + 
    
    geom_vline(xintercept = NCE_max, color = "black", linetype = "dashed", linewidth = 0.2) +
    annotate("text", x = NCE_max, y = 2.5, label = round(NCE_max, 1), 
             color = "black", angle = 90, vjust = -0.5, hjust = 0, size=2.8) +
    
    geom_vline(xintercept = CE_min, color = "navy", linetype = "dashed", linewidth = 0.2) +
    annotate("text", x = CE_min, y = 2.5, 
             label = round(CE_min, 1), 
             color = "black", angle = 90, vjust = -0.5, hjust = 0, size=2.8) +
    
    theme_linedraw() + 
    coord_cartesian(ylim = c(0.5, 2.7), expand = FALSE)+
    theme(axis.title = element_text(size = 9),
          axis.text =  element_text(size = 8.5))
  
  
  b <-tempa %>%
    mutate(log_incbest = {{indicator}}) %>%   # scale first
    ggplot(aes(x = CE, y = log_incbest, fill = CE)) +
    geom_boxplot(width = 0.6, alpha = 0.5, outlier.shape = 16, outlier.size = 2) +
    stat_summary(fun = median, geom = "point", size = 3, color = "black") +
    scale_y_continuous(trans = "log10") +   
    labs(x = "CE", y = labels) + theme_linedraw()+ 
    theme(legend.position = "none",
          axis.title = element_text(size = 9),
          axis.text =  element_text(size = 8.5))+
    xlab("")
  
  
  c <- a+b+plot_annotation(tag_levels = 'a')
  c
}


tempa <- CEA%>%
  mutate(inc_u5=1e5*inc_u5,
         notif_u5=1e5*notif_u5)|>
  filter(threshold==0.3)|>
  mutate(CE=ifelse(ENB30>0,"Cost-effective", "Not cost-effective"))

#xx <- tempa %>%select(iso3, ENB30,CDR,inc_u5, notif_u5,inc_all, notif_all,CE)

# p1<-plot_threshold(notif_all)
# p2<-plot_threshold(inc_all)
# p3<-plot_threshold(notif_u5)
p4<-plot_threshold(inc_u5)

# ggsave(p1,file = here("plots/f_threshold_allnotif.png"), w = 7, h = 3.2)
# ggsave(p2,file = here("plots/f_threshold_allinc.png"), w = 7, h = 3.2)
# ggsave(p3,file = here("plots/f_threshold_u5notif.png"), w = 7, h = 3.2)
ggsave(p4,file = here("plots/FS11.png"), w = 7, h = 3.2) # incU5


 
CEA_sens%>%
  filter(who_region=="Global") %>%
  ggplot(aes(model,mid, fill=model))+
  geom_bar(stat = "identity", position=position_dodge())+
  facet_wrap(~metric, scales="free")+
  scale_fill_manual(values =c("Maroon", "lightblue", "Sandy Brown", "pink"))+
  theme_linedraw()+ theme(legend.title = element_blank(),
                          legend.position = "bottom",
                          axis.title.x = element_blank(),
                          axis.title.y = element_text(size=10),
                          axis.text.x = element_text(angle=90),
                          
                          legend.key.size = unit(0.5, "lines")) + 
  ylab("Estimates")
  
#******************************************#
#=====sensitivity analysis plots========
#******************************************#


sz_ytitle <- 10
sz_legend <- 8
sz_facet <- 8


  
bar_mid <- ggplot(CEA_sens %>% 
                    filter(who_region == "Global"),
                  aes(x = model, y = mid, fill = model)) +
  geom_col() +
  scale_fill_manual(values =c("Maroon", "lightblue", "Sandy Brown", "pink"))+
  
  facet_wrap(~metric, scales = "free") +
  ylab("Estimates (median)") +
  theme_linedraw() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.5, "lines"),
    legend.title = element_blank(),
    legend.text=element_text(size=sz_legend),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=sz_ytitle),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 10)
  )


#ggsave(bar_mid,file = here("plots/f_bar_sensitivity.png"), w = 6, h = 3.5)


bar_mid_reg <- ggplot(CEA_sens %>% filter(!who_region =="Global", iso3=="XX",  
                                          metric=="ICER"),
                  aes(x = model, y = mid, fill = model)) +
  geom_col() +
  facet_wrap(~who_region, scales = "free", 
             labeller = labeller(.multi_line = FALSE)) +
  scale_fill_manual(values =c("Maroon", "lightblue", "Sandy Brown", "pink"))+
  
  ylab("ICER (median)") +
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

#ggsave(bar_mid_reg,file = here("plots/f_bar_sensitivity_r.png"), w = 6.5, h = 3.5)



ice_incr <-CEA_sens %>% filter(who_region =="Global", 
                        iso3=="XX", metric=="ICER")%>%
  mutate(p= 100+round(100*(mid-mid[model=="PostTB(Y),TBM(Y)"])/mid[model=="PostTB(Y),TBM(Y)"], 1))%>%
  ggplot(aes(x = reorder(model,p), y = p, fill = model)) +
  geom_col() +
  # facet_wrap(who_region~variable, scales = "free", 
  #            labeller = labeller(.multi_line = FALSE)) +
  # facet_grid(~metric, scales = "free", 
  #            labeller = labeller(.multi_line = FALSE)) +
  # exclude labels from first bar
  geom_text(aes(label = ifelse(model == "PostTB(Y),TBM(Y)", NA, paste0(round(p-100, 1), "%"))),
    position = position_dodge(width = 0.8),
    vjust = -0.1,
    size = 3, na.rm = TRUE)+
  ylab("Percentage increase in median ICER") +
  geom_hline(yintercept = 100, linetype="dashed",col="gray", lwd=0.5)+
  theme_linedraw() +
  scale_fill_manual(values =c("Maroon", "lightblue", "Sandy Brown", "pink"))+
  theme(
    strip.text = element_text(size = sz_facet),
    legend.key.size = unit(0.5, "lines"),
    legend.position = "none",
    legend.text=element_text(size=sz_legend),
    
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=8),
    axis.title.y = element_text(size=sz_ytitle),
    
    plot.title = element_text(size = 8)
  )

#ggsave(ice_incr,file = here("plots/f_bar_perc_incr_icer.png"), w = 5, h = 3.5)




enb_decr <-CEA_sens %>% filter(who_region =="Global", 
                               iso3=="XX", metric=="ENB30") %>%

  mutate(p= 100*round((mid-mid[model=="PostTB(Y),TBM(Y)"])/mid[model=="PostTB(Y),TBM(Y)"], 3)) %>%
  ggplot(aes(x = reorder(model,p, decreasing=TRUE), y = p, fill = model)) +
  geom_col() +
  # facet_wrap(who_region~variable, scales = "free", 
  #            labeller = labeller(.multi_line = FALSE)) +
  #facet_wrap(~metric, scales = "free", 
             #labeller = labeller(.multi_line = FALSE)) +
  # exclude labels from first bar
  geom_text(aes(label = ifelse(model == "PostTB(Y),TBM(Y)", NA, paste0(round(p, 2), "%"))),
            #label =paste0(round(p, 2), "%")),
            position = position_dodge(width = 0.5),
            vjust = -0.1,
            size = 3, na.rm = TRUE)+
  ylab("Percentage decrease in median ENB") +
  geom_hline(yintercept = 0, linetype="dashed",col="gray", lwd=0.7)+
  theme_linedraw() +
  scale_fill_manual(values =c("Maroon", "lightblue", "Sandy Brown", "pink"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + #add a little gap above max mar
  
  theme(
    strip.text = element_text(size = sz_facet),
    legend.key.size = unit(0.5, "lines"),
    legend.text=element_text(size=sz_legend),
    
    legend.position = "none",
    
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=7),
    axis.title.y = element_text(size=sz_ytitle),
    
    plot.title = element_text(size = 8)
  ) 

#ggsave(enb_decr,file = here("plots/f_bar_perc_dec_ENB.png"), w = 4.5, h = 3.5)


ice_incr_r <-CEA_sens %>% filter(#who_region!="Global", 
                               iso3=="XX", metric=="ICER")%>%
  mutate(p= 100+round(100*(mid-mid[model=="PostTB(Y),TBM(Y)"])/mid[model=="PostTB(Y),TBM(Y)"], 1))%>%
  mutate(who_region= factor(who_region, levels=c("Global", "AFR", "AMR","EMR","EUR", "SEA","WPR" )))%>%
  ggplot(aes(x = reorder(model,p), y = p, fill = model)) +
  geom_col() +
  # facet_wrap(who_region~variable, scales = "free",
  #            labeller = labeller(.multi_line = FALSE)) +
  facet_wrap(~who_region,ncol=4, #scales = "free",
             labeller = labeller(.multi_line = FALSE)) +

  # exclude labels from first bar
  geom_text(aes(label = ifelse(model == "PostTB(Y),TBM(Y)", NA, paste0(round(p-100, 1), "%"))),
            position = position_dodge(width = 0.8),
            vjust = -0.1,
            size = 2.5, na.rm = TRUE)+
  ylab("Percentage increase in median ICER") +
  geom_hline(yintercept = 100, linetype="dashed",col="gray", lwd=0.5)+
  theme_linedraw() +
  scale_fill_manual(values =c("Maroon", "lightblue", "Sandy Brown", "pink"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + #add a little gap above max mar
  theme(
    strip.text = element_text(size = sz_facet),
    legend.key.size = unit(0.5, "lines"),
    legend.position = "bottom",
    
    legend.text=element_text(size=sz_legend),
    
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=sz_ytitle),
    
    plot.title = element_text(size = 10)
    )
  

ggsave(ice_incr_r,file = here("plots/FS12.png"), w = 5.5, h = 4.2)

CEAp <- CEA_cntr%>%
  group_by(model)%>%
  summarise(across(c(ICER, ENB30),median))%>%
  mutate(Country="Global")%>%
  bind_rows(
    CEA_cntr%>%
      group_by(Country=who_region, model)%>%
      summarise(across(c(ICER, ENB30),median)))%>%
  pivot_longer(cols = -c(model,Country), 
               values_to = "mid", names_to = "variable")%>%as.data.table()


  
ice_incr <-CEAp %>% filter(Country=="Global", 
                                variable=="ICER") %>%
  group_by(Country)%>%
  mutate(p= 100+round(100*(mid-mid[model=="PostTB(Y),TBM(Y)"])/mid[model=="PostTB(Y),TBM(Y)"], 1)) %>%
  ggplot(aes(x = reorder(model,p), y = p, fill = model)) +
  geom_col() +
  # facet_wrap(who_region~variable, scales = "free",
  #            labeller = labeller(.multi_line = FALSE)) +
 
  # exclude labels from first bar
  geom_text(aes(label = ifelse(model == "PostTB(Y),TBM(Y)", NA, paste0(round(p-100, 1), "%"))),
            position = position_dodge(width = 0.8),
            vjust = -0.1,
            size = 2.5, na.rm = TRUE)+
  ylab("Percentage increase in median ICER") +
  geom_hline(yintercept = 100, linetype="dashed",col="gray", lwd=0.5)+
  theme_linedraw() +
  scale_fill_manual(values =c("Maroon", "lightblue", "Sandy Brown", "pink"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + #add a little gap above max mar
  theme(
    strip.text = element_text(size = 7),
    legend.key.size = unit(0.5, "lines"),
    legend.position = "bottom",
    legend.text=element_text(size=6.5),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=9),
    
    plot.title = element_text(size = 10)
  )


enb_decr <-CEAp %>% filter(Country =="Global", 
                                variable=="ENB30") %>%
  group_by(Country)%>%
  mutate(p= 100*round((mid-mid[model=="PostTB(Y),TBM(Y)"])/mid[model=="PostTB(Y),TBM(Y)"], 3)) %>%
  ggplot(aes(x = reorder(model,p, decreasing=TRUE), y = p, fill = model)) +
  #ggplot(aes(x = model, y = p, fill = model)) +
  geom_col() +

  # facet_wrap(~source, scales = "free", 
  #            labeller = labeller(.multi_line = FALSE)) +
  # exclude labels from first bar
  geom_text(aes(label = ifelse(model == "PostTB(Y),TBM(Y)", NA, paste0(round(p, 2), "%"))),
            #label =paste0(round(p, 2), "%")),
            position = position_dodge(width = 0.5),
            vjust = -0.1,
            size = 3, na.rm = TRUE)+
  ylab("Percentage decrease in median ENB") +
  geom_hline(yintercept = 0, linetype="dashed",col="gray", lwd=0.7)+
  theme_linedraw() +
  scale_fill_manual(values =c("Maroon", "lightblue", "Sandy Brown", "pink"))+
  theme(
    strip.text = element_text(size = 8),
    legend.key.size = unit(0.5, "lines"),
    
    legend.position = "none",
    
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    #axis.text.x = element_text(size=8, angle = 90),
    axis.title.y = element_text(size=9),
    
    plot.title = element_text(size = 8)
  ) 

# ggsave(enb_decr,file = here("plots/f_perc_dec_ENB_comb.png"), w = 4.5, h = 3.5)
# ggsave(ice_incr,file = here("plots/f_perc_inc_ICER_comb.png"), w = 4.5, h = 3.5)
# 


epi <- CEA%>%
  distinct(iso3,.keep_all = TRUE)%>%
  dplyr::select(g_whoregion, region,iso3, inc_all, BCG)%>%
  pivot_longer(cols = c("inc_all", "BCG")) %>%
  mutate(name=ifelse(name=="inc_all", "Incidence per 100,000"," Proportion BCG vaccinated"))%>%
  ggplot(aes(x=g_whoregion, y=value, fill=g_whoregion)) + 
  geom_boxplot()+facet_wrap(~name, scales="free", ncol=1)+
  theme_linedraw()+xlab("") +ylab("")+theme(legend.position = "none",
                                            axis.text.x = element_text(size=7.5),
                                            axis.text.y = element_text(size=7.5),
                                            axis.title.y = element_text(size=sz_ytitle))
  
#ggsave(epi,file = here("plots/f_inc_BCG.png"), w = 3.5, h = 3.5)


bcg <- read_excel("indata/Bacillus Calmette–Guérin (BCG) vaccination coverage 2025-04-03 10-35 UTC.xlsx")

bcg_cov <- bcg%>%
  filter(COVERAGE_CATEGORY=="WUENIC")%>%
  dplyr::select(iso3=CODE, year=YEAR, coverage=COVERAGE)%>%
  inner_join(whokey, by= "iso3") %>%
  filter(year>=2019)%>%
  group_by(iso3, g_whoregion, region)%>%
  mutate(cv= 100*sd(coverage)/mean(coverage))%>%
  group_by(region)%>%
  summarise(m=quantile(cv, 0.5, na.rm=TRUE), l=quantile(cv, 0.25, na.rm=TRUE), u=quantile(cv, 0.75, na.rm=TRUE))



ggplot(bcg_cov, aes(region, m)) +
  geom_point() +
  geom_pointrange(aes(ymin=l, ymax=u))+
  theme_classic() +
    theme(legend.position = "none",
          strip.text = element_text(size=sz_ytitle),
          axis.title.y = element_text(size=sz_ytitle-2.5),
          axis.title.x = element_text(size=sz_ytitle-2.5),
          axis.text.x = element_text(size =sz_ytitle-1),
          axis.text.y = element_text(size =sz_ytitle-2.5)
  )+
  ylab("Coefficient of variation in BCG coverage: median and interquartile range")+xlab("")+coord_flip()
  

ggsave(file = here("plots/FS3.png"), w = 5, h = 3)

#******************************************#
#=====Uncertainties in unit costs========
#******************************************#
## ATT unit-cost plot

uc <- readRDS(file = here("outdata/gdp_inc_le_costs.rds")) %>%
  filter(cov_cat=="WUENIC", !is.na(notif), best>0) %>%
  dplyr::select(iso3,region=who_region,contains("cost"),
         contains("uc"), contains("lo"), contains("hi"), contains("med"),
         contains("sd"), bcg_cov=bcg_coverage )


uc$iso3 <- factor(uc$iso3, levels = unique(uc[order(ucost_dstb.m)]$iso3), ordered = TRUE)

ggplot(uc, aes(iso3, ucost_dstb.m)) +
  geom_point() +
  geom_pointrange(aes(ymin=ucost_dstb.m-ucost_dstb.sd, ymax=ucost_dstb.m+ucost_dstb.sd))+
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  facet_wrap(~region, scales = "free") +
  theme_linedraw()+
  theme(legend.position = "top",
        legend.box.spacing = unit(0, "pt"),   # no gap between legend and plot
        legend.margin = margin(0, 0, 0, 0),   # no internal padding in legend
        plot.margin = margin(0, 5, 5, 5))+
  xlab("Country ISO3 code") +
  ylab(expression("Unit cost of treating TB (" * Mean %+-% SD * ")"))

ggsave(file = here("plots/FS4.png"), w = 9, h = 8)


## cost categories in vaccine delivery unit-costs 

uc$iso3 <- factor(uc$iso3, levels = unique(uc[order(uc_tot_vax_delv_ave)]$iso3), ordered = TRUE)

ggplot(uc, aes(iso3, uc_tot_vax_delv_ave)) +
  geom_point() +
  geom_pointrange(aes(ymin=uc_tot_vax_delv_lo, ymax=uc_tot_vax_delv_hi))+
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  facet_wrap(~region, scales = "free") +
  theme_linedraw()+
  theme(legend.position = "top",
        legend.box.spacing = unit(0, "pt"),   # no gap between legend and plot
        legend.margin = margin(0, 0, 0, 0),   # no internal padding in legend
        plot.margin = margin(0, 5, 5, 5))+
  xlab("Country ISO3 code") +
  ylab("Vaccine delivery unit cost per BCG dose (Mean with 95% confidence interval)")

ggsave(file = here("plots/FS5.png"), w = 9, h = 8)


#******************************************#
#=====Cost components========
#******************************************#
load(here("costdata/outdata/cost_components.RData"))

rx_costs <- inner_join(cost_components,whokey, by="iso3")%>%
  filter(iso3%in% CEA$iso3)

rx_costs%>%
  pivot_longer(cols = starts_with("c_"))%>%
  mutate(name=case_when(name=="c_dstb_opd_tx"~"OPD care",
                        name=="c_dstb_ipd_tx"~"Inpatient care",
                        name=="c_dstb_drugs_tx"~"ATT drugs", 
                        TRUE~"National TB program"))%>%
  group_by(region,iso3)%>%
  mutate(p=value/sum(value))%>%
  ggplot(aes(fill=name, y=p, x=iso3)) + 
  geom_bar(position="fill", stat="identity")+ 
  facet_wrap(~region, scales="free")+ 
  scale_fill_manual(values =c( "lightblue", "Sandy Brown", "pink", "Maroon 2"))+
  
  theme_linedraw()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(0.5, "lines"))+ ylab("")+ xlab("Country ISO3 code")+
  coord_flip()

ggsave(file = here("plots/FS6.png"), w = 9, h = 8)


vax_comps <-inner_join(uc%>%dplyr::select(-region),whokey, by="iso3")%>%
  filter(iso3%in% CEA$iso3) %>%
  dplyr::select(iso3, region, ucost_proc_bcg, contains("ave"), 
         -uc_tot_vax_delv_ave) %>%
  
  pivot_longer(cols = starts_with("uc")) %>%
  mutate(name=case_when(name=="ucost_proc_bcg"~"BCG price",
                        name=="uc_labor_ave"~"Labour",
                        name=="uc_sc_ave"~"Supply chain", 
                        name=="uc_servd_ave"~"Service delivery",
                        TRUE~"Capital")) %>%
  group_by(region,iso3)%>%
  mutate(p=value/sum(value))%>%
  ggplot(aes(fill=name, y=p, x=iso3)) + 
  geom_bar(position="fill", stat="identity")+ 
  facet_wrap(~region, scales="free")+ 
  scale_fill_manual(values =c( "lightblue", "Sandy Brown", "pink", "Maroon 2","lightgreen"))+
  
  theme_linedraw()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.key.size = unit(0.5, "lines"))+ ylab("")+ xlab("Country ISO3 code")+
  coord_flip()

  
ggsave(file = here("plots/FS7.png"), w = 9, h = 8)






