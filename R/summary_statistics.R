
## README

# This script creates summary statistics for the write up

rm(list = ls())
pacman::p_load(here,data.table, dplyr, tidyr, stringr, 
               flextable, officer,kableExtra,ggplot2, ggrepel, patchwork)

load(here("outputs/PSA.RData"))                   # full PSA data with results. 
load(here("outputs/CEA_BUF.RData"))               # CEA with data with buffersz
load(here("outputs/CEA.RData"))                   # CEA data without buffer
load(here("outputs/CEA_sens.RData"))              # cntry level CEA wz orwzout posttb, TBM etc
load(here("outputs/cntrs_cost_eff.RData"))        # aggr CEA wz or wzout posttb, TBM etc
load(here("outputs/ceacq.RData"))                 # CEAC quantile
load(here("outputs/out_tab_r.RData"))             # output table - regional
load(here("outputs/CEAAs.RData"))                 # output for regression analysis
load(here("outputs/avert_deaths.RData"))          # averted deaths
load(here("outputs/avrt_table.RData"))            # averetd outputs

summary_tab <- data.frame(variable= "Number of countries",
                          value= length(unique(CEAAs$iso3)),
                          Description = "Number of countries")%>%
  bind_rows(CEAAs%>%
              group_by(variable=region)%>%
              summarise(value= length(unique(iso3)))%>%
              mutate(Description = "Number of countries"))%>%
  mutate(value= as.character(value)) %>%
  bind_rows(CEAAs%>%
              mutate(variable= "Global")%>%
              group_by(variable)%>%
              mutate(ICER=round(ICER, 0))%>%
              summarise(value= paste0(median(ICER), "(IQR =", quantile(ICER, 0.25)," to ",quantile(ICER, 0.75), ")")) %>%
              mutate(Description = "ICER with IQR - median global")) %>%
  
  bind_rows(CEAAs%>%
              mutate(variable= "Global")%>%
              group_by(variable)%>%
              summarise(value= paste0(round(mean(ICER),1), "( sd=", round(sd(ICER), 1), ")")) %>%
              mutate(Description = "ICER with sd - mean global")) %>%
  
  bind_rows(CEAAs%>%
              group_by(variable=region)%>%
              mutate(ICER= round(ICER, 0))%>%
              #summarise(value= paste0(median(ICER),"(IQR=", IQR(ICER),")"))%>%
              summarise(value= paste0(median(ICER), "(IQR =", quantile(ICER, 0.25)," to ",quantile(ICER, 0.75), ")")) %>%
              mutate(Description = "ICER - Median with IQR (Regional)")) %>%
  #buffer
  bind_rows(
    CEAAs%>%
      filter(Bf2>0)%>%
      mutate(variable="Global")%>%
      group_by(variable)%>%
      summarise(value= paste0(median(Bf2, na.rm = TRUE), "(IQR =", quantile(Bf2, 0.25, na.rm=TRUE)," to ",
                              quantile(Bf2, 0.75, na.rm=TRUE), ")")) %>%
      
      mutate(Description = "Buffer size- Median with IQR (Global)-10%"),
    
    CEAAs%>%
      filter(Bf2>0)%>%
      group_by(variable=region)%>%
      group_by(variable)%>%
      summarise(value= paste0(median(Bf2, na.rm = TRUE), "(IQR =", quantile(Bf2, 0.25, na.rm=TRUE)," to ",quantile(Bf2, 0.75, na.rm=TRUE), ")")) %>%
      
      mutate(Description = "Buffer size- Median wz IQR (Regional)-10%"),
    
    #Buffer 1
    CEAAs%>%
      filter(Bf1>0)%>%
      mutate(variable="Global")%>%
      group_by(variable)%>%
      summarise(value= paste0(median(Bf1, na.rm = TRUE), "(IQR =", quantile(Bf1, 0.25, na.rm=TRUE)," to ",
                              quantile(Bf1, 0.75, na.rm=TRUE), ")")) %>%
      
      mutate(Description = "Buffer size- Median with IQR (Global)-5%"),
    
    CEAAs%>%
      filter(Bf1>0)%>%
      group_by(variable=region)%>%
      group_by(variable)%>%
      summarise(value= paste0(median(Bf1, na.rm = TRUE), "(IQR =", quantile(Bf1, 0.25, na.rm=TRUE)," to ",quantile(Bf1, 0.75, na.rm=TRUE), ")")) %>%
      
      mutate(Description = "Buffer size- Median wz IQR (Regional)-5%"),
    
    
    #Buffer 3
    CEAAs%>%
      filter(Bf3>0)%>%
      mutate(variable="Global")%>%
      group_by(variable)%>%
      summarise(value= paste0(median(Bf3, na.rm = TRUE), "(IQR =", quantile(Bf3, 0.25, na.rm=TRUE)," to ",
                              quantile(Bf3, 0.75, na.rm=TRUE), ")")) %>%
      
      mutate(Description = "Buffer size- Median with IQR (Global)-15%"),
    
    CEAAs%>%
      filter(Bf3>0)%>%
      group_by(variable=region)%>%
      group_by(variable)%>%
      summarise(value= paste0(median(Bf3, na.rm = TRUE), "(IQR =", quantile(Bf3, 0.25, na.rm=TRUE)," to ",quantile(Bf3, 0.75, na.rm=TRUE), ")")) %>%
      
      mutate(Description = "Buffer size- Median wz IQR (Regional)-15%"),
    
    
    
    
    CEAAs%>%
      mutate(variable= "ICER/GDP")%>%
      mutate(value=ICER/GDP)%>%
      group_by(variable)%>%
      summarise(value= paste0(round(mean(value),1), "(sd=", round(sd(value), 1), ")")) %>%
      mutate(Description = "ICER to GDP ratio - mean wz sd (Global)- 10%"))%>%
  
  bind_rows(
    CEAAs%>%
      mutate(variable= "ICER/GDP")%>%
      mutate(value=ICER/GDP)%>%
      group_by(variable)%>%
      summarise(value= paste0(round(median(value),1), "(IQR=", round(IQR(value), 1), ")")) %>%
      mutate(Description = "ICER to GDP ratio - median with IQR (Global)")
  )%>%
  
  bind_rows(
    CEAAs%>%
      mutate(variable= region)%>%
      mutate(value=ICER/GDP)%>%
      group_by(variable)%>%
      summarise(value= paste0(round(median(value),2), "(IQR=", round(IQR(value), 2), ")")) %>%
      mutate(Description = "ICER to GDP ratio - median with IQR (Regional)")
  )%>%
  # prop cntrs cost effective
  bind_rows(CEAAs %>%
              mutate(ICER_is_cost_effective = ICER < (GDP / 2)) %>%
              mutate(variable="prop_cost_effect")%>%
              group_by(variable)%>%
              summarise(value = round(mean(ICER_is_cost_effective, na.rm = TRUE),2)
              ) %>% 
              mutate(value= as.character(value),
                     Description = "Proportion of countries with ICER <GDP/2"),
            CEAAs %>%
              mutate(ICER_is_cost_effective = ICER < (GDP / 3)) %>%
              mutate(variable="prop_cost_effect")%>%
              group_by(variable)%>%
              summarise(value = round(mean(ICER_is_cost_effective, na.rm = TRUE),2)
              ) %>% 
              mutate(value= as.character(value),
                     Description = "Proportion of countries with ICER <GDP/3"), 
            
            CEAAs %>%
              mutate(ICER_is_cost_effective = ICER < (GDP / 2)) %>%
              mutate(variable=region)%>%
              group_by(variable)%>%
              summarise(value = round(mean(ICER_is_cost_effective, na.rm = TRUE),2)) %>% 
              mutate(value= as.character(value),
                     Description = "Proportion of countries with ICER <GDP/2 (Regional)"),
            
            CEAAs %>%
              mutate(ICER_is_cost_effective = ICER < (GDP / 3)) %>%
              mutate(variable=region)%>%
              group_by(variable)%>%
              summarise(value = round(mean(ICER_is_cost_effective, na.rm = TRUE),2)) %>% 
              mutate(value= as.character(value),
                     Description = "Proportion of countries with ICER <GDP/3 (Regional)"), 
            
            # prop averted deaths in top cntrs
            avert_deaths%>% 
              filter(av>0)%>%
              group_by(iso3)%>%
              summarise(av=mean(av, na.rm = TRUE))%>%
              arrange(desc(av))%>%
              group_by(variable="Prop death averted")%>%
              summarise(value = sum(av[1:10]) / sum(av))%>%
              mutate(value= as.character(round(value, 2)),
                     Description = "Prop avert deaths in top 10 cntrs by death aversion"), 
            
            avert_deaths%>% 
              filter(av>0)%>%
              group_by(iso3)%>%
              summarise(av=mean(av, na.rm = TRUE))%>%
              arrange(desc(av))%>%
              group_by(variable="Prop death averted")%>%
              summarise(value = sum(av[1:3]) / sum(av))%>%
              mutate(value= as.character(round(value, 2)),
                     Description = "Prop avert deaths in top 3 cntrs by death aversion")
  )|>
  
  bind_rows(
    CEAAs%>%
      mutate(variable= "Unit cost")%>%
      group_by(variable)%>%
      summarise(value= paste0(round(median(ucvax),2), "(IQR=", round(IQR(ucvax), 2), ")")) %>%
      mutate(Description = "Unit cost for vaccine delivery - median with IQR (Global)"),
    CEAAs%>%
      mutate(variable= "Unit cost")%>%
      group_by(variable)%>%
      summarise(value= paste0(round(median(uctb),2), "(IQR=", round(IQR(uctb), 2), ")")) %>%
      mutate(Description = "Unit cost for TB treatment - median with IQR (Global)"), 
    
    CEAAs%>%
      mutate(variable= "Unit cost")%>%
      group_by(variable)%>%
      summarise(value= paste0(round(median(uctbm),2), "(IQR=", round(IQR(uctbm), 2), ")")) %>%
      mutate(Description = "Unit cost for TBM treatment - median with IQR (Global)")
  )%>%
  
  
  bind_rows(
    CEAAs%>%
      mutate(variable= region)%>%
      group_by(variable)%>%
      summarise(value= paste0(round(median(ucvax),2), "(IQR=", round(IQR(ucvax), 2), ")")) %>%
      mutate(Description = "Unit cost for vaccine delivery - median with IQR (Global)"),
    CEAAs%>%
      mutate(variable= region)%>%
      group_by(variable)%>%
      summarise(value= paste0(round(median(uctb),2), "(IQR=", round(IQR(uctb), 2), ")")) %>%
      mutate(Description = "Unit cost for TB treatment - median with IQR (Global)"),
    CEAAs%>%
      mutate(variable= region)%>%
      group_by(variable)%>%
      summarise(value= paste0(round(median(uctbm),2), "(IQR=", round(IQR(uctbm), 2), ")")) %>%
      mutate(Description = "Unit cost for TBM treatment - median with IQR (Global)")
  )%>%
  # BCG doses
  bind_rows(
    out_tab_r%>%
      filter(variable%in% c("rslt_bcg_doses"))%>%
      group_by(variable="BCG doses")|>
      summarise(value= as.character(round(mean(sq)), 0))|>
      mutate(Description = "BCG doses - Number of doses (Global)"), 
    
    out_tab_r%>%
      filter(variable%in% c("rslt_bcg_doses"))%>%
      group_by(variable=who_region)|>
      summarise(value= as.character(round(mean(sq)), 0))|>
      mutate(Description = "BCG doses - Number of doses (Regional)")
  )%>%
  # Averted ATT
  bind_rows(
    out_tab_r%>%
      filter(variable%in% c("rslt_att"))%>%
      group_by(variable="Averted ATT")|>
      summarise(value= as.character(round(mean(av, na.rm=TRUE)), 0))|>
      mutate(Description = "Averted ATT -  Number (Global)"), 
    
    out_tab_r%>%
      filter(variable%in% c("rslt_att"))%>%
      group_by(variable=who_region)|>
      summarise(value= as.character(round(mean(av, na.rm=TRUE)), 0))|>
      mutate(Description = "Averted ATT- Number (Regional)")
  )%>%
  # averted deaths per dose
  bind_rows(
    
    
    D%>%
      mutate(TBdeaths=rslt_tb_deaths_cf- rslt_tb_deaths_sq,
             TBndeaths=rslt_tbn_deaths_cf- rslt_tbn_deaths_sq,
             TBMdeaths=rslt_tbm_deaths_cf- rslt_tbm_deaths_sq)%>%
      select(iso3, who_region,TBdeaths, TBndeaths, TBMdeaths)%>%
      pivot_longer(cols = -c(iso3,who_region), names_to = "Description") %>%
      group_by(variable="Averted deaths", Description)%>%
      summarise(value= as.character(round(mean(value, na.rm=TRUE), 6))) %>% 
      mutate(Description=paste0("Mean averted ", Description, " per dose- Global")), 
    
    D%>%
      mutate(TBdeaths=rslt_tb_deaths_cf- rslt_tb_deaths_sq,
             TBndeaths=rslt_tbn_deaths_cf- rslt_tbn_deaths_sq,
             TBMdeaths=rslt_tbm_deaths_cf- rslt_tbm_deaths_sq)%>%
      select(iso3, who_region,TBdeaths, TBndeaths, TBMdeaths)%>%
      pivot_longer(cols = -c(iso3,who_region), names_to = "Description") %>%
      group_by(variable=who_region, Description)%>%
      summarise(value= as.character(round(mean(value, na.rm=TRUE), 6)))%>%
      mutate(Description=paste0("Mean averted ", Description, " per dose- Regional"))%>%
      
      # ICER to GDP ratio
      bind_rows(
        CEAAs%>%
          mutate(variable= "Global")%>%
          group_by(variable)%>%
          summarise(value= paste0(round(median(ICER/GDP),3), 
                                  "(IQR =", round(quantile(ICER/GDP, 0.25),3), " to ", 
                                  round(quantile(ICER/GDP, 0.75),3),")")) %>%
          mutate(Description = "ICER/GDP ratio - Median(IQR) global"),
        
        CEAAs%>%
          mutate(variable= region)%>%
          group_by(variable)%>%
          summarise(value= paste0(round(median(ICER/GDP),3), 
                                  "(IQR =", round(quantile(ICER/GDP, 0.25),3), " to ", 
                                  round(quantile(ICER/GDP, 0.75),3),")")) %>%
          mutate(Description = "ICER/GDP ratio - Median(IQR) Regional"))%>%
      bind_rows(
        ceacq%>%
          group_by(variable="Global")%>%
          summarise(value= as.character(round(median(iqr2median_ratio), 2)))%>%
          mutate(Description = "Median of the ratio of IQR of ICER to Median ICER "),
        ceacq%>%
          group_by(variable=g_whoregion)%>%
          summarise(value= as.character(round(median(iqr2median_ratio), 2)))%>%
          mutate(Description = "Median of the ratio of IQR of ICER to Median ICER "),
        
        CEAAs[,CF:=ifelse(ENB30<0, "NCE", "CE")]%>%
          filter(CF=="NCE")%>%
          group_by(variable="Incidence threshold")|>
          summarise(value= paste0(round(max(1e5 * incbest), 1), " per 100,000"))%>%
          mutate(Description = "Inc threshold above which BCG is cost-effective always"),
        
        CEAAs[,CF:=ifelse(ENB30<0, "NCE", "CE")]%>%
          filter(CF=="CE")%>%
          group_by(variable="Incidence threshold")|>
          summarise(value= paste0(round(min(1e5 * incbest), 1), " per 100,000"))%>%
          mutate(Description = "Inc threshold below which BCG is not cost-effective always"))%>%as.data.table()
  )


fwrite(summary_tab, file = here("outputs/statistics.csv"))



