
rm(list = ls())

pacman::p_load(here,data.table, dplyr, stringr, forcats,tidyr,
               flextable, officer)


outa<- fread(here("outdata/output_table.csv")) %>%
  dplyr::select(variable,sq_txt, cf_txt, av_txt)|>
  filter(variable%in% c("rslt_bcg_doses","rslt_att", "rslt_hosp_tbm",
    
    "rslt_att_cost", "rslt_attm_cost", "rslt_bcg_cost", "rslt_cost",
    #"rslt_health",
    
    "rslt_tbminc", "rslt_inc",
    
    "rslt_tb_deaths", "rslt_tbm_deaths", "rslt_tbmsev_seq",
    
    "rslt_ly_tbm", "rslt_ly_tb" #"rslt_tb_seq", "rslt_tbmseq"
    
    ))%>%
  mutate(Type= case_when(variable%in% c("rslt_bcg_doses","rslt_att", "rslt_hosp_tbm") ~ "Resource",
                         grepl("cost", variable) ~ "Cost($M)", TRUE ~"Health"))%>%
  dplyr::select(Type, Item=variable, `Status quo`=sq_txt, Counterfactual=cf_txt, Incremental= av_txt)%>%
  mutate(Type= factor(Type, levels= c("Resource", "Cost($M)", "Health")))%>%
  arrange(factor(Type, levels = c("Resource", "Cost($M)", "Health")))%>%
  mutate(Item= case_when(Item=="rslt_att" ~"ATT for TB",
                         Item=="rslt_bcg_doses" ~"BCG doses",
                         Item=="rslt_inc" ~"TB incidence",
                         Item=="rslt_tbminc" ~"TBM incidence",
                         Item=="rslt_tb_deaths" ~"TB deaths",
                         Item=="rslt_tbm_deaths" ~"TBM deaths",
                         Item=="rslt_att_cost" ~ "ATT cost for TB",
                         Item=="rslt_attm_cost" ~ "ATT cost for TBM",
                         Item=="rslt_cost" ~ "All costs",
                         Item=="rslt_ly_tb" ~ "DALYs from TB",
                         Item=="rslt_ly_tbm" ~ "DALYs from TBM",
                         #Item=="rslt_health"~ "QALYs(M)",
                         Item=="rslt_hosp_tbm"~ "TBM hospitalizations",
                         Item=="rslt_bcg_cost"~ "BCG vaccination cost",
                         Item=="rslt_tbmsev_seq" ~ "TBM severe sequelae"
                         
                         # Item=="rslt_tb_seq"~ "TB sequelae",
                         # Item=="rslt_tbmseq"~ "TBM sequelae"
                         
                         ))%>%as.data.frame() %>%
  group_by(Type)%>%
  arrange(factor(Item,
                 levels = c("BCG doses","ATT for TB","TBM hospitalizations",
                            "BCG vaccination cost","ATT cost for TB",
                            "ATT cost for TBM","All costs","TB incidence", "TBM incidence",
                            "TB deaths","TBM deaths","TBM severe sequelae", #"QALYs(M)",
                            "DALYs from TB",
                            "DALYs from TBM"#, "TB sequelae","TBM sequelae"
                            )))%>%
  
  # paste M into values
  mutate(`Status quo`=case_when(Item%in%c("BCG doses","DALYs from TB","DALYs from TBM") ~ paste0(`Status quo`, " M"),
                                TRUE ~`Status quo`),
         Counterfactual=case_when(Item%in%c("BCG doses","DALYs from TB","DALYs from TBM") ~ paste0(Counterfactual, " M"),
                                TRUE ~Counterfactual),
         Incremental=case_when(Item%in%c("BCG doses","DALYs from TB","DALYs from TBM") ~ paste0(Incremental, " M"),
                                  TRUE ~Incremental))%>%
  ## adding astrics
  mutate(Item = ifelse(grepl("TB(\\s|$)", Item),
                       sub("TB(\\s|$)", "TB* ", Item),
                       Item))%>%
  as.data.table()

setnames(outa, old = "Counterfactual", new = "Counterfactual(no BCG)")
setnames(outa, old = "Incremental", new = "Counterfactual(no BCG) - Status quo")


## this creates data with merged column for tabling
outg <- as_grouped_data(x = outa, groups = c("Type"))

set_flextable_defaults(
  font.family = "Verdana",
  font.size = 7.0,
  border.color = "#CCCCCC",
  line_spacing = 1.0
)

#outg <- as_grouped_data(x = outa, groups = c("Type"))
first_col <- names(outg)[1] # dynamically detect column to be merged
zz <- flextable::as_flextable(outg) %>%
  compose(
    i = ~ !is.na(get(first_col)),
    j = 1, value = as_paragraph(gsub(paste0("^", first_col, ":\\s*"), "", as.character(get(first_col)))
    )) %>%
  bold(i = ~ !is.na(get(first_col)), j = 1, bold = TRUE, part = "body") %>%
  bold(part = "header", bold = TRUE) %>%
  #font(part = "all", fontname = "Verdana")|>
  autofit() |>
  add_footer_lines(values ="BCG =Bacillus Calmette-Guérin, TB=Tuberculosis, TBM=Tuberculosis meningitis, ATT= Anti-TB treatment, DALYs= Disability Adjusted Life Years, M = Million")|>
  italic(part = "footer", italic = TRUE) %>%
  bold(part = "footer", bold = FALSE) %>%
  fontsize(part = "footer", size = 6) %>%
  align(part = "footer", align = "left")


# convert asterisk into superscript
tb_rows <- which(grepl("\\*", outg$Item))
for (i in tb_rows) {
  parts <- strsplit(outg$Item[i], "\\*")[[1]]
  zz <- compose(zz, part = "body", i = i, j = "Item",
                value = as_paragraph(parts[1], as_sup("*"), if(length(parts) > 1) parts[2] else NULL))
}

zz

doc <- read_docx() |>
  body_add_flextable(value = zz) |>
  body_add_par(" ", style = "Normal") 

print(doc, target = "outdata/table1.docx")

#####===== region====

outr<- fread(here("outdata/output_table_who.csv")) %>%
  dplyr::select(Region=who_region,variable,sq_txt, cf_txt, av_txt)|>
  filter(variable%in% c("rslt_bcg_doses","rslt_att", "rslt_hosp_tbm",
                        
                        "rslt_att_cost", "rslt_attm_cost", "rslt_bcg_cost", "rslt_cost",
                        #"rslt_health",
                        
                        "rslt_tbminc", "rslt_inc",
                        
                        "rslt_tb_deaths", "rslt_tbm_deaths","rslt_tbmsev_seq",
                        
                        "rslt_ly_tbm", "rslt_ly_tb" #, "rslt_tb_seq", "rslt_tbmseq"
                        
  ))%>%
  mutate(Type= case_when(variable%in% c("rslt_bcg_doses","rslt_att", "rslt_hosp_tbm") ~ "Resource",
                         grepl("cost", variable) ~ "Cost($M)", TRUE ~"Health"))%>%
  select(Region,Type, Item=variable, `Status quo`=sq_txt, Counterfactual=cf_txt, Incremental= av_txt)%>%
  mutate(Type= factor(Type, levels= c("Resource", "Cost($M)", "Health")))%>%
  arrange(factor(Type, levels = c("Resource", "Cost($M)", "Health")))%>%
  mutate(Item= case_when(Item=="rslt_att" ~"ATT for TB",
                         Item=="rslt_bcg_doses" ~"BCG doses",
                         Item=="rslt_inc" ~"TB incidence",
                         Item=="rslt_tbminc" ~"TBM incidence",
                         Item=="rslt_tb_deaths" ~"TB deaths",
                         Item=="rslt_tbm_deaths" ~"TBM deaths",
                         Item=="rslt_att_cost" ~ "ATT cost for TB",
                         Item=="rslt_attm_cost" ~ "ATT cost for TBM",
                         Item=="rslt_cost" ~ "All costs",
                         Item=="rslt_ly_tb" ~ "DALYs from TB",
                         Item=="rslt_ly_tbm" ~ "DALYs from TBM",
                         #Item=="rslt_health"~ "QALYs(M)",
                         Item=="rslt_hosp_tbm"~ "TBM hospitalizations",
                         Item=="rslt_bcg_cost"~ "BCG vaccination cost",
                         Item=="rslt_tbmsev_seq" ~ "TBM severe sequelae"
                         # Item=="rslt_tb_seq"~ "TB sequelae",
                         # Item=="rslt_tbmseq"~ "TBM sequelae"
  ))%>%as.data.frame() %>%
  group_by(Type)%>%
  arrange(factor(Item,
                 levels = c("BCG doses","ATT for TB","TBM hospitalizations",
                            "BCG vaccination cost",
                            "ATT cost for TB",
                            "ATT cost for TBM", "All costs",
                            "TB incidence", "TBM incidence",
                            "TB deaths","TBM deaths","TBM severe sequelae", #"QALYs(M)","QALYs with TB(M)",
                            #"QALYs with TBM", 
                            "DALYs from TB",
                            "DALYs from TBM"
                            #"TB sequelae","TBM sequelae"
                            ))) %>%
  
  # paste M into values
  mutate(`Status quo`=case_when(Item%in%c("BCG doses","DALYs from TB","DALYs from TBM") ~ paste0(`Status quo`, " M"),
                                TRUE ~`Status quo`),
         Counterfactual=case_when(Item%in%c("BCG doses","DALYs from TB","DALYs from TBM") ~ paste0(Counterfactual, " M"),
                                  TRUE ~Counterfactual),
         Incremental=case_when(Item%in%c("BCG doses","DALYs from TB","DALYs from TBM") ~ paste0(Incremental, " M"),
                               TRUE ~Incremental))%>%
  ## adding astrics
  mutate(Item = ifelse(grepl("TB(\\s|$)", Item),
                       sub("TB(\\s|$)", "TB* ", Item),
                       Item))%>%
  as.data.table()

setnames(outr, old = "Counterfactual", new = "Counterfactual(no BCG)")
setnames(outr, old = "Incremental", new = "Counterfactual(no BCG) - Status quo")




set_flextable_defaults(
  font.family = "Verdana",
  font.size = 6.0,
  border.color = "#CCCCCC",
  line_spacing = 1.0
)

outrg <- as_grouped_data(x = outr, groups = c("Type"))


Ltable <- outr %>%
  arrange(Region, Type, Item) %>%
  group_by(Region, Type) %>%
  do({
    type_row <- data.frame(
      Region = NA_character_,       
      Item = unique(.$Type),    
      `Status quo`=NA             
    )
    bind_rows(type_row, .)
  }) %>%
  ungroup() %>%
  group_by(Region) %>%
  mutate(
    Region = if_else(row_number() == 1, Region, NA_character_),  # keep only first row of Region
    Item = if_else(is.na(`Status quo`), paste0(Item), paste0("   ", Item))
    
  ) %>%
  ungroup()%>%select(-Type,-`Status.quo` )

num_regions <- length(unique(outr$Region))
rows_per_region <- Ltable%>% distinct(Item)%>%tally()%>%as.integer()
line_rows <- seq(rows_per_region, rows_per_region * num_regions, by = rows_per_region)

ft_r <- flextable(Ltable) %>%
  merge_v(j = "Region") %>%          
  bold(i = ~ is.na(`Counterfactual(no BCG)`), j = "Item", bold = TRUE, part = "body") %>% 
  bold(part = "header", bold = TRUE) %>%
  autofit() %>%
  add_footer_lines(values ="BCG =Bacillus Calmette-Guérin, TB=Tuberculosis, TBM=Tuberculosis meningitis, ATT= Anti-TB treatment, DALYs= Disability Adjusted Life Years, M = Million")|>
  italic(part = "footer", italic = TRUE) %>%
  fontsize(part = "footer", size = 6) %>%
  align(part = "footer", align = "left")%>%
  hline(i = line_rows, border = fp_border(width = 1, color = "gray")) |>
  line_spacing(space = 0.8) |>
  fix_border_issues()

doc <- read_docx() |>
  body_add_flextable(value = ft_r) |>
  body_add_par(" ", style = "Normal") 

#print(doc, target = "outputs/table2_r.docx")


##====effect in top ten countries=====
top10_cntrs <- fread("outdata/top10_cntrs.csv")%>%
  rename(Item="BCG effect")%>%
  #group_by(Country)|>
  mutate(Country = factor(Country, levels = unique(Country))) %>%
  mutate(Item=factor(Item,
                     levels = c("BCG doses","Averted TB incidence", "Averted TBM incidence",
                                "Averted TB deaths",  "Averted DALYs from TB", "Averted ATT for TB")))|>
  group_by(Country)|>
  arrange(Country, factor(Item,
                 levels = c("BCG doses","Averted TB incidence", "Averted TBM incidence",
                            "Averted TB deaths",  "Averted DALYs from TB", "Averted ATT for TB")))

n_cntrs <- length(unique(top10_cntrs$Country))
n_items_per_cntry <- length(unique(top10_cntrs$Item))
line_rows <- seq(n_items_per_cntry, n_items_per_cntry * n_cntrs, by = n_items_per_cntry)


#out10 <- as_grouped_data(x = top10_cntrs, groups = c("Country"))


ft_10 <- flextable(top10_cntrs) %>%
  merge_v(j = "Country") %>%          
  bold(part = "header", bold = TRUE) %>%
  autofit() %>%
  add_footer_lines(values ="BCG =Bacillus Calmette-Guérin, TB=Tuberculosis, TBM=Tuberculosis meningitis, ATT= Anti-TB treatment, DALYs= Disability Adjusted Life Years, M = Million")|>
  italic(part = "footer", italic = TRUE) %>%
  fontsize(part = "footer", size = 6) %>%
  align(part = "footer", align = "left")%>%
  hline(i = line_rows, border = fp_border(width = 1, color = "gray")) |>
  line_spacing(space = 1) |>
  fix_border_issues()

doc <- read_docx() |>
  body_add_flextable(value = ft_10) |>
  body_add_par(" ", style = "Normal") 

print(doc, target = "outdata/table3_10cntrs.docx")




# Desired orders
region_order <- c("AFR","AMR","EMR","EUR","SEA","WPR")
type_order <- c("Resource","Cost($M)","Health")
item_order <- unique(na.omit(outrg$Item))

# Fill down Type to data rows


outrg_filled <- outrg %>%
  fill(Type, .direction = "down") %>%
  mutate(
    Region_f = factor(Region, levels = region_order),
    Type_f   = factor(Type, levels = type_order),
    Item_f   = factor(Item, levels = item_order)
  ) %>%
  arrange(Region_f, Type_f, Item_f) %>%   # region first!
  select(-Region_f, -Type_f, -Item_f)%>%
  select(Region, Type, Item,`Status quo`, `Counterfactual(no BCG)`, `Counterfactual(no BCG) - Status quo`)%>%
  filter(!is.na(Region))



# Create flextable
ft <- flextable(outrg_filled) %>%
  merge_v(j = "Region") %>%   # merge Region column vertically
  merge_v(j = "Type") %>%     # merge Type column vertically
  autofit() %>%
  add_footer_lines(values ="BCG =Bacillus Calmette-Guérin, TB=Tuberculosis, TBM=Tuberculosis meningitis, ATT= Anti-TB treatment, DALYs= Disability Adjusted Life Years, M = Million")|>
  bold(i = ~ !is.na(Type) & is.na(Region) & is.na(Item), bold = TRUE) %>%
  theme_vanilla()

doc <- read_docx() |>
  body_add_flextable(value = ft) |>
  body_add_par(" ", style = "Normal") 

print(doc, target = "outdata/table2_r.docx")
