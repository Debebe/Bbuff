
pacman::p_load(here,data.table, dplyr, tidyr, stringr, 
               flextable, officer)


outg<- fread(here("outputs/output_table.csv"))%>%
  dplyr::select(variable,sq_txt, cf_txt, av_txt)|>
  filter(variable%in% c("rslt_bcg_doses","rslt_att", "rslt_hosp_tbm",
    
    "rslt_att_cost", "rslt_attm_cost", "rslt_bcg_cost",
    "rslt_health",
    
    "rslt_tbminc", "rslt_inc",
    
    "rslt_tb_deaths", "rslt_tb_deaths",
    
    "rslt_ly_tbm", "rslt_ly_tb"
    
    ))%>%
  mutate(Type= case_when(variable%in% c("rslt_bcg_doses","rslt_att", "rslt_hosp_tbm") ~ "Resource",
                         grepl("cost", variable) ~ "Cost($M)", TRUE ~"Health"))%>%
  select(Type, Item=variable, `Status quo`=sq_txt, Counterfactual=cf_txt, Incremental= av_txt)%>%
  mutate(Type= factor(Type, levels= c("Resource", "Cost($M)", "Health")))%>%
  arrange(factor(Type, levels = c("Resource", "Cost($M)", "Health")))%>%
  mutate(Item= case_when(Item=="rslt_att" ~"ATT for TB",
                         Item=="rslt_bcg_doses" ~"BCG doses(M)",
                         Item=="rslt_inc" ~"TB incidence",
                         Item=="rslt_tbminc" ~"TBM incidence",
                         Item=="rslt_tb_deaths" ~"TB Deaths",
                         Item=="rslt_tbm_deaths" ~"TBM Deaths",
                         Item=="rslt_att_cost" ~ "ATT cost for TB",
                         Item=="rslt_attm_cost" ~ "ATT cost for TBM",
                         Item=="rslt_ly_tb" ~ "QALYs with TB (M)",
                         Item=="rslt_ly_tbm" ~ "QALYs with TBM",
                         Item=="rslt_health"~ "QALYs (M)",
                         Item=="rslt_hosp_tbm"~ "TBM hospitalizations",
                         Item=="rslt_bcg_cost"~ "BCG vaccination cost",
                         ))%>%as.data.frame() %>%
  group_by(Type)%>%
  arrange(factor(Item,
                 levels = c("BCG doses(M)","ATT for TB","TBM hospitalizations",
                            "BCG vaccination cost","ATT cost for TB",
                            "ATT cost for TBM","TB incidence", "TBM incidence",
                            "TB Deaths","QALYs (M)","QALYs with TB (M)",
                            "QALYs with TBM")))

## this creates data with merged column for tabling
outg <- as_grouped_data(x = outg, groups = c("Type"))


set_flextable_defaults(
  font.family = "Verdana",
  font.size = 7.0,
  border.color = "#CCCCCC",
  line_spacing = 1.0
)

first_col <- names(outg)[1] # dynamically detect column to be merged
zz <- flextable::as_flextable(outa) %>%
  compose(
    i = ~ !is.na(get(first_col)),
    j = 1, value = as_paragraph(gsub(paste0("^", first_col, ":\\s*"), "", as.character(get(first_col)))
    )) %>%
  bold(i = ~ !is.na(get(first_col)), j = 1, bold = TRUE, part = "body") %>%
  bold(part = "header", bold = TRUE) %>%
  #font(part = "all", fontname = "Verdana")|>
  autofit()|>
  add_footer_lines(values = "M - Million")|>
  italic(part = "footer", italic = TRUE) %>%
  fontsize(part = "footer", size = 6) %>%
  align(part = "footer", align = "left")


doc <- read_docx() |>
  body_add_flextable(value = zz) |>
  body_add_par(" ", style = "Normal") 

print(doc, target = "outputs/table1.docx")

##### region====

outr<- fread(here("outputs/output_table_who.csv")) %>%
  dplyr::select(Region=who_region,variable,sq_txt, cf_txt, av_txt)|>
  filter(variable%in% c("rslt_bcg_doses","rslt_att", "rslt_hosp_tbm",
                        
                        "rslt_att_cost", "rslt_attm_cost", "rslt_bcg_cost",
                        "rslt_health",
                        
                        "rslt_tbminc", "rslt_inc",
                        
                        "rslt_tb_deaths", "rslt_tb_deaths",
                        
                        "rslt_ly_tbm", "rslt_ly_tb"
                        
  ))%>%
  mutate(Type= case_when(variable%in% c("rslt_bcg_doses","rslt_att", "rslt_hosp_tbm") ~ "Resource",
                         grepl("cost", variable) ~ "Cost($M)", TRUE ~"Health"))%>%
  select(Region,Type, Item=variable, `Status quo`=sq_txt, Counterfactual=cf_txt, Incremental= av_txt)%>%
  mutate(Type= factor(Type, levels= c("Resource", "Cost($M)", "Health")))%>%
  arrange(factor(Type, levels = c("Resource", "Cost($M)", "Health")))%>%
  mutate(Item= case_when(Item=="rslt_att" ~"ATT for TB",
                         Item=="rslt_bcg_doses" ~"BCG doses(M)",
                         Item=="rslt_inc" ~"TB incidence",
                         Item=="rslt_tbminc" ~"TBM incidence",
                         Item=="rslt_tb_deaths" ~"TB Deaths",
                         Item=="rslt_tbm_deaths" ~"TBM Deaths",
                         Item=="rslt_att_cost" ~ "ATT cost for TB",
                         Item=="rslt_attm_cost" ~ "ATT cost for TBM",
                         Item=="rslt_ly_tb" ~ "QALYs with TB (M)",
                         Item=="rslt_ly_tbm" ~ "QALYs with TBM",
                         Item=="rslt_health"~ "QALYs (M)",
                         Item=="rslt_hosp_tbm"~ "TBM hospitalizations",
                         Item=="rslt_bcg_cost"~ "BCG vaccination cost",
  ))%>%as.data.frame() %>%
  group_by(Type)%>%
  arrange(factor(Item,
                 levels = c("BCG doses(M)","ATT for TB","TBM hospitalizations",
                            "BCG vaccination cost","ATT cost for TB",
                            "ATT cost for TBM","TB incidence", "TBM incidence",
                            "TB Deaths","QALYs (M)","QALYs with TB (M)",
                            "QALYs with TBM")))

set_flextable_defaults(
  font.family = "Verdana",
  font.size = 6.0,
  border.color = "#CCCCCC",
  line_spacing = 1.0
)

long_table <- outr %>%
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
rows_per_region <- 15
line_rows <- seq(rows_per_region, rows_per_region * num_regions, by = 15)

ft_r <- flextable(long_table) %>%
  merge_v(j = "Region") %>%          
  bold(i = ~ is.na(Incremental), j = "Item", bold = TRUE, part = "body") %>% 
  bold(part = "header", bold = TRUE) %>%
  autofit() %>%
  add_footer_lines(values = "M = Million") %>%
  italic(part = "footer", italic = TRUE) %>%
  fontsize(part = "footer", size = 6) %>%
  align(part = "footer", align = "left")%>%
  hline(i = line_rows, border = fp_border(width = 1, color = "gray")) |>
  fix_border_issues()

doc <- read_docx() |>
  body_add_flextable(value = ft_r) |>
  body_add_par(" ", style = "Normal") 

print(doc, target = "outputs/table2_r.docx")


##====effect in top ten countries=====
top10_cntrs <- fread("outputs/top10_cntrs.csv")
line_rows <- seq(6, 6 * 10, by = 6)

ft_10 <- flextable(top10_cntrs) %>%
  merge_v(j = "Country") %>%          
  bold(part = "header", bold = TRUE) %>%
  autofit() %>%
  #add_footer_lines(values = "M = Million") %>%
  italic(part = "footer", italic = TRUE) %>%
  fontsize(part = "footer", size = 6) %>%
  align(part = "footer", align = "left")%>%
  hline(i = line_rows, border = fp_border(width = 1, color = "gray")) |>
  fix_border_issues()

doc <- read_docx() |>
  body_add_flextable(value = ft_10) |>
  body_add_par(" ", style = "Normal") 

print(doc, target = "outputs/table3_10cntrs.docx")

