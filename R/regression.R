
rm(list = ls())

# This script conducts regression analysis on ENB and does decomposition and 
# the plotting
library(dplyr)
library(tidyverse)
library(relaimpo) # for variance decomposition
library(RColorBrewer)

load(here("outputs/CEA.RData"))                 # output for regression analysis

tmp <- CEA %>%
  filter(threshold==0.3)|>
  mutate(across(c(ENB30,ICER, GDP,inc_u5,notif_u5,
                  inc_all,notif_all,CDR,BCG,ucvax,uctb,uctbm), ~ as.numeric(scale(.))))


fit_lm <- function(data, response, dynamic_vars, 
                   fixed_vars = c("GDP","CDR","BCG","ucvax","uctb")) {
 
  predictors <- c(dynamic_vars,fixed_vars)
  fml <- reformulate(predictors, response)
  mod <- lm(fml, data = data)
 
  coefs <- summary(mod)$coefficients[, c("Estimate", "Pr(>|t|)")]
  cis <- confint(mod)
  
 res <- cbind(coefs, cis)%>%
    as.data.frame()%>%
    rownames_to_column()%>%
    rename(Predictors=rowname,`P-value`= "Pr(>|t|)")%>%
    mutate(Predictors = case_when(Predictors%in%c("inc_u5","inc_all","notif_u5","notif_all")~"Incidence/Notifications",
                                  Predictors=="BCG" ~ "BCG coverage",
                                  Predictors=="CDR" ~ "Case detection rate",
                                  Predictors=="ucvax" ~"Vaccine delivery unit cost",
                                  Predictors=="uctb" ~"TB treatment unit cost", TRUE ~Predictors)) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))%>%
    mutate(`Estimate (95% CI)`= paste0(Estimate, " (",`2.5 %`, ", ",`97.5 %`, ")" )) %>%
    dplyr::select(Predictors,`Estimate (95% CI)`,`P-value`)
  

 #Rsquared
 Rsq <-data.table(predictor= dynamic_vars,
                  Rsq=  summary(mod)$r.squared,
                  Rsqadj=  summary(mod)$adj.r.squared)

 
 #=====Decompose R² ============
 
 result <- calc.relimp(mod, type = "lmg", rela = TRUE)
 print(result)
 
 df <- data.frame(
   class = names(result$lmg),
   prop = as.numeric(result$lmg)) %>%
   mutate(
     lab.ypos = cumsum(prop) - 0.5*prop,           # label position
     #label = ifelse(prop == max(prop), paste0(round(100*prop, 1), "%"), "") 
     label = ifelse(prop >0.035, paste0(round(100*prop, 1), "%"), "") 
   )%>%
   mutate(class= case_when(class %in%c("inc_u5","inc_all", "notif_u5","notif_all") ~ "Incidence/Notifications",
                           class=="BCG" ~ "BCG coverage",
                           class=="CDR" ~ "Case detection rate",
                           class=="ucvax" ~"Vaccine delivery unit cost",
                           class=="uctb" ~"TB treatment unit cost", TRUE ~class
   ))
 
 df <- data.frame(
   class = names(result$lmg),
   prop = as.numeric(result$lmg)
 ) %>%
   arrange(desc(class)) %>% 
   mutate(
     lab.ypos = cumsum(prop) - 0.5 * prop,
     label = ifelse(prop > 0.04, paste0(round(100 * prop, 1), "%"), ""))%>%
   mutate(class= case_when(class %in%c("inc_u5","inc_all", "notif_u5","notif_all") ~ "Incidence/Notifications",
                           class=="BCG" ~ "BCG coverage",
                           class=="CDR" ~ "Case detection rate",
                           class=="ucvax" ~"Vaccine delivery unit cost",
                           class=="uctb" ~"TB treatment unit cost", TRUE ~class
   ))
 
 mycols <- colorRampPalette(brewer.pal(8, "Dark2"))(nrow(df))
 pie_var <- ggplot(df, aes(x = 2, y = prop, fill = class)) +
   geom_bar(stat = "identity", color = "white"
   ) +
   coord_polar(theta = "y", start = 0) +
   geom_text(aes(y = lab.ypos, label = label), color = "white", size = 2.5) +
   scale_fill_manual(values = mycols) +
   ggplot2::theme_void() +
   xlim(0.5, 2.5) +
   #labs(title = "Decomposition of R-squared") +
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.5, "lines"),
         plot.title = element_text(hjust = 0.5, face = "bold", size=10))
 
 
  # Return a list with all results
  list(reg_tab = res,fig= pie_var, Rsq= Rsq)
}

res_inc_u5 <- fit_lm(tmp, response = "ENB30", dynamic_vars = "inc_u5") # Change here
res_notif_u5 <- fit_lm(tmp, response = "ENB30", dynamic_vars = "notif_u5") # Change here
res_inc_all <- fit_lm(tmp, response = "ENB30", dynamic_vars = "inc_all") # Change here
res_notif_all <- fit_lm(tmp, response = "ENB30", dynamic_vars = "notif_all") # Change here

Rsq <-bind_rows(res_inc_u5$Rsq,res_notif_u5$Rsq, res_inc_all$Rsq,res_notif_all$Rsq)

fwrite(Rsq, file = here("outputs/Rsq.csv"))



ggsave(res_notif_all$fig, file= here("plots/f_pie_notif_all.png"), w = 4, h = 3)
ggsave(res_notif_u5$fig, file= here("plots/f_pie_notif_u5.png"), w = 4, h = 3)
ggsave(res_inc_all$fig, file= here("plots/f_pie_inc_all.png"), w = 4, h = 3)
ggsave(res_inc_u5$fig, file= here("plots/f_pie_inc_u5.png"), w = 4, h = 3)

a <-res_notif_all$fig + ggtitle("Notifications (overall)")
b <-res_notif_u5$fig + ggtitle("Notifications (U5)")
c <-res_inc_all$fig + ggtitle("Incidence (overall)")
d <-res_inc_u5$fig + ggtitle("Incidence (U5)")


combined <- (a + b)/(c + d) +
  plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = "Decomposition of R-squared") & 
  theme(legend.position = "bottom",
    plot.title = element_text(hjust = 0.5) #center the title
  )

combined
ggsave(combined, file= here("plots/f_pie_comb.png"), w = 4, h = 5)

ft_lm <- res_inc_u5$reg_tab%>%
  flextable()|>
  bold(part = "header", bold = TRUE) %>%
  autofit()

doc <- read_docx() |>
  body_add_flextable(value = ft_lm) |>
  body_add_par(" ", style = "Normal") 

print(doc, target = "outputs/table4_lm.docx")


# 
# 
# lm_enb30 <- lm(ENB30 ~ GDP +inc_all +CDR+BCG+ucvax+uctb, tmp) 
# lm_enb30 <- lm(ENB30 ~ GDP +notif_all +CDR+BCG+ucvax+uctb, tmp) 
# coefs <- summary(lm_enb30)$coefficients[, c("Estimate", "Pr(>|t|)")]
# cis <- confint(lm_enb30)
# 
# reg_ENB30 <- cbind(coefs, cis)%>%
#   as.data.frame()%>%
#   rownames_to_column()%>%
#   rename(Predictors=rowname,`P-value`= "Pr(>|t|)")%>%
#   mutate(Predictors = case_when(Predictors=="incbest" ~ "TB incidence",
#                                 Predictors=="BCG" ~ "BCG coverage",
#                                 Predictors=="CDR" ~ "Case detection rate",
#                                 Predictors=="ucvax" ~"Vaccine delivery unit cost",
#                                 Predictors=="uctb" ~"TB treatment unit cost", TRUE ~Predictors)) %>%
#   mutate(across(where(is.numeric), ~ round(.x, 3)))%>%
#   mutate(`Estimate (95% CI)`= paste0(Estimate, " (",`2.5 %`, " , ",`97.5 %`, ")" )) %>%
#   dplyr::select(Predictors,`Estimate (95% CI)`,`P-value`)
# 
# fwrite(reg_ENB30, file = here("outputs/regression_ENB.csv"))
# 
# #=====Decompose R² ============
# 
# result <- calc.relimp(lm_enb30, type = "lmg", rela = TRUE)
# print(result)
# 
# df <- data.frame(
#   class = names(result$lmg),
#   prop = as.numeric(result$lmg)) %>%
#   mutate(
#     lab.ypos = cumsum(prop) - 0.5*prop,           # label position
#     #label = ifelse(prop == max(prop), paste0(round(100*prop, 1), "%"), "") 
#     label = ifelse(prop >0.035, paste0(round(100*prop, 1), "%"), "") 
#   )%>%
#   mutate(class= case_when(class=="incbest" ~ "TB incidence",
#                           class=="BCG" ~ "BCG coverage",
#                           class=="CDR" ~ "Case detection rate",
#                           class=="ucvax" ~"Vaccine delivery unit cost",
#                           class=="uctb" ~"TB treatment unit cost", TRUE ~class
#   ))
# 
# df <- data.frame(
#   class = names(result$lmg),
#   prop = as.numeric(result$lmg)
# ) %>%
#   arrange(desc(class)) %>% 
#   mutate(
#     lab.ypos = cumsum(prop) - 0.5 * prop,
#     label = ifelse(prop > 0.04, paste0(round(100 * prop, 1), "%"), ""))%>%
#   mutate(class= case_when(class=="incbest" ~ "TB incidence",
#                           class=="BCG" ~ "BCG coverage",
#                           class=="CDR" ~ "Case detection rate",
#                           class=="ucvax" ~"Vaccine delivery unit cost",
#                           class=="uctb" ~"TB treatment unit cost", TRUE ~class
#   ))
# 
# mycols <- colorRampPalette(brewer.pal(8, "Dark2"))(nrow(df))
# pie_var <- ggplot(df, aes(x = 2, y = prop, fill = class)) +
#   geom_bar(stat = "identity", color = "white"
#   ) +
#   coord_polar(theta = "y", start = 0) +
#   geom_text(aes(y = lab.ypos, label = label), color = "white", size = 2.5) +
#   scale_fill_manual(values = mycols) +
#   ggplot2::theme_void() +
#   xlim(0.5, 2.5) +
#   labs(title = "Decomposition of variace explained (R-squared)") +
#   theme(legend.position = "bottom", legend.title = element_blank(),
#         legend.key.size = unit(0.5, "lines"),
#         plot.title = element_text(hjust = 0.5, face = "bold", size=10))
# 
# ggsave(pie_var, file= here("plots/f_pie_var.png"), w = 4, h = 3)
# 
# 
# 
# ##ICER
# lms <- lm(ICER ~ GDP +inc_all+CDR+BCG+ ucvax+uctb, tmp)
# coefs <- summary(lms)$coefficients[, c("Estimate", "Pr(>|t|)")]
# cis <- confint(lms)
# reg_icer <- cbind(coefs, cis)%>%
#   as.data.frame()%>%
#   rownames_to_column()%>%
#   rename(Predictors=rowname,`P-value`= "Pr(>|t|)")%>%
#   mutate(across(where(is.numeric), ~ round(.x, 3)))%>%
#   mutate(`Estimate (95%CI)`= paste0(Estimate, " (",`2.5 %`, " , ",`97.5 %`, ")" ))%>%
#   dplyr::select(Predictors,`Estimate (95%CI)`,`P-value`)
# 
# fwrite(reg_icer, file = here("outputs/regression_icer.csv"))
# 
# 
# #====Regress ICER to GDP ratio========
# 
# lm1 <- lm(ICER/GDP ~ inc_all +CDR+BCG+ ucvax+uctb+uctbm, CEAAs) #TODO - scaling makes things wrong here
# lm0 <- lm(ICER/GDP ~ CDR+BCG+ ucvax+uctb+uctbm, CEAAs) #TODO - scaling makes things wrong here
# 
# summary(lm1)
# coefs <- summary(lm1)$coefficients[, c("Estimate", "Pr(>|t|)")]
# cis <- confint(lm1)
# 
# # how much variance does inc explain?
# # R2_diff <- summary(lm1)$adj.r.squared - summary(lm0)$adj.r.squared
# # prop_explained <- R2_diff / summary(lm1)$adj.r.squared
# 
# # Combine into one table
# reg_icer2gdp <- cbind(coefs, cis)%>%
#   as.data.frame()%>%
#   rownames_to_column()%>%
#   rename(Predictors=rowname,`P-value`= "Pr(>|t|)")%>%
#   mutate(across(where(is.numeric), ~ round(.x, 4)))%>%
#   mutate(`Estimate (95%CI)`= paste0(Estimate, " (",`2.5 %`, " , ",`97.5 %`, ")" ))%>%
#   dplyr::select(Predictors,`Estimate (95%CI)`,`P-value`)
# 
# fwrite(reg_icer2gdp, file = here("outputs/reg_icer2gdp.csv"))
# 
# 
# 


