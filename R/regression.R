
rm(list = ls())

# This script conducts regression analysis on ENB and does decomposition and 
# the plotting
library(dplyr)
library(tidyverse)
library(relaimpo) # for variance decomposition
library(RColorBrewer)


load(here("outputs/CEAAs.RData"))                 # output for regression analysis

tmp <- CEAAs %>%
  mutate(across(c(ENB30,ICER, GDP, incbest,CDR,BCG, ucvax, uctb, uctbm), ~ as.numeric(scale(.))))

lm_enb30 <- lm(ENB30 ~ GDP +incbest +CDR+BCG+ucvax+uctb, tmp) 
coefs <- summary(lm_enb30)$coefficients[, c("Estimate", "Pr(>|t|)")]
cis <- confint(lm_enb30)

reg_ENB30 <- cbind(coefs, cis)%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(Predictors=rowname,`P-value`= "Pr(>|t|)")%>%
  mutate(Predictors = case_when(Predictors=="incbest" ~ "TB incidence",
                                Predictors=="BCG" ~ "BCG coverage",
                                Predictors=="CDR" ~ "Case detection rate",
                                Predictors=="ucvax" ~"Vaccine delivery unit cost",
                                Predictors=="uctb" ~"TB treatment unit cost", TRUE ~Predictors)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))%>%
  mutate(`Estimate (95% CI)`= paste0(Estimate, " (",`2.5 %`, " , ",`97.5 %`, ")" )) %>%
  dplyr::select(Predictors,`Estimate (95% CI)`,`P-value`)

fwrite(reg_ENB30, file = here("outputs/regression_ENB.csv"))


lms <- lm(ICER ~ GDP +incbest+CDR+BCG+ ucvax+uctb, tmp)
coefs <- summary(lms)$coefficients[, c("Estimate", "Pr(>|t|)")]
cis <- confint(lms)
reg_icer <- cbind(coefs, cis)%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(Predictors=rowname,`P-value`= "Pr(>|t|)")%>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))%>%
  mutate(`Estimate (95%CI)`= paste0(Estimate, " (",`2.5 %`, " , ",`97.5 %`, ")" ))%>%
  select(Predictors,`Estimate (95%CI)`,`P-value`)

fwrite(reg_icer, file = here("outputs/regression_icer.csv"))


#====Regress ICER to GDP ratio========

lm1 <- lm(ICER/GDP ~ incbest +CDR+BCG+ ucvax+uctb+uctbm, CEAAs) #TODO - scaling makes things wrong here
lm0 <- lm(ICER/GDP ~ CDR+BCG+ ucvax+uctb+uctbm, CEAAs) #TODO - scaling makes things wrong here

summary(lm1)
coefs <- summary(lm1)$coefficients[, c("Estimate", "Pr(>|t|)")]
cis <- confint(lm1)

# how much variance does inc explain?
# R2_diff <- summary(lm1)$adj.r.squared - summary(lm0)$adj.r.squared
# prop_explained <- R2_diff / summary(lm1)$adj.r.squared

# Combine into one table
reg_icer2gdp <- cbind(coefs, cis)%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(Predictors=rowname,`P-value`= "Pr(>|t|)")%>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))%>%
  mutate(`Estimate (95%CI)`= paste0(Estimate, " (",`2.5 %`, " , ",`97.5 %`, ")" ))%>%
  dplyr::select(Predictors,`Estimate (95%CI)`,`P-value`)

fwrite(reg_icer2gdp, file = here("outputs/reg_icer2gdp.csv"))





#=====Decompose R² ============

result <- calc.relimp(lm_enb30, type = "lmg", rela = TRUE)
print(result)

df <- data.frame(
  class = names(result$lmg),
  prop = as.numeric(result$lmg)) %>%
  mutate(
    lab.ypos = cumsum(prop) - 0.5*prop,           # label position
    label = ifelse(prop == max(prop), paste0(round(100*prop, 1), "%"), "") 
  )%>%
  mutate(class= case_when(class=="incbest" ~ "TB incidence",
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
  geom_text(aes(y = lab.ypos, label = label), color = "white", 
            size = 4) +
  scale_fill_manual(values = mycols) +
  ggplot2::theme_void() +
  xlim(0.5, 2.5) +
  labs(title = "Decomposition of variace explained (R-squared)") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.key.size = unit(0.5, "lines"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=10))

ggsave(pie_var, file= here("plots/f_pie_var.png"), w = 4, h = 3)


