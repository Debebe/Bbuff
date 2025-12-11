
rm(list = ls())

# This script conducts regression analysis on ENB and does decomposition and 
# the plotting
library(dplyr)
library(tidyverse)
library(relaimpo) # for variance decomposition
library(RColorBrewer)
library(here)
library(data.table)

load(here("outdata/CEA.RData"))                 # output for regression analysis

tmp <- CEA %>%
  filter(threshold==0.3)|>
  mutate(across(c(ENB30,ICER, GDP,inc_u5,notif_u5,
                  inc_all,notif_all,CDR,BCG,ucvax,uctb,uctbm), ~ as.numeric(scale(.))))


lm_enb30 <- lm(ENB30 ~ GDP +inc_u5 +CDR+ucvax+uctb, tmp)
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

fwrite(reg_ENB30, file = here("outdata/regression_ENB.csv"))

ft_lm <-reg_ENB30%>%
  flextable()|>
  bold(part = "header", bold = TRUE) %>%
  autofit()

doc <- read_docx() |>
  body_add_flextable(value = ft_lm) |>
  body_add_par(" ", style = "Normal") 


print(doc, target = "outdata/table4_lm.docx")

#=====Decompose R² ============

result <- calc.relimp(lm_enb30, type = "lmg", rela = TRUE)
print(result)

df <- data.frame(
  class = names(result$lmg),
  prop = as.numeric(result$lmg)) %>%
  mutate(
    lab.ypos = cumsum(prop) - 0.5*prop,           # label position
    #label = ifelse(prop == max(prop), paste0(round(100*prop, 1), "%"), "")
    label = ifelse(prop >0.035, paste0(round(100*prop, 1), "%"), "")
  )%>%
  mutate(class= case_when(class=="incbest" ~ "TB incidence",
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
  geom_text(aes(y = lab.ypos, label = label), color = "white", size = 2.5) +
  scale_fill_manual(values = mycols) +
  ggplot2::theme_void() +
  xlim(0.5, 2.5) +
  labs(title = "Decomposition of variace explained (R-squared)") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.key.size = unit(0.5, "lines"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=10))

ggsave(pie_var, file= here("plots/f_pie_var.png"), w = 4, h = 3)


