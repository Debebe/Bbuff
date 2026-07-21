
library(data.table)
library(ggrepel)
library(dplyr)


bcg <- readRDS("~/Documents/BCG/data/bcg.Rds")|>as.data.table()

bcgt <- bcg[, .(CODE, NAME, YEAR,COVERAGE_CATEGORY, TARGET_NUMBER, DOSES)]
cov_wuenic <- bcg[COVERAGE_CATEGORY=="WUENIC", .(CODE, YEAR, COVERAGE)]

bcg_official <- bcgt[COVERAGE_CATEGORY=="ADMIN" & !is.na(TARGET_NUMBER)]
comb <-dplyr::inner_join(bcg_official, cov_wuenic, by= c("CODE", "YEAR"))

comb[, demand:=TARGET_NUMBER*COVERAGE/100]

avail_dt <- readRDS("~/Documents/GitHub/Bbuff/outdata/gdp_inc_le_costs.rds")|>
  filter(cov_cat=="WUENIC", !is.na(cdr), )|>
  select(iso3)

comb_avail <-comb|>filter(CODE%in%avail_dt$iso3)|>as.data.table()


df<-comb_avail|>
  group_by(iso3=CODE,NAME)|>
  summarise(M= mean(demand),
            std= sd(demand))|>
  mutate(CV=std/M)|>as.data.table()




df %>%
  arrange(CV) %>%
  mutate(iso3 = factor(iso3, levels = iso3)) %>%
  ggplot(aes(x = iso3, y = CV)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Coefficient of variation",
    title = "Variation in vaccine demand by country"
  ) +
  theme_minimal()


ggplot(df, aes(M, CV)) +
  geom_point(size = 2) +
  labs(
    x = "Mean demand",
    y = "Coefficient of variation"
  ) +
  theme_minimal()


ggplot(df, aes(CV)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue") +
  labs(
    x = "Coefficient of variation",
    y = "Number of countries"
  ) +
  theme_minimal()




ggplot(df, aes(M, CV, label = iso3)) +
  geom_point(size = 2) +
  geom_text_repel(size = 3, max.overlaps = Inf) +
  scale_x_log10() +
  labs(
    x = "Mean annual demand (log scale)",
    y = "Coefficient of variation"
  ) +
  theme_minimal()

