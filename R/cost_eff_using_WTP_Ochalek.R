
rm(list = ls())

library(countrycode)
library(data.table)
library(dplyr)

Ochalek_2018_CET <- setDT(read_excel("indata/Ochalek_2018_CET.xlsx"))
load(here("outdata/CEA.RData"))


df <- Ochalek_2018_CET %>%
  mutate(
    iso3 = countrycode(
      Country,
      origin = "country.name",
      destination = "iso3c"
    )
  )|>
  mutate(DALY1 = gsub("\\$", "", DALY1),
         DALY2 = gsub("\\$", "", DALY2),
         DALY3 = gsub("\\$", "", DALY3),
         DALY4 = gsub("\\$", "", DALY4))|>
  mutate(DALY1=as.numeric(gsub(",","", DALY1)),
         DALY2=as.numeric(gsub(",","", DALY2)),
         DALY3=as.numeric(gsub(",","", DALY3)),
         DALY4=as.numeric(gsub(",","", DALY4)),
         )|>
  dplyr::select(Country,iso3,DALY1,DALY2,DALY3,DALY4)





load("~/Documents/GitHub/Bbuff/tmpdata/PSA.RData")

dff <- inner_join(df,D, by="iso3")|>as.data.table()


CEA <- dff[, .(
  ## expected net benefit at WTP=30%GDP
  ENB1 = mean(DALY1 * (rslt_health_sq - rslt_health_cf) -(rslt_cost_sq - rslt_cost_cf)),
  ENB2 = mean(DALY2 * (rslt_health_sq - rslt_health_cf) -(rslt_cost_sq - rslt_cost_cf)),
  ENB3 = mean(DALY3* (rslt_health_sq - rslt_health_cf) -(rslt_cost_sq - rslt_cost_cf)),
  ENB4 = mean(DALY4* (rslt_health_sq - rslt_health_cf) -(rslt_cost_sq - rslt_cost_cf)),
  
  WTP1 = mean(DALY1),
  WTP2 = mean(DALY2),
  WTP3 = mean(DALY3),
  WTP4 = mean(DALY4),
  

  ## ICER
  ICER = mean(rslt_cost_sq - rslt_cost_cf) /
    mean(rslt_health_sq - rslt_health_cf)),

by = .(Region=who_region,iso3)
]



CEA[, ICER_Label := ifelse(ICER < WTP1, "Cost-effective", "Not cost effective")]

CEA[, iso3 := factor(
  iso3,
  levels = unique(iso3[order(ICER)]),
  ordered = TRUE
)]

ggplot(CEA[ICER > 0], aes(x = iso3)) +
  
  # ICER
  geom_point(
    aes(y = ICER, shape = ICER_Label),
    size = 1.5
  ) +
  
  # WTP threshold
  geom_point(
    aes(y = WTP1, shape = "Threshold"), colour = "darkred",
    size = 1.0
  ) +
  
  scale_shape_manual(
    name = "ICER",
    values = c(
      "Cost-effective" = 19,
      "Not cost effective" = 1,
      "Threshold" = 3
    )
  ) +
  
  
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  facet_wrap(~Region, scales = "free") +
  
  theme_linedraw() +
  theme(
    legend.position = "top",
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(0, 5, 5, 5)
  ) +
  
  xlab("Country ISO3 code") +
  ylab("Incremental cost-effectiveness ratio (USD/DALY)")

ggsave(file = here("plots/FS14.png"), w = 9, h = 8)

