
rm(list = ls())

library(countrycode)
library(data.table)
library(dplyr)
library(WDI)
library(here)

## load threshold harmonised by Pete
thresholds_all <- read_csv("indata/thresholds_harmonized_long.csv")|>as.data.table()
## main data from model simulation that contains data.table D
load("~/Documents/GitHub/Bbuff/tmpdata/PSA.RData")

## If there is "Ochalek 2026 (update)" select that, 
## if not go for "Ochalek 2018"  if not then "Woods 2016"

# thresholds <- copy(thresholds_all)[
#   , priority := fcase(
#     source == "Ochalek 2026 (update)", 1L,
#     source == "Ochalek 2018",          2L,
#     source == "Woods 2016",            3L,
#     default = 999L
#   )
# ][
#   order(iso3, priority)
# ][
#   , .SD[1], by = iso3
# ][
#   , priority := NULL
# ][,.(source, iso3, low, mid,high)]


## We need to inflate thresholds to similar year

#' Looking at teh World bank data, we don't have consumer index price for year 2025 and 2026.
#' However we have GDP deflator for 2025. All threshold were in the USD terms
#' Woods was expressed in 2013 USD term, Ochalek 2018 was expressed in the 2015 terms and Ochalek 2026
#' was in 2026 USD terms. 
#' 
#' We chose to inflate these into 2026 USD using GDP deflator. However looking
#' again into tghe deflator data we didn't have value for 2026. However looking over 4 years
#' we can see that deflators increased by 3 percentage points. we approximated the 2026
#' deflator by adding 3 points to the 2025 value. 
#' 
#' Another note - in countries where we have the updated Ochalek we used that value
#' if no updated Ochalek, we took the 2018 Ochalek and  else we took the woods 2016 but 
#' all inflated to the 2026 USD values


indicator <- c("NY.GDP.DEFL.ZS") ## selecting GDP deflator as it is mostly available
inflation <- WDI(indicator = indicator) |>
  filter(year%in% c(2013, 2015,2020, 2021,2022, 2023, 2024, 2025)) |> 
  pivot_longer(
    cols = -c(iso2c, iso3c, country, year),
    names_to = "indicator",
    values_to = "value"
  ) |>
  mutate(indicator_label = "GDP deflator") |>
  rename(iso2 = iso2c, iso3 = iso3c, year = year)|> 
  filter(iso3=="USA" &  year%in%c(2013, 2015,2025))|>as.data.table()

## creating gdp deflator for 2025
inflation <- rbind(inflation, copy(inflation[year == 2025])[,
                                                            `:=`(year = 2026L, value = value + 3)
])


gdp_def <-  inflation[, usd_gdp_defl_rate:=value[year==2026]/value][year!=2025,.(year,usd_gdp_defl_rate)]

def_rate_woods <-as.numeric(gdp_def[year==2013,usd_gdp_defl_rate])
def_rate_Ochalek <-as.numeric(gdp_def[year==2015,usd_gdp_defl_rate])

## fcase in data.table is is like if else 
thresholds_all[, mid := fcase(
  source == "Ochalek 2018", mid * def_rate_Ochalek,
  source == "Woods 2016",   mid * def_rate_woods,
  default = mid
)]


dff <- inner_join(thresholds_all,D, by="iso3")|>as.data.table()


CEA <- dff[, .(
  ## expected net benefit at WTP=30%GDP
  ENB = mean(mid * (rslt_health_sq - rslt_health_cf) -(rslt_cost_sq - rslt_cost_cf)),
  WTP = mean(mid),
  ## ICER
  ICER = mean(rslt_cost_sq - rslt_cost_cf) /
    mean(rslt_health_sq - rslt_health_cf)),
  
  by = .(Region=who_region,iso3, source)
]


CEA <- na.omit(CEA)


CEA[, ICER_Label := ifelse(ICER < WTP, "Cost-effective", "Not cost effective")]

# CEA[, iso3 := factor(
#   iso3,
#   levels = unique(iso3[order(ICER)]),
#   ordered = TRUE
# )]

CEA[, source := trimws(gsub("(update)", "", source, fixed = TRUE))]

CEA[, iso3_sorce:= paste(iso3,source)]


CEA[, iso3_sorce := factor(
  iso3_sorce,
  levels = unique(iso3_sorce[order(ICER)]),
  ordered = TRUE
)]

all_colors <- c("black", "black", 2, 4, 5) # colors

all_colors <- c( 2, 4, 5) # colors


#ggplot(CEA[ICER > 0 & Region == "AFR"], aes(x = iso3_sorce)) +
ggplot(CEA[ICER > 0], aes(x = iso3)) +
  
  # ICER points
  geom_point(
    aes(y = ICER, shape = ICER_Label),
    size = 1.5
  ) +
  
  # WTP threshold colored by source
  geom_point(
    aes(y = WTP, shape = "Threshold", colour = source),
    size = 1.5
  ) +
  
  scale_shape_manual(
    name = "",
    values = c(
      "Cost-effective" = 19,
      "Not cost effective" = 1,
      "Threshold" = 3
    )) +
    
  #scale_colour_discrete(name = "Threshold source", values= all_colors) +
  scale_colour_manual(name = "Threshold source", values= all_colors) +
    
  scale_y_log10(labels = scales::comma) +
  
  facet_wrap(~Region, scales = "free") + #xlim(0, 10000)+

  
  theme_linedraw() +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(0, 5, 5, 5), 
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 6.5),
    strip.text = element_text(size = 10), 
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10.2),
  ) +
  
  xlab("Country ISO3 code") +
  ylab("Incremental cost-effectiveness ratio (USD/DALY)") +   
  coord_flip()

ggsave(file = here("plots/FS14.png"), w = 9, h = 8.3)
#ggsave(file = here("plots/FS14.pdf"), w = 9, h = 8.2)


## number and proportion of cntrs cost-effective using Ochalek and woods thresholds
CEA |> 
  group_by(source) |> 
  count(ICER_Label) |> 
  mutate(prop = n / sum(n))|>
  group_by(source)|>
  mutate(N=sum(n))|>filter(ICER_Label=="Cost-effective")
 
