
rm(list = ls())

library(countrycode)
library(data.table)
library(dplyr)
library(WDI)
library(here)
library(readxl)
library(tidyr)
library(patchwork)
library(gt) # for tabling


## load threshold harmonised by Pete
thresholds_all <- fread("indata/thresholds_harmonized_long.csv")
## main data from model simulation that contains data.table D
load(here("tmpdata/PSA.RData"))



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
  default = as.double(mid)
)]

thresholds_all[, low := fcase(
  source == "Ochalek 2018", low * def_rate_Ochalek,
  source == "Woods 2016",   low * def_rate_woods,
  default = as.double(low)
)]

thresholds_all[, high := fcase(
  source == "Ochalek 2018", high * def_rate_Ochalek,
  source == "Woods 2016",   high * def_rate_woods,
  default = as.double(high)
)]

dff <- inner_join(thresholds_all,D, by="iso3")|>as.data.table()


CEA <- dff[, .(
  ## expected net benefit at WTP=30%GDP
  ENB = mean(mid * (rslt_health_sq - rslt_health_cf) -(rslt_cost_sq - rslt_cost_cf)),
  WTP = mean(mid),
  lo= mean(low),
  hi=mean(high),
  ## ICER
  ICER = mean(rslt_cost_sq - rslt_cost_cf) /
    mean(rslt_health_sq - rslt_health_cf)),
  
  by = .(Region=who_region,iso3, source)
]


CEA <- na.omit(CEA)


CEA[, ICER_Label := ifelse(ICER < WTP, "Cost-effective", "Not cost effective")]



CEA[, source := trimws(gsub("(update)", "", source, fixed = TRUE))]
CEA[, iso3_sorce:= paste(iso3,source)]


CEA[, iso3_sorce := factor(
  iso3_sorce,
  levels = unique(iso3_sorce[order(ICER)]),
  ordered = TRUE
)]

saveRDS(CEA, file= here("tmpdata/CEA_ochalek_co.Rds"))

all_colors <- c("black", "black", 2, 4, 5) # colors

all_colors <- c( 2, 4, 5) # colors



## number and proportion of cntrs cost-effective using Ochalek and woods thresholds
CEA |> 
  group_by(source) |> 
  count(ICER_Label) |> 
  mutate(prop = n / sum(n))|>
  group_by(source)|>
  mutate(N=sum(n))|>filter(ICER_Label=="Cost-effective")

## without dodges
make_cea_plot <- function(region) {
  
  data_region <- CEA[ICER > 0 & Region == region]
  data_region[, plot_id := reorder(iso3_sorce, ICER)]
  #data_region[, plot_id := reorder(iso3, ICER)]
  
  ggplot(data_region,
    aes(x = plot_id)) +
    geom_point(aes(y = ICER, shape = ICER_Label), size = 1.0) +
    geom_point(aes(y = WTP, shape = "Threshold", colour = source), size = 1.0) +
    geom_errorbar(aes(ymin = lo, ymax = hi, colour = source),width = 0.2) +
    scale_shape_manual(name = "",values = c("Cost-effective" = 19, "Not cost effective" = 1,
        "Threshold" = 3)) +
    scale_colour_manual(name = "Source", values = all_colors) +
    scale_y_log10(labels = scales::comma) +
    scale_x_discrete(labels = function(x) sub(" .*", "", x)) +
    facet_wrap(~Region, scales = "free") +
    theme_linedraw() +
    theme(
      legend.position = "top",
      plot.margin = margin(0, 5, 5, 5),
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7),
      strip.text = element_text(size=9),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10, face = "bold"))  + coord_flip()
}

## with dodges
make_cea_plot <- function(region) {
  
  data_region <- CEA[ICER > 0 & Region == region]
  
  # Order countries by their median ICER
  data_region[, country_order := median(ICER, na.rm = TRUE), by = iso3]
  data_region[, plot_id := reorder(iso3, country_order)]
  
  # Same dodge used for all layers so everything lines up
  dodge <- position_dodge(width = 0.65)
  
  ggplot(
    data_region,
    aes(x = plot_id)
  ) +
    
    # ICER
    geom_point(
      aes(
        y = ICER,
        shape = ICER_Label#,
        # colour = source,
        # group = source
      ),
      size = 1.0,
      position = dodge
    ) +
    
    # WTP threshold
    geom_point(
      aes(
        y = WTP,
        shape = "Threshold",
        colour = source,
        group = source
      ),
      size = 1.0,
      position = dodge
    ) +
    
    # Uncertainty interval
    geom_errorbar(
      aes(
        ymin = lo,
        ymax = hi,
        colour = source,
        group = source
      ),
      width = 0.15,
      position = dodge
    ) +
    
    scale_shape_manual(
      name = "",
      values = c(
        "Cost-effective" = 19,
        "Not cost effective" = 1,
        "Threshold" = 3
      )
    ) +
    
    scale_colour_manual(
      name = "Source",
      values = all_colors
    ) +
    
    scale_y_log10(
      labels = scales::comma
    ) +
    
    scale_x_discrete(
      drop = FALSE
    ) +
    
    facet_wrap(
      ~Region,
      scales = "free"
    ) +
    
    theme_linedraw() +
    
    theme(
      legend.position = "top",
      plot.margin = margin(0, 5, 5, 5),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      strip.text = element_text(size = 9),
      legend.text = element_text(size = 10),
      legend.title = element_text(
        size = 10,
        face = "bold"
      )
    ) +
    
    coord_flip()
}


eur <- make_cea_plot("EUR") + ylab("") + xlab("")
amr <- make_cea_plot("AMR") + ylab("") + xlab("")
sea <- make_cea_plot("SEA") + ylab("") + xlab("")
afr <- make_cea_plot("AFR") + ylab("") + xlab("")
emr <- make_cea_plot("EMR") + ylab("Incremental cost-effectiveness ratio (USD/DALY)") + xlab("")
wpr <- make_cea_plot("WPR") + ylab("") + xlab("")


comb <- (afr+eur + amr)/(sea + emr + wpr) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") 

ggsave(comb, file = here("plots/FS14_comb.png"), w = 9, h = 10)


## check sampled VE after latitude adjustment

bcg_haz <-D[, .(iso3, iter,bcg_haz_tb)]
bcg_haz[, VE:=1-bcg_haz_tb]


lat_ve <- ggplot(bcg_haz, aes(x = VE)) +
  geom_density(fill = "grey70", alpha = 0.6) +
  facet_wrap(~iso3, scales = "free_y") +
  labs(
    x = "Vaccine effectiveness (VE)",
    y = "Density"
  ) +
  theme_linedraw() +
  theme(
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 7)
  )

ggsave(lat_ve, file = here("plots/lat_ve.png"), w = 13, h = 8)


## compare CE between main analysis and alternative thresholds

load("~/Documents/GitHub/Bbuff/outdata/CEA.RData")

CEA_main <- CEA
CEA_main[, "WTP = 0.3xGDP":= ifelse(ICER < ICER_val, "Cost-effective", "Not cost effective")]
CEA_main <-CEA_main[threshold==0.3, .(iso3,region,`WTP = 0.3xGDP`)]

CEA_ochalek_co <- readRDS("~/Documents/GitHub/Bbuff/tmpdata/CEA_ochalek_co.Rds")
CEA_ochalek_co <- CEA_ochalek_co|>select(Region, iso3,source,"WTP=Ochalek/Woods"= ICER_Label)

both <- left_join(CEA_main,CEA_ochalek_co, by= "iso3")|>
  mutate(source= factor(source, levels = c("Woods 2016","Ochalek 2018","Ochalek 2026")))


both <-na.omit(both)



d <- both[
  !is.na(source) &
    !is.na(`WTP=Ochalek/Woods`)
]

# Counts
tab <- d[
  ,
  .N,
  by = .(
    `WTP = 0.3xGDP`,
    `WTP=Ochalek/Woods`,
    source
  )
]

# Row percentages within source
tab[
  ,
  pct := N / sum(N) * 100,
  by = .(`WTP = 0.3xGDP`, source)
]


tab[, n_pct := sprintf("%d (%.1f%%)", N, pct)]

# Wide table
result <- dcast(
  tab,
  `WTP = 0.3xGDP` ~ source + `WTP=Ochalek/Woods`,
  value.var = "n_pct",
  fill = "0 (0.0%)"
)

result_gt <-result %>%
  gt() %>%
  cols_label(
    `WTP = 0.3xGDP` = "WTP = 0.3 × GDP",
    `Woods 2016_Cost-effective` = "CE",
    `Woods 2016_Not cost effective` = "NCE",
    `Ochalek 2018_Cost-effective` = "CE",
    `Ochalek 2018_Not cost effective` = "NCE",
    `Ochalek 2026_Cost-effective` = "CE",
    `Ochalek 2026_Not cost effective` = "NCE"
  ) %>%
  tab_spanner(
    label = "Woods 2016",
    columns = c(
      `Woods 2016_Cost-effective`,
      `Woods 2016_Not cost effective`
    )
  ) %>%
  tab_spanner(
    label = "Ochalek 2018",
    columns = c(
      `Ochalek 2018_Cost-effective`,
      `Ochalek 2018_Not cost effective`
    )
  ) %>%
  tab_spanner(
    label = "Ochalek 2026",
    columns = c(
      `Ochalek 2026_Cost-effective`,
      `Ochalek 2026_Not cost effective`
    )
  )

gtsave(result_gt, here(here("outdata/CE_crosstab.png")))
gtsave(
  result_gt,
  here("outdata/WTP_crosstab.pdf"),
  vwidth = 1200,
  vheight = 300
)
