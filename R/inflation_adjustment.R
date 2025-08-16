
rm(list = ls())
library(here)
library(tidyverse)
library(readxl)
library(scales)


# Load the data
#cost_data <- readRDS(here("indata", "gdp_inc_le.rds"))
full_data <- readRDS(here("data", "gdp_inc_le.rds"))

names(full_data)
length(unique(full_data$country)) # 127 countries

# Load the CPI data
##' methods used to adjust for inflation in `https://pubmed.ncbi.nlm.nih.gov/31511179/`
##' 1. exchange the local currency to US$ or international dollars and then inflate using US inflation rates
##' 2. inflate the local currency using local inflation rates and then exchange to US$ or international dollars
##' 3. split the costs into tradable and nontradable resources and using method 1 on the tradable resources and method 2 on the nontradable resources

if (!("worldbank_inflation.RData" %in% list.files(here("data/cost/indata")))) {
  # download US inflation rates
  library(WDI)

  # Set the indicator code for CPI
  indicator <- c("FP.CPI.TOTL", "FP.CPI.TOTL.ZG", "PA.NUS.PPPC.RF",
                 "NY.GDP.DEFL.ZS", "PA.NUS.FCRF")

  # Set the country code for the United States
  # country <- c("US", "ZWE", "ZAF")

  # Download the data
  inflation <- WDI(indicator = indicator) |>
    filter(year >= 2018) |>
    pivot_longer(
      cols = -c(iso2c, iso3c, country, year),
      names_to = "indicator",
      values_to = "value"
    ) |>
    mutate(indicator_label = case_when(
      indicator == "FP.CPI.TOTL" ~ "Consumer price index (2010 = 100)",
      indicator == "FP.CPI.TOTL.ZG" ~ "Inflation, consumer prices (annual %)",
      indicator == "PA.NUS.PPPC.RF" ~
        "Price level ratio of PPP conversion factor (GDP) to market exchange rate",
      indicator == "NY.GDP.DEFL.ZS" ~ "GDP deflator",
      indicator == "PA.NUS.FCRF" ~
        "Official exchange rate (LCU per US$, period average)",
      TRUE ~ indicator
    )) |>
    rename(iso2 = iso2c, iso3 = iso3c, year = year)

  head(inflation)

  save(inflation, file = here("data/cost/indata/worldbank_inflation.RData"))

  # load saved data
} else {
  load(here("data/cost/indata/worldbank_inflation.RData"))
}



# standardize country names
cost_data <- full_data %>%
  rename(iso3 = Iso3) %>%
  mutate(country = countrycode::countrycode(country,
    origin = "country.name",
    destination = "country.name",
    warn = FALSE
  ))


inflation <- inflation %>%
  mutate(country = countrycode::countrycode(country,
    origin = "country.name",
    destination = "country.name",
    warn = FALSE
  ))



# quick cheks
table(inflation$indicator)
inflation |> filter(iso3 == "ZWE" & year >= 2018 & indicator == "FP.CPI.TOTL")
inflation |> filter(iso3 == "ZWE" & year >= 2018 & indicator == "FP.CPI.TOTL.ZG")
inflation |> filter(iso3 == "ZWE" & year >= 2018 & indicator == "NY.GDP.DEFL.ZS")

# quite a bit of missing data
inflation %>%
  filter(country %in% cost_data$country) |>
  # filter(year %in% c(2018, 2024)) |>
  group_by(indicator, year) %>%
  summarise(miss = sum(is.na(value))) |>
  pivot_wider(names_from = year, values_from = miss)

# # plot these inflation rates by country
# inflation %>%
#   filter(country %in% cost_data$country) |>
#   # filter(year %in% c(2018, 2024)) |>
#   # group_by(country, indicator) %>%
#   # fill(value, .direction = "down") %>%
#   filter(indicator_label == "Consumer price index (2010 = 100)") %>%
#   ggplot(aes(x = year, y = value, color = country)) +
#   geom_line() +
#   facet_wrap(~country, scales = "free_y") +
#   labs(
#     title = "",
#     x = "Year",
#     y = "Consumer price index (2010 = 100)"
#   ) +
#   # theme_minimal() +
#   theme(legend.position = "none")

# ggsave(filename = here('plots','CPI_series.png'),
#        w=15,h=10, dpi = 600)
# ggsave(filename = here('plots','CPI_series.pdf'),
#        w=15,h=10,
#        dpi = 600)
# inflation %>%
#   filter(country %in% cost_data$country) |>
#   arrange(country, indicator, year) %>%
#   group_by(country, indicator) %>%
#   fill(value, .direction = "down") %>%
#   filter(year %in% c(2018, 2023)) |>
#   filter(indicator_label == "Consumer price index (2010 = 100)") %>%
#   ggplot(aes(x = year, y = value, color = country)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~country, scales = "free_y") +
#   labs(
#     title = "",
#     x = "Year",
#     y = "Consumer price index (2010 = 100)"
#   ) +
#   # theme_minimal() +
#   theme(legend.position = "none")
#
#
# ggsave(filename = here('plots','CPI_series_short.png'),
#        w=15,h=10, dpi = 600)
# ggsave(filename = here('plots','CPI_series_short.pdf'),
#        w=15,h=10,
#        dpi = 600)

# inflation %>%
#   filter(country %in% cost_data$country) |>
#   arrange(country, indicator, year) %>%
#   group_by(country, indicator) %>%
#   fill(value, .direction = "down") %>%
#   filter(year %in% c(2018, 2023)) |>
#   filter(indicator_label == "Inflation, consumer prices (annual %)") %>%
#   ggplot(aes(x = year, y = value, color = country)) +
#   geom_line() +
#   facet_wrap(~country, scales = "free_y") +
#   labs(
#     title = "",
#     x = "Year",
#     y = "Inflation, consumer prices (annual %)"
#   ) +
#   # theme_minimal() +
#   theme(legend.position = "none")

# ggsave(filename = here('plots','inflation_rates_percent.png'),
#        w=15,h=10, dpi = 600)
# # ggsave(filename = here('plots','inflation_rates_by_country.pdf'),
# #        w=10,h=10,
# #        dpi = 600)

# inflation %>%
#   filter(country %in% cost_data$country) |>
#   arrange(country, indicator, year) %>%
#   group_by(country, indicator) %>%
#   fill(value, .direction = "down") %>%
#   filter(year %in% c(2018, 2023)) |>
#   filter(indicator == "NY.GDP.DEFL.ZS") %>%
#   ggplot(aes(x = year, y = value, color = country)) +
#   geom_line() +
#   facet_wrap(~country, scales = "free_y") +
#   labs(
#     title = "",
#     x = "Year",
#     y = "GDP deflator"
#   ) +
#   # theme_minimal() +
#   theme(legend.position = "none")

# ggsave(filename = here('plots','GDP_deflator.png'),
#        w=15,h=10, dpi = 600)
# ggsave(filename = here('plots','inflation_rates_by_country.pdf'),
#        w=10,h=10,
#        dpi = 600)


cpi_inflation <- inflation %>%
  # filter(country %in% cost_data$country) |>
  arrange(country, indicator, year) %>%
  group_by(country, indicator) %>%
  fill(value, .direction = "downup") %>%
  ungroup() |>
  filter(
    year %in% c(2018, 2023),
    indicator_label == "Consumer price index (2010 = 100)"
  ) %>%
  group_by(country) %>%
  distinct(country, year, .keep_all = TRUE) %>%
  mutate(
    base_year = 2018,
    base_cpi = value[year == base_year],
    cpi_rate = value / base_cpi
  ) %>% # value[year==2023]/value[year=2018]
  filter(year != base_year) %>%
  select(country, iso3, cpi_rate)


gdpdeflator_inflation <- inflation %>%
  # filter(country %in% cost_data$country) |>
  arrange(country, indicator, year) %>%
  group_by(country, indicator) %>%
  fill(value, .direction = "downup") %>%
  ungroup() |>
  filter(
    year %in% c(2018, 2023),
    indicator == "NY.GDP.DEFL.ZS"
  ) %>%
  group_by(country) %>%
  distinct(country, year, .keep_all = TRUE) %>%
  mutate(
    base_year = min(year),
    base_cpi = value[year == base_year],
    gdp_rate = value / base_cpi
  ) %>%
  filter(year != base_year) %>%
  select(country, iso3, gdp_rate)



# plot inflation rates by country
# cpi_inflation %>%
#   ggplot(aes(x = cpi_rate, y = country)) +
#   geom_point() +
#   geom_text(aes(label = ifelse(cpi_rate > 10, country, '')),
#             hjust = 1.1, size = 3) +
#   labs(
#     title = "Inflation Rates by Country (CPI)",
#     x = "Inflation Rate",
#     y = "Country"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")
#
# gdpdeflator_inflation %>%
#   ggplot(aes(x = gdp_rate, y = country)) +
#   geom_point() +
#   geom_text(aes(label = ifelse(gdp_rate > 10, country, '')),
#             hjust = 1.1, size = 3) +
#   labs(
#     title = "Inflation Rates by Country (GDP Deflator)",
#     x = "Inflation Rate",
#     y = "Country"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")

exchange_rates <- inflation |>
  # filter(indicator == "PA.NUS.FCRF")
  # filter(country %in% cost_data$country) |>
  arrange(country, indicator, year) %>%
  group_by(country, indicator) %>%
  fill(value, .direction = "downup") %>%
  ungroup() |>
  filter(year %in% c(2018, 2023) & indicator == "PA.NUS.FCRF") |>
  pivot_wider(
    names_from = year,
    names_prefix = "XR",
    values_from = value
  ) |>
  select(country, iso3, XR2018, XR2023)


exchange_rates

# merge the two inflation rates
inflation_rates <- cpi_inflation |>
  left_join(gdpdeflator_inflation, by = c("country", "iso3"))

inflation_rates |>
  filter(is.na(cpi_rate) | is.na(gdp_rate))

no_cpi <- inflation_rates |>
  filter(is.na(cpi_rate) & !is.na(gdp_rate)) |>
  distinct()
no_gdp <- inflation_rates |>
  filter(!is.na(cpi_rate) & is.na(gdp_rate)) |>
  distinct()

no_rates <- inflation_rates |>
  filter(is.na(cpi_rate) & is.na(gdp_rate)) |>
  distinct()

no_cpi
no_gdp
no_rates

# ## use gdp_rate as cpi_rate if cpi_rate is missing and viceversa
# inflation_rates <- inflation_rates |>
#   mutate(
#     # fill missing CPI rates with gdp_rate
#     cpi_rate = ifelse(is.na(cpi_rate) & !is.na(gdp_rate), gdp_rate, cpi_rate), 
#     # fill missing GDP deflator rates with cpi_rate
#     gdp_rate = ifelse(!is.na(cpi_rate) & is.na(gdp_rate), cpi_rate, gdp_rate)  
#   )
inflation_rates |>
  filter(is.na(cpi_rate) | is.na(gdp_rate))

inflation_rates |>
  filter(iso3 == "USA")

# just use the USA data for these
usa_rates <- inflation_rates |>
  ungroup() |>
  filter(iso3 == "USA") |>
  select(usa_cpi = cpi_rate, usa_gdp = gdp_rate)

inflation_rates <- cbind(inflation_rates, usa_rates) |>
  mutate(
    cpi_rate = coalesce(cpi_rate, usa_cpi),
    gdp_rate = coalesce(gdp_rate, usa_gdp)
  ) #|>
# select(-usa_cpi, -usa_gdp)

inflation_rates |>
  filter(is.na(cpi_rate) | is.na(gdp_rate))

# do the same for the exchange rates
exchange_rates |>
  filter(is.na(XR2018) | is.na(XR2023))

# fill missing exchange rates with 1

exchange_rates <- exchange_rates |>
  mutate(
    XR2018 = ifelse(is.na(XR2018), 1, XR2018),
    XR2023 = ifelse(is.na(XR2023), 1, XR2023)
  )

# a large difference for these countries
large_diff <- inflation_rates %>%
  mutate(
    diff = cpi_rate - gdp_rate,
    perc_diff = abs(100 * diff / cpi_rate)
  ) %>%
  arrange(desc(perc_diff)) |>
  filter(perc_diff > 29)

large_diff
length(unique(inflation_rates$country)) # 218 countries
# plot comparing the two inflation rates
inflation_rates %>%
  mutate(
    diff = cpi_rate - gdp_rate,
    perc_diff = abs(100 * diff / cpi_rate)
  ) %>%
  arrange(desc(perc_diff)) |>
  filter(gdp_rate < 2) |> # drop the outliers
  ggplot(aes(x = cpi_rate, y = gdp_rate, label = country)) +
  geom_point() +
  ggrepel::geom_text_repel(size = 3, segment.color = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "",
    x = "CPI Inflation Rate",
    y = "GDP Deflator Inflation Rate"
  ) +
  theme_bw()

ggsave(
  filename = here("plots", "inflation_rates_comparison.png"),
  w = 10, h = 10, dpi = 600
)


# adjust the costs: could use either the CPI or GDP deflator inflation rates

##' Using method 1 described in `https://pubmed.ncbi.nlm.nih.gov/31511179/`
##' 1. Convert back to the local currency using the exchange rate relating to the time period
##'     the cost data were collected. 
##' 2. Adjust for inflation using the GDP deflator/CPI.
##' 3. Convert to US dollars using the exchange rate relating to the time period of interest.
##' 4. Convert to international dollars using the PPP conversion factor.
##' 
# just using the CPI inflation rates for now
setdiff(
  unique(cost_data$country),
  unique(inflation_rates$country)
) # no missing countries
setdiff(
  unique(cost_data$country),
  unique(exchange_rates$country)
) # no missing countries
# adjust uc_tot_vax_delv_ave 2018 for inflation to 2023 prices

# merge costs with exchange rates & convert to LCU using 2018 exchange rate
cost_data <- cost_data %>%
  left_join(exchange_rates, by = c("country", "iso3")) |>
  left_join(inflation_rates, by = c("country", "iso3"))

length(unique(cost_data$country)) # 127 countries
# check if there are any missing values in adjustment factors
cost_data %>%
  select(country, iso3, XR2018, XR2023, cpi_rate, gdp_rate) |>
  filter(is.na(XR2018) | is.na(XR2023) | is.na(cpi_rate) | is.na(gdp_rate))

vax_del_cost_cat <- c(
  "uc_tot_vax_delv_ave", "uc_tot_vax_delv_med", "uc_tot_vax_delv_lo",
  "uc_tot_vax_delv_hi", "uc_labor_ave", "uc_labor_med", "uc_labor_lo",
  "uc_labor_hi", "uc_sc_ave", "uc_sc_med", "uc_su_lo", "uc_sc_hi", "uc_servd_ave",
  "uc_servd_med", "uc_servd_lo", "uc_servd_hi", "uc_capital_ave",
  "uc_capital_med", "uc_capital_lo", "uc_capital_hi"
)

# cost_data_adj <- cost_data %>%
#   # Convert to Local currency in 2028 (base year)
#   mutate(across(all_of(vax_del_cost_cat), function(x) x * XR2018))%>%
#   # Inflate to the 2023 LCU
#   mutate(across(all_of(vax_del_cost_cat), function(x) x * cpi_rate))%>%
#   # Convert to USD 2023
#   mutate(across(all_of(vax_del_cost_cat), function(x) x / XR2023))%>%as.data.table()
cost_data_adj <- cost_data %>%
  mutate(across(
    all_of(vax_del_cost_cat), ~ ifelse(
      is.na(XR2018), .x * usa_cpi, # LCU missing, then inflate using USA_cpi
      # LCUA available--->convert USD to LCU2018, then inflate to 2023 locally then convert back to USD
      (.x * XR2018) * cpi_rate / XR2023
    )
  )) %>%
  as.data.table()

saveRDS(cost_data_adj, file = here("data/gdp_inc_le_costs.rds"))

# adjust to 2023 prices
cost_datas <- cost_data %>%
  mutate(
    # exchange to 2018 LCU
    uc_tot_vax_delv_ave_LCU2018 = uc_tot_vax_delv_ave * XR2018,
    uc_tot_vax_delv_lo_LCU2018 = uc_tot_vax_delv_lo * XR2018,
    uc_tot_vax_delv_hi_LCU2018 = uc_tot_vax_delv_hi * XR2018,

    # adjust to 2023 LCU
    uc_tot_vax_delv_ave_LCU2023 = uc_tot_vax_delv_ave_LCU2018 * cpi_rate,
    uc_tot_vax_delv_lo_LCU2023 = uc_tot_vax_delv_lo_LCU2018 * cpi_rate,
    uc_tot_vax_delv_hi_LCU2023 = uc_tot_vax_delv_hi_LCU2018 * cpi_rate
  )

names(cost_datas)

# convert back to USD
cost_datas <- cost_datas %>%
  mutate(
    uc_tot_vax_delv_ave_USD2023 = uc_tot_vax_delv_ave_LCU2023 / XR2023,
    uc_tot_vax_delv_lo_USD2023 = uc_tot_vax_delv_lo_LCU2023 / XR2023,
    uc_tot_vax_delv_hi_USD2023 = uc_tot_vax_delv_hi_LCU2023 / XR2023
  )

# check the results
cost_datas %>%
  select(
    country, iso3, uc_tot_vax_delv_ave, uc_tot_vax_delv_ave_LCU2018,
    uc_tot_vax_delv_ave_LCU2023, uc_tot_vax_delv_ave_USD2023
  ) %>%
  arrange(desc(uc_tot_vax_delv_ave_USD2023)) %>%
  head(10)

cost_datas %>%
  select(
    country, iso3, uc_tot_vax_delv_ave, uc_tot_vax_delv_ave_LCU2018,
    uc_tot_vax_delv_ave_LCU2023, uc_tot_vax_delv_ave_USD2023
  ) %>%
  arrange(desc(uc_tot_vax_delv_ave_USD2023)) %>%
  tail(10)

cost_datas %>%
  filter(country %in% large_diff$country) %>%
  select(
    country, iso3, uc_tot_vax_delv_ave, uc_tot_vax_delv_ave_LCU2018,
    uc_tot_vax_delv_ave_LCU2023, uc_tot_vax_delv_ave_USD2023
  ) |>
  distinct()

# alternatively just use the US inflation rate on the USD costs
inflation_rates$country[grepl("United", inflation_rates$country)]
us_inflation_rate <- inflation_rates %>%
  filter(country == "United States")
us_inflation_rate

# apply the US inflation rate to the costs
# US inflation rate from 2018 to 2023
cost_datas <- cost_datas %>%
  # US inflation rate from 2018 to 2023
  mutate(
    uc_tot_vax_delv_ave_USD2023_alt = uc_tot_vax_delv_ave * us_inflation_rate$cpi_rate,
    uc_tot_vax_delv_lo_USD2023_alt = uc_tot_vax_delv_lo * us_inflation_rate$cpi_rate,
    uc_tot_vax_delv_hi_USD2023_alt = uc_tot_vax_delv_hi * us_inflation_rate$cpi_rate
  )



# check the results
cost_datas %>%
  select(
    country, iso3, uc_tot_vax_delv_ave, uc_tot_vax_delv_ave_USD2023,
    uc_tot_vax_delv_ave_USD2023_alt
  ) %>%
  mutate(
    diff = uc_tot_vax_delv_ave_USD2023 - uc_tot_vax_delv_ave_USD2023_alt
  ) %>%
  arrange(desc(diff)) %>%
  head(10)

cost_datas %>%
  select(
    country, iso3, uc_tot_vax_delv_ave, uc_tot_vax_delv_ave_USD2023,
    uc_tot_vax_delv_ave_USD2023_alt
  ) %>%
  mutate(
    diff = abs(uc_tot_vax_delv_ave_USD2023 - uc_tot_vax_delv_ave_USD2023_alt)
  ) %>%
  arrange(desc(diff)) %>%
  head(10)


cost_datas %>%
  filter(country %in% large_diff$country) %>%
  select(
    country, iso3, uc_tot_vax_delv_ave_USD2023,
    uc_tot_vax_delv_ave_USD2023_alt
  ) |>
  distinct()


# plot the results: USD vs USD adjusted for US inflation
cost_datas %>%
  select(
    country, iso3, uc_tot_vax_delv_ave_USD2023,
    uc_tot_vax_delv_ave_USD2023_alt
  ) %>%
  # filter(uc_tot_vax_delv_ave_USD2023 < 20) %>%
  ggplot(aes(
    x = uc_tot_vax_delv_ave_USD2023_alt,
    y = uc_tot_vax_delv_ave_USD2023,
    fill = country
  )) +
  geom_point() +
  geom_text(aes(label = iso3), size = 3, hjust = 0.5, vjust = -0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "",
    x = "Cost (USD) - Adjusted for US inflation",
    y = "Cost (USD) - Adjusted for local inflation"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


ggsave(
  filename = here("plots", "inflation_adjusted_costs_comparison.png"),
  w = 10, h = 10, dpi = 600
)

