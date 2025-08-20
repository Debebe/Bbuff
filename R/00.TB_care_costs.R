rm(list = ls())
library(here)
library(tidyverse)
library(data.table)
library(scales)
library(viridis)
library(readxl)
library(httr)
# set_here('~/Dropbox/BCG')

# Download relevant data from https://www.who.int/teams/global-tuberculosis-programme/data

# Expenditure and utilization of health services for TB since fiscal year 2023 
if(!file.exists(here::here('data/cost/indata/df_exp.Rdata'))){
  df_exp<- fread("https://extranet.who.int/tme/generateCSV.asp?ds=expenditure_utilisation", header = T)
  save(df_exp,file=here::here('data/cost/indata/df_exp.Rdata'))
} else {
  load(here::here('data/cost/indata/df_exp.Rdata'))
}

print(paste("Most recent year of data is:", max(unique(df_exp$year))))

# # Budgets for TB since fiscal year 2024 - currently not being used much 
if(!file.exists(here::here('data/cost/indata/df_bdg.Rdata'))){
  df_bdg<- fread("https://extranet.who.int/tme/generateCSV.asp?ds=budget", header = T)
  save(df_bdg,file=here::here('data/cost/indata/df_bdg.Rdata'))
} else {
  load(here::here('data/cost/indata/df_bdg.Rdata'))
}

print(paste("Most recent year of data is:", max(unique(df_bdg$year))))

# Estimated number of incident cases (all forms) 
if(!file.exists(here::here('data/cost/indata/df.Rdata'))){
  df<- fread("https://extranet.who.int/tme/generateCSV.asp?ds=estimates", header = T)
  save(df,file=here::here('data/cost/indata/df.Rdata'))
} else {
  load(here::here('data/cost/indata/df.Rdata'))
}

print(paste("Most recent year of data is:", max(unique(df$year))))

# Estimated number of incident cases (MDR TB) 
if(!file.exists(here::here('data/cost/indata/df_mdr.Rdata'))){
  df_mdr<- fread("https://extranet.who.int/tme/generateCSV.asp?ds=mdr_rr_estimates", header = T)
  save(df_mdr,file=here::here('data/cost/indata/df_mdr.Rdata'))
} else {
  load(here::here('data/cost/indata/df_mdr.Rdata'))
}

print(paste("Most recent year of data is:", max(unique(df_mdr$year))))

# TB Data dictionary
if(!file.exists(here::here('data/cost/indata/df_dic.Rdata'))){
  df_dic<-fread("https://extranet.who.int/tme/generateCSV.asp?ds=dictionary")  
  save(df_dic,file=here::here('data/cost/indata/df_dic.Rdata'))
} else {
  load(here::here('data/cost/indata/df_dic.Rdata'))
}

# load ISO country codes - for cleaning up 
load(here::here('data/cost/indata/isodict.Rdata'))
code <- read_csv(here::here("data/cost/indata", "all.csv")) # has more details on codes
code <- code %>% dplyr::rename(country=name, iso3=`alpha-3`) %>% select(country, `country-code`, iso3, region, `sub-region`)

# country names differ from those in WHO data
ISO$country <-as.character(ISO$country)
ISO$country[ISO$country=="Czech Republic"] <- "Czechia"
ISO$country[ISO$country=="Serbia & Montenegro"] <- "Serbia"
ISO$country[ISO$country=="Swaziland"] <- "Eswatini"
ISO$country[ISO$country=="The Former Yugoslav Republic of Macedonia"] <- "North Macedonia" 

SM <- code %>% filter(country %in% c("Serbia", "Montenegro")) %>% select(country, iso3, `sub-region`) 
names(SM)[3]<-"g_whoregion"
SM$g_whoregion[SM$g_whoregion=="Southern Europe"] <- "EUR"


#============================Debebe=================
#TODO Debebe changing WHO region code- was WPAR

df_exp[country=="Indonesia", g_whoregion:="SEA"]
#======================================================
df_ISO <- ISO %>% select(country, iso3, g_whoregion) 
df_ISO <- rbind(df_ISO,SM)
df_exp <- df_ISO %>% left_join(df_exp, by=c("country", "iso3", "g_whoregion"))
df_bdg <- df_ISO %>% left_join(df_bdg, by=c("country", "iso3", "g_whoregion"))

# Other relevant data - some are used later in the MDRHHCM unit costs code

# World Bank GDP data & Country and Lending Groups
if(!file.exists(here::here('data/cost/indata/r.gdp.Rdata'))){
url <- 'https://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
r.gdp <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3)) # GDP (current US$)
save(r.gdp,file=here::here('data/cost/indata/r.gdp.Rdata'))
} else {
  load(here::here('data/cost/indata/r.gdp.Rdata'))
}

tail(names(r.gdp), 1)
indicators <- c('Country Name', 'Country Code', '2016', '2017', '2018', '2019', '2020','2021','2022','2023',
                '2024')
r.gdp <- r.gdp %>% select(indicators)

# World Bank GDP per capita data & Country and Lending Groups
if(!file.exists(here::here('data/cost/indata/gdp.Rdata'))){
url <- 'https://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.CD?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
gdp <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3))
# income_group <- fread(here('data', 'income.csv'), blank.lines.skip=TRUE) 
# world_bank <- data.table(read_csv(here('data', 'gdppc_current.csv')))
income_group <- setDT(readxl::read_excel(tf, sheet = 'Metadata - Countries', skip=0)) 
save(gdp,income_group,file=here::here('data/cost/indata/gdp.Rdata'))
} else {
  load(here::here('data/cost/indata/gdp.Rdata'))
}

tail(names(gdp), 1)
gdp <- gdp %>% select(all_of(indicators))


# World Bank Exchange rates 
if(!file.exists(here::here('data/cost/indata/exch.Rdata'))){
url <- 'https://api.worldbank.org/v2/en/indicator/PA.NUS.FCRF?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
exch <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3)) # Official exchange rate (LCU per US$, period average) 
save(exch,file=here::here('data/cost/indata/exch.Rdata'))
} else {
  load(here::here('data/cost/indata/exch.Rdata'))
}

tail(names(exch), 1)
exch <- exch %>% select(indicators)

# World Bank Price level ratio of PPP conversion factor (GDP) to market exchange rate
if(!file.exists(here::here('data/cost/indata/exch_ppp.Rdata'))){
url <- 'https://api.worldbank.org/v2/en/indicator/PA.NUS.PPP?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
exch_ppp <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3)) # PPP conversion factor, GDP (LCU per international $)
# ppp <- setDT(read_csv(here('data','ppp.csv')))
save(exch_ppp,file=here::here('data/cost/indata/exch_ppp.Rdata'))
} else {
  load(here::here('data/cost/indata/exch_ppp.Rdata'))
}

names(exch_ppp)
exch_ppp <- exch_ppp %>% select(indicators)

# World Bank Purchasing power parity conversion factors 
if(!file.exists(here::here('data/cost/indata/ppp.Rdata'))){
url <- 'https://api.worldbank.org/v2/en/indicator/PA.NUS.PPPC.RF?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
ppp <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3)) # Price level ratio of PPP conversion factor (GDP) to market exchange rate
# ppp <- setDT(read_csv(here('data','ppp.csv')))
save(ppp,file=here::here('data/cost/indata/ppp.Rdata'))
} else {
  load(here::here('data/cost/indata/ppp.Rdata'))
}

names(ppp)
ppp <- ppp %>% select(indicators)

# World Bank GDP deflators
if(!file.exists(here::here('data/cost/indata/defl.Rdata'))){
url <- 'https://api.worldbank.org/v2/en/indicator/NY.GDP.DEFL.ZS?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
defl <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3)) # GDP deflator (base year varies by country)
# ppp <- setDT(read_csv(here('data','ppp.csv')))
save(defl,file=here::here('data/cost/indata/defl.Rdata'))
} else {
  load(here::here('data/cost/indata/defl.Rdata'))
}

tail(names(defl), 1)
defl <- defl %>% 
  select(`Country Name`, `Country Code`, `2010`, `2011`, `2012`, `2013`, 
         `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`,`2021`,`2022`,`2023`, `2024`)

un_gdp <- setDT(read_csv(here('data/cost/indata', 'ungdppc.csv')))
# un_gdppp <- setDT(read_csv(here('data', 'ungdppcpp.csv')))
tb_budget <- setDT(df_bdg)
df_exp <- setDT(df_exp)

#  Replace Missing Values with previous year data
r.gdp <- r.gdp %>% 
  pivot_longer(cols = -c(`Country Name`, `Country Code`), 
               names_to = "year", values_to = "r.gdp") %>%
  arrange(`Country Name`, `Country Code`) %>% fill(r.gdp)

gdp <- gdp %>% 
  pivot_longer(cols = -c(`Country Name`, `Country Code`), 
               names_to = "year", values_to = "gdp") %>%
  arrange(`Country Name`, `Country Code`) %>% 
  fill(gdp)

exch <- exch %>% 
  pivot_longer(cols = -c(`Country Name`, `Country Code`), 
               names_to = "year", values_to = "exch") %>%
  arrange(`Country Name`, `Country Code`) %>% 
  fill(exch)

ppp <- ppp %>% 
  pivot_longer(cols = -c(`Country Name`, `Country Code`), 
               names_to = "year", values_to = "ppp") %>%
  arrange(`Country Name`, `Country Code`) %>% 
  fill(ppp)

exch_ppp <- exch_ppp %>% 
  pivot_longer(cols = -c(`Country Name`, `Country Code`), 
               names_to = "year", values_to = "exch_ppp") %>%
  arrange(`Country Name`, `Country Code`) %>% 
  fill(exch_ppp)

defl <- defl %>% 
  pivot_longer(cols = -c(`Country Name`, `Country Code`),
               names_to = "year", values_to = "defl") %>%
  arrange(`Country Name`, `Country Code`) %>% 
  fill(defl) 


gdp <- r.gdp %>% select(c('Country Name', 'Country Code',year, r.gdp)) %>% left_join(gdp, by=c('Country Name', 'Country Code', 'year'))
gdp <- exch %>% select(c('Country Name', 'Country Code',year, exch)) %>% left_join(gdp, by=c('Country Name', 'Country Code', 'year'))
gdp <- ppp %>% select(c('Country Name', 'Country Code',year, ppp)) %>% left_join(gdp, by=c('Country Name', 'Country Code', 'year'))
gdp <- exch_ppp %>% select(c('Country Name', 'Country Code',year, exch_ppp)) %>% left_join(gdp, by=c('Country Name', 'Country Code', 'year'))

# More cleaning up 

# gdp$`Country Name`[!gdp$`Country Code` %in% df_exp$iso3]

gdp <- gdp %>% filter(!`Country Name` %in%
                        c("Africa Eastern and Southern", "Africa Western and Central",
                          "Arab World","Caribbean small states",
                          "Central Europe and the Baltics","Channel Islands",
                          "Early-demographic dividend","East Asia & Pacific",
                          "East Asia & Pacific (excluding high income)",
                          "East Asia & Pacific (IDA & IBRD countries)","Euro area",
                          "Europe & Central Asia","Europe & Central Asia (excluding high income)",
                          "Europe & Central Asia (IDA & IBRD countries)","European Union","Faroe Islands",
                          "Fragile and conflict affected situations","Gibraltar",
                          "Heavily indebted poor countries (HIPC)","High income","IBRD only",
                          "IDA & IBRD total","IDA blend","IDA only","IDA total","Isle of Man",
                          "Kosovo","Late-demographic dividend","Latin America & Caribbean",
                          "Latin America & Caribbean (excluding high income)",
                          "Latin America & the Caribbean (IDA & IBRD countries)", 
                          "Least developed countries: UN classification","Liechtenstein",
                          "Low & middle income","Low income","Lower middle income",
                          "Middle East & North Africa","Middle East & North Africa (excluding high income)",
                          "Middle East & North Africa (IDA & IBRD countries)",    
                          "Middle income","North America","Not classified" ,"OECD members",
                          "Other small states","Pacific island small states",
                          "Post-demographic dividend","Pre-demographic dividend","Small states",
                          "South Asia","South Asia (IDA & IBRD)","St. Martin (French part)",
                          "Sub-Saharan Africa","Sub-Saharan Africa (excluding high income)",
                          "Sub-Saharan Africa (IDA & IBRD countries)" ,"Upper middle income",
                          "Virgin Islands (U.S.)","World"))

# name_mismatch <- gdp$`Country Name`[!gdp$`Country Name` %in% df_exp$country]
# name_mismatch_codes <- gdp$`Country Code`[gdp$`Country Name` %in% name_mismatch]
# unique(df_exp$country[df_exp$iso3 %in% name_mismatch_codes])

# b<- df_ISO %>% filter(!country %in% births_2$country)
# births_1$country[grep("Czechia", births_1$country) ]
# df_ISO$country[grep("Greenland", df_ISO$country) ]
# Changing some country names to match the WHO TB data and ISO codes #

country_mapping <- c(
  "Bahamas, The" = "Bahamas",
  "Bolivia" = "Bolivia (Plurinational State of)",
  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
  "Congo, Rep." = "Congo",
  "Cote d'Ivoire" = "Côte d'Ivoire",
  "Curacao" = "Curaçao",
  "Czech Republic" = "Czechia",
  "Egypt, Arab Rep." = "Egypt",
  "Gambia, The" = "Gambia",
  "Hong Kong SAR, China" = "China, Hong Kong SAR",
  "Iran, Islamic Rep." = "Iran (Islamic Republic of)",
  "Korea, Dem. People’s Rep." = "Democratic People's Republic of Korea",
  "Korea, Rep." = "Republic of Korea",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Lao PDR" = "Lao People's Democratic Republic",
  "Macao SAR, China" = "China, Macao SAR",
  "Micronesia, Fed. Sts." = "Micronesia (Federated States of)",
  "Moldova" = "Republic of Moldova",
  "Slovak Republic" = "Slovakia",
  "St. Kitts and Nevis" = "Saint Kitts and Nevis",
  "St. Lucia" = "Saint Lucia",
  "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines",
  "Tanzania" = "United Republic of Tanzania",
  "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland",
  "United States" = "United States of America",
  "Venezuela, RB" = "Venezuela (Bolivarian Republic of)",
  "Vietnam" = "Viet Nam",
  "West Bank and Gaza" = "occupied Palestinian territory, including east Jerusalem",
  "Yemen, Rep." = "Yemen"
)

gdp <- gdp %>%
  mutate(`Country Name` = recode(`Country Name`, !!!country_mapping))

# GDP <- gdp |>  filter(year==max(year))
names <- unique(df_exp$country[!df_exp$iso3 %in% gdp$`Country Code`])
codes <- unique(df_exp$iso3[!df_exp$iso3 %in% gdp$`Country Code`])
years <- max(gdp$year)
# r.gdp <- un_gdp$Value[un_gdp$`Country or Area` %in% names]

gdps <- un_gdp$Value[un_gdp$`Country or Area` %in% names]

missing_gdp <- data.table(
  "Country Name" = names[1:length(gdps)],  # Trim to match gdps
  "Country Code" = codes[1:length(gdps)],
  "year" = years[1:length(gdps)],
  "ppp" = NA,
  "exch" = NA,
  "exch_ppp" = NA,
  "r.gdp" = NA,
  "gdp" = gdps  # Now all columns have the same length
)
gdp <- rbind(gdp,missing_gdp)

gdp <- gdp %>% mutate(ppp=ifelse(is.na(ppp),1,ppp))

GDP <- gdp |> 
  #filter(year == max(as.numeric(as.character(year)), na.rm = TRUE)) |>
  filter(year == max(year, na.rm = TRUE)) |>
  select(iso3 = `Country Code`, gdp) |> 
  distinct()

target_year <-2023

GDP <- gdp |> 
  #filter(year == max(as.numeric(as.character(year)), na.rm = TRUE)) |>
  filter(year == target_year) |>
  select(iso3 = `Country Code`, gdp) |> 
  distinct()

save(GDP, file = here("data/cost/outdata/GDP1.RData"))
saveRDS(GDP, file = here("data/cost/outdata/GDP1.Rds"))

names(tb_budget)
# budget data currently not being used in this analysis
tb_budget <- tb_budget[,c("country","iso2","iso3","iso_numeric","g_whoregion","year", 
                          "budget_cpp_dstb","budget_cpp_mdr","budget_cpp_tpt","budget_cpp_xdr",
                          "budget_fld","budget_lab","budget_mdrmgt","budget_orsrvy","budget_oth",
                          "budget_patsup","tx_dstb",	"tx_mdr",	"tx_tpt",	"tx_xdr")]

tb_budget[,bdstb:=rowSums(.SD), .SDcols= c("budget_fld","budget_lab","budget_mdrmgt",
                                           "budget_orsrvy","budget_oth","budget_patsup")]

budget <- c('budget_cpp_dstb', 'budget_cpp_mdr', 'budget_cpp_tpt', 'budget_cpp_xdr',
            'tx_dstb', 'tx_mdr', 'tx_tpt', 'tx_xdr')

# selecting the relevant expenditure categories
expend <- c('hcfvisit_dstb','hcfvisit_mdr', 'hosp_type_mdr','hospd_dstb_dur','hospd_dstb_prct','hospd_mdr_dur','hospd_mdr_prct',
            'exp_fld', 'exp_lab', 'exp_mdrmgt', 'exp_prog', 'exp_sld', 'exp_staff', 'exp_orsrvy', 'exp_oth')

# take a look at variable definitions using the package getTBinR which can be installed using devtools::install_github("seabbs/getTBinR")
# getTBinR::search_data_dict(var = expend)

tb_exp <- df_exp[,.(country, iso2, iso3, iso_numeric, g_whoregion, year, hcfvisit_dstb, hcfvisit_mdr, 
                    hosp_type_mdr, hospd_dstb_dur, hospd_dstb_prct, hospd_mdr_dur, hospd_mdr_prct, 
                    exp_fld, exp_lab, exp_mdrmgt, exp_prog,	exp_sld, exp_staff, exp_orsrvy, exp_patsup,
                    exp_cpp_dstb, exp_cpp_mdr, exp_cpp_xdr, exp_cpp_tpt)]

# specify analysis year
df_list <- list(tb_budget = tb_budget, 
                tb_exp = tb_exp,
                gdp = gdp, 
                exch = exch,
                ppp = ppp,
                exch_ppp = exch_ppp,
                defl = defl)  

max_years <- lapply(df_list, function(df) max(df$year, na.rm = TRUE))
print(unlist(max_years))
analysis_year <- as.numeric(min(unlist(max_years)))

tb_budget <- tb_budget[year==analysis_year]
tb_exp <- tb_exp[year==analysis_year]
gdp <- data.table(gdp)
gdp <- gdp[year==analysis_year,]

#tb_exp <- tb_budget %>% select(iso3,tx_dstb,tx_mdr,	tx_xdr) %>% left_join(tb_exp, by = "iso3")
tb_exp <- tb_budget %>% select(iso3,tx_dstb,tx_mdr,	tx_xdr) %>% right_join(tb_exp, by = "iso3")

gdp$iso3 <- gdp$`Country Code`

income_group <- income_group %>% 
  rename(`Country Name` = TableName) %>%
  mutate(`Country Name` = ifelse(`Country Name` == 'Venezuela, RB', 'Venezuela (Bolivarian Republic of)', `Country Name`))

gdp <- income_group %>% 
  select(`Country Name`, `Country Code`, IncomeGroup) %>% 
  filter(`Country Code` %in% gdp$`Country Code`) %>% 
  right_join(gdp,  by=c('Country Name', 'Country Code'))

# gdp$gdp <- gdp$'2019'
tb_exp <- gdp %>% select(iso3, r.gdp, gdp,ppp, exch_ppp) %>% left_join(tb_exp, by = "iso3")
tb_exp <- df %>% filter(year==analysis_year) %>% select(iso3, e_inc_num) %>% left_join(tb_exp, by = "iso3")
tb_exp <- df_mdr %>% filter(year==analysis_year) %>% select(iso3, e_inc_rr_num) %>% left_join(tb_exp, by = "iso3")

# search_data_dict(var = c("e_inc_num", "e_inc_rr_num"))
tb_exp[, e_inc_ds_num := ifelse(is.na(e_inc_num) & is.na(e_inc_rr_num), NA_real_, 
                                rowSums(.SD, na.rm = T)), .SDcols = c("e_inc_num", "e_inc_rr_num")]

names(tb_exp)
# Actual expenditure on laboratory infrastructure,staff, equipment and supplies (US Dollars)
expenditures <- c('exp_lab', 'exp_staff', 'exp_prog', 'exp_orsrvy', 'exp_patsup', 'exp_mdrmgt') 
resource.use <- c('hcfvisit_dstb', 'hospd_dstb_prct', 'hospd_dstb_dur', 'hcfvisit_mdr', 'hospd_mdr_prct', 'hospd_mdr_dur', 'hosp_type_mdr')
patients <- c('tx_dstb', 'tx_mdr', 'tx_xdr', 'e_inc_ds_num', 'e_inc_rr_num') #Number of TB patients
drugs <- c('exp_cpp_dstb', 'exp_cpp_mdr', 'exp_cpp_xdr', 'exp_cpp_tpt') #exp_cpp_dstb - average cost of drugs spent per patient starting first-line TB treatment

# explore data
# missing.exp <- tb_exp %>% select(exp_lab,exp_staff, exp_prog, exp_orsrvy, exp_patsup, exp_mdrmgt)  %>%
#   gather(key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   group_by(key) %>%
#   mutate(total = n()) %>%
#   group_by(key, total, isna) %>%
#   summarise(num.isna = n()) %>%
#   mutate(pct = num.isna / total * 100)
# 
# levels <-
#   (missing.exp  %>% filter(isna == T) %>% arrange(desc(pct)))$key
# 
# percentage.plot <- missing.exp %>%
#   ggplot() +
#   geom_bar(aes(x = reorder(key, desc(pct)), 
#                y = pct, fill=isna), 
#            stat = 'identity', alpha=0.8) +
#   scale_x_discrete(limits = levels) +
#   scale_fill_manual(name = "", 
#                     values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
#   coord_flip() +
#   labs(title = "Percentage of missing values", x =
#          'Variable', y = "% of missing values")
# 
# percentage.plot
# ggsave(plot=percentage.plot, filename = here('output','Missing NTP expenditures.png'), w=10,h=5, dpi = 600)
# 
# row.plot <- tb_exp %>% select(expenditures) %>%
#   mutate(id = row_number()) %>%
#   gather(-id, key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   ggplot(aes(key, id, fill = isna)) +
#   geom_raster(alpha=0.8) +
#   scale_fill_manual(name = "",
#                     values = c('steelblue', 'tomato3'),
#                     labels = c("Present", "Missing")) +
#   scale_x_discrete(limits = levels) +
#   labs(x = "Variable",
#        y = "Row Number", title = "Missing values in rows") +
#   coord_flip()
# 
# row.plot
# ggsave(plot=row.plot, filename = here('output','Missing NTP expenditures1.png'), w=10,h=5, dpi = 600)
# 
# missing.res.use <- tb_exp %>% select(resource.use) %>%
#   gather(key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   group_by(key) %>%
#   mutate(total = n()) %>%
#   group_by(key, total, isna) %>%
#   summarise(num.isna = n()) %>%
#   mutate(pct = num.isna / total * 100)
# 
# levels <-
#   (missing.res.use  %>% filter(isna == T) %>% arrange(desc(pct)))$key
# 
# percentage.plot <- missing.res.use %>%
#   ggplot() +
#   geom_bar(aes(x = reorder(key, desc(pct)), 
#                y = pct, fill=isna), 
#            stat = 'identity', alpha=0.8) +
#   scale_x_discrete(limits = levels) +
#   scale_fill_manual(name = "", 
#                     values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
#   coord_flip() +
#   labs(title = "Percentage of missing values", x =
#          'Variable', y = "% of missing values")
# 
# percentage.plot
# ggsave(plot=percentage.plot, filename = here('output','Missing resource use.png'), w=10,h=5, dpi = 600)
# 
# row.plot <- tb_exp %>% select(resource.use) %>%
#   mutate(id = row_number()) %>%
#   gather(-id, key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   ggplot(aes(key, id, fill = isna)) +
#   geom_raster(alpha=0.8) +
#   scale_fill_manual(name = "",
#                     values = c('steelblue', 'tomato3'),
#                     labels = c("Present", "Missing")) +
#   scale_x_discrete(limits = levels) +
#   labs(x = "Variable",
#        y = "Row Number", title = "Missing values in rows") +
#   coord_flip()
# 
# row.plot
# ggsave(plot=row.plot, filename = here('output','Missing resource use1.png'), w=10,h=5, dpi = 600)

# summary(tb_exp %>% select(expenditures))
# sum(complete.cases(tb_exp %>% select(expenditures)))
exp_complete <- tb_exp %>% drop_na(exp_lab,exp_staff, exp_prog, exp_orsrvy, exp_patsup, exp_mdrmgt)
exp_complete <- setDT(exp_complete)
# 
# summary(tb_exp %>% select(resource.use))
# sum(complete.cases(tb_exp %>% select(resource.use)))

# impute resource use - impute missing data with mean by WHO Region
names(tb_exp)

resources_complete <- tb_exp
resources_complete <- resources_complete %>% group_by(g_whoregion) %>%
  mutate(across(all_of(c(resource.use,drugs)), ~ifelse(is.na(.x), mean(.x,na.rm=TRUE),.x))) %>%
  ungroup()

# summary(tb_exp %>% select(resource.use))
# summary(tb_exp %>% drop_na(hcfvisit_dstb, hospd_dstb_prct, hospd_dstb_dur, hcfvisit_mdr, hospd_mdr_prct, hospd_mdr_dur, hosp_type_mdr) %>% select(resource.use))
# summary(resources_complete %>% select(resource.use))

# (tb_exp %>% filter(across(resource.use, ~is.na(.x))) %>% filter(is.na(g_whoregion)) %>% select(iso3, iso2, country, g_whoregion)) 

# TODO: not sure why i'm doing this

# resources_complete <- resources_complete %>% 
#   drop_na(hcfvisit_dstb, hospd_dstb_prct, hospd_dstb_dur, hcfvisit_mdr, hospd_mdr_prct, hospd_mdr_dur, hosp_type_mdr)

# sum(complete.cases(resources_complete %>% select(resource.use)))
resources_complete <- setDT(resources_complete)

names(resources_complete)
names(resources_complete)

  df_dic |> 
    filter(variable_name %in% c("hcfvisit_dstb", "hospd_dstb_prct", "hospd_dstb_dur", 
                                "hcfvisit_mdr", "hospd_mdr_prct", "hospd_mdr_dur")) |> 
    select(variable_name, definition)|> 
  knitr::kable()
  
  resources_complete |> 
    select(iso3, g_whoregion, 
           hcfvisit_dstb, hospd_dstb_prct, hospd_dstb_dur, 
           hcfvisit_mdr, hospd_mdr_prct, hospd_mdr_dur) |>  
    pivot_longer(
      cols = -c(iso3,g_whoregion),
      names_to = 'variable_name',
      values_to = "value") |>
    left_join(df_dic |> 
                filter(variable_name %in% c("hcfvisit_dstb", "hospd_dstb_prct", "hospd_dstb_dur", 
                                            "hcfvisit_mdr", "hospd_mdr_prct", "hospd_mdr_dur")) |> 
                select(variable_name, definition), by = "variable_name") |> 
    group_by(g_whoregion, variable_name, definition) |> 
    summarise(mean = mean(value, na.rm = TRUE))
  
# WHO-CHOICE country-specific costs for inpatient and outpatient health service delivery
# Based on https://resource-allocation.biomedcentral.com/articles/10.1186/s12962-018-0095-x
# Values of variables used for prediction of the unit cost
# Ownership is set to public provider; location is set to urban location for outpatient care

# Inpatient care
admission.l3 <- 4971
admission.l4.5 <- 14028
occupancy.l3 <- 0.756
occupancy.l4.5 <- 0.810
a.los.l3 <- 7.14
a.los.l4.5 <- 9.75

# Total outpatient visits per facility per year (p80)
opd.vsts.l1 <- 67656
opd.vsts.l2 <- 46434
opd.vsts.l3 <- 93739
opd.vsts.l4.5 <- 281156

# Visits per provider per day (p80)
vsts.per.provider.fl1 <- 8.96
vsts.per.provider.fl2 <- 9.52
vsts.per.provider.fl3 <- 3.22
vsts.per.provider.fl4.5 <- 2.36

# Regression coefficients and 95% confidence interval: natural log of cost per outpatient visit expressed in 2007
gdppc.opd <- 0.865; gdppc.opd.lo <- 0.826; gdppc.opd.hi <- 0.905
visits <- -0.0142; visits.lo <- -0.0272; visits.hi <-  -0.00119
vsts.per.provider <- -0.0412; vsts.per.provider.lo <- -0.0578; vsts.per.provider.hi <- -0.0246
urban.opd <- 0.352; urban.opd.lo <- 0.268; urban.opd.hi <- 0.435
public.opd <- -0.290; public.opd.lo <- -0.330; public.opd.hi <- -0.249
private.opd <- 0.0532; private.opd.lo <- 0.00479; private.opd.hi <- 0.102
level.2 <- 0.208; level.2.lo <- 0.144; level.2.hi <- 0.271
level.3 <- 0.304; level.3.lo <- 0.213; level.3.hi <- 0.395
level.4 <- 0.348; level.4.lo <- 0.279; level.4.hi <- 0.417
Colombia.opd <- 0.628; Colombia.opd.lo <- 0.542; Colombia.opd.hi <- 0.713
Brazil.opd <- -1.563; Brazil.opd.lo <- -1.656; Brazil.opd.hi <- -1.470
Brazil.level.3 <- -0.245; Brazil.level.3.lo <- -0.337; Brazil.level.3.hi <- -0.153
constant.opd <- -4.534; constant.opd.lo <- -4.797; constant.opd.hi <- -4.271

# OPD visit unit costs
resources_complete[,c_opd.m:=exp(constant.opd + gdppc.opd*log(gdp) + visits*log(opd.vsts.l1) + vsts.per.provider*log(vsts.per.provider.fl1) + public.opd)]
resources_complete[,c_opd.lo:=exp(constant.opd.lo + gdppc.opd.lo*log(gdp) + visits.lo*log(opd.vsts.l1) + vsts.per.provider.lo*log(vsts.per.provider.fl1) + public.opd.lo)]
resources_complete[,c_opd.hi:=exp(constant.opd.hi + gdppc.opd.hi*log(gdp) + visits.hi*log(opd.vsts.l1) + vsts.per.provider.hi*log(vsts.per.provider.fl1) + public.opd.hi)]
resources_complete[,c_opd.sd:=(c_opd.hi-c_opd.lo)/4]

# Inpatient care

# Total inpatient admissions per facility per year (p80)
admission.l3 <- 4971
admission.l4.5 <- 14028

# Bed occupancy rate (p80)
occupancy.l3 <- 0.756
occupancy.l4.5 <- 0.810

# Average length of stay, inpatient (p80)
a.los.l3 <- 7.14
a.los.l4.5 <- 9.75

# Regression coefficients and 95% confidence interval: natural log of cost per inpatient bed day expressed in 2007
gdppc.ipd <- 1.192; gdppc.ipd.lo <- 1.111; gdppc.ipd.hi <- 1.272;
occupancy <- -0.0201; occupancy.lo <- -0.0340; occupancy.hi <- -0.00623;
a.los <- -0.600; a.los.lo <- -0.649; a.los.hi <- -0.550;
admissions <- 0.0252; admissions.lo <- 0.00471; admissions.hi <- 0.0457;
district <- -0.204;  district.lo <- -0.275; district.hi <- -0.132;
teaching <- 0.257; teaching.lo <- 0.163; teaching.hi <- 0.351;
public.ipd <- -0.144; public.ipd.lo <- -0.182; public.ipd.hi <- -0.107;
private.ipd <- 0.110; private.ipd.lo <- 0.0710; private.ipd.hi <- 0.148;
Brazil.ipd <-  -1.638; Brazil.ipd.lo <- -1.694; Brazil.ipd.hi <- -1.583;
constant.ipd <- -4.277; constant.ipd.lo <- -5.035; constant.ipd.hi <- -3.519;

# TODO: To consider using data on hospital type used for MRD-TB hospitalization
# getTBinR::search_data_dict('hosp_type_mdr')
# table(tb_exp$g_whoregion,tb_exp$hosp_type_mdr)

# IPD stay unit costs
resources_complete[,c_ipd.m:=exp(constant.ipd + gdppc.ipd*log(gdp) + occupancy*log(occupancy.l3) + a.los*log(a.los.l3) + admissions*log(admission.l3) + district + public.ipd + Brazil.ipd)]
resources_complete[,c_ipd.lo:=exp(constant.ipd.lo + gdppc.ipd.lo*log(gdp) + occupancy.lo*log(occupancy.l3) + a.los.lo*log(a.los.l3) + admissions.lo*log(admission.l3) + district.lo + public.ipd.lo + Brazil.ipd.lo)]
resources_complete[,c_ipd.hi:=exp(constant.ipd.hi + gdppc.ipd.hi*log(gdp) + occupancy.hi*log(occupancy.l3) + a.los.hi*log(a.los.l3) + admissions.hi*log(admission.l3) + district.hi + public.ipd.hi + Brazil.ipd.hi)]
resources_complete[,c_ipd.sd:=(c_ipd.hi-c_ipd.lo)/4]

# OPD visit costs for DSTB
resources_complete[,c.opd_dstb.m:=hcfvisit_dstb * c_opd.m]
resources_complete[,c.opd_dstb.lo:=hcfvisit_dstb * c_opd.lo]
resources_complete[,c.opd_dstb.hi:=hcfvisit_dstb * c_opd.hi]
resources_complete[,c.opd_dstb.sd:=(c.opd_dstb.hi-c.opd_dstb.lo)/4]

# OPD visit costs for MDRTB
resources_complete[,c.opd_mdrtb.m:=hcfvisit_mdr * c_opd.m]
resources_complete[,c.opd_mdrtb.lo:=hcfvisit_mdr * c_opd.lo]
resources_complete[,c.opd_mdrtb.hi:=hcfvisit_mdr * c_opd.hi]
resources_complete[,c.opd_mdrtb.sd:=(c.opd_mdrtb.hi-c.opd_mdrtb.lo)/4]

# IPD stay costs for DSTB
resources_complete[,c.ipd_dstb.m:=hospd_dstb_prct/100*hospd_dstb_dur* c_ipd.m]
resources_complete[,c.ipd_dstb.lo:=hospd_dstb_prct/100*hospd_dstb_dur* c_ipd.lo]
resources_complete[,c.ipd_dstb.hi:=hospd_dstb_prct/100*hospd_dstb_dur* c_ipd.hi]
resources_complete[,c.ipd_dstb.sd:=(c.ipd_dstb.hi-c.ipd_dstb.lo)/4]

# IPD stay costs for MDRTB
resources_complete[,c.ipd_mdrtb.m:=hospd_mdr_prct/100*hospd_mdr_dur*c_ipd.m]
resources_complete[,c.ipd_mdrtb.lo:=hospd_mdr_prct/100*hospd_mdr_dur*c_ipd.lo]
resources_complete[,c.ipd_mdrtb.hi:=hospd_mdr_prct/100*hospd_mdr_dur*c_ipd.hi]
resources_complete[,c.ipd_mdrtb.sd:=(c.ipd_mdrtb.hi-c.ipd_mdrtb.lo)/4]

summary(resources_complete)

# Drug costs 
resources_complete[,c('cpp_dstb', 'cpp_mdr', 'cpp_xdr','cpp_tpt'):=
                     .(exp_cpp_dstb, exp_cpp_mdr, exp_cpp_xdr, exp_cpp_tpt)]

# these look extremely high
resources_complete %>%
  select(country, iso3, g_whoregion, 
         cpp_dstb, cpp_mdr, cpp_xdr, cpp_tpt) %>%
  pivot_longer(cols = c(cpp_dstb, cpp_mdr, cpp_xdr, cpp_tpt),
               names_to = "drug_type", values_to = "cost") %>%
  filter(cost > 20000) 

names(tb_exp)
tb_exp |> 
  filter(country=='Marshall Islands') |> 
  select(iso3, country, exp_lab, exp_staff, exp_prog, exp_orsrvy, exp_patsup, 
         tx_dstb, tx_mdr, tx_xdr,
         exp_mdrmgt, exp_cpp_dstb, exp_cpp_mdr, exp_cpp_xdr, exp_cpp_tpt)
          
# Define the threshold for extreme values (e.g., 20000)
extreme_threshold <- 20000


# First replace extreme values with NA, then replace NAs with regional mean
resources_complete <- resources_complete %>% 
  mutate(across(c(cpp_dstb, cpp_mdr, cpp_xdr, cpp_tpt), 
                ~ifelse(.x > extreme_threshold, NA, .x))) %>%
  group_by(g_whoregion) %>%
  mutate(across(c(cpp_dstb, cpp_mdr, cpp_xdr, cpp_tpt), 
                ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))) %>%
  ungroup()

# check
resources_complete %>%
  select(country, iso3, g_whoregion, 
         cpp_dstb, cpp_mdr, cpp_xdr, cpp_tpt) %>%
  pivot_longer(cols = c(cpp_dstb, cpp_mdr, cpp_xdr, cpp_tpt),
               names_to = "drug_type", values_to = "cost") %>%
  filter(cost > 20000) 

# NTP programme costs

# Correction for Russia as described in the Global TB report 2019
exp_complete[,exp_lab:=ifelse(country=='Russian Federation',exp_lab * 29/100, exp_lab)]
exp_complete[,exp_staff:=ifelse(country=='Russian Federation',exp_staff * 29/100, exp_staff)]
exp_complete[,exp_mdrmgt:=ifelse(country=='Russian Federation', (exp_mdrmgt + exp_lab * 71/100 + exp_staff * 71/100), exp_mdrmgt)]

# DSTB & MDRTB programme costs
exp_complete[,exp_dstb:=rowSums(.SD), .SDcol=c('exp_lab', 'exp_prog', 'exp_staff', 'exp_orsrvy', 'exp_patsup')]
exp_complete[,exp_mdrtb:=rowSums(.SD), .SDcol=c('exp_mdrmgt')]

# DSTB & MDRTB programme unit costs (dividing total costs by number of patients treated)
# Currently using Number of patients expected to start drug-susceptible TB treatment
exp_complete[,exp_dstb:=exp_dstb/e_inc_ds_num]
exp_complete[,exp_mdrtb:=exp_mdrtb/e_inc_rr_num]

# Drugs are excluded
# exp_complete[,dstb_drugs:=exp_fld]
# exp_complete[,mdrtb_drugs:=exp_sld]
# exp_complete[,xdr_drugs:=exp_cpp_xdr]
# exp_complete[,tpt_drugs:=exp_cpp_tpt]

# tx_dstb - Number of patients expected to start drug-susceptible TB treatment
# tx_mdr - Number of patients expected to start MDR-TB treatment
names(exp_complete)
length(unique(exp_complete$iso3))
drugs <- exp_complete[,.(country, iso3, g_whoregion, gdp, year,
                         dstb=exp_cpp_dstb, mdrtb=exp_cpp_mdr, xdr=exp_cpp_xdr,tpt=exp_cpp_tpt)]
length(unique(drugs$iso3))
drugs <- unique(drugs)

prog_costs <- exp_complete[,.(country, iso3, g_whoregion, gdp, tx_dstb,tx_mdr, tx_xdr, exp_dstb, exp_mdrtb, e_inc_ds_num, e_inc_rr_num)]
prog_costs <- prog_costs %>% mutate_all(function(x) ifelse(is.infinite(x), 0, x))
prog_costs <- prog_costs %>% mutate_all(function(x) ifelse(is.nan(x), 0, x))
summary(prog_costs %>% select(exp_dstb, exp_mdrtb))

resources_complete <- data.table(resources_complete)
care_costs<- resources_complete[,.(country, iso3, g_whoregion, gdp, 
                                   tx_dstb,tx_mdr, tx_xdr,
                                   c.opd_dstb.m, c.ipd_dstb.m, 
                                   c.opd_mdrtb.m, c.ipd_mdrtb.m, 
                                   cpp_dstb, cpp_mdr, cpp_xdr, cpp_tpt,
                                   e_inc_ds_num, e_inc_rr_num)]

summary(care_costs %>% select(c.opd_dstb.m, c.ipd_dstb.m, 
                              c.opd_mdrtb.m, c.ipd_mdrtb.m,
                              cpp_dstb, cpp_mdr, cpp_xdr, cpp_tpt,))

# NTP program cost regression fitting - to be used for countries with missing expenditure data
ntpcst <- prog_costs %>% select(country, g_whoregion, e_inc_ds_num, e_inc_rr_num, gdp, exp_dstb, exp_mdrtb) 
summary(ntpcst %>% select(exp_dstb, exp_mdrtb))

exp <- c('exp_dstb', 'exp_mdrtb')
# (ntpcst %>% filter(across(exp, ~is.na(.x))))

ntpcst <- ntpcst[!(exp_dstb==0)]; ntpcst <- ntpcst[!(exp_mdrtb==0)]
ntpcst <- ntpcst %>% gather(tb, ntpexp, exp_dstb, exp_mdrtb)
ntpcst<-setDT(ntpcst)
ntpcst[,log.gdp:=log(gdp)]; ntpcst[,log.ntpcst:=log(ntpexp)];
ntpcst[,tb:=ifelse(tb=='exp_dstb', 'dstb', 'mdrtb')];
tb <- c('dstb', 'mdrtb')
plot.labels <- c(dstb='Drug-susceptible TB (DS TB)', mdrtb='Multidrug-resistant TB (MDR TB)')


ntpcst <- ntpcst[!is.na(log.ntpcst)]
ntpcst <- ntpcst[!(log.ntpcst=='Inf')]

# Log-Log linear regression
# Relationship between gdp and costs “look more normal” after transformation.
# The relationship between the log transformed variables is more linear

# log(cost) = β0 + β1. log(gdp)
llr.ntpcst <- lm(log.ntpcst~log.gdp, ntpcst)
llr.ntpcst.dstb <- lm(log.ntpcst~log.gdp, subset(ntpcst, tb=='dstb'))
llr.ntpcst.mdrtb <- lm(log.ntpcst~log.gdp, subset(ntpcst, tb=='mdrtb'))
# summary(llr.ntpcst.dstb)
# summary(llr.ntpcst.mdrtb)

# Ploting the log-log regression model results in the original scales
ntp.data = data.table(x = exp(llr.ntpcst$model$log.gdp),
                  y = exp(predict(llr.ntpcst)), 
                  lwr=exp(predict(llr.ntpcst, interval = "confidence")[,2]), 
                  upr = exp(predict(llr.ntpcst, interval = "confidence")[,3]))

names(ntp.data) <- c("gdp1","cstntp",'lwr', 'upr')
ntp.data <- cbind(ntpcst, ntp.data)

plot <- ggplot(ntp.data, aes(x = gdp, y = ntpexp))  +
  geom_jitter() +
  # geom_point(aes(size=tx_mdr+tx_xdr, fill=g_whoregion), alpha = 0.75, shape = 21) +
  # geom_smooth(span   = 1,    color = viridis(1, begin = 0.6), se = FALSE, linetype =
  #               "dashed")  +
  geom_line(data = ntp.data, aes(x = gdp, y = cstntp),
            color = viridis::viridis(1, end = 0), size = 0.7) +
  geom_ribbon(data = ntp.data, aes(ymin = lwr, ymax = upr), alpha = .15)+
  facet_wrap(~tb, scales = "free", labeller = labeller(tb=plot.labels)) +
  scale_x_continuous(trans = 'log2',labels = comma, breaks = c(100,500,1000,5000,10000,20000,50000,150000)) +
  scale_y_continuous(trans = 'log2',labels = comma, breaks = c(100,500,1000,5000,10000,20000)) +
  ggthemes::theme_base() +
  # ggthemes::theme_economist_white() +
  labs(x="GDP per capita (2020 US$, log scale)", y="Cost per patient treated (2020 US$, log scale)") 

# plot
# ggsave(plot=plot, filename = here('output','NTP prog costs.png'), w=10,h=5, dpi = 600)

# Health care cost regression fitting - to be used for countries with missing opd/ipd data
tbcst.care <- care_costs %>% select(country, g_whoregion, 
                                    e_inc_ds_num, e_inc_rr_num, gdp, 
                                    c.opd_dstb.m, c.opd_mdrtb.m, 
                                    c.ipd_dstb.m, c.ipd_mdrtb.m,
                                    cpp_dstb, cpp_mdr, cpp_xdr, cpp_tpt) 
summary(tbcst.care %>% select(c.opd_dstb.m, c.opd_mdrtb.m, 
                              c.ipd_dstb.m, c.ipd_mdrtb.m,
                              cpp_dstb, cpp_mdr, cpp_xdr, cpp_tpt))

care <- c('gdp','c.opd_dstb.m', 'c.opd_mdrtb.m', 
          'c.ipd_dstb.m', 'c.ipd_mdrtb.m', 
          'cpp_dstb', 'cpp_mdr', 'cpp_xdr', 'cpp_tpt')
# (tbcst.care %>% filter(across(care, ~is.na(.x))))

tbcst.care <- tbcst.care %>% filter_at(vars(care), all_vars((.) != 0)) 
tbcst.care <- tbcst.care %>%
  mutate_at(care, list(log = ~ log(.)))

tb <- c('dstb.opd', 'mdrtb.opd','dstb.ipd', 'mdrtb.ipd',
        'cpp_dstb', 'cpp_mdr', 'cpp_xdr', 'cpp_tpt')
plot.labels <- c(dstb.opd='Drug-susceptible TB (DS TB), OPD', 
                 mdrtb.opd='Multidrug-resistant TB (MDR TB), OPD', 
                 dstb.ipd='Drug-susceptible TB (DS TB), IPD', 
                 mdrtb.ipd='Multidrug-resistant TB (MDR TB), IPD',
                 cpp_dstb='Drug-susceptible TB (DS TB), Drugs',
                 cpp_mdr='Multidrug-resistant TB (MDR TB), Drugs',
                 cpp_xdr='Extensively drug-resistant TB (XDR TB), Drugs',
                 cpp_tpt='Tuberculosis preventive treatment (TPT), Drugs')
tbcst.care <- tbcst.care %>% 
  gather(tb, log.cost, c.opd_dstb.m_log, c.opd_mdrtb.m_log, 
         c.ipd_dstb.m_log, c.ipd_mdrtb.m_log, 
         cpp_dstb_log, cpp_mdr_log, cpp_xdr_log, cpp_tpt_log)
tbcst.care<-setDT(tbcst.care)
tbcst.care <- tbcst.care[!is.na(log.cost)]
tbcst.care <- tbcst.care[!(log.cost=='Inf')]

# Log-Log linear regression
# Relationship between gdp and costs “look more normal” after transformation.
# The relationship between the log transformed variables is more linear

# log(cost) = β0 + β1. log(gdp)
llr.carecst<- lm(log.cost~gdp_log, tbcst.care)
llr.carecst.dstb.opd <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='c.opd_dstb.m_log'))
llr.carecst.mdrtb.opd <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='c.opd_mdrtb.m_log'))
llr.carecst.dstb.ipd <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='c.ipd_dstb.m_log'))
llr.carecst.mdrtb.ipd <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='c.ipd_mdrtb.m_log'))
llr.carecst.dstb.drugs <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='cpp_dstb_log'))
llr.carecst.mdrtb.drugs <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='cpp_mdr_log'))
llr.carecst.xdr.drugs <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='cpp_xdr_log'))
llr.carecst.tpt.drugs <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='cpp_tpt_log'))
# summary(llr.carecst.dstb.opd)
# summary(llr.carecst.mdrtb.opd)
# summary(llr.carecst.dstb.ipd)
# summary(llr.carecst.mdrtb.ipd)

# Ploting the log-log regression model results in the original scales
care.data = data.table(x = exp(llr.carecst$model$gdp_log),
                  y = exp(predict(llr.carecst)), 
                  lwr=exp(predict(llr.carecst, interval = "confidence")[,2]), 
                  upr = exp(predict(llr.carecst, interval = "confidence")[,3]))

names(care.data) <- c("gdp1","care.cost",'lwr', 'upr')
care.data <- cbind(tbcst.care, care.data)

care.data <- care.data %>% mutate(tb=factor(tb))
care.data <- care.data %>% 
  mutate(tb=recode_factor(tb,
                          'c.opd_dstb.m_log'='dstb.opd',
                          'c.opd_mdrtb.m_log'='mdrtb.opd',
                          'c.ipd_dstb.m_log'='dstb.ipd',
                          'c.ipd_mdrtb.m_log'='mdrtb.ipd',
                          'cpp_dstb_log'='cpp.dstb',
                          'cpp_mdr_log'='cpp.mdr',
                          'cpp_xdr_log'='cpp.xdr',
                          'cpp_tpt_log'='cpp.tpt'))
plot <- ggplot(care.data, aes(x = gdp, y = exp(log.cost)))  +
  geom_jitter() +
  # geom_point(aes(size=tx_mdr+tx_xdr, fill=g_whoregion), alpha = 0.75, shape = 21) +
  # geom_smooth(span   = 1,    color = viridis(1, begin = 0.6), se = FALSE, linetype =
  #               "dashed")  +
  geom_line(data = care.data, aes(x = gdp, y = care.cost),
            color = viridis::viridis(1, end = 0), size = 0.7) +
  geom_ribbon(data = care.data, aes(ymin = lwr, ymax = upr), alpha = .15)+
  scale_x_continuous(trans = 'log2',labels = comma, breaks = c(100,500,1000,5000,10000,20000,50000,150000)) +
  scale_y_continuous(trans = 'log2',labels = comma, breaks = c(100,1000,5000,20000)) +
  facet_wrap(~tb, scales = "free_y", labeller = labeller(tb=plot.labels)) +
  ggthemes::theme_base() +
  # ggthemes::theme_economist_white() +
  labs(x="GDP per capita (2020 US$, log scale)", y="Cost per patient treated (2020 US$, log scale)") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.4),
        text = element_text(size=14))
# plot
# ggsave(plot=plot, filename = here('output','TB care costs1.png'), w=10,h=5, dpi = 600)

