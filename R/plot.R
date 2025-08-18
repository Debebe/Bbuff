library(here)
library(ggplot2)

BCG <- readRDS(here("data/BCG.rds"))


high_tb_iso3 <- c(
  "AGO", "BGD", "BRA", "CAF", "CHN", "COG", "PRK", "COD", "ETH", "GAB",
  "IND", "IDN", "KEN", "LSO", "LBR", "MNG", "MOZ", "MMR", "NAM", "NGA",
  "PAK", "PNG", "PHL", "SLE", "SOM", "ZAF", "THA", "UGA", "TZA", "ZMB"
)




bb <- BCG %>% filter(Iso3 %in% high_tb_iso3)

# 
# tar_counttries <-gdp_inc_le%>%filter(Iso3%in% high_tb_iso3)
# 
# tar_data <- gdp_inc_le%>%filter(Iso3%in% high_tb_iso3)%>%
#   select(Country= country,Iso3, Year,Pop,Age,GDP, incbest, inclo, inchi, LE,
#          ucost_delv_aver, ucost_delv_median, ucost_delv_lower, ucost_delv_upper) 

isos <- unique(BCG$Iso3)

bcgplot <- function(BCG, Isos) {
  BCG %>%
    filter(Year == 2023, !is.na(coverage), Iso3 %in% Isos) %>%
    ggplot(aes(cat, coverage, fill = cat)) +
    geom_bar(stat = "identity") +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.title = element_blank()
    ) +
    geom_text(aes(x = cat, label = coverage),
      vjust = 1.6,
      color = "black", size = 3.0
    ) +
    facet_wrap(~Iso3) +
    xlab("") +
    ylab("BCG coverage")
}

bcgplot(BCG, Isos = isos[1:36]) %>% ggsave(file = here("plots/p1.png"))
bcgplot(BCG, Isos = isos[37:72]) %>% ggsave(file = here("plots/p2.png"))
bcgplot(BCG, Isos = isos[73:114]) %>% ggsave(file = here("plots/p3.png"))
bcgplot(BCG, Isos = isos[115:150]) %>% ggsave(file = here("plots/p4.png"))
bcgplot(BCG, Isos = isos[151:168]) %>% ggsave(file = here("plots/p5.png"))
bcgplot(bb, Isos = high_tb_iso3) %>% ggsave(file = here("plots/hi_tb_cnts.png"))

