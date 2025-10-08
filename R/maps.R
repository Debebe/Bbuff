library(here)
library(data.table)
library(ggplot2)
library(sf)
library(wbmapdata) ## https://github.com/petedodd/wbmapdata


## =============== deaths
load(here("outputs/deaths.RData"))
deaths <- dcast(
  deaths[, .(
    iso3, variable, type, av
  )],
  iso3 ~ variable + type,
  value.var = "av"
)

## merge
dmap <- sp::merge(deaths, world, by = "iso3", all.y = TRUE)
dmap <- st_as_sf(dmap)


##  version without points
p <- ggplot(data = dmap) +
  geom_sf(aes(fill = 1e4 * rslt_tb_deaths_percapita)) +
  scale_fill_distiller(
    name = "TB deaths averted per 10,000",
    na.value = "grey", trans = "sqrt",
    palette = "Reds", direction = 1
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(1, "lines")
  ) +
  guides(
    fill = guide_colourbar(order = 1, position = "top"),
    size = guide_legend(order = 2, position = "bottom")
  )
p

## version with points
sznm <- "TB deaths averted (millions)"
p2 <- p +
  geom_sf(
    aes(
      geometry = mid,
      size = rslt_tb_deaths_absolute / 1e6
    ),
    show.legend = "point",
    shape = 1
  ) +
  scale_size_continuous(name = sznm)
p2


ggsave(p2, file = here("outputs/map_deaths.png"), w = 12, h = 10)


## =========== ICER
load(here("outputs/CEA.RData"))

## merge
cmap <- sp::merge(CEA, world, by = "iso3", all.y = TRUE)
cmap <- st_as_sf(cmap)

##  version without points
lbz <- c(0, 0.3, 1, 2, 5, 10)
p <- ggplot(data = cmap) +
  geom_sf(aes(fill = ICER / GDP)) +
  scale_fill_gradient2(
    name = "ICER / GDP (log scale)",
    midpoint = 0,
    low = "blue",
    high = "red",
    trans = "log",
    breaks = lbz,
    labels = lbz
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(1, "lines")
  ) +
  guides(
    fill = guide_colourbar(position = "top")
  )
p


ggsave(p, file = here("outputs/map_ICERs.png"), w = 12, h = 10)
