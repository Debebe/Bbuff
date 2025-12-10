## meta-analysis for BCG conditional efficacy against TBM
library(here)
library(meta)
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)  # for draw_label
library(magick) # to stich images together


## copying data from google doc into here manually
severe_TBM <- data.table(
  study = c(
    "Sumaya, 1975", "Doerr, 1995", "Paganini, 2000",
    "Degefie, 2003", "Kumar, 2005", "VanWell, 2009",
    "Tinsa, 2010"
  ),
  n_sever_sequalae = c(7, 4, 9, 9, 60, 77, 1),
  n_sequalae = c(13, 4, 19, 9, 107, 217, 4),
  RXDur = c(
    "24months", "12months", "10months",
    "10months", "12months", "6months", "38months"
  )
)


## fit:
m <- metaprop(
  event = n_sever_sequalae,
  n = n_sequalae,
  studlab = study,
  data = severe_TBM[RXDur != "6months"],
  # data = severe_TBM,
  sm = "PLOGIT", # or use "PFT" / "PLN" / "PRAW"
  method = "Inverse", title = "Debebe"
)
summary(m)

## save
save(m, file = here("indata/m.Rdata"))

## plot
png(
  filename = here("outputs/forest.png"),
  width = 20, height = 10,
  units = "cm", pointsize = 12,
  bg = "white", res = 250
)
forest(m,
      leftcols = c("studlab","n", "event"),
      leftlabs = c("Study", "TBM", "Severe sequelae"),
  fontsize = 8,
  fontfamily = "serif", 
  xlab = "Proportion with TBM sequelae (severe)"
  )
dev.off()


##======Meta RR of TBM========
bcg_TBM<- read_excel("indata/bcg_tb_meningitis.xlsx") %>%
  dplyr::select(study, vac=vaccine, TB, MTB)%>%
  pivot_wider(names_from = vac, 
              values_from = c(TB, MTB), names_prefix = "vac") %>%
  filter(TB_vac1>0)%>%
  mutate(study= case_when(grepl("Chicago", study) ~"Rosenthal, 1945", 
                          grepl("Trub", study) ~"Trub, 1970",
                          grepl("Chandra",study) ~ "Chandra, 1975",
                          grepl("Tidjani", study) ~ "Tidjani, 1992",
                          grepl("Mahomed", study) ~ "Mahomed, 2006",
                          TRUE ~study))

m1 <- metabin(
  event.e = MTB_vac1, n.e = TB_vac1,  
  event.c = MTB_vac0, n.c = TB_vac0,  
  studlab = study,
  data = bcg_TBM,
  sm = "RR",         # summary measure
  method = "MH",     # Mantel–Haenszel
  incr = 0.5,
  random = TRUE     # use random effects model
)

summary(m1)
forest(m1)

png(
  filename = here("plots/f_forest_TBM_RR.png"),
  width = 17, height = 7.0,
  units = "cm", pointsize = 12,
  bg = "white", res = 250
)

forest(
  m1,
  font=8,
  common = FALSE,
  comb.random = TRUE,
  text.random = "Random-effects RR",
  xlab = "Risk Ratio (RR)",
  leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
  leftlabs = c("Study", "TBM", "TB", "TBM", "TB"),
  label.e= "Vaccinated",
  label.c= "Unvaccinated",
  digits = 2,
  colgap.left = "0.5cm",
  fontsize = 8,        
  fontfamily = "serif"   # Font family ("Arial", "Times", "Helvetica")
)


png(here("plots/forest_a.png"), width = 18, height = 8, units = "cm", res = 250)
forest(
  m1,
  font=8,
  smlab = "", # to remove title above the plots
  common = FALSE,
  comb.random = TRUE,
  text.random = "Random-effects RR",
  xlab = "Risk Ratio (RR)",
  leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
  leftlabs = c("Study", "TBM", "TB", "TBM", "TB"),
  label.e= "Vaccinated",
  label.c= "Unvaccinated",
  digits = 2,
  colgap.left = "0.5cm",
  fontsize = 8,        
  fontfamily = "serif"   
)
dev.off()

# Save each forest plot separately
png(here("plots/forest_b.png"), width = 18, height = 8, units = "cm", res = 250)
forest(m,font=8.5,
       common = FALSE,
       leftcols = c("studlab","n", "event"),
       leftlabs = c("Study", "TBM", "Sequelae"),
       fontsize = 8.5,
       fontfamily = "serif", 
       xlab = "Severe sequelae (proportion)")
dev.off()

imgA <- image_read(here("plots/forest_a.png"))
imgB <- image_read(here("plots/forest_b.png"))
imgA_grob <- rasterGrob(imgA, interpolate = TRUE)
imgB_grob <- rasterGrob(imgB, interpolate = TRUE)


p1 <- ggplot() +
  annotation_custom(imgA_grob, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,0))

p1 <- ggdraw(p1) +
  draw_label("a", x = 0.05, y = 0.95, fontface = "bold", fontfamily = "serif", size = 10, hjust = 0, vjust = 1)

p2 <- ggplot() +
  annotation_custom(imgB_grob, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,0))

p2 <- ggdraw(p2) +
  draw_label("b", x = 0.05, y = 0.95, fontface = "bold", fontfamily = "serif", size = 10, hjust = 0, vjust = 1)

combined <- plot_grid(p1, p2, ncol=1)

ggsave(
  filename = here("plots/forest_combined.png"),
  plot = combined,
  width = 17.5, height = 16,
  units = "cm", dpi = 250, bg = "white"
)





metabin(10, 20, 15, 20, sm = "OR") 
metabin(10, 20, 15, 20, sm = "RR") 


# default for metabin is RR
# By default, the Mantel–Haenszel method is used to combine the results 
# default method = “MH”

#======Compare meta and metafor TBM========
dat_rr <- escalc(measure = "RR", 
                 ai = MTB_vac1, n1i = TB_vac1,
                 ci = MTB_vac0, n2i = TB_vac0, 
                 data = bcg_TBM, 
                 slab = study)

model_rr <- rma(yi, vi, data = dat_rr)
summary(model_rr)


# compare with metabin
summary(m1)

