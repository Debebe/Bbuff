## meta-analysis for BCG conditional efficacy against TBM
library(here)
library(meta)

## copying data from google doc into here manually
severe_TBM <- data.table(
  study = c(
    "Sumaya et al (1975)", "Doerr et al (1995)", "Paganini et al (2000)",
    "Degefie et al (2003)", "Kumar et al (2005)", "VanWell et al (2009)",
    "Tinsa et al (2010)"
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
save(m, file = here("data/m.Rdata"))

## plot
png(
  filename = here("outputs/forest.png"),
  width = 20, height = 10,
  units = "cm", pointsize = 12,
  bg = "white", res = 250
)
forest(m,
  fontsize = 8,
  xlab = "Proportion with severe sequelae, 95%CI"
  )
dev.off()


##======Meta RR of TBM========
bcg_TBM<- read_excel("data/bcg_tb_meningitis.xlsx") %>%
  dplyr::select(study, vac=vaccine, TB, MTB)%>%
  pivot_wider(names_from = vac, 
              values_from = c(TB, MTB), names_prefix = "vac") %>%
  filter(TB_vac1>0)%>%
  mutate(study= case_when(grepl("Chicago", study) ~"Rosenthal,1945", 
                          grepl("Trub", study) ~"Trub, 1970",
                          grepl("Chandra",study) ~ "Chandra,1975",
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
dev.off()


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

exp(c(-0.5496,-1.6436,0.5445))

# compare with metabin

summary(m1)


#=====Post TB HRQL=======

# data extracted from https://pmc-ncbi-nlm-nih-gov.sheffield.idm.oclc.org/articles/PMC8080025/
dat <- data.frame(
  study = c("Awaisu, 2012", "Kastien-H, 2017", "Kittikraisak, 2012 (1)",
            "Kittikraisak, 2012 (2)", "Saleem, 2018"),
  mean  = c(0.91, 0.88, 0.98, 0.893, 0.88),
  sd    = c(0.14, 0.17, 0.32, 0.203, 0.11),
  n     = c(46, 32, 129, 129, 176)
)

# Compute variance of the mean
dat <- dat %>%
  mutate(vi = (sd^2)/n)

#Compute 95% CI for each study
dat <- dat %>%
  mutate(
    ci_lb = mean - 1.96 * sqrt(vi),
    ci_ub = mean + 1.96 * sqrt(vi)
  )

# Step 3: Cap the upper CI of the 3rd study at 1
dat$ci_ub[3] <- min(dat$ci_ub[3], 1)

#adjust variance to match new CI (approximation)
dat$vi[3] <- ((dat$ci_ub[3] - dat$mean[3]) / 1.96)^2

# Step 4: Random-effects meta-analysis
res <- rma(yi = mean, vi = vi, data = dat, method = "REML")

summary(res)

# Step 5: Forest plot
forest(res, slab = dat$study, xlab = "Post-TB EQ-5D utility", alim = c(0.7, 1.0),)


#=====to use meta
dat$sd_adjusted <- dat$sd
# recompute SD to match upper CI capped at 1
dat$sd_adjusted[3] <- ((1 - dat$mean[3]) / 1.96) * sqrt(dat$n[3])

# Run meta-analysis using adjusted SD

meta_res <- metamean(
  n = dat$n,
  mean = dat$mean,
  sd = dat$sd_adjusted,
  studlab = dat$study,
  data = dat,
  #sm = "MLN",
  sm ="MRAW",
  method.tau = "REML"
)

meta::forest(meta_res, sortvar = TE,
             fontsize = 8.5, 
             leftcols = c("studlab"),
             leftlabs = c("Study"),
             rightlabs = c("Estimate", "95% CI", "Weight\nCommon", "Weight\nRandom"),
             smlab = "Mean(utility)",
             xlab = "Post-TB utility score based on EQ-5D scores")









