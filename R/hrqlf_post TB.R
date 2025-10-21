
# Read me -

# PostTB related quality of life derived from figure 2 this study- # 
# https://pmc.ncbi.nlm.nih.gov/articles/PMC8080025/


# Data
dat <- data.frame(
  study = c("Awaisu, 2012", "Kastien-H, 2017", "Kittikraisak, 2012", "Saleem, 2018"),
  mean  = c(73.9, 96.1, 85.0, 76.6)/100,
  sd    = c(12.0, 18.1, 15.0, 7.5)/100,
  n     = c(46, 129, 32, 176)
)

# Compute sampling variances (sd^2 / n)
dat$vi <- (dat$sd^2) / dat$n

# Random-effects meta-analysis of means
res <- rma(yi = mean, vi = vi, data = dat, method = "REML")

# Print summary
summary(res)

# Optional: Forest plot
forest(res, slab = dat$study, xlab = "Post-TB utility score based on EQ-VAS", alim = c(0.60, 1.10))



# based on EQ-ED5
dat <- data.frame(
  study = c("Awaisu, 2012", "Kastien-H, 2017", "Kittikraisak, 2012","Kittikraisak, 2012", "Saleem, 2018"),
  mean  = c(0.91, 0.88, 0.98, 0.893,0.88),
  sd    = c(0.14, 0.17, 0.32,0.203,0.11),
  n     = c(46, 32, 129,129, 176)
)

# Compute sampling variances (sd^2 / n)
dat$vi <- (dat$sd^2) / dat$n

# Random-effects meta-analysis of means
res <- rma(yi = mean, vi = vi, data = dat, method = "REML")

# Print summary
summary(res)

# Optional: Forest plot
forest(res, slab = dat$study, xlab = "Post-TB utility score based on EQ-5D", alim = c(0.80, 1.0))



# --- 2. Data Preparation: Calculate Events (r) ---
# metaprop requires the number of events (r) and total participants (n).
# Since mean is the proportion (p), we calculate r = round(p * n).
dat$r <- round(dat$mean * dat$n)


# Install the 'meta' package if you don't have it already
# install.packages("meta")





# Original data
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

# Forest plot with random-effects weights
forest(meta_res, comb.fixed = TRUE, comb.random = TRUE)

meta::forest(meta_res, sortvar = TE,
             leftcols = c("studlab"),
             leftlabs = c("Study"),
             rightlabs = c("Estimate", "95% CI", "Weight\nCommon", "Weight\nRandom"),
             xlab = "Post-TB utility score based on EQ-5D scores")


