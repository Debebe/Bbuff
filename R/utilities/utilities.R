## some simple helper functions
## uncertainty aggregation
ssum <- function(x) sqrt(sum(x^2))
## output formatting
rf <- function(x) {
  dg <- ifelse(abs(x) > 0.01 & abs(x) < 100, 2, 3)
  x2 <- signif(x, dg)
  format(
    x2,
    digits = dg,
    nsmall = 0L,
    big.mark = ",", # or " "
    justify = "right",
    drop0trailing = TRUE,
    scientific = FALSE
  )
}
brkt0 <- function(x, y, z) paste0(x, " (", y, " to ", z, ")")
brkt0(1, 2, 3)
brkt <- function(x, y, z) {
  ans <- brkt0(rf(x), rf(y), rf(z))
  ans <- gsub("\\([[:space:]]+", "\\(", ans)
  ans <- gsub("to[[:space:]]+", "to ", ans)
  ans
}
rd <- function(x) round(1e2 * x, 1) # round as %
brktpc <- function(x, y, z) { # bracket %
  brkt0(
    paste0(rd(x), "%"),
    paste0(rd(y), "%"),
    paste0(rd(z), "%")
  )
}


gamma_sample <- function(mean, sd, n=1) {
  shape <- (mean / sd)^2
  scale <- (sd^2) / mean
  rgamma(n, shape = shape, scale = scale)
}


sample_gamma <- function(mean, sd = NULL, lo = NULL, hi = NULL, n = 1) {
  # If sd not provided, compute from lo/hi
  if (is.null(sd)) {
    if (is.null(lo) || is.null(hi)) {
      stop("Either sd or both lo and hi must be provided")
    }
    sd <- (hi - lo) / 3.92
  }
  
  shape <- (mean / sd)^2
  scale <- (sd^2) / mean
  rgamma(n, shape = shape, scale = scale)
}



sample_truncn <- function(mu, lo, hi, n = 1) {
  sd <- (hi - lo) / 3.92
  rtruncnorm(n, a =0, b = Inf, mean = mu, sd = sd)
}


sample_beta <- function(mean, lo, hi, n = 1) {
  sd <- (hi - lo) / 3.92
  var <- sd^2
  tmp <- mean * (1 - mean) / var - 1
  alpha <- mean * tmp
  beta  <- (1 - mean) * tmp
  rbeta(n, shape1 = alpha, shape2 = beta)
}

# sample_beta(mean = 0.034, lo = 0.0187, hi = 0.060)

sample_LN <- function(mean, lower, upper, n=1) {
  z025 <- qnorm(0.025) 
  z975 <- qnorm(0.975)
  
  log_L <- log(lower)
  log_U <- log(upper)
  sd_log <- (log_U - log_L) / (z975-z025)
  
  mu_log_median <- log_L - sd_log * z025
  mu_log <- mu_log_median
  derived_mean <- exp(mu_log + sd_log^2 / 2)
  
  rlnorm(n, mu_log, sd_log)
}

#sample_LN(0.019, 0.005, 0.071, 5)




get_LN_values <- function(mean_val, lower, upper) {
  z025 <- qnorm(0.025) 
  z975 <- qnorm(0.975)

  log_L <- log(lower)
  log_U <- log(upper)
  sd_log <- (log_U - log_L) / (z975-z025)
  
  mu_log_median <- log_L - sd_log * z025
  mu_log <- mu_log_median
  derived_mean <- exp(mu_log + sd_log^2 / 2)
  

  return(list(
    meanlog = mu_log, 
    sdlog = sd_log
  ))
}

# Example Usage
# rlsample <- get_LN_values(0.019, 0.005, 0.071)  # CFR treated TB
# rlsample <- get_LN_values(0.193, 0.140,0.261)   # CFR treated TBM
# rlsample <- get_LN_values(0.436, 0.368,0.506)   # CFR untreated TB
# 
# rlsample <- get_LN_values(1.79, 1.09,2.95)  # mortality RR for TBM survivors
# rlsample <- get_LN_values(1.78, 1.61, 1.98) # mortality RR for TBM survivors

## check confidence interval if it recovers the original inputs
# quantile(rlnorm(1e5,get_LN_values$meanlog, 
#                 get_LN_values$sdlog), 
#          probs = c(0.025,0.5, 0.975))

make_flextable <- function(data) {
  ftall <- data %>%
    # select(-ISO) %>%
    flextable::flextable() |>
    #flextable::set_caption("Optimal buffer size under different demand uncertainties") |>
    flextable::autofit() |>
    flextable::merge_v(j = 1) |>
    flextable::align(align = "left", part = "all") |>
    flextable::valign(valign = "top", j = 1) |>
    flextable::fontsize(size = 7, part = "all") |>
    flextable::autofit() |>
    theme_booktabs() %>%
    hline_bottom(part = "body", border = fp_border(width = 1.5))

  return(ftall)
}

beta_distr <- function(mean, lo, hi, n = 1) {
  sd <- (hi - lo) / 3.92
  var <- sd^2
  tmp <- mean * (1 - mean) / var - 1
  alpha <- mean * tmp
  beta  <- (1 - mean) * tmp
  paste("Beta","(",round(alpha,2), ",", round(beta,2),")", sep="")
}


gamma_distr <- function(mean, sd = NULL, lo = NULL, hi = NULL, n = 1) {
  # If sd not provided, compute from lo/hi
  if (is.null(sd)) {
    if (is.null(lo) || is.null(hi)) {
      stop("Either sd or both lo and hi must be provided")
    }
    sd <- (hi - lo) / 3.92
  }
  
  shape <- (mean / sd)^2
  scale <- (sd^2) / mean
  rgamma(n, shape = shape, scale = scale)
  paste("Gamma","(","shape=", round(shape,3), ","," scale=",round(scale,3),")", sep="")
}


truncn_distr <- function(mu, lo, hi, n = 1) {
  sd <- (hi - lo) / 3.92
  rtruncnorm(n, a =0, b = Inf, mean = mu, sd = sd)
  paste("HalfNormal","(","mean=", round(mu,4), ",","sd=",round(sd,2),")", sep="")
  
}


# bcg_eff <-beta_distr(mean = bcg_eff_tb.m, lo = bcg_eff_tb.lo, bcg_eff_tb.hi);bcg_eff
# tmb_prop <-beta_distr(prop_tbm.ave,prop_tbm.lo,prop_tbm.hi);tmb_prop
# bcg_haztbm = gamma_distr(bcg_eff_tbm.m,bcg_eff_tbm.lo,bcg_eff_tbm.h); bcg_haztbm


# gamma_distr(0.193, 0.140,0.261)
# beta_distr(0.031, 0.018, 0.050) #DW mild
# beta_distr(0.203, 0.134, 0.290) #DW moderate
# beta_distr(0.542, 0.374, 0.702) #DW severe
# quantile(rbeta(1e5,13.94,435.83), probs = c(0.025,0.5, 0.975))
# quantile(rbeta(1e5,20.54,80.62), probs = c(0.025,0.5, 0.975))
# quantile(rbeta(1e5,18.68,15.78), probs = c(0.025,0.5, 0.975))
