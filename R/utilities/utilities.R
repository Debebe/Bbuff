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
  ans <- brkt0(x, y, z)
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

#sample_beta(mean = 0.034, lo = 0.0187, hi = 0.060)









