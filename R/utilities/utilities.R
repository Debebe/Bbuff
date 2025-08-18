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

