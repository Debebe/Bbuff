## demand uncertainty re-analysis for the paper's supplementary material
library(data.table)
library(ggplot2)
library(ggrepel)
library(here)
library(readxl)
library(MASS)
library(fitdistrplus)

## read data
fn <- here("indata/Bacillus Calmette–Guérin (BCG) vaccination coverage 2025-04-03 10-35 UTC.xlsx")
bcg <- as.data.table(read_excel(fn))
bcgt <- bcg[, .(CODE, NAME, YEAR, COVERAGE_CATEGORY, TARGET_NUMBER, DOSES)]
cov_wuenic <- bcg[COVERAGE_CATEGORY == "WUENIC", .(CODE, YEAR, COVERAGE)]
bcg_official <- bcgt[COVERAGE_CATEGORY == "ADMIN" & !is.na(TARGET_NUMBER)]
comb <- merge(bcg_official, cov_wuenic, by = c("CODE", "YEAR")) # inner join
comb[, demand := TARGET_NUMBER * COVERAGE / 100]

avail_dt <- as.data.table(readRDS(here("outdata/gdp_inc_le_costs.rds")))
avail_dt <- unique(avail_dt[cov_cat == "WUENIC" & !is.na(cdr), .(iso3)])

comb_avail <- comb[CODE %in% avail_dt$iso3]
setorder(comb_avail, CODE, YEAR)

## country-level CV: use log demand & consecutive years
raw_tab <- comb_avail[,
  .(n = .N, M = mean(demand), std = sd(demand)),
  by = .(CODE, NAME)
]
raw_tab[, CV_raw := std / M]

diff_one <- function(d) {
  setorder(d, YEAR)
  gap <- diff(d$YEAR)
  ld <- diff(log(d$demand))
  keep <- gap == 1 # consecutive years only
  data.table(YEAR = d$YEAR[-1][keep], logret = ld[keep])
}
rets <- comb_avail[, diff_one(.SD), by = .(CODE, NAME)]

cv_tab <- rets[,
  .(n = .N, mu = mean(logret), CV_yoy = sd(logret)),
  by = .(CODE, NAME)
]

allcv <- merge(
  raw_tab[, .(CODE, NAME, n, CV_raw)],
  cv_tab[, .(CODE, n_yoy = n, mu, CV_yoy)],
  by = "CODE"
)

fwrite(allcv, here("outdata/demand_cv_by_country.csv"))


## CHECK
nrow(allcv)
summary(allcv$CV_raw)
summary(allcv$CV_yoy)

smy <- data.table(
  CV_type = c("raw", "yoy"),
  mean = c(mean(allcv$CV_raw), mean(allcv$CV_yoy)),
  median = c(median(allcv$CV_raw), median(allcv$CV_yoy)),
  sd = c(sd(allcv$CV_raw), sd(allcv$CV_yoy)),
  min = c(min(allcv$CV_raw), min(allcv$CV_yoy)),
  max = c(max(allcv$CV_raw), max(allcv$CV_yoy))
)

fwrite(smy, here("outdata/demand_cv_summary.csv"))

## figure by country
p_cv <- ggplot(allcv, aes(CV_yoy)) +
  geom_histogram(
    binwidth = 0.02
  ) +
  geom_vline(
    xintercept = c(0.05, 0.10, 0.15),
    linetype = "dashed", colour = "red"
  ) +
  annotate("text",
    x = c(0.05, 0.10, 0.15), y = Inf, label = c("5%", "10%", "15%"),
    vjust = 1.5, hjust = -0.1, size = 3
  ) +
  coord_cartesian(xlim = c(0, 0.5)) +
  labs(
    x = "Empirical year-on-year CV in demand",
    y = "Number of countries"
  ) +
  theme_linedraw()

p_cv

ggsave(
  here("plots/demand_CV_hist_country.png"),
  p_cv,
  width = 7, height = 5
)

## GOF comparisons

## restrict to countries with decent length
pc_codes <- rets[, .N, by = CODE][N >= 15, CODE]

fit_country <- function(cc) {
  x <- exp(rets[CODE == cc]$logret)
  x <- x[is.finite(x) & x > 0]
  aics <- c(
    normal = fitdist(x, "norm")$aic,
    gamma  = fitdist(x, "gamma")$aic,
    lnorm  = fitdist(x, "lnorm")$aic
  )
  data.table(
    CODE = cc, n = length(x), best = names(which.min(aics)),
    aic_normal = aics["normal"],
    aic_gamma = aics["gamma"],
    aic_lnorm = aics["lnorm"]
  )
}


gof_tab <- rbindlist(lapply(pc_codes, fit_country))
gof_tab <- merge(gof_tab, unique(comb_avail[, .(CODE, NAME)]), by = "CODE")


## write out
fwrite(gof_tab, here("outdata/demand_dist_gof_by_country.csv"))

## write summary
(smy_gof <- gof_tab[, .N, by = best][order(-N)])
fwrite(smy_gof, here("outdata/demand_dist_gof_summary.csv"))


## Pooled fit: all countries
r_pool <- exp(rets$logret)
r_pool <- r_pool[is.finite(r_pool) & r_pool > 0]
pooled_fits <- list(
  normal = fitdist(r_pool, "norm"),
  gamma = fitdist(r_pool, "gamma"),
  lnorm = fitdist(r_pool, "lnorm")
)

pool_aic <- vapply(
  pooled_fits,
  function(f) f$aic,
  numeric(1)
)
pool_aic

fwrite(data.table(
  distribution = names(pool_aic),
  AIC = pool_aic
), here("outdata/demand_dist_gof_pooled.csv"))



## look for outlying 'shock' country-years
rets_z <- merge(rets, cv_tab[, .(CODE, mu, CV_yoy)], by = "CODE")
rets_z[, z := (logret - mu) / CV_yoy]
z <- rets_z$z
z <- (z - mean(z)) / sd(z) # unit variance

demand_shock_evidence <- data.table(
  sigma = c(">2 SD", ">3 SD"),
  normal_expected = c(2 * pnorm(-2), 2 * pnorm(-3)), # abs(z), 2 tails
  observed = c(mean(abs(z) > 2), mean(abs(z) > 3))
)
demand_shock_evidence

fwrite(demand_shock_evidence, here("outdata/demand_shock_evidence.csv"))

fit_norm_z <- fitdistr(z, "normal")
fit_t_z <- fitdistr(z, "t",
  start = list(m = 0, s = 1, df = 5), lower = c(-Inf, 1e-3, 1e-3)
)

demand_shock_fits <- data.table(
  distribution = c("normal", "Student-t"),
  AIC = c(AIC(fit_norm_z), AIC(fit_t_z)),
  m = c(fit_norm_z$estimate["mean"], fit_t_z$estimate["m"]),
  s = c(fit_norm_z$estimate["sd"], fit_t_z$estimate["s"]),
  df = c(NA, fit_t_z$estimate["df"])
)
demand_shock_fits #t better

fwrite(demand_shock_fits, here("outdata/demand_shock_fits.csv"))

## top 15 events
rets_z[order(z)][1:15, .(CODE, NAME, YEAR, z)]
fwrite(rets_z[order(z)][1:15], here("outdata/demand_severe_shock_events.csv"))


## year counts
rets_z[z < -2, .N, by = YEAR][order(YEAR)]
fwrite(
  rets_z[z < -2, .N, by = YEAR][order(YEAR)],
  here("outdata/demand_severe_shock_counts.csv")
)


## normal vs t safety stocks
## main analysis: CV = 0.1 -> median 11.6% safety stock
## p_star = g/(g + h)
p_star <- pnorm(1.16)
p_star # 0.877

p_star <- c(
  0.70, 0.75, 0.80, 0.85,
  0.877, 0.90, 0.925, 0.95, 0.975, 0.99,
  0.995
)
m_t <- fit_t_z$estimate["m"]
s_t <- fit_t_z$estimate["s"]
df_t <- fit_t_z$estimate["df"]

q_compare <- data.table(
  p_star = p_star,
  q_normal = qnorm(p_star),
  q_t = m_t + s_t * qt(p_star, df_t)
)
q_compare[, ratio_t_to_normal := q_t / q_normal]
q_compare[, safety_stock_normal_CV10 := 100 * 0.10 * q_normal]
q_compare[, safety_stock_t_CV10 := 100 * 0.10 * q_t]
q_compare

fwrite(q_compare, here("outdata/demand_safety_stock_normal_vs_t.csv"))


## how much difference does it make
load(here("outdata/CEA.RData")) # loads CEA, inc.: iso3, g, u, threshold, ...
setDT(CEA)
p_star_dt <- CEA[threshold == 0.3, .(CODE = iso3, g, u, p_star = g / (g + u))]
p_star_dt <- p_star_dt[
  is.finite(p_star) & p_star > 0 & p_star < 1
] # valid ratio

## countries with both a usable historical series (n>=15) and a valid g,h
ss_codes <- intersect(pc_codes, p_star_dt$CODE)
length(ss_codes) #74

fit_country_ss <- function(cc) {
  x <- exp(rets[CODE == cc]$logret)
  x <- x[is.finite(x) & x > 0]
  p <- p_star_dt[CODE == cc, p_star]
  ## normal
  f_norm <- fitdist(x, "norm")
  mu_n <- f_norm$estimate["mean"]
  sd_n <- f_norm$estimate["sd"]
  ss_normal <- 100 * (qnorm(p, mu_n, sd_n) / mu_n - 1)
  ## gamma
  f_gamma <- fitdist(x, "gamma")
  shape_g <- f_gamma$estimate["shape"]
  rate_g <- f_gamma$estimate["rate"]
  mu_g <- shape_g / rate_g
  ss_gamma <- 100 * (qgamma(p, shape_g, rate_g) / mu_g - 1)
  ## lognormal
  f_lnorm <- fitdist(x, "lnorm")
  mlog <- f_lnorm$estimate["meanlog"]
  slog <- f_lnorm$estimate["sdlog"]
  mu_l <- exp(mlog + slog^2 / 2)
  ss_lnorm <- 100 * (qlnorm(p, mlog, slog) / mu_l - 1)
  ## output
  data.table(
    CODE = cc, n = length(x), p_star = p,
    ss_normal = ss_normal, ss_gamma = ss_gamma, ss_lnorm = ss_lnorm
  )
}

ss_tab <- rbindlist(lapply(ss_codes, fit_country_ss))
ss_tab <- merge(ss_tab, unique(comb_avail[, .(CODE, NAME)]), by = "CODE")
ss_tab <- merge(
  ss_tab, gof_tab[, .(CODE, best)],
  by = "CODE"
) # AIC-preferred family
ss_tab[, `:=`(
  diff_gamma_vs_normal = ss_gamma - ss_normal,
  diff_lnorm_vs_normal = ss_lnorm - ss_normal
)]
setorder(ss_tab, ss_normal)

fwrite(ss_tab, here("outdata/demand_safety_stock_by_country_dist.csv"))


ss_summary <- data.table(
  distribution = c("normal", "gamma", "lognormal"),
  mean = c(
    mean(ss_tab$ss_normal),
    mean(ss_tab$ss_gamma),
    mean(ss_tab$ss_lnorm)
  ),
  median = c(
    median(ss_tab$ss_normal),
    median(ss_tab$ss_gamma),
    median(ss_tab$ss_lnorm)
  ),
  q25 = c(
    quantile(ss_tab$ss_normal, .25),
    quantile(ss_tab$ss_gamma, .25),
    quantile(ss_tab$ss_lnorm, .25)
  ),
  q75 = c(
    quantile(ss_tab$ss_normal, .75),
    quantile(ss_tab$ss_gamma, .75),
    quantile(ss_tab$ss_lnorm, .75)
  )
)

fwrite(ss_summary, here("outdata/demand_safety_stock_by_dist_summary.csv"))

## graph 1: dotchart
ss_long <- melt(ss_tab,
  id.vars = c("CODE", "NAME", "n", "p_star", "best"),
  measure.vars = c("ss_normal", "ss_gamma", "ss_lnorm"),
  variable.name = "distribution", value.name = "safety_stock"
)
ss_long[, distribution := factor(distribution,
  labels = c("Normal", "Gamma", "Lognormal")
)]
ss_long[, CODE := factor(CODE, levels = ss_tab$CODE)]
ss_breaks <- c(-10, 0, 10, 25, 50, 100, 200, 300)

p_ss_country <- ggplot(
  ss_long,
  aes(x = safety_stock, y = CODE, colour = distribution)
) +
  geom_point(size = 1.6, shape = 1) +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(sigma = 5),
    breaks = ss_breaks
  ) +
  labs(
    x = "Optimal safety stock (% of expected demand, pseudo-log scale)",
    y = NULL, colour = "Distribution"
  ) +
  theme_linedraw() +
  theme(axis.text.y = element_text(size = 5), legend.position = "top")

p_ss_country

ggsave(
  here("plots/demand_safety_stock_by_country_dist.png"), p_ss_country,
  width = 7, height = 0.12 * nrow(ss_tab) + 1.5, limitsize = FALSE
)

## graph 2: scatter
ss_long <- merge(
  ss_long,
  ss_tab[, .(CODE, ss_normal_ref = ss_normal)],
  by = "CODE"
)
ss_long[
  distribution != "Normal",
  diff_from_normal := safety_stock - ss_normal_ref
]

noteworthy_codes <- ss_tab[
  pmax(abs(diff_gamma_vs_normal), abs(diff_lnorm_vs_normal)) > 2,
  CODE
]

p_ss_scatter <- ggplot(
  ss_long[distribution != "Normal"],
  aes(ss_normal_ref, safety_stock)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(
    aes(colour = best == tolower(distribution)),
    size = 1.8, shape = 1
  ) +
  ggrepel::geom_text_repel(
    data = ss_long[distribution != "Normal" & CODE %in% noteworthy_codes],
    aes(label = CODE), size = 3, colour = "black",
    max.overlaps = Inf, segment.size = 0.3, seed = 1
  ) +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(sigma = 5),
    breaks = ss_breaks
  ) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = 5),
    breaks = ss_breaks,
    expand = expansion(mult = c(0.05, 0.12))
  ) +
  facet_wrap(~distribution) +
  labs(
    x = "Safety stock under Normal (%, pseudo-log scale)",
    y = "Safety stock under alternative (%, pseudo-log scale)",
    colour = "AIC-preferred"
  ) +
  theme_linedraw() +
  theme(legend.position = "top")

p_ss_scatter

ggsave(
  here("plots/demand_safety_stock_dist_scatter.png"),
  p_ss_scatter,
  width = 10, height = 6
)
