## ---------------------------------------------------------------------
## Uncertainty analysis of the PSA output
## ---------------------------------------------------------------------
library(here)
library(data.table)
library(ggplot2)
library(mgcv)
library(relaimpo)

## ---------------------------------------------------------------------
## 0. Load PSA output and derive the outcome
## ---------------------------------------------------------------------
load(here("tmpdata/PSAreduct.RData")) # loads data.table `CEA`

CEA[, inc_health := rslt_health_sq - rslt_health_cf] # health gained by BCG
CEA[, inc_cost := rslt_cost_sq - rslt_cost_cf] # extra cost of BCG
CEA[, lambda := 0.3 * GDP] # CE threshold, 0.3 x GDP
CEA[, NB := lambda * inc_health - inc_cost] # per-draw net benefit

## Parameters that are redrawn every iteration,
## candidates for within-country Sobol
param_cols <- c(
  "incbest", "uc_tot_vax_delv_ave", "ucost_dstb.m", "ucost_tbm.m",
  "bcg_haz_tb", "bcg_haz_tbm", "prop_tbm", "post_tb_mort_hz",
  "post_tbm_mort_hz", "cfr_treat", "cfr_utreat", "cfr_treat_tbm",
  "prop_sev_seq", "tbm_hrqol_mil_seq", "tbm_hrqol_mod_seq",
  "tbm_hrqol_sev_seq", "prop_mild_seq", "prop_mod_seq"
)


## Structural covariates: fixed per country, candidates for the
## between-country regression.
struct_cols <- c(
  "notif", "cdr", "bcg_coverage", "GDP", "ucost_dstb.sd", "ucost_tbm.sd",
  "who_region"
)

## ---------------------------------------------------------------------
## 1. Total variance decomposition (law of total variance) on NB
## ---------------------------------------------------------------------
decompose_total_variance <- function(dt, y_col, group_col) {
  by_group <- dt[, .(
    mean_y = mean(get(y_col)),
    var_y = var(get(y_col))
  ), by = group_col]
  within <- mean(by_group$var_y) # E_j[ Var_i(Y | j) ]
  between <- var(by_group$mean_y) # Var_j[ E_i(Y | j) ]
  total <- within + between
  pooled <- var(dt[[y_col]]) # sanity check vs pooled variance
  list(
    within = within,
    between = between,
    total = total,
    pooled = pooled,
    pct_within = 100 * within / total,
    pct_between = 100 * between / total,
    by_group = by_group
  )
}


tv <- decompose_total_variance(CEA, "NB", "iso3")
cat(sprintf(
  "Within-country (parameter uncertainty):     %.1f%%\n", tv$pct_within
))
cat(sprintf(
  "Between-country (structural heterogeneity): %.1f%%\n", tv$pct_between
))
cat(sprintf(
  "Check -- decomposed vs pooled variance: %.1f vs %.1f\n",
  tv$total, tv$pooled
))

## ---------------------------------------------------------------------
## 2. Within-country component: GAM-regression Sobol indices
##    (Strong, Oakley & Brennan 2014)
## ---------------------------------------------------------------------
## S_hat = Var(g_hat) / Var(Y), where g_hat = fitted(gam(Y ~ s(theta))).
gam_sobol_index <- function(theta, Y) {
  fit <- gam(Y ~ s(theta, bs = "cr"))
  var(fitted(fit)) / var(Y)
}

gam_sobol_by_group <- function(theta_df,
                               Y,
                               group,
                               min_group_n = 200 # safety (not needed)
) {
  idx_by_group <- split(seq_along(Y), group)
  idx_by_group <- idx_by_group[
    vapply(idx_by_group, length, integer(1)) >= min_group_n
  ]
  rbindlist(lapply(names(idx_by_group), function(g) {
    idx <- idx_by_group[[g]]
    Yg <- Y[idx]
    rbindlist(lapply(names(theta_df), function(p) {
      data.table(
        group = g, parameter = p,
        S_hat = gam_sobol_index(theta_df[[p]][idx], Yg)
      )
    }))
  }))
}


## CHECK smoothing for one country & parameter
(example_iso <- CEA[which.max(incbest), iso3][1])
one_country <- CEA[iso3 == example_iso]
fit_diag <- gam(NB ~ s(bcg_haz_tb, bs = "cr"), data = one_country)
diag_df <- data.table(
  bcg_haz_tb = one_country$bcg_haz_tb,
  NB = one_country$NB,
  fitted = fitted(fit_diag)
)[order(bcg_haz_tb)]

p_diag <- ggplot(
  diag_df,
  aes(x = bcg_haz_tb, y = NB)
) +
  geom_point(alpha = 0.15, size = 0.5) +
  geom_line(aes(y = fitted), colour = 2, linewidth = 1) +
  labs(
    x = "bcg_haz_tb", y = "NB",
    title = sprintf("GAM fit: %s, bcg_haz_tb", example_iso)
  ) +
  theme_linedraw()
p_diag

ggsave(here("tmpdata/UQ_check_gam_fit.png"), p_diag, width = 6, height = 4)

sobol_by_country <- gam_sobol_by_group(
  theta_df = CEA[, ..param_cols],
  Y = CEA$NB,
  group = CEA$iso3,
  min_group_n = 200
)
setnames(sobol_by_country, "group", "iso3")

## Global ranking: median (and IQR)  S_hat across the
## 110 countries.
sobol_global_rank <- sobol_by_country[, .(
  median_S = median(S_hat), q25 = quantile(S_hat, .25),
  q75 = quantile(S_hat, .75)
), by = parameter][order(-median_S)]
print(sobol_global_rank)

## Region-level summary
region_lookup <- unique(CEA[, .(iso3, who_region)])
sobol_by_country <- merge(sobol_by_country, region_lookup, by = "iso3")

sobol_region_summary <- sobol_by_country[, .(
  median_S = median(S_hat), q25 = quantile(S_hat, .25),
  q75 = quantile(S_hat, .75), n_countries = .N
), by = .(who_region, parameter)]

## Figure: per-country S_hat by parameter, ranked by global median.
sobol_by_country[, parameter := factor(
  parameter,
  levels = rev(sobol_global_rank$parameter)
)]

p_sobol <- ggplot(
  sobol_by_country,
  aes(x = parameter, y = S_hat)
) +
  geom_boxplot(outlier.size = 0.5, fill = "grey") +
  coord_flip() +
  labs(
    x = NULL, y = "Country-level first-order Sobol indices",
    title = ""
  ) +
  theme_classic() +
  ggpubr::grids()
p_sobol

ggsave(here("plots/UQ_sobol_by_country.png"), p_sobol, width = 6, height = 4)
## TODO better names?

## ---------------------------------------------------------------------
## 2b. Total-effect Sobol indices for the top 5 first-order parameters
## ---------------------------------------------------------------------
top5_params <- sobol_global_rank$parameter[1:5]

## CHECK independence of the top 5 parameters in one country
indep_check <- cor(one_country[, ..top5_params])
diag(indep_check) <- NA
cat(sprintf(
  "Top-5 parameter independence check (max |r|, one country): %.3f\n",
  max(abs(indep_check), na.rm = TRUE)
))

## Fits with mgcv::bam(discrete = TRUE) rather than gam() as faster
## creates GAM regression with all pairwise + single terms
## 3-way interaction assumed negligible
joint_gam_var_explained <- function(params, dt, k_main = 6, k_int = 4) {
  main_terms <- sprintf("s(%s, k = %d)", params, k_main)
  pairs <- combn(params, 2, simplify = FALSE)
  int_terms <- vapply(pairs, function(p) {
    sprintf("ti(%s, %s, k = %d)", p[1], p[2], k_int)
  }, character(1))
  form <- as.formula(paste(
    "NB ~", paste(c(main_terms, int_terms), collapse = " + ")
  ))
  fit <- bam(form, data = dt, discrete = TRUE)
  var(fitted(fit)) / var(dt$NB)
}

total_effect_by_group <- function(params, dt, group,
                                  min_group_n = 200 # again not needed here
) {
  idx_by_group <- split(seq_len(nrow(dt)), group)
  idx_by_group <- idx_by_group[
    vapply(idx_by_group, length, integer(1)) >= min_group_n
  ]
  rbindlist(lapply(names(idx_by_group), function(g) {
    dtg <- dt[idx_by_group[[g]]]
    s_group <- joint_gam_var_explained(params, dtg)
    per_param <- rbindlist(lapply(params, function(i) {
      others <- setdiff(params, i)
      s_minus_i <- joint_gam_var_explained(others, dtg)
      data.table(group = g, parameter = i, S_T_hat = 1 - s_minus_i)
    }))
    per_param[, S_group := s_group]
    per_param
  }))
}

total_effect_res <- total_effect_by_group(
  top5_params, CEA, CEA$iso3,
  min_group_n = 200
)
setnames(total_effect_res, "group", "iso3")

total_effect_summary <- total_effect_res[, .(
  median_ST = median(S_T_hat), q25 = quantile(S_T_hat, .25),
  q75 = quantile(S_T_hat, .75)
), by = parameter]

cat(sprintf(
  "Closed group (top 5, with interactions) median variance explained: %.3f\n",
  median(total_effect_res$S_group)
))

## Comparison table: first-order vs total-effect
comparison_df <- merge(
  sobol_global_rank[
    parameter %in% top5_params,
    .(parameter, median_S1 = median_S, q25_S1 = q25, q75_S1 = q75)
  ],
  total_effect_summary[, .(parameter, median_ST, q25, q75)],
  by = "parameter"
)
setnames(comparison_df, c("q25", "q75"), c("q25_ST", "q75_ST"))
comparison_df[, interaction_gap := median_ST - median_S1]
print(comparison_df[order(-median_ST)])

## Figure: first-order vs total-effect index per parameter
plot_df <- rbind(
  comparison_df[, .(
    parameter, type = "first-order",
    S = median_S1, q25 = q25_S1, q75 = q75_S1
  )],
  comparison_df[, .(
    parameter, type = "total-effect",
    S = median_ST, q25 = q25_ST, q75 = q75_ST
  )]
)
plot_df[, parameter := factor(
  parameter,
  levels = comparison_df[order(median_ST), parameter]
)]

p_total <- ggplot(plot_df, aes(x = parameter, y = S, colour = type)) +
  geom_segment(
    data = comparison_df,
    aes(x = parameter, xend = parameter, y = median_S1, yend = median_ST),
    colour = "black", inherit.aes = FALSE,
    arrow = arrow(ends = "both", length = unit(0.08, "in"), type = "closed")
  ) +
  geom_pointrange(
    aes(ymin = q25, ymax = q75),
    position = position_dodge(width = 0.3)
  ) +
  coord_flip() +
  scale_colour_manual(
    values = c("first-order" = "blue", "total-effect" = "red")
  ) +
  labs(
    x = NULL,
    y = "Sobol indices (median & IQR across countries)",
    colour = NULL,
    title = ""
  ) +
  theme_classic() +
  ggpubr::grids()
p_total


ggsave(here("plots/UQ_sobol_total_vs_first.png"),
  p_total,
  width = 7, height = 6
)

## ---------------------------------------------------------------------
## 3. Between-country component: LMG decomposition of a linear
##    regression on structural covariates
## ---------------------------------------------------------------------
country_level <- CEA[, c(
  .(ENB = mean(NB), mean_incidence = mean(incbest)),
  lapply(.SD, function(x) x[1])
), by = iso3, .SDcols = struct_cols]
country_level[, who_region := as.factor(who_region)]

lm_fit <- lm(ENB ~ ., data = country_level[, -"iso3"])
cat(sprintf("Regression R-squared: %.3f\n", summary(lm_fit)$r.squared))

lmg <- calc.relimp(lm_fit, type = "lmg", rela = FALSE)

## Direction of effect: standardise to "SD
## change in ENB per SD change in the covariate".
cont_cols <- setdiff(names(country_level), c("iso3", "ENB", "who_region"))
country_std <- copy(country_level)
country_std[, c("ENB", cont_cols) := lapply(
  .SD, function(x) as.numeric(scale(x))
), .SDcols = c("ENB", cont_cols)]
lm_std <- lm(ENB ~ ., data = country_std[, -"iso3"])

coef_ci <- confint(lm_std)
direction_df <- data.table(
  covariate = rownames(coef_ci)[-1], # drop intercept
  std_estimate = coef(lm_std)[-1],
  ci_lo = coef_ci[-1, 1], ci_hi = coef_ci[-1, 2]
)

## who_region is categorical term  -- tagged "n/a" for direction
## per-region estimates are shown in the forest plot instead.
lmg_df <- data.table(covariate = names(lmg@lmg), lmg_share = lmg@lmg)
sign_df <- direction_df[
  !grepl("^who_region", covariate),
  .(covariate, sign = sign(std_estimate))
]
sign_df <- rbind(sign_df, data.table(covariate = "who_region", sign = NA_real_))

importance_df <- merge(lmg_df, sign_df, by = "covariate")[order(-lmg_share)]
print(importance_df)
print(direction_df[, .(covariate, std_estimate, ci_lo, ci_hi)])

p_imp <- ggplot(
  importance_df,
  aes(
    x = reorder(covariate, lmg_share), y = lmg_share,
    fill = factor(
      sign,
      levels = c(-1, 1, NA), labels = c("negative", "positive", "n/a"),
      exclude = NULL
    )
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "negative" = "#FF3D7FFF", "positive" = "#3FB8AFFF", "n/a" = "grey"
    )
  ) +
  labs(
    x = NULL, y = "LMG share of Var(ENB) (summing to model R-squared)",
    fill = "direction",
    title = ""
  ) +
  theme_classic() +
  ggpubr::grids()
p_imp

ggsave(here("plots/UQ_lmg_importance.png"),
  p_imp,
  width = 7, height = 6
)


## Forest plot of the who_region effects relative to AFR,
## holding all other structural covariates fixed.
region_df <- direction_df[grepl("^who_region", covariate)]
region_df[, region := sub("^who_region", "", covariate)]

p_region <- ggplot(
  region_df, aes(x = reorder(region, std_estimate), y = std_estimate)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi)) +
  coord_flip() +
  labs(
    x = "WHO region", y = "SD change in ENB vs AFR (reference)",
    title = ""
  ) +
  theme_classic() +
  ggpubr::grids()
p_region

ggsave(here("plots/UQ_who_region_forest.png"),
  p_region,
  width = 6, height = 4
)
