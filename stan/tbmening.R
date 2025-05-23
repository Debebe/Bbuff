
library(rstan)
library(bayesplot)
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)


extract_pi_df <- function(fit) {

  # Get summary as a data frame
  sum_df <- as.data.frame(summary(fit)$summary)
  
  # Filter rows for `pi[...]`
  pi_rows <- grep("^pi\\[", rownames(sum_df))
  pi_df <- sum_df[pi_rows, ]
  
  # Add parameter names as a column
  pi_df$param <- rownames(pi_df)
  
  # Extract study and vac from the parameter name
  pi_df <- pi_df %>%
    mutate(
      rowid = seq_len(n())#,
      #study = as.integer(str_extract(param, "(?<=\\[)\\d+")),
      #vac   = as.integer(str_extract(param, "(?<=,)(\\d+)"))#,
      #vac   = ifelse(vac == 1, "control", "vaccine")
    )
  
  return(pi_df)
}

#aa <- extract_pi_df(fit = fit)


#========Hierarchical global P========
# This doesn't allow study level variation

stan_code <- "
data {
  int<lower=1> S;                 // Number of studies
  int<lower=0> TB[S];             // TB cases per study
  int<lower=0> MTB[S];            // TBM cases per study
  int<lower=0, upper=1> vac[S];   // Vaccine status: 0 = unvaccinated, 1 = vaccinated
}

parameters {
  // Core model parameters
  real<lower=0, upper=1> p;       // Baseline TBM probability for unvaccinated
  real a; // Hyperparameters for the normal prior on `a`
  real mu_logRR;                 // Mean of prior for log(RR)
  real<lower=0> sigma_logRR;     // SD of prior for log(RR)

  // Hyperparameters for beta prior on p
  real<lower=0> alpha_p;
  real<lower=0> beta_p;
}

transformed parameters {
  real<lower=0, upper=1> pi[S];

  for (s in 1:S) {
    real rr = exp(a);
    pi[s] = p * (vac[s] == 1 ? rr : 1);
  }
}

model {
  // Hyperpriors
  mu_logRR ~ normal(log(0.12), 0.5);          // Prior for mean log(RR)
  sigma_logRR ~ cauchy(0, 1);                // Prior for SD (half-Cauchy)
  
  alpha_p ~ gamma(2, 0.5);                   // Weakly informative hyperpriors
  beta_p ~ gamma(2, 0.5);                    // You can adjust these as needed

  // Priors using hyperparameters
  a ~ normal(mu_logRR, sigma_logRR);
  p ~ beta(alpha_p, beta_p);

  // Likelihood
  for (s in 1:S) {
    MTB[s] ~ binomial(TB[s], pi[s]);
  }
}

generated quantities {
  real RR = exp(a);              
}
"
stan_model_obj <- stan_model(model_code = stan_code)

bcg <- read_excel("data/bcg_tb_meningitis.xlsx")

dd <- bcg%>%
  mutate(rowid=seq_len(n()),
         study= as.integer(factor(study)))%>%
  select(rowid,study,vac= vaccine, TB, MTB)%>%filter(TB>0)

S <- nrow(dd)

# Prepare data: Separate the columns
t <- dd$TB          # Total TB cases
m <- dd$MTB         # MTB cases
vac <- dd$vac       # Vaccine status (0 or 1)

# Bundle the data into a list for Stan
stan_data <- list(
  S = S,
  TB = t,
  MTB = m,
  vac = vac
)

fit <- sampling(stan_model_obj, data = stan_data, chains = 4, iter = 2000, seed = 123)
summary(fit)
mcmc_dens(fit, pars = c("RR", "p", "mu_logRR", "sigma_logRR", "mu_logRR", "alpha_p", "beta_p"))
print(fit, pars = c("a", "RR"), probs = c(0.025, 0.5, 0.975))
summary_fit <- summary(fit)$summary

aa <- extract_pi_df(fit = fit)

tmp <- inner_join(dd%>%select(rowid, study, vac, TB, MTB)%>%
                    mutate(pobs=MTB/TB),aa)%>%
  mutate(vac=ifelse(vac==1, 'control', "vaccine"))

ggplot(tmp, aes(y = reorder(study, mean), x = mean, col=vac)) +
  geom_point() + facet_wrap(~vac, scales="free")+
  geom_errorbarh(aes(xmin = `2.5%`, xmax = `97.5%`), height = 0.2) +
  geom_point(aes(x = pobs), shape = 24, fill = "navy", size = 1) +
  
  labs(
    x = "Prob. meningeal TB",
    y = NULL,
    title = "Estimated TB meningitis probability given TB by BCG status"
  ) +
  theme_minimal(base_size = 14)


a=traceplot(fit, pars = "RR")
b=mcmc_dens(fit, pars = "RR")

a+b




#========Hierarchical II===================

# this allows study level variation

stan_code <- "
data {
  int<lower=1> S;               // Number of studies
  int<lower=1> V;               // Number of vaccine arms (2)
  int<lower=0> TB[S, V];        // Total TB cases in each  study and vaccine group
  int<lower=0> MTB[S, V];       // TBM cases by study and vaccine group
}

parameters {
  vector<lower=0, upper=1>[S] p_raw; // Baseline TBM risk per study (vac=0)
  real a;                            // Log relative risk of TBM in vaccinated
  
  real mu_logRR;                 // Mean of prior on log(RR)
  real<lower=0> sigma_logRR;     // SD of prior for log(RR)

  // Hyperparameters for beta prior on p
  real<lower=0> alpha_p;
  real<lower=0> beta_p;
}

transformed parameters {
  matrix[S, V] pi;                  // TBM probability per study and vaccine group
  real rr = exp(a);
  
  for (s in 1:S) {
    pi[s, 1] = p_raw[s];            // vac = 0
    pi[s, 2] = p_raw[s] * rr;       // vac = 1 
  }
}

model {
  // Hyperpriors
  mu_logRR ~ normal(log(0.12), 0.5);          // Prior for mean log(RR)
  sigma_logRR ~ cauchy(0, 0.5);                // Prior for SD (half-Cauchy)
  
  alpha_p ~ gamma(2, 0.5);                   // Unformative hyperpriors
  beta_p ~ gamma(2, 0.5);                   

  // Priors 
  a ~ normal(mu_logRR, sigma_logRR);
  p_raw ~ beta(alpha_p, beta_p);
  
  for (s in 1:S) {
    for (v in 1:V) {
      MTB[s, v] ~ binomial(TB[s, v], pi[s, v]);
    }
  }
}

generated quantities {
  real RR = exp(a);
}
"

# converting into matrix outline

dd_wide <- dd %>%
  filter(study!=6) %>%
  select(-rowid)%>% #important
  pivot_wider(names_from = vac, values_from = c(TB, MTB), names_prefix = "vac") %>%
  mutate(pv=MTB_vac1/TB_vac1, puv= MTB_vac0/TB_vac0)%>%
  arrange(study)

# Extract S and V
S <- nrow(dd_wide)
V <- 2  # vac=0 and vac=1

# Create TB and MTB matrices (S x V)
TB_mat <- as.matrix(dd_wide[, c("TB_vac0", "TB_vac1")])
MTB_mat <- as.matrix(dd_wide[, c("MTB_vac0", "MTB_vac1")])

stan_data <- list(
  S = S,
  V = V,
  TB = TB_mat,
  MTB = MTB_mat
)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

model <- stan_model(model_code = stan_code)
fit_mat <- sampling(model, data = stan_data, chains = 4, iter = 2000, seed = 123)

print(fit_mat, pars = c("p_raw", "a", "RR"))
summary(fit_mat)

mcmc_dens(fit_mat, pars = c("RR", "mu_logRR", "sigma_logRR", "mu_logRR", "alpha_p", "beta_p"))
mcmc_trace(fit_mat, pars = c("RR", "mu_logRR", "sigma_logRR", "mu_logRR", "alpha_p", "beta_p"))


# Get full summary table (matrix) for all parameters including pi
sum_df <- as.data.frame(summary(fit_mat)$summary)

# The rownames include parameter names like "pi[1,1]", "pi[1,2]", ...
rownames(sum_df)[1:10]
pi_rows <- grep("^pi\\[", rownames(sum_df))
pi_df <- sum_df[pi_rows, ]

# Add parameter names as a column
pi_df$param <- rownames(pi_df)

# Extract study and vac indices from the param names
pi_df <- pi_df %>%
  mutate(
    study = as.integer(str_extract(param, "(?<=\\[)\\d+")),
    vac   = as.integer(str_extract(param, "(?<=,)(\\d+)"))
  )%>%
  mutate(vac=ifelse(vac==1, 'control', "vaccine"))

pi_df <- pi_df %>%
  select(study, vac, mean, `2.5%`, `97.5%`)

# Then join with your original data, which needs to be in wide format or long format matching study & vac
obs <-dd_wide%>%select(study, pv, puv)%>%
  pivot_longer(cols = c(pv, puv),names_to = "vac", values_to = "obs")%>%
  mutate(vac=ifelse(vac=="puv", 'control', "vaccine"))

#
comb <- inner_join(pi_df,obs, by= c("study", "vac"))

comb%>%
  flextable::flextable()

p <-ggplot(comb, aes(y = reorder(study, mean), x = mean, col=vac)) +
  geom_point() + facet_wrap(~vac, scales="free")+
  geom_errorbarh(aes(xmin = `2.5%`, xmax = `97.5%`), height = 0.2) +
  geom_point(aes(x = obs), shape = 24, fill = "navy", size = 1) +
  
  labs(
    x = "Probability of TB meningitis given TB",
    y = NULL,
    title = "Estimated TB meningitis probability given TB"
  ) +
  theme_minimal(base_size = 14)

ggsave(p, file =here("outputs/predicted_prop_meing.png"))

c=traceplot(fit_mat, pars = "RR")
d=mcmc_dens(fit_mat, pars = "RR")

p2 <-c+d
ggsave(p2, file =here("outputs/predicted_meningTB_RR.png"))
print(fit_mat, pars = c("a", "RR"), probs = c(0.025, 0.5, 0.975))


