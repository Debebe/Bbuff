## work to generate the parameters distributions & samplers
# some parameters
bcg_eff_tb <- 0.62
bcg_eff_tbm <- 0.73 # 0.73 (0.67 - 0.79), 0.69(0.60- 0.76 in Asia see Trunz

bcg_eff_tb.m  <- 0.63
bcg_eff_tb.lo <- 0.49
bcg_eff_tb.hi <- 0.81

# bcg_eff_tbm.m  <- 0.73
# bcg_eff_tbm.lo <- 0.67
# bcg_eff_tbm.hi <- 0.79


bcg_eff_tbm.m  <- 0.58
bcg_eff_tbm.lo <- 0.20
bcg_eff_tbm.hi <- 1.74


bcg_haz_tb <- 1 - bcg_eff_tb
bcg_haz_tbm <- 1 - bcg_eff_tbm

cfr_treat <- 0.019 # 1·9 (0·5–7·1) tb-mortality treated
cfr_utreat <- 0.436 # 43·6 (36·8–50·6) tb-mort untreated
ucost_proc_bcg <- 0.1205 # for serum institute India- 2023
disc_rate <- 0.03


#post_tb_dis_wt <- 0.04
post_tb_hrqol <- 0.91 # 0.87- 0.95 based on from https://pmc.ncbi.nlm.nih.gov/articles/PMC8080025/

post_tb_hrqol_dur3 <- 1-0.041 #Tomeny
post_tb_hrqol_post3 <- 1-0.025

post_tb_mort_hz <- 1.78

tbm_mort_hz <- 1.79 # TODO guess
# tbm_hrqol     <- 0.60   # TODO  guess
cfr_treat_tbm <- 0.193 # 19·3% (95% CI 14·0–26·1)
cfr_untreat_tbm <- 1 # 19·3% (95% CI 14·0–26·1)

# additional inputs
#prop_tbm <- 0.034
prop_tbm.ave <- 0.034
prop_tbm.lo <-0.0187
prop_tbm.hi <- 0.06

prop_sev_seq <- 0.56 # 0.56[0.47-0. 63] meta-analysis
prop_nsev_seq <- 0.44 # 0.44 [0.37, 0.53]
prop_mild_seq <- 0.70 * prop_nsev_seq
prop_mod_seq <- 0.30 * prop_nsev_seq

tbm_hrqol_mil_seq <- 1 - 0.031 # 0·031 (0·018–0·050) #Global burden of disease, 2013
tbm_hrqol_mod_seq <- 1 - 0.203 # 0·203 (0·134–0·290)
tbm_hrqol_sev_seq <- 1 - 0.542 # 0·542 (0·374–0·702)

LEref <- 88.871895

