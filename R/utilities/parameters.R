## work to generate the parameters distributions & samplers


bcg_eff_tb.m  <- 0.63
bcg_eff_tb.l <- 0.49
bcg_eff_tb.h <- 0.81

# bcg_eff_tbm.m  <- 0.73
# bcg_eff_tbm.lo <- 0.67
# bcg_eff_tbm.hi <- 0.79


bcg_haz_tbm.m  <- 0.58 #is hazard not efficacy
bcg_haz_tbm.l <- 0.20
bcg_haz_tbm.h <- 1.74

# bcg_haz_tb <- 1 - bcg_eff_tb
# bcg_haz_tbm <- 1 - bcg_eff_tbm



disc_rate <- 0.03

cfr_treat_tb.m <- 0.019 # 1·9 (0·5–7·1) tb-mortality treated
cfr_treat_tb.l <- 0.005 # 1·9 (0·5–7·1) tb-mortality treated
cfr_treat_tb.h <- 0.071 # 1·9 (0·5–7·1) tb-mortality treated

cfr_utreat_tb.m <- 0.436 # 43·6 (36·8–50·6) tb-mort untreated
cfr_utreat_tb.l <- 0.368 # 43·6 (36·8–50·6) tb-mort untreated
cfr_utreat_tb.h <- 0.506 # 43·6 (36·8–50·6) tb-mort untreated


cfr_treat_tbm.m <- 0.193 # 19·3% (95% CI 14·0–26·1) tbm-mortality treated
cfr_treat_tbm.l <- 0.140 
cfr_treat_tbm.h <- 0.261 

cfr_untreat_tbm <- 1 


# post_tb_hrqol_dur3 <- 1-0.041 #from Tommeny
# post_tb_hrqol_post3 <- 1-0.025


post_tb_mort_hz.m <- 1.78 # posttb morthaz 1.78, 1.61, 1.98
post_tb_mort_hz.l <- 1.61 # posttb morthaz 1.78, 1.61, 1.98
post_tb_mort_hz.h <- 1.98 # posttb morthaz 1.78, 1.61, 1.98

post_tbm_mort_hz.m <- 1.79 # posttbm morthaz 1.79, 1.09, 2.95
post_tbm_mort_hz.l <- 1.09 # posttbm morthaz 1.79, 1.09, 2.95
post_tbm_mort_hz.h <- 2.95 # posttbm morthaz 1.79, 1.09, 2.95


prop_tbm.ave <- 0.034
prop_tbm.lo <- 0.0187
prop_tbm.hi <- 0.06

prop_sev_seq.m <- 0.56 # 0.56[0.47-0. 63] meta-analysis
prop_sev_seq.l <- 0.47 # 0.56[0.47-0. 63] meta-analysis
prop_sev_seq.h <- 0.63 # 0.56[0.47-0. 63] meta-analysis


#prop_mild_disab <- 0.70 #proportion of nonsever tbm that is mild
prop_mild_disab.m <- 0.70 #proportion of nonsever tbm that is mild
prop_mild_disab.l= 0.48
prop_mild_disab.h <-0.87

# prop_mild_seq <- prop_mild_disab * (1-prop_sev_seq)
# prop_mod_seq   <-(1-prop_mild_disab) * (1-prop_sev_seq)

tbm_dis_wt_mild.m <- 0.031
tbm_dis_wt_mild.l <- 0.018
tbm_dis_wt_mild.h <- 0.050

tbm_dis_wt_mod.m <- 0.203
tbm_dis_wt_mod.l <- 0.134
tbm_dis_wt_mod.h <- 0.290

tbm_dis_wt_sev.m <- 0.542
tbm_dis_wt_sev.l <- 0.374
tbm_dis_wt_sev.h <- 0.702

# tbm_hrqol_mil_seq <- 1 - 0.031 # 0·031 (0·018–0·050) #Global burden of disease, 2013
# tbm_hrqol_mod_seq <- 1 - 0.203 # 0·203 (0·134–0·290)
# tbm_hrqol_sev_seq <- 1 - 0.542 # 0·542 (0·374–0·702)

LEref <- 88.871895

