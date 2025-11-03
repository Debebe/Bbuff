## this is just to house the results calculations
## conventions:
## prepend rslt_ for results
## postpend _sq for status quo (current BCG coverage)
## postpend _cf for counterfactual (no BCG)

## life-expectancies
# D[, LE_disc := (1 - exp(-disc_rate * LE)) / disc_rate] # assumes birth cohort
# D[, Ltb := post_tb_hrqol *
#   (1 - exp(-disc_rate * LE / post_tb_mort_hz)) /
#   disc_rate]
# 
# # D[, Ltbm := post_tb_hrqol *
# #   (1 - exp(-disc_rate * LE / post_tb_mort_hz)) /
# #     disc_rate] #placeholder
# D[
#   ,
#   Ltbm_mil :=
#     prop_mild_seq * tbm_hrqol_mil_seq *
#       (1 - exp(-disc_rate * LE / tbm_mort_hz)) / disc_rate
# ]
# 
# #D[
# #  ,
# #  Ltbm_mil :=
# #    prop_mild_seq * (
# #tbm_hrqol_mil_seq1 *
# #      (1 - exp(-disc_rate * 3))+
# #tbm_hrqol_mil_seq2 *
# #      (exp(-disc_rate * 3) - exp(-disc_rate * LE / tbm_mort_hz))
# #) / disc_rate
# #]
# 
# D[
#   ,
#   Ltbm_mod :=
#     prop_mod_seq * tbm_hrqol_mod_seq *
#     (1 - exp(-disc_rate * LE / tbm_mort_hz)) / disc_rate
# ]
# D[
#   ,
#   Ltbm_sev :=
#     prop_sev_seq * tbm_hrqol_sev_seq *
#       (1 - exp(-disc_rate * LE / tbm_mort_hz)) / disc_rate
# ]
# D[, Ltbm := Ltbm_mil + Ltbm_mod + Ltbm_sev]
# D[, c("Ltbm_mil", "Ltbm_mod", "Ltbm_sev") := NULL]




## Calculate LE in a separate file

#====assumptions: TB disease occurs at age 0 
        #  also first 3 years postTB have different HRQL

# LEu5[, LE_disc0 := (1 - exp(-disc_rate * LEref)) / disc_rate] # discounted LE at birth
# LEu5[, Ltb0 := post_tb_hrqol_dur3 * (exp(-disc_rate * 0)  - exp(-disc_rate * 3)) / disc_rate +
#       post_tb_hrqol_post3 * (exp(-disc_rate * 3) - exp(-disc_rate * LEref/post_tb_mort_hz)) / disc_rate]
# 
# LEu5[, Ltbm0 := 
#        prop_mild_seq*(tbm_hrqol_mil_seq * (exp(-disc_rate * 0)  - exp(-disc_rate * 3)) / disc_rate +
#                                 tbm_hrqol_mil_seq * (exp(-disc_rate * 3) - exp(-disc_rate * LEref/tbm_mort_hz)) / disc_rate) + 
#       # moderate seqaele
#       prop_mod_seq*(tbm_hrqol_mod_seq * (exp(-disc_rate * 0)  - exp(-disc_rate * 3)) / disc_rate +
#                       tbm_hrqol_mod_seq * (exp(-disc_rate * 3) - exp(-disc_rate * LEref/tbm_mort_hz)) / disc_rate) + 
#        # severe seqaele
#       prop_sev_seq*(tbm_hrqol_sev_seq * (exp(-disc_rate * 0)  - exp(-disc_rate * 3)) / disc_rate +
#                       tbm_hrqol_sev_seq * (exp(-disc_rate * 3) - exp(-disc_rate * LEref/tbm_mort_hz)) / disc_rate)]


# Assuming infection occurs at each age (0, 1, 2, 3, 4)
# 
LEu5[, LE_disc := (exp(-disc_rate * Age) - exp(-disc_rate * LE)) / disc_rate] # discounted LE at each age
LEu5[, Ltb := post_tb_hrqol_dur3 * (exp(-disc_rate * Age) - exp(-disc_rate * (Age + 3))) +
      post_tb_hrqol_post3 * (exp(-disc_rate * (Age + 3)) - exp(-disc_rate * (Age + LE / post_tb_mort_hz))) / disc_rate ]
LEu5[, Ltbm :=
      prop_mild_seq*(tbm_hrqol_mil_seq * (exp(-disc_rate * Age) - exp(-disc_rate * (Age + 3))) +
                       tbm_hrqol_mil_seq * (exp(-disc_rate * (Age + 3)) - exp(-disc_rate * (Age + LE / tbm_mort_hz))) / disc_rate) + 
      
      prop_mod_seq*(tbm_hrqol_mod_seq * (exp(-disc_rate * Age) - exp(-disc_rate * (Age + 3))) +
                      tbm_hrqol_mod_seq * (exp(-disc_rate * (Age + 3)) - exp(-disc_rate * (Age + LE / tbm_mort_hz))) / disc_rate) +
      prop_sev_seq*(tbm_hrqol_sev_seq * (exp(-disc_rate * Age) - exp(-disc_rate * (Age + 3))) +
                      tbm_hrqol_sev_seq * (exp(-disc_rate * (Age + 3)) - exp(-disc_rate * (Age + LE / tbm_mort_hz))) / disc_rate)]

LED <- LEu5%>%
  group_by(Iso3)%>%
  summarise(LE_disc= weighted.mean(LE_disc, w = wt),
         Ltb=weighted.mean(Ltb, w = wt), # weighted LE
         Ltbm= weighted.mean(Ltbm, w = wt) # weighted LE
        
  ) %>%
  dplyr::select(iso3=Iso3,LE_disc, Ltb, Ltbm) %>%
  as.data.table()

D <- merge(samp, LED,by="iso3", allow.cartesian = TRUE)

#D[, c("LE", "Ltb", "Ltbm"):=NULL]

# TODO selecting required LE
# LEDs <- LED%>%select(iso3,
#                      LE_disc=LE_disc_ihme,
#                      Ltb=Ltb_ihme, #TODO if UN LE required  Ltb=Ltb_un
#                      Ltbm=Ltbm_ihme) #TODO if UN LE required  Ltbm=Ltbm_un
# 
# D <- merge(D,LEDs, by = "iso3")


## TBM fractions
D[, m_sq := prop_tbm]
D[, m_cf := m0_from_m(prop_tbm, bcg_coverage, bcg_haz_tb, bcg_haz_tbm)]

## TB incidence
D[, rslt_inc_sq := incbest]
D[, rslt_inc_cf := inc0_from_inc(incbest, bcg_coverage, bcg_haz_tb)]

## TBM incidence
D[, rslt_tbminc_sq := rslt_inc_sq * m_sq]
D[, rslt_tbminc_cf := rslt_inc_cf * m_cf]

## ATT courses
D[, rslt_att_sq := att_courses(rslt_inc_sq, cdr)] #TODO this should be just notes to reduce unc?
D[, rslt_att_cf := att_courses(rslt_inc_cf, cdr)]
D[, rslt_attn_sq := attn_courses(rslt_inc_sq, m_sq, cdr)]
D[, rslt_attn_cf := attn_courses(rslt_inc_cf, m_cf, cdr)]
D[, rslt_attm_sq := attm_courses(rslt_inc_sq, m_sq, cdr)]
D[, rslt_attm_cf := attm_courses(rslt_inc_cf, m_cf, cdr)]

## TB deaths
D[, rslt_tb_deaths_sq := tb_deaths(
  rslt_inc_sq, m_sq, cdr, cfr_utreat, cfr_treat,
  cfr_untreat_tbm, cfr_treat_tbm
  )]
D[, rslt_tb_deaths_cf := tb_deaths(
  rslt_inc_cf, m_cf, cdr, cfr_utreat, cfr_treat,
  cfr_untreat_tbm, cfr_treat_tbm
)]
D[, rslt_tbm_deaths_sq := tbm_deaths(
  rslt_inc_sq, m_sq, cdr,
  cfr_untreat_tbm, cfr_treat_tbm
)]
D[, rslt_tbm_deaths_cf := tbm_deaths(
  rslt_inc_cf, m_cf, cdr,
  cfr_untreat_tbm, cfr_treat_tbm
)]
D[, rslt_tbn_deaths_sq := tbn_deaths(
  rslt_inc_sq, m_sq, cdr, cfr_utreat, cfr_treat
)]
D[, rslt_tbn_deaths_cf := tbn_deaths(
  rslt_inc_cf, m_cf, cdr, cfr_utreat, cfr_treat
)]

## life-years lived (discounted) after:
D[, rslt_ly_tb_sq := tb_ly(
  rslt_inc_sq, m_sq, cdr, cfr_utreat, cfr_treat,
  cfr_untreat_tbm, cfr_treat_tbm, Ltb, Ltbm
  )]
D[, rslt_ly_tb_cf := tb_ly(
  rslt_inc_cf, m_cf, cdr, cfr_utreat, cfr_treat,
  cfr_untreat_tbm, cfr_treat_tbm, Ltb, Ltbm
)]
D[, rslt_ly_tbm_sq := tbm_ly(
  rslt_inc_sq, m_sq, cdr,
  cfr_untreat_tbm, cfr_treat_tbm, Ltbm
)]
D[, rslt_ly_tbm_cf := tbm_ly(
  rslt_inc_cf, m_cf, cdr,
  cfr_untreat_tbm, cfr_treat_tbm,Ltbm
  )]
D[, rslt_ly_tbn_sq := tbn_ly(
  rslt_inc_sq, m_sq, cdr,
  cfr_utreat, cfr_treat, Ltb
)]
D[, rslt_ly_tbn_cf := tbn_ly(
  rslt_inc_cf, m_cf, cdr,
  cfr_utreat, cfr_treat, Ltb
)]


## tbm hospitalisation

D[, rslt_hosp_tbm_sq := tbm_hosp(rslt_inc_sq, m_sq, cdr)]
D[, rslt_hosp_tbm_cf := tbm_hosp(rslt_inc_cf, m_cf, cdr)]



## ======= health
## health returns
D[, rslt_health_sq := qalys(
  rslt_inc_sq, m_sq,
  cdr, cfr_utreat, cfr_treat,
  cfr_untreat_tbm, cfr_treat_tbm,
  LE_disc, Ltb, Ltbm
)]
D[, rslt_health_cf := qalys(
  rslt_inc_cf, m_cf,
  cdr, cfr_utreat, cfr_treat,
  cfr_untreat_tbm, cfr_treat_tbm,
  LE_disc, Ltb, Ltbm
)]


## ======= costs etc

## ATT costs
D[, rslt_att_cost_sq := att_cost(
  rslt_attn_sq, rslt_attm_sq, ucost_dstb.m, ucost_tbm.m #TODO cost PSA
)]
D[, rslt_att_cost_cf := att_cost(
  rslt_attn_cf, rslt_attm_cf, ucost_dstb.m, ucost_tbm.m # TODO cost PSA
)]


## total costs
D[, rslt_cost_sq :=
  rslt_att_cost_sq + bcg_coverage*(ucost_proc_bcg * uc_tot_vax_delv_ave)] # TODO cost PSA
D[, rslt_cost_cf := rslt_att_cost_cf ] # TODO cost PSA

##===attm cost====

D[, rslt_attm_cost_sq := attm_cost(rslt_attm_sq, ucost_tbm.m)] #TODO cost PSA
D[, rslt_attm_cost_cf := attm_cost(rslt_attm_cf, ucost_tbm.m)]
 
## BCG cost
D[, rslt_bcg_cost_sq := bcg_coverage *(ucost_proc_bcg + uc_tot_vax_delv_ave)] #TODO cost PSA
D[, rslt_bcg_cost_cf := 0]


##=====BCG doses=======

D[, rslt_bcg_doses_sq := bcg_coverage]
D[, rslt_bcg_doses_cf := 0]


