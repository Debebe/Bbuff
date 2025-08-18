## this is just to house the results calculations
## conventions:
## prepend rslt_ for results
## postpend _sq for status quo (current BCGcoverage)
## postpend _cf for counterfactual (no BCG)

## life-expectancies
D[, LE_disc := (1 - exp(-disc_rate * LE)) / disc_rate]
D[, Ltb := post_tb_hrqol *
  (1 - exp(-disc_rate * LE / post_tb_mort_hz)) /
  disc_rate]
D[, Ltbm := post_tb_hrqol *
  (1 - exp(-disc_rate * LE / post_tb_mort_hz)) /
    disc_rate] #placeholder

## TBM fractions
D[, m_sq := prop_tbm]
D[, m_cf := m0_from_m(prop_tbm, bcg_coverage, bcg_hr, bcg_haz_tbm)]

## TB incidence
D[, rslt_inc_sq := tbinc]
D[, rslt_inc_cf := inc0_from_inc(tbinc, bcg_coverage, bcg_hr)]

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
  rslt_att_cost_sq + bcg_coverage * uc_tot_vax_delv_ave] # TODO cost PSA
D[, rslt_cost_cf := rslt_att_cost_cf ] # TODO cost PSA

