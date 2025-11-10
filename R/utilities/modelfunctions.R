## functions for use in the modelling
## these functions have the following arguments in common:
##
## inc =  TB incidence
## m =  proportion TBM
## cdr =  CDR any TB
## cfr0 =  CFR (non-M)TB w/o tx
## cfr1 =  CFR (non-M)TB w/  tx
## cfrm0 =  CFR TBM w/o tx
## cfrm1 =  CFR TBM w/  tx
## L =  discounted LE
## Ltb =  discounted, QA post-(non-M)TB LE
## Ltbm = discounted, QA post-TBM LE

## fraction of TB that is TBM as a function of BCG coverage/efficacy
m_by_cov <- function(m0, f, v, vm) {
  m0 * (f * v * vm + (1 - f)) / (f * v + (1 - f))
}

## reverse engineer I0 from I
inc0_from_inc <- function(inc, f, v) {
  inc / (f * v + (1 - f))
}

## reverse engineer I0 from I
m0_from_m <- function(m, f, v, vm) {
  m * (f * v + (1 - f)) / (f * v * vm + (1 - f))
}

## all TB incidence as a function of BCG coverage/efficacy
tb_incidence <- function(inc0, f, v) {
  inc0 * (f * v + (1 - f))
}

## TBM incidence as a function of BCG coverage/efficacy
tbm_incidence <- function(inc0, m0, f, v, vm) {
  m0 * inc0 * (f * v * vm + (1 - f))
}


## 'health' over tree
qalys <- function(inc, m, cdr, cfr0, cfr1, cfrm0, cfrm1, L, Ltb, Ltbm) {
  (1 - inc) * L +
    inc * (1 - m) * (cdr * (1 - cfr1) + (1 - cdr) * (1 - cfr0)) * Ltb +
    inc * (m) * (cdr * (1 - cfrm1) + (1 - cdr) * (1 - cfrm0)) * Ltbm
}


## number of ATT courses (including TBM)
att_courses <- function(inc, cdr) {
  inc * cdr
}

## number of ATT courses (excluding TBM)
attn_courses <- function(inc, m, cdr) {
  inc * (1 - m) * cdr
}

## number of ATT courses for TBM (only)
attm_courses <- function(inc, m, cdr) {
  inc * m * cdr
}

## all TB deaths
tb_deaths <- function(inc, m, cdr, cfr0, cfr1, cfrm0, cfrm1) {
  inc * (1 - m) * (cdr * cfr1 + (1 - cdr) * cfr0) +
    inc * (m) * (cdr * cfrm1 + (1 - cdr) * cfrm0)
}

## TBM deaths
tbm_deaths <- function(inc, m, cdr, cfrm0, cfrm1) {
    inc * (m) * (cdr * cfrm1 + (1 - cdr) * cfrm0)
}

## (non-M)TB deaths
tbn_deaths <- function(inc, m, cdr, cfr0, cfr1) {
  inc * (1 - m) * (cdr * cfr1 + (1 - cdr) * cfr0)
}

## all TB discounted, qa LY
tb_ly <- function(inc, m, cdr, cfr0, cfr1, cfrm0, cfrm1, Ltb, Ltbm) {
  inc * (1 - m) * (cdr * (1 - cfr1) + (1 - cdr) * (1 - cfr0)) * Ltb +
    inc * (m) * (cdr * (1 - cfrm1) + (1 - cdr) * (1 - cfrm0)) * Ltbm
}


## TBM  discounted, qa LY
tbm_ly <- function(inc, m, cdr, cfrm0, cfrm1, Ltbm) {
  inc * (m) * (cdr * (1 - cfrm1) + (1 - cdr) * (1 - cfrm0)) * Ltbm
}

## (non-M)TB discounted, qa LY
tbn_ly <- function(inc, m, cdr, cfr0, cfr1, Ltb) {
  inc * (1 - m) * (cdr * (1 - cfr1) + (1 - cdr) * (1 - cfr0)) * Ltb
}

## treatment costs
att_cost <- function(attn, attm, ucostn, ucostm) {
  attn * ucostn + attm * ucostm
}


## number of hosp (excluding TBM)
# tbn_hosp <- function(inc, m, cdr, phosp) {
#   inc * (1 - m) * cdr*phosp
# }

## number of TBM  hospitalized - all TBM are hosp
tbm_hosp <- function(inc, m, cdr) {
  inc * m * cdr
}

attm_cost <- function(attm, ucostm) {
  attm * ucostm
}











