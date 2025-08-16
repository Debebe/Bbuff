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
## Lmtb = discounted, QA post-TBM LE


## all TB incidence as a function of BCG coverage/efficacy
tb_incidence <- function(inc0, f, v) {
  inc0 * (f * v + (1 - f))
}


## TBM incidence as a function of BCG coverage/efficacy
tbm_incidence <- function(inc0, m0, f, v, vm) {
  m0 * inc0 * (f * v * vm + (1 - f))
}

## fraction of TB that is TBM as a function of BCG coverage/efficacy
m_by_cov <- function(m0, f, v, vm) {
  m0 * (f * v * vm + (1 - f)) / (f * v + (1 - f))
}

## 'health' over tree
qalys <- function(inc, m, cdr, cfr0, cfr1, cfrm0, cfrm1, L, Ltb, Lmtb) {
  (1 - inc) * L +
    inc * (1 - m) * (cdr * (1 - cfr1) + (1 - cdr) * (1 - cfr0)) * Ltb +
    inc * (m) * (cdr * (1 - cfrm1) + (1 - cdr) * (1 - cfrm0)) * Lmtb
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

## all TB discounted, qa LYL
tb_lyl <- function(inc, m, cdr, cfr0, cfr1, cfrm0, cfrm1, L, Ltb, Lmtb) {
  inc * (1 - m) * (cdr * (1 - cfr1) + (1 - cdr) * (1 - cfr0)) * Ltb +
    inc * (m) * (cdr * (1 - cfrm1) + (1 - cdr) * (1 - cfrm0)) * Lmtb
}


## TBM  discounted, qa LYL
tbm_lyl <- function(inc, m, cdr, cfrm0, cfrm1, Lmtb) {
  inc * (m) * (cdr * (1 - cfrm1) + (1 - cdr) * (1 - cfrm0)) * Lmtb
}

## (non-M)TB discounted, qa LYL
tbn_lyl <- function(inc, m, cdr, cfr0, cfr1, Ltb) {
  inc * (1 - m) * (cdr * (1 - cfr1) + (1 - cdr) * (1 - cfr0)) * Ltb
}






