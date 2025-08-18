library(data.table)

# ReadME
# This code generates TB that is TBM in notifications. 
# Approach - Used the proportions of TB that is TBM from du Preez meta-analysis (appendix) and 
# weighted those proportion using their corresponding population sizes in the GitHub account of 
# that publication.

# https://github.com/petedodd/TBMK/tree/main

url <- "https://github.com/petedodd/TBMK/raw/main/modelling/data/POP.Rdata"
# Download and load the Rdata file directly from the internet
load(url(url))


#load("~/Documents/TBMK-main/modelling/data/POP.Rdata")

Pops <- POP[acat == "1-4" | acat == "<1", ]


# proportion TB that is TBM in notifications 
TBMUnd1 <- data.frame(M = 0.0446, L = 0.0243, U = 0.0803)
TMB1to5 <- data.frame(M = 0.0312, L = 0.0173, U = 0.0554)

Pops[acat == "<1", TBM_M := pop * TBMUnd1$M]
Pops[acat == "<1", TBM_L := pop * TBMUnd1$L]
Pops[acat == "<1", TBM_U := pop * TBMUnd1$U]

Pops[acat == "1-4", TBM_M := pop * TMB1to5$M]
Pops[acat == "1-4", TBM_L := pop * TMB1to5$L]
Pops[acat == "1-4", TBM_U := pop * TMB1to5$U]

P_summed <- Pops[, lapply(.SD, sum), by = iso3, .SDcols = is.numeric]
P_summed[, `:=`(M = TBM_M / pop, L = TBM_L / pop, U = TBM_U / pop)]

colMeans(P_summed[, .(M, L, U)]) # ok because the proportions don't vary between countries
