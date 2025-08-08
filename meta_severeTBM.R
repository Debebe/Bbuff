
# copying data from google doc into here manually
severe_TBM <- data.table(study=c("Sumaya et al (1975)", "Doerr et al (1995)","Paganini et al (2000)",
                                 "Degefie et al (2003)", "Kumar et al (2005)", "VanWell et al (2009)",
                                 "Tinsa et al (2010)"),
                         n_sever_sequalae=c(7, 4,9,9,60,77,1),
                         n_sequalae=c(13,4,19,9, 107, 217,4),
                         RXDur = c("24months" ,"12months","10months","10months","12months", "6months", "38months"))


library(meta)

m <- metaprop(
  event = n_sever_sequalae,
  n = n_sequalae,
  studlab = study,
  data = severe_TBM[RXDur!="6months"],
  #data = severe_TBM,
  sm = "PLOGIT",    # or use "PFT" / "PLN" / "PRAW"
  method = "Inverse", title = "Debebe"
      
)

summary(m)
forest(m,
       fontsize = 8,
       xlab = "Proportion with severe sequelae, 95%CI"
)
