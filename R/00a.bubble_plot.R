rm(list = ls())
library(here)
library(ggthemes)

source(here('R/00.TB_care_costs.R')) 

# NTP program, inpatient and outpatient cost regression fitting - to be used for countries with missing data
prog.cst <- prog_costs %>% select(country, g_whoregion, e_inc_ds_num, e_inc_rr_num, gdp, exp_dstb, exp_mdrtb)
care.cst <- care_costs %>% select(country, g_whoregion, c.opd_dstb.m, c.opd_mdrtb.m, c.ipd_dstb.m, c.ipd_mdrtb.m) 

tbcst <- prog.cst %>% left_join(care.cst, by=c('country', 'g_whoregion'))
tb <- c('exp_dstb', 'exp_mdrtb', 'cdstb.opd', 'cmdrtb.opd','cdstb.ipd', 'cmdrtb.ipd')

tbcst <- tbcst %>% gather(tb, tbcst, exp_dstb, exp_mdrtb, c.opd_dstb.m, c.opd_mdrtb.m, c.ipd_dstb.m, c.ipd_mdrtb.m)
tbcst <- setDT(tbcst); 
tbcst <- tbcst[!(tbcst==0)]; 

tbcst[,log.gdp:=log(gdp)]; tbcst[,log.tbcst:=log(tbcst)];
plot.labels <- c(exp_dstb='Drug-susceptible TB (DS TB), NTP costs', exp_mdrtb='Multidrug-resistant TB (MDR TB), NTP costs', dstb.opd='Drug-susceptible TB (DS TB), OPD costs', mdrtb.opd='Multidrug-resistant TB (MDR TB), OPD costs', dstb.ipd='Drug-susceptible TB (DS TB), IPD costs', mdrtb.ipd='Multidrug-resistant TB (MDR TB), IPD costs')
tbcst <- tbcst[!is.na(log.tbcst)]
tbcst <- tbcst[!(log.tbcst=='Inf')]

# Log-Log linear regression
# Relationship between gdp and costs “look more normal” after transformation.
# The relationship between the log transformed variables is more linear

# log(cost) = β0 + β1. log(gdp)
log.log.lr<- lm(log.tbcst~log.gdp, tbcst)
log.log.lr.dstb.opd <- lm(log.tbcst~log.gdp, subset(tbcst, tb=='c.opd_dstb.m'))
log.log.lr.mdrtb.opd <- lm(log.tbcst~log.gdp, subset(tbcst, tb=='c.opd_mdrtb.m'))
log.log.lr.dstb.ipd <- lm(log.tbcst~log.gdp, subset(tbcst, tb=='c.ipd_dstb.m'))
log.log.lr.mdrtb.ipd <- lm(log.tbcst~log.gdp, subset(tbcst, tb=='c.ipd_mdrtb.m'))
summary(log.log.lr.dstb.opd)
summary(log.log.lr.mdrtb.opd)
summary(log.log.lr.dstb.ipd)
summary(log.log.lr.mdrtb.ipd)

# Ploting the log-log regression model results in the original scales
reg.data = data.table(x = exp(log.log.lr$model$log.gdp),
                      y = exp(predict(log.log.lr)), 
                      lwr=exp(predict(log.log.lr, interval = "confidence")[,2]), 
                      upr = exp(predict(log.log.lr, interval = "confidence")[,3]))

names(reg.data) <- c("gdp1","fitted.cst",'lwr', 'upr')
reg.data <- cbind(tbcst, reg.data)

reg.data <- reg.data %>% mutate(tb=factor(tb))
reg.data <- reg.data %>% mutate(tb=recode_factor(tb,
                                                 'exp_dstb'='exp_dstb',
                                                 'exp_mdrtb'='exp_mdrtb',
                                                 'c.opd_dstb.m'='dstb.opd',
                                                 'c.opd_mdrtb.m'='mdrtb.opd',
                                                 'c.ipd_dstb.m'='dstb.ipd',
                                                 'c.ipd_mdrtb.m'='mdrtb.ipd'))



plot <- ggplot(reg.data, aes(x = gdp, y = tbcst))  +
  geom_jitter() +
  geom_line(data = reg.data, aes(x = gdp, y = fitted.cst),
            color = viridis::viridis(1, end = 0), size = 0.7) +
  geom_ribbon(data = reg.data, aes(ymin = lwr, ymax = upr), alpha = .15)+
  scale_x_continuous(trans = 'log2',labels = comma, breaks = c(100,500,1000,5000,10000,20000,30000)) +
  scale_y_continuous(trans = 'log2',labels = comma, breaks = c(100,500,1000,5000,10000,20000, 50000,100000,200000, 500000)) +
  facet_wrap(~tb, scales = "free_y", labeller = labeller(tb=plot.labels), ncol = 2) +
  ggthemes::theme_base() +
  labs(x="GDP per capita (2020 US$)", y="Cost per patient treated (2020 US$)") 

ggsave(plot=plot, filename = here('plots','TB costs regression simple.png'), w=10,h=5, dpi = 600)
ggsave(plot=plot, filename = here('plots','TB costs regression simple.pdf'), w=10,h=5, dpi = 600)

reg.ggplot <- ggplot(data=reg.data,
                     mapping= aes(x=gdp, y=tbcst))+
  geom_point(aes(size=e_inc_ds_num/pi, fill=g_whoregion), alpha = 0.75, shape = 21) +
  geom_line(data = reg.data, aes(x = gdp, y = fitted.cst),
            color = viridis::viridis(1, end = 0), size = 0.7) +
  geom_ribbon(data = reg.data, aes(ymin = lwr, ymax = upr), alpha = .15)+
  ggrepel::geom_text_repel(data= reg.data %>% filter(e_inc_ds_num>100),aes(label = country),
                           size = 3, segment.color = NA,
                           point.padding = unit(0.1, "lines")) +
  facet_wrap(~tb, scales = "free", labeller = labeller(tb=plot.labels), ncol = 2) +
  # theme_classic() +
  ggthemes::theme_economist_white() +
  scale_size(range = c(0.05, 20)) +
  # guides(size = FALSE) +
  theme(legend.position = 'top', legend.direction = "horizontal", legend.title=element_blank())+
  guides(fill = guide_legend(nrow = 1))+
  scale_x_continuous(trans = 'log2',labels = comma, breaks = c(0,500,1000,5000,10000,20000,30000,50000)) +
  scale_y_continuous(trans = 'log2',labels = comma, breaks = c(0,100,500,1000,5000,10000,20000,50000,100000,200000, 500000)) 

library(dplyr); library(ggplot2)
legend_bubbles <- data.frame(
  label = c("1 000", "2 000", "3 000"),
  size  = c(1E6/1, 2E6/1, 3E6/1)
) %>%
  mutate(radius = sqrt(size*2000 / pi))  

reg.plot<-  reg.ggplot + geom_point(data = legend_bubbles,
                                    #  The "radius/50" was trial and error. Better way?
                                    aes(x = 600, y = 6000 + radius/5, size = size),
                                    shape = 21, color = "black", fill = NA) +
  geom_text(data = legend_bubbles, size = 3,
            aes(x = 400, y = -15000 + 1 * radius/1, label = label)) +
  annotate("text", x = 300, y = 35000, label = "TB caseload (notified TB cases)")

reg.plot
  
ggsave(plot=reg.plot, filename = here('plots','TB costs regression bubble.png'), w=10,h=5, dpi = 600)
ggsave(plot=reg.plot, filename = here('plots','TB costs regression bubble.pdf'), w=10,h=5, dpi = 600)
  