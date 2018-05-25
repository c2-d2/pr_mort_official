source("../ref/base.R")

lib_eval("magrittr")

fig3.a <- readRDS("../data/rdata/figure3a.RDS")

#Table S2
fig3.a %>% aggregate(value~strata+key, data=., FUN=function(x) (paste(mean(x)%>%round(0), sd(x)%>%round(0))))
