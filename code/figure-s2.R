#Figure S2
source("../ref/base.R")

lib_eval("tidyverse")
lib_eval("reshape2")

deaths_official <- readRDS("../data/rdata/deaths_official.RDS")

deaths_official %>%
  dplyr::select(-Pop) %>%
  melt(id.vars = "Year") %>%
  #rbind(x) %>%
  subset(!(Year == "surv"))-> data

fig.s2 <- ggplot(data=data,aes(x=variable, y=as.numeric(value), group=as.factor(Year), color=as.factor(Year),linetype=as.factor(Year))) +
  geom_point(size=2.5) +
  geom_line() +
  scale_color_manual(name="Years",
                     breaks=c("2010","2017","adj1"),
                     labels=c("2010-2016","2017 official\n Estimates",
                              "Adjusted for 1-person\nhouseholds\n"),
                     values=c("grey90","grey89","grey88","grey87",
                              "grey86","grey85","grey84","#2c7bb6","#de2d26",
                              "#fb6a4a","#a50f15")) +
  geom_line() +
  scale_linetype_manual(values=c(replicate(7,"solid"),"blank"),
                        guide=F) +
  theme_classic() +
  xlab("Month") + ylab("Number of deaths") + ylim(c(1500, 3500))

ggsave("figure-s2.pdf", fig.s2, "pdf", "../figures/",
       units="in", width=3, height = 2, scale=3)
