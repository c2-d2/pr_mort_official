#Figure S6
#analysis of death rates among neighbors by strata
source("../ref/base.R")

lib_eval("tidyverse")

households <- readRDS("../data/rdata/hh_main.RDS")

#cleaning data for neighborhood analysis
households$mort_neighbor_f <- ordered(households$mort_neighbor,
                                      levels = c(99,0,1,2,3,4,5,6),
                                      labels = c("Don't Know", 
                                                 "0","1","2","3","4","5", "7"))
households$neighbors_num_f <- ordered(households$neighbors_num,
                                      levels = c(1,2,3,4,5),
                                      labels = c(9, 25, 50, 100, 200))


fig.s6 <- households %>%
  subset(!mort_neighbor == "---")%>% 
  mutate(n_rate = as.numeric(as.character(mort_neighbor_f))/as.numeric(as.character(neighbors_num_f))) %>%
  mutate(n_rate = ifelse(is.na(n_rate), 0, n_rate)) %>%
  {aggregate(n_rate~strata, data = ., FUN = mean)} %>%
  mutate(n_rate = n_rate*1000) %>% 
  ggplot(aes(y=n_rate, x=strata)) + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Remoteness", limits=c(1:8), 
                   labels=c("1 - Least Remote", "2", "3", "4",
                            "5", "6", "7", "8 - Most Remote")) +
  ylab("Number of Deaths per 1,000 People") + 
  #ggtitle("Figure S6: Lower Bound of Rate of Deaths Reported Among \nNeighbors Post Hurricane by Strata") + 
  theme_classic() + 
  geom_hline(yintercept = 14.4, color="red", linetype =2) + 
  geom_label(aes(0,14.4,label = "Estimated Post-Hurricane Mortality Rate", vjust = -1,hjust=.1))

ggsave("figure-s6.pdf", fig.s6, "pdf", "../figures/",
       units="in", width=3, height = 3, scale=2)

