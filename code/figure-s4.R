#Figure S5
source("../ref/base.R")

lib_eval("tidyverse")

hh_main <- readRDS("../data/rdata/hh_main.RDS")

#proportion missing per strata after January 26th
fig.s4 <- hh_main %>%
  subset(flag == 1) %>%
  mutate(house_status = ifelse(house_status == "---", "1", house_status)) %>%
  {table(.$house_status, .$strata)} %>% prop.table() %>% as.data.frame()%>%
  ggplot(aes(x=Var2, y=Freq)) +
  geom_bar(stat="identity") +
  #ggtitle("Figure S3: Association of Remoteness on Proportion of \nAbandoned Houses / Incomplete Surveys") +
  xlab("Remoteness") + 
  ylab("Proportion of households without consent") + 
  scale_x_discrete("Remoteness", expand = c(0, 0), breaks = 1:8,
                   labels = c("1: Least \nRemote", "2", "3", "4",
                              "5", "6", "7", "8: Most \nRemote"))+
  theme_classic()

ggsave("figure-s4.pdf", fig.s4, "pdf", "../figures/",
       units="in", width=5, height = 3, scale=2)

