#Figure S5

source("../ref/base.R")

lib_eval("ggridges")
lib_eval("tidyverse")

hh_main <- readRDS("../data/rdata/hh_main.RDS")

#Impact of remoteness on water / power
#utilities plot
fig.s5.data <- hh_main %>%
  rowwise() %>%
  mutate_at(vars(electricity.sept, electricity.oct, electricity.nov, electricity.dec,
                 water.sept, water.oct, water.nov, water.dec,
                 cell.sept, cell.oct, cell.nov, cell.dec), 
            funs(case_when(
              is.na(as.numeric(.)) ~ 0.0,
              as.numeric(.) == 0 ~ 0.0,
              as.numeric(.) == 1 ~ 4,
              as.numeric(.) == 2 ~ 7.5,
              as.numeric(.) == 3 ~ 22.5,
              as.numeric(.) == 4 ~ 30.0,
              TRUE ~ 0.0))) %>%
  ungroup() %>%
  mutate(util_3 = electricity.sept+electricity.oct+electricity.nov+electricity.dec) %>%
  mutate(util_1 = water.sept+water.oct+water.nov+water.dec) %>%
  mutate(util_2 = cell.sept+cell.oct+cell.nov+cell.dec) %>%
  dplyr::select(strata,util_1,util_2,util_3) %>%
  gather(key,value,-strata) %>%
  mutate(key = factor(key,
                      levels=c("util_1", "util_2", "util_3"),
                      labels=c("Water", "Cellular Coverage", "Electricity")))


fig.s5 <- ggplot(fig.s5.data, aes(x = value, y = as.factor(strata))) + 
  geom_density_ridges(scale = 1) + facet_wrap(~key) + theme_classic() +
  scale_y_discrete(name="Remoteness",limits=rev(levels(as.factor(fig.s5.data$strata)))) +
  xlab("Number of Days")+
  #ggtitle("Figure 3: Minimum Number of Days without Utilities since Sept 1st by Strata of Remoteness") +
  theme(plot.title = element_text(hjust=0.5)) 

ggsave("figure-s5.pdf", fig.s5, "pdf", "../figures/",
       units="in", width=4, height = 5, scale=2)
