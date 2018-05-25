#Figure S3
source("../ref/base.R")

lib_eval("tidyverse")


hh_main <- readRDS("../data/rdata/hh_main.RDS")
individuals <- readRDS("../data/rdata/individuals.RDS")

hh_main %>% 
{table(.$hh_size)} %>%
  prop.table() %>% 
  as.data.frame() %>%
  ggplot(aes(y=Freq, x=Var1)) + 
  geom_bar(stat="identity") + 
  theme_classic() + 
  theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), 
        legend.position = c(1, .99), 
        legend.justification = c(1, 1)) +
  xlab("Household Size") + 
  ylab("Proportion of Sample") -> fig.s3a #+
  #ggtitle("Figure S2: Histogram of Proportion of Household Sizes in Sample")

aggregate(age~hh_id, data=individuals, FUN=median) %>%
{merge(hh_main, ., by="hh_id", all.x=T)} %>%
  ggplot(aes(x=as.factor(hh_size), y = age)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(size=8)) +
  labs(x = "Household Size", y = "Median Household Age (Years)", 
       title = "\nMedian household age by household size") -> fig.s3b

ggsave("figure-s3a.pdf", fig.s3a, "pdf", "../figures/",
       units="in", width=3, height = 3, scale=2)

ggsave("figure-s3b.pdf", fig.s3b, "pdf", "../figures/",
       units="in", width=3, height = 2, scale=2)

