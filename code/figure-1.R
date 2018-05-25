#Generate Figure 1
source("../ref/base.R")

lib_eval("tidyverse")
lib_eval("ggsci")
lib_eval("grid")
lib_eval("gridExtra")

load('../data/rdata/ACS2016.Rdata')
age_df <- readRDS("../data/rdata/cleaned_age_counts_acs_vs_survey.RDS")
hh_df <- readRDS("../data/rdata/cleaned_hh_size_acs_vs_survey.RDS")
individuals <- readRDS("../data/rdata/individuals.RDS")
hh_main <- readRDS("../data/rdata/hh_main.RDS")

## Plot the age comparisons
fig1a <- ggplot(age_df, aes(x = age_cat, y = prop, fill = src_cat)) + 
  geom_bar(stat = 'identity', alpha = .85, position = 'dodge', width = .8) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size=8), 
        legend.position = "none") + 
  scale_fill_nejm(name = NULL) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = seq(0, .16, .04), color = "white", alpha = .5) +
  labs(x = "Age group", y = "Proportion", 
       title = "\na) Age distribution")

## Plot the HH size comparisons
fig1b <- ggplot(hh_df, aes(x = hh_cat, y = prop, fill = src_cat)) + 
  geom_bar(stat = 'identity', alpha = .85, position = 'dodge', width = .8) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size=8), 
        legend.position = c(1, .99), 
        legend.justification = c(1, 1)) + 
  scale_fill_nejm(name = NULL) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = c(.1, .2, .3), color = "white", alpha = .2) +
  labs(x = "Number of people in household", y = NULL, 
       title = "\nb) Household size")

fig1c <- aggregate(age~hh_id, data=individuals, FUN=median) %>%
{merge(hh_main, ., by="hh_id", all.x=T)} %>%
  ggplot(aes(x=as.factor(hh_size), y = age)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(size=8)) +
  labs(x = "Household Size", y = "Median Household Age (Years)", 
       title = "\nc) Median household age by household size")

fig1 <- grid.arrange(
  top = textGrob(
    "",
    gp = gpar(fontsize = 14)
    ),
  fig1a, fig1b,
  layout_matrix = t(as.matrix(c(1,2)))
)

ggsave("figure1.pdf", fig1, "pdf", "../figures/",
       units="in", width=5, height = 3, scale=2)


