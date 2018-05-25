## "Who left?" Figure

## Imports ----
source("../ref/base.R")
lib_eval("tidyverse")
lib_eval("ggsci")
lib_eval("patchwork")

## Data ----
load('../data/rdata/ACS2016.Rdata')
individuals <- readRDS("../data/rdata/ind_hh.RDS")
where_df <- readRDS("../data/rdata/where_df.RDS")
hh_main <- readRDS("../data/rdata/hh_main.RDS")

## Merge individuals with HH strata
ind_hh <- individuals %>% 
    left_join(
        hh_main %>% 
            dplyr::select(hh_id, strata)
    ) %>% 
    mutate(
        left_final_cat = factor(left_final, 
                                levels = rev(c(1:6, 0, "---")), 
                                labels = rev(c("Somewhere\nelse in PR",
                                               "Florida", 
                                               "New York", 
                                               "Texas", 
                                               "Another\nState", 
                                               "Another\nCountry", 
                                               "Don't Know", 
                                               NA)), 
                                ordered = TRUE))

left_df <- ind_hh %>% 
    mutate(left = ifelse(grepl("---", left_final, fixed = TRUE), 0, 1),
           age_cat = base::cut(age, 
                               c(seq(0, 85, 10), Inf), 
                               include.lowest = TRUE, 
                               right = FALSE, 
                               ordered = TRUE)) %>% 
    group_by(left) %>% 
    mutate(total_n = n()) %>% 
    group_by(age_cat, left_final_cat, left) %>% 
    summarize(
        total_n = first(total_n), 
        n_obs = n(), 
        p_obs = n() / total_n)
levels(left_df$age_cat)[length(levels(left_df$age_cat))] <- "80+"

left_df2 <- left_df[left_df$left == 0, ] 
left_df2$left_final_cat2 <- "Still in Household/Died in 2017"
class(left_df2$left_final_cat2)

fig2 <- ggplot() + 
    geom_bar(data = left_df %>% filter(left == 1), 
             aes(x = as.integer(age_cat) + .225, y = p_obs, 
                 fill = left_final_cat), 
             stat = "identity", alpha = .85, width = .45) + 
    scale_fill_manual(name = "Left in 2017", values = rev(pal_nejm()(7))) + 
    geom_bar(data = left_df2 %>% filter(left == 0), 
         aes(x = as.integer(age_cat) - .225, y = p_obs, colour = left_final_cat2),
         stat = "identity", alpha = 1, width = .45) + 
    scale_colour_manual(name="",values="grey") +
    theme_classic() + 
    theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), 
        legend.title=element_text(size = 9),
        legend.position = c(1, .99), 
        legend.justification = c(1, 1)) +
    scale_x_continuous(expand = c(0, 0),
                     breaks = 1:9,
                     labels = levels(left_df$age_cat)) +
    scale_y_continuous(expand = c(0, 0), 
                       breaks = seq(0, .3, .05), 
                       labels = c("0.0", "", "0.1", "", "0.2", "", "0.3")) +
    geom_hline(yintercept = seq(0, .3, .05), 
               color = "white", alpha = .5) + 
    labs(x = "Age group", y = "Proportion")


fig2

ggsave('../figures/figure2.pdf', fig2, 
       scale = 1.25, width = 5.5, height = 6)

