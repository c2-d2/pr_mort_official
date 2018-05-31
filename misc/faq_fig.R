#FAQ Image
source("../ref/base.R")
lib_eval("ggplot2")
lib_eval("ggsci")
lib_eval("dplyr")
lib_eval("scales")
lib_eval("cowplot")

## plot estimates
#Santos-Lozada and Howard (2018) : Calculating 95% CI 

# estimates from Santos-Lozada and Howard (2018)
sep.exp.ll <- 2296
sep.exp.ul <- 2469

oct.exp.ll <- 2380
oct.exp.ul <- 2476

sep.exp = 2383
oct.exp = 2428


sep.ll <- 2900
sep.ul <- 3074

oct.ll <- 2995
oct.ul <- 3091

sep.est <- 2987
oct.est <- 3042

# calculating upper and lower bounds for 95% CI
alexis <- sep.est + oct.est - sep.exp - oct.exp 
alexis.ul <- sep.ul + oct.ul - sep.exp.ll - oct.exp.ll
alexis.ll <- sep.ll + oct.ll - sep.exp.ul - oct.exp.ul

excess_df <- data.frame(cbind(c("Unadjusted survey estimate", "Adjusted survey estimate", "Official death count", "Santos-Lozada and Howard (2018)", "New York Times"),
                              c(4645,5740, 64, alexis, 1052), c(793, 1506, NA, alexis.ll, NA), c(8498,9889, NA, alexis.ul, NA)))
names(excess_df) <- c("type", "estimate", "lower", "upper")
excess_df$estimate <- as.numeric(as.character(excess_df$estimate))
excess_df$days <- 102
excess_df$days[c(4:5)] <- 41
excess_df <- excess_df[order(excess_df$days, excess_df$estimate),]
excess_df$month <- c("Sep - Oct 31","Sep - Oct 31", "Sep - Dec 31","Sep - Dec 31","Sep - Dec 31")
excess_df$month <- factor(excess_df$month, levels = excess_df$month[c(1,3)])


excess_df$lower <- as.numeric(as.character(excess_df$lower))
excess_df$upper <- as.numeric(as.character(excess_df$upper))
excess_df$type <- factor(excess_df$type, levels = excess_df$type)

faq_img <- ggplot(data = excess_df, aes(x = type, y = estimate, color = type)) +
  geom_point(size = 3) +
  scale_color_manual(name = "", values = rev(pal_nejm()(7))) +
  #scale_color_brewer(name = "",palette="Dark2") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width = 0.1) +
  geom_text(label = as.character(excess_df$estimate), size = 3, vjust = -0.2, nudge_x = 0.2) +
  theme_classic() +
  theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), 
    legend.title=element_text(size = 9),
    legend.position = c(0.35, .99), 
    legend.justification = c(1, 1)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_y_continuous("Excess death estimate", limits = c(0, NA), expand = c(0.01,0)) +
  facet_grid(~month, scales = "free_x", space = "free_x", switch = "x") +
  theme(strip.placement = "bottom") +
  theme(panel.spacing.x = unit(1, "cm"),
        strip.background = element_blank())+
  geom_text(data = excess_df[c(4,5),], aes(x = type, y = upper, 
                             label = upper), hjust = -0.5, size = 3)+
  geom_text(data = excess_df[c(4,5),], aes(x = type, y = lower, 
                             label = lower), hjust = -0.5, size = 3)


ggsave('../misc/faq_fig.png', faq_img, 
       dpi=300)

