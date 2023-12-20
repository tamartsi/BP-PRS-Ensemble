library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(formattable)
library(scales)
library(tidyverse)


results <- read.csv("/2022_BP_ensemble/Results/Baseline_model_MGBB_SBP_DBP_w_CIs.csv")


CI_lower_bound_numerical_value_string <- round(as.numeric(sub("%.*", "", results$CI_lower_bound)), 1)
PVE_numerical_value_string <- round(as.numeric(sub("%.*", "", results$PVE)), 1)
CI_upper_bound_numerical_value_string <- round(as.numeric(sub("%.*", "", results$CI_upper_bound)), 1)


results$CI_lower_bound <- CI_lower_bound_numerical_value_string
results$PVE <- PVE_numerical_value_string
results$CI_upper_bound <- CI_upper_bound_numerical_value_string


h <- ggplot(data = results, aes(x = fct_relevel(group, "Overall", after = 4), PVE, group = group, fill=group)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = FALSE) +
  geom_errorbar( aes(x=group, ymin=CI_lower_bound, ymax=CI_upper_bound), width=0.2, colour="chocolate3", alpha=0.9, size=0.75, position = position_dodge(0.7)) +
  geom_text(aes(label=PVE, y=CI_upper_bound), size=2.5, position=position_dodge2(width=0.7, preserve='single'), vjust= -1.0) + 
  labs(y = "PVE (%)", x = "",) +
  theme(plot.title = element_text(hjust = 1), 
        axis.title.x = element_text(size = 1),
        axis.text.x = element_text(size = 1),
        axis.title.y = element_text(size = 1),
        axis.text.y = element_text(size = 1)) +
  ylim(0.0,37.0) +
  scale_fill_manual(values = wes_palette("Darjeeling1")) + 
  facet_wrap("Baseline model" ~ phenotype, ncol=1, strip.position="right") + theme(legend.position = "none") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                                                                  panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                                                                  strip.background = element_rect(color = "black", size = 0.1))


h

#ggsave(filename  = "/2022_BP_ensemble/Figures/MGBB_Baseline_model_perf_vertical_strip_title_w_CIs.png", width = 6, height = 4, device='png', dpi=600)


