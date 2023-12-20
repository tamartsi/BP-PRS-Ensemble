library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(viridis)  
library(RColorBrewer)
library("wesanderson")
library(forcats)



results <- read.csv(file = "/2022_BP_ensemble/Results/Baseline_model_TOPMed_SBP_DBP_w_CIs.csv")


percent_value_string <- results$PVE
numerical_value_string <- round(as.numeric(substr(percent_value_string, 1,5)), 1)
results$PVE <- numerical_value_string

CI_lower_percent_value_string <- results$CI_lower_bound
CI_lower_numerical_value_string <- as.numeric(sub("%", "", CI_lower_percent_value_string))
results$CI_lower_bound <- CI_lower_numerical_value_string


CI_upper_percent_value_string <- results$CI_upper_bound
CI_upper_numerical_value_string <- as.numeric(sub("%", "", CI_upper_percent_value_string))
results$CI_upper_bound <- CI_upper_numerical_value_string



h <- ggplot(data = results, aes(x = fct_relevel(group, "Overall", after = 4), PVE, group = model_name, fill=model_name)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = TRUE) +
  geom_errorbar( aes(x=group, ymin=CI_lower_bound, ymax=CI_upper_bound), width=0.2, colour="chocolate3", alpha=0.9, size=0.75, position = position_dodge(0.7)) +
  geom_text(aes(label=PVE, y=CI_upper_bound), size=2.5, position=position_dodge2(width=0.7, preserve='single'), vjust= -1.0) + 
  labs(y = "PVE (%)", x = "", fill = "Model type") +
  theme(plot.title = element_text(hjust = 1), 
        axis.title.x = element_text(size = 1),
        axis.text.x = element_text(size = 1),
        axis.title.y = element_text(size = 1),
        axis.text.y = element_text(size = 1)) +
  ylim(0.0,37.0) +
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(4,5)]) + 
  facet_wrap("Baseline model"~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                               #facet_wrap(~ phenotype,ncol=1) + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.1), 
                                                                                               strip.background = element_rect(color = "black", size = 0.1))
h + theme(legend.position="top")

#ggsave(filename  = "/2022_BP_ensemble/Figures/TOPMed_Baseline_model_perf_vertical_strip_text_CIs.png", width = 6, height = 4, device='png', dpi=600)


