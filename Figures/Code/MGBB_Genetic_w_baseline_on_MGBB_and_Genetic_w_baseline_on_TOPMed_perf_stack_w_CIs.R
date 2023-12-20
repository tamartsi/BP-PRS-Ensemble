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


#add genetic model w baseline model fitted on TOPMed

results <- read.csv(file = "/2022_BP_ensemble/Results/MGBB_w_TOPMed_baseline_weights_SBP_DBP_full_PRS_w_CIs.csv")


CI_lower_bound_numerical_value_string <- round(as.numeric(sub("%.*", "", results$CI_lower_bound)), 1)
PVE_numerical_value_string <- round(as.numeric(sub("%.*", "", results$PVE)), 1)
CI_upper_bound_numerical_value_string <- round(as.numeric(sub("%.*", "", results$CI_upper_bound)), 1)


results$CI_lower_bound <- CI_lower_bound_numerical_value_string
results$PVE <- PVE_numerical_value_string
results$CI_upper_bound <- CI_upper_bound_numerical_value_string



XGB_data <- results[results$model_name == 'XGBoost', ]

#only genetic model
XGB_data <- XGB_data[-which(XGB_data$model_type == "Ensemble"),]


h <- ggplot(data = XGB_data, aes(x = fct_relevel(group, "Overall", after = 4), PVE, group = model_level, fill=model_level)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = TRUE) +
  geom_errorbar( aes(x=group, ymin=CI_lower_bound, ymax=CI_upper_bound), width=0.2, colour="chocolate3", alpha=0.9, size=0.75, position = position_dodge(0.7)) +
  geom_text(aes(label=PVE, y=CI_upper_bound), size=2.5, position=position_dodge2(width=0.7, preserve='single'), vjust= -1.0) + 
  labs(y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 1), 
        axis.title.x = element_text(size = 1),
        axis.text.x = element_text(size = 1),
        axis.title.y = element_text(size = 1),
        axis.text.y = element_text(size = 1)) +
  ylim(-6.0, 12.0) +
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(3,4,5)])  + 
  facet_wrap("Genetic model"~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                              panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                              strip.background = element_rect(color = "black", size = 0.1))
h + theme(legend.position="top")



#add genetic model w baseline model fitted on MGBB

results <- read.csv(file = "/2022_BP_ensemble/Results/MGBB_SBP_DBP_full_PRS_w_CIs.csv")

CI_lower_bound_numerical_value_string <- round(as.numeric(sub("%.*", "", results$CI_lower_bound)), 1)
PVE_numerical_value_string <- round(as.numeric(sub("%.*", "", results$PVE)), 1)
CI_upper_bound_numerical_value_string <- round(as.numeric(sub("%.*", "", results$CI_upper_bound)), 1)


results$CI_lower_bound <- CI_lower_bound_numerical_value_string
results$PVE <- PVE_numerical_value_string
results$CI_upper_bound <- CI_upper_bound_numerical_value_string



XGB_data <- results[results$model_name == 'XGBoost', ]

#only genetic model
XGB_data <- XGB_data[-which(XGB_data$model_type == "Ensemble"),]


q <- ggplot(data = XGB_data, aes(x = fct_relevel(group, "Overall", after = 4), PVE, group = model_level, fill=model_level)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = TRUE) +
  geom_errorbar( aes(x=group, ymin=CI_lower_bound, ymax=CI_upper_bound), width=0.2, colour="chocolate3", alpha=0.9, size=0.75, position = position_dodge(0.7)) +
  geom_text(aes(label=PVE, y=CI_upper_bound), size=2.5, position=position_dodge2(width=0.7, preserve='single'), vjust= -1.0) + 
  labs(y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 1), 
        axis.title.x = element_text(size = 1),
        axis.text.x = element_text(size = 1),
        axis.title.y = element_text(size = 1),
        axis.text.y = element_text(size = 1)) +
  ylim(-6.0, 12.0) +
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(3,4,5)])  + 
  facet_wrap("Genetic model"~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                              panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                              strip.background = element_rect(color = "black", size = 0.1))
q + theme(legend.position="top")


k <- grid.arrange(h, q, ncol = 1) 



k


#ggsave(filename  = "/2022_BP_ensemble/Figures/MGBB_Genetic_w_baseline_on_MGBB_and_Genetic_w_baseline_on_TOPMed_perf_stack_w_CIs.png", plot = k, width = 8, height = 6, device='png', dpi=600)



