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
library(grid)
library(tidyverse)


TOPMed_results <- read.csv(file = "/2022_BP_ensemble/Results/TOPMed_SBP_DBP_full_PRS_w_CIs.csv", stringsAsFactors = FALSE)


CI_lower_bound_numerical_value_string <- as.numeric(sub("%.*", "", TOPMed_results$CI_lower_bound))
PVE_numerical_value_string <- as.numeric(sub("%.*", "", TOPMed_results$PVE) )
CI_upper_bound_numerical_value_string <- as.numeric(sub("%.*", "", TOPMed_results$CI_upper_bound) )


TOPMed_results$CI_lower_bound <- CI_lower_bound_numerical_value_string
TOPMed_results$PVE <- PVE_numerical_value_string
TOPMed_results$CI_upper_bound <- CI_upper_bound_numerical_value_string


#only XGB results
XGB_data <- TOPMed_results[TOPMed_results$model_name == 'XGBoost', ]

#only genetic model
XGB_data <- XGB_data[-which(XGB_data$model_type == "Ensemble"),]

#exclude csx model
XGB_data <- XGB_data[-which(XGB_data$model_level == "csx"),]

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
  ylim(-2.0,10.0) + 
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(3,4,5)]) +
  facet_wrap("Genetic model" ~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                               strip.background = element_rect(color = "black", size = 0.1))


h + theme(legend.position="top")


# add ensemble model:

#only XGB results
XGB_data <- TOPMed_results[TOPMed_results$model_name == 'XGBoost', ]

#only genetic model
XGB_data <- XGB_data[-which(XGB_data$model_type == "Genetic"),]

#exclude csx model
XGB_data <- XGB_data[-which(XGB_data$model_level == "csx"),]



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
  ylim(0.0,45.0) +
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(3,4,5)]) +
  facet_wrap("Ensemble model"~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                               strip.background = element_rect(color = "black", size = 0.1))


q + theme(legend.position="top")

k <- grid.arrange(h, q, ncol = 1) 





#ggsave(filename  = "/2022_BP_ensemble/Figures/TOPMed_combined_Ensemble_and_Genetic_plots_model_perf_full_PRS_vertical_strip_text_CIs.png", plot = k, width = 8, height = 6, device='png', dpi=600)
