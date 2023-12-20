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
library("wesanderson")
library(tidyverse)

TOPMed_results <- read.csv(file = "/2022_BP_ensemble/Results/TOPMed_SBP_DBP_full_PRS_w_CIs.csv", stringsAsFactors = TRUE)

CI_lower_bound_numerical_value_string <- as.numeric(sub("%.*", "", TOPMed_results$CI_lower_bound))
PVE_numerical_value_string <- as.numeric(sub("%.*", "", TOPMed_results$PVE) )
CI_upper_bound_numerical_value_string <- as.numeric(sub("%.*", "", TOPMed_results$CI_upper_bound) )


TOPMed_results$CI_lower_bound <- CI_lower_bound_numerical_value_string
TOPMed_results$PVE <- PVE_numerical_value_string
TOPMed_results$CI_upper_bound <- CI_upper_bound_numerical_value_string


#only XGB results
XGB_data <- TOPMed_results[TOPMed_results$model_name == 'XGBoost', ]

#only Ensemble model
XGB_data <- XGB_data[-which(XGB_data$model_type == "Genetic"),]

#exclude csx model
XGB_data <- XGB_data[-which(XGB_data$model_level == "csx"),]

#exclude Level 1 model
XGB_data <- XGB_data[-which(XGB_data$model_level == "Level 1"),]

#exclude Level 2 model
XGB_data <- XGB_data[-which(XGB_data$model_level == "Level 2"),]


#only get the fields we need to merge it with LR below

XGB_data <- select(XGB_data, -model_level, -model_type)


#here we do not have different model levels because local PRSs only one level model

results <- read.csv(file = "/2022_BP_ensemble/Results/TOPMed_SBP_DBP_local_PRS_w_CIs.csv", stringsAsFactors = FALSE)


CI_lower_bound_numerical_value_string <- as.numeric(sub("%.*", "", results$CI_lower_bound))
PVE_numerical_value_string <- as.numeric(sub("%.*", "", results$PVE) )
CI_upper_bound_numerical_value_string <- as.numeric(sub("%.*", "", results$CI_upper_bound) )


results$CI_lower_bound <- CI_lower_bound_numerical_value_string
results$PVE <- PVE_numerical_value_string
results$CI_upper_bound <- CI_upper_bound_numerical_value_string


LR_data <- results[results$model_name == 'Linear Regression', ]


#only Ensemble model
LR_data <- LR_data[-which(LR_data$model_type == "Genetic"),]

#drop columns we don't need to bind w XGBoost
LR_data <- select(LR_data, -model_level, -model_type)

#now rbind to plot together LR and XGB

XGB_LR_together <- rbind(XGB_data, LR_data)


#rename XGBoost to XGBoost Level 3 to include in the legend plot
XGB_LR_together <- XGB_LR_together %>% mutate(model_name=recode(model_name, 'XGBoost'='Global PRS: XGBoost (Level 3)'))
XGB_LR_together <- XGB_LR_together %>% mutate(model_name=recode(model_name, 'Linear Regression'='Local PRS: Linear Regression'))



j <- ggplot(data = XGB_LR_together, aes(x = fct_relevel(group, "Overall", after = 4), PVE, group = model_name, fill=model_name)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = FALSE) +
  geom_errorbar( aes(x=group, ymin=CI_lower_bound, ymax=CI_upper_bound), width=0.2, colour="chocolate3", alpha=0.9, size=0.75, position = position_dodge(0.7)) +
  #geom_text(aes(label=PVE, y=CI_upper_bound), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -1.0) +  
  geom_text(aes(label=PVE, y=CI_upper_bound), size=2.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -1.0) +  
  labs(y = "PVE (%)", x = "", fill = "Model") + 
  theme(plot.title = element_text(hjust = 1), 
        axis.title.x = element_text(size = 1),
        axis.text.x = element_text(size = 1),
        axis.title.y = element_text(size = 1),
        axis.text.y = element_text(size = 1)) +
        ylim(-4.0,40.0) + 
        scale_fill_manual(values = wes_palette("Darjeeling1")[c(4,5)]) + 
        facet_wrap("Ensemble model" ~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                                panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                                strip.background = element_rect(color = "black", size = 0.1))


j #+ theme(legend.position="top")







#ggsave(filename  = "/2022_BP_ensemble/Figures/TOPMed_Ensemble_MCL3_full_PRS_and_LM_local_PRS_plots_model_perf_vertical_strip_text_CIs.png", plot = j, width = 6, height = 3, device='png', dpi=600)






