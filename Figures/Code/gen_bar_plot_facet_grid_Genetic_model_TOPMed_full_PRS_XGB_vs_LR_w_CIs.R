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
library("wesanderson")


TOPMed_results <- read.csv(file = "/2022_BP_ensemble/Results/TOPMed_SBP_DBP_full_PRS_w_CIs.csv", stringsAsFactors = FALSE)


CI_lower_bound_numerical_value_string <- as.numeric(sub("%.*", "", TOPMed_results$CI_lower_bound))
PVE_numerical_value_string <- as.numeric(sub("%.*", "", TOPMed_results$PVE) )
CI_upper_bound_numerical_value_string <- as.numeric(sub("%.*", "", TOPMed_results$CI_upper_bound) )


TOPMed_results$CI_lower_bound <- CI_lower_bound_numerical_value_string
TOPMed_results$PVE <- PVE_numerical_value_string
TOPMed_results$CI_upper_bound <- CI_upper_bound_numerical_value_string



#In the Genetic model we only use XGBoost alg because from the Baseline model we saw that XGB performs better than LR

#first separate df to only use XGB results
XGB_data <- TOPMed_results[TOPMed_results$model_name == 'XGBoost', ]

#only genetic model
XGB_data <- XGB_data[-which(XGB_data$model_type == "Ensemble"),]

#exclude csx model
XGB_data <- XGB_data[-which(XGB_data$model_level == "csx"),]

#second separate df to only use XGB results
LR_data <- TOPMed_results[TOPMed_results$model_name == 'Linear Regression', ]

#exclude csx model
LR_data <- LR_data[-which(LR_data$model_level == "csx"),]

#only genetic model
LR_data <- LR_data[-which(LR_data$model_type == "Ensemble"),]

#add a column with models and levels
XGB_data <- XGB_data %>% mutate(model_level=recode(model_level, 'Level 1'='Level 1 XGBoost', 'Level 2'='Level 2 XGBoost', 'Level 3'='Level 3 XGBoost'))

LR_data <- LR_data %>% mutate(model_level=recode(model_level, 'Level 1'='Level 1 Linear Regression', 'Level 2'='Level 2 Linear Regression', 'Level 3'='Level 3 Linear Regression'))


results_2 <- rbind(XGB_data, LR_data)



h <- ggplot(data = results_2, aes(x = fct_relevel(group, "Overall", after = 4), PVE, group = model_level, fill=model_level)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = TRUE) +
  geom_errorbar( aes(x=group, ymin=CI_lower_bound, ymax=CI_upper_bound), width=0.2, colour="orange", alpha=0.9, size=0.75, position = position_dodge(0.7)) +
  geom_text(aes(label=PVE, y=CI_upper_bound), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -1.0) +  
  labs(y = "PVE (%)", x = "", fill = "Model") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-2.0,10) + 
  scale_fill_manual(values=c("plum4", "plum3", "plum1", "yellow4", "yellow3", "yellow1")) +
  facet_wrap("Genetic model" ~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                               strip.background = element_rect(color = "black", size = 0.1))


h

h + theme(legend.position="top")

#ggsave(filename  = "/2022_BP_ensemble/Figures/TOPMed_Genetic_model_perf_full_PRS_XGB_vs_LR_vertical_strip_title_CIs.png", width = 10, height = 5, device='png', dpi=600)

