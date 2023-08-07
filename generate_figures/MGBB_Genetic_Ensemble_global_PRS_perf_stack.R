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

MGBB_results_dbp <- read.csv(file = "/Users/yana/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/2022_BP_ensemble/Results/20230705/MGBB_results/Without\ Meds/dbp_mgbb_full_prs_results\ 20230705.csv", stringsAsFactors = TRUE)

MGBB_results_sbp <- read.csv(file = "/Users/yana/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/2022_BP_ensemble/Results/20230705/MGBB_results/Without\ Meds/sbp_mgbb_full_prs_results\ 20230705.csv", stringsAsFactors = TRUE)


results <- rbind(MGBB_results_sbp,MGBB_results_dbp)

results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))
results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))

results$test_result <- percent(results$test_result, accuracy = 0.1)
percent_value_string <- as.numeric(sub("%", "", results$test_result)) 
numerical_value_string <- round(as.numeric(percent_value_string), 1)
results$test_result <- numerical_value_string

results$full_result_test <- percent(results$full_result_test, accuracy = 0.1)
percent_value_string <- as.numeric(sub("%", "", results$full_result_test)) 
numerical_value_string <- round(as.numeric(percent_value_string), 1)
results$full_result_test <- numerical_value_string

XGB_data <- results[results$genetic_model_type == 'XGBoost', ]

#drop the Hispanic/Latinos due to low sample size
XGB_data <- XGB_data[-(which(XGB_data$group=="Hispanic/Latino")),]


h <- ggplot(data = XGB_data, aes(group, test_result, group = Model.Complexity, fill=Model.Complexity)) +
  geom_col(width = 0.9, position = position_dodge(0.9),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=test_result, y=test_result), size=2.5, position=position_dodge2(width=0.9, preserve='single'), vjust= -0.5) +  
  labs(y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-2.0,10.0) +
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(3,4,5)])  + 
  facet_wrap("Genetic model"~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                 panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                 strip.background = element_rect(color = "black", size = 0.1))
h

q <- ggplot(data = XGB_data, aes(group, full_result_test, group = Model.Complexity, fill=Model.Complexity)) +
  geom_col(width = 0.9, position = position_dodge(0.9),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=full_result_test, y=full_result_test), size=2.5, position=position_dodge2(width=0.9, preserve='single'), vjust= -0.5) +  
  labs(y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,25.0) +
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(3,1,2)])  + 
  facet_wrap("Ensemble model"~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                               panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                               strip.background = element_rect(color = "black", size = 0.1))
q

k <- grid.arrange(h, q, ncol = 1) 



k

ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/MGBB_combined_Ensemble_and_Genetic_plots_model_perf_PVE_vertical_strip_title.png", plot = k, width = 10, height = 7, device='png', dpi=600)
