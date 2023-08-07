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

#good facet plot tutorial: http://zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/

TOPMed_results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/COMBINED_full_prs_results_20230413.csv", stringsAsFactors = TRUE)
results <- TOPMed_results[1:11]

#drop rows that had NAs (do not contain any info)
results <- results [-(61:100),]

results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))
results <- results %>% mutate(model=recode(model, 'model_1'='Level 1', 'model_2'='Level 2', 'model_3'='Level 3'))
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))



results$test_result_xgb <- percent(results$test_result_xgb, accuracy = 0.1)

percent_value_string <- results$test_result_xgb
numerical_value_string <- as.numeric(substr(percent_value_string, 1,3))



results$test_result_xgb <- numerical_value_string



#In the Genetic model we only use XGBoost alg because from the Baseline model we saw that XGB performs better

#first separate df to only use XGB results
XGB_data <- results[results$genetic_model_type == 'XGBoost', ]

h <- ggplot(data = XGB_data, aes(group, test_result_xgb, group = model, fill=model)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=test_result_xgb, y=test_result_xgb), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -0.5) +  
  labs(y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,10.0) +
  scale_fill_manual(values = wes_palette("Rushmore1")[c(3,4,5)]) + 
  facet_wrap("Genetic model" ~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                      panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                      strip.background = element_rect(color = "black", size = 0.1))


h + theme(legend.position="top")


### add ensemble model:
TOPMed_results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/COMBINED_full_prs_results_20230413.csv", stringsAsFactors = TRUE)
results <- TOPMed_results[1:11]

#drop rows that had NAs (do not contain any info)
results <- results [-(61:100),]

results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))
results <- results %>% mutate(model=recode(model, 'model_1'='Level 1', 'model_2'='Level 2', 'model_3'='Level 3'))
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))


results$full_result_test <- percent(results$full_result_test, accuracy = 0.1)

percent_value_string <- results$full_result_test
numerical_value_string <- as.numeric(substr(percent_value_string, 1,4))

results$full_result_test <- numerical_value_string





#In the Genetic model we only use XGBoos alg because from the Baseline model we saw that XGB performs better

#first separate df to only use XGB results
XGB_data <- results[results$genetic_model_type == 'XGBoost', ]

q <- ggplot(data = XGB_data, aes(group, full_result_test, group = model, fill=model)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=full_result_test, y=full_result_test), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -0.5) +  
  labs(y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,40.0) +
  scale_fill_manual(values = wes_palette("BottleRocket2")[c(1,4,5)]) + 
  facet_wrap("Ensemble model"~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                      panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                      strip.background = element_rect(color = "black", size = 0.1))


q + theme(legend.position="top")

k <- grid.arrange(h, q, ncol = 1) 

k



ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/TOPMed_combined_Ensemble_and_Genetic_plots_model_perf_full_PRS_vertical_strip_text.png", plot = k, width = 12, height = 8, device='png', dpi=600)

