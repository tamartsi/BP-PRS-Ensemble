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

#good facet plot tutorial: http://zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/


dbp_no_meds <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/Results/MGBB_no_meds_dbp_full_prs_results_20230316.csv", stringsAsFactors = TRUE)
sbp_no_meds <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/Results/MGBB_no_meds_sbp_full_prs_results_20230316.csv", stringsAsFactors = TRUE)
results <- rbind(dbp_no_meds, sbp_no_meds)


results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))
results <- results %>% mutate(Model.Complexity=recode(Model.Complexity, 'Model 1'='Level 1', 'Model 2'='Level 2', 'Model 3'='Level 3'))
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))

results$test_result <- percent(results$test_result, accuracy = 0.01)

percent_value_string <- results$test_result
numerical_value_string <- substr(percent_value_string, 1,4)
percent_value_numerical <- as.numeric(numerical_value_string)

results$test_result <- percent_value_numerical

colnames(results)[which(names(results) == "Model.Complexity")] <- "model"

#In the Genetic model we only use XGBoost alg because from the Baseline model we saw that XGB performs better

#first separate df to only use XGB results
XGB_data <- results[results$genetic_model_type == 'XGBoost', ]


h <- ggplot(data = XGB_data, aes(group, test_result, group = model, fill=model)) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=test_result, y=test_result), size=2.5, position=position_dodge2(width=1.0, preserve='single'), vjust=-0.25) + 
  labs(title = "Genetic model", y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,6.0) +
  #scale_y_continuous(expand = c(0, 0), breaks = seq(0.0,15.0, by = 1)) +
  scale_fill_viridis_d(option = "viridis") +
  facet_wrap(~ phenotype)
h
#note:expand = c(0, 0) gives extra space at the bottom

ggsave(filename  = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/figures/MGBB_no_meds_XGB_Genetic_model_perf_full_PRS.png", width = 12, height = 7, device='png', dpi=300)

