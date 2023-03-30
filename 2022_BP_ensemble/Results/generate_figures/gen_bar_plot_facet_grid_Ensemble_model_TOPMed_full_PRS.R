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

dbp_lr <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/Results/TOPMed_dbp_lr_full_prs_results_20230316.csv", stringsAsFactors = TRUE)
sbp_lr <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/Results/TOPMed_sbp_lr_full_prs_results_20230316.csv", stringsAsFactors = TRUE)
dbp_sbp_lr <- rbind(dbp_lr, sbp_lr)

dbp_xgb <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/Results/TOPMed_dbp_xgb_full_prs_results_20230316.csv", stringsAsFactors = TRUE)
sbp_xgb <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/Results/TOPMed_sbp_xgb_full_prs_results_20230316.csv", stringsAsFactors = TRUE)
dbp_sbp_xgb <- rbind(dbp_xgb, sbp_xgb)


colnames(dbp_sbp_xgb)[which(names(dbp_sbp_xgb) == "train_result_xgb")] <- "train_result"
colnames(dbp_sbp_xgb)[which(names(dbp_sbp_xgb) == "test_result_xgb")] <- "test_result"

results <- rbind(dbp_sbp_lr, dbp_sbp_xgb)

results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))
results <- results %>% mutate(model=recode(model, 'model_1'='Level 1', 'model_2'='Level 2', 'model_3'='Level 3'))
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))

results$full_result_test <- percent(results$full_result_test, accuracy = 0.01)


percent_value_string <- results$full_result_test
numerical_value_string <- substr(percent_value_string, 1,4)
percent_value_numerical <- as.numeric(numerical_value_string)

results$full_result_test <- percent_value_numerical



#In the Genetic model we only use XGBoos alg because from the Baseline model we saw that XGB performs better

#first separate df to only use XGB results
XGB_data <- results[results$genetic_model_type == 'XGBoost', ]

h <- ggplot(data = XGB_data, aes(group, full_result_test, group = model, fill=model)) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=full_result_test, y=full_result_test), size=2.5, position=position_dodge2(width=1.0, preserve='single'), vjust=-0.25) + 
  labs(title = "Ensemble model", y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,40.0) +
  #scale_y_continuous(expand = c(0, 0), breaks = seq(0.0,40.0, by = 5)) +
  scale_fill_viridis_d(option = "viridis") +
  facet_wrap(~ phenotype)
h
#note:expand = c(0, 0) gives extra space at the bottom

ggsave(filename  = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/figures/TOPMed_Ensemble_model_perf_full_PRS.png", width = 12, height = 7, device='png', dpi=300)

