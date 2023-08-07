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

TOPMed_results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/COMBINED_full_prs_results_20230413.csv", stringsAsFactors = TRUE)
results <- TOPMed_results[1:11]

#drop rows that had NAs (do not contain any info)
results <- results [-(61:100),]



results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))
results <- results %>% mutate(model=recode(model, 'model_1'='Level 1', 'model_2'='Level 2', 'model_3'='Level 3'))
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))




results$train_result_xgb <- percent(results$train_result_xgb, accuracy = 0.1)
results$test_result_xgb <- percent(results$test_result_xgb, accuracy = 0.1)
results$full_result_train <- percent(results$full_result_train, accuracy = 0.1)
results$full_result_test <- percent(results$full_result_test, accuracy = 0.1)




results <- results[,-1]
names(results)[names(results) == "phenotype"] <- "Phenotype"
names(results)[names(results) == "genetic_model_type"] <- "Model"
names(results)[names(results) == "group"] <- "Group"
names(results)[names(results) == "n_train"] <- "Training set (N)"
names(results)[names(results) == "n_test"] <- "Testing set (N)"
names(results)[names(results) == "train_result_xgb"] <- "Training PVE Genetic model"
names(results)[names(results) == "test_result_xgb"] <- "Testing PVE Genetic model"
names(results)[names(results) == "full_result_train"] <- "Training PVE Ensemble model"
names(results)[names(results) == "full_result_test"] <- "Testing PVE Ensemble model"
names(results)[names(results) == "model"] <- "Model complexity level"



write.csv(results, "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/formatted_files_for_SuppMat/TOPMed_genetic_ansemble_model_perf_full_PRS.csv", row.names=FALSE)
