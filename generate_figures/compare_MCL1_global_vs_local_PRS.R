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


dbp_xgb <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/TOPMed_dbp_xgb_full_prs_results_20230316.csv", stringsAsFactors = TRUE)
sbp_xgb <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/TOPMed_sbp_xgb_full_prs_results_20230316.csv", stringsAsFactors = TRUE)
results <- rbind(dbp_xgb, sbp_xgb)


colnames(results)[which(names(results) == "train_result_xgb")] <- "train_result"
colnames(results)[which(names(results) == "test_result_xgb")] <- "test_result"


results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))
results <- results %>% mutate(model=recode(model, 'model_1'='Level 1', 'model_2'='Level 2', 'model_3'='Level 3'))
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))

results$full_result_test <- percent(results$full_result_test, accuracy = 0.01)


percent_value_string <- results$full_result_test
numerical_value_string <- substr(percent_value_string, 1,4)
percent_value_numerical <- as.numeric(numerical_value_string)

results$full_result_test <- percent_value_numerical


# now select only Level 1 and XGB
full_PRS_L1 <- results[which(results$model == "Level 1"),]

#load local PRS TOPMed results

results2 <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/Local_PRS_output/local_prs_lasso_results_20230330.csv", stringsAsFactors = TRUE)

results2 <- results2 %>% mutate(group=recode(group, 'aa'='Black', 'asa'='Asian', 'ea'='White', 'ha'='Hispanic/Latino', 'overall' = 'Overall'))


results2$full_test_evr <- 100 * (results2$full_test_evr)

results2$full_test_evr <- format(round(results2$full_test_evr, 1), nsmall = 2)

results2$full_test_evr <- as.numeric(results2$full_test_evr)









#In the Genetic model we only use XGBoos alg because from the Baseline model we saw that XGB performs better

#first separate df to only use XGB results
#XGB_data <- results[results$genetic_model_type == 'XGBoost', ]


#plot full PRS L1
h <- ggplot(data = full_PRS_L1, aes(group, full_result_test, group = group, fill=group)) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=full_result_test, y=full_result_test), size=2.5, position=position_dodge2(width=1.0, preserve='single'), vjust=-0.25) + 
  labs(title = "Ensemble model MCL 1 global PRS", y = "PVE (%)", x = "", fill = "Population") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,40.0) +
  #scale_y_continuous(expand = c(0, 0), breaks = seq(0.0,40.0, by = 5)) +
  scale_fill_viridis_d(option = "viridis") +
  facet_wrap(~ phenotype) + theme(legend.position = "none")
h
#note:expand = c(0, 0) gives extra space at the bottom

ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures/TOPMed_Ensemble_model_perf_full_PRS_MCL1.png", width = 12, height = 7, device='png', dpi=300)


#plot local PRS L1
k <- ggplot(data = results2, aes(group, full_test_evr, group = group, fill=group)) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=full_test_evr, y=full_test_evr), size=2.5, position=position_dodge2(width=1.0, preserve='single'), vjust=-0.25) + 
  labs(title = "Ensemble model MCL 1 local PRS", y = "PVE (%)", x = "", fill = "Population") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,35.0) +
  #scale_y_continuous(expand = c(0, 0), breaks = seq(0.0,40.0, by = 5)) +
  scale_fill_viridis_d(option = "viridis") +
  facet_wrap(~ phenotype) + theme(legend.position = "none")
k
#note:expand = c(0, 0) gives extra space at the bottom

ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/TOPMed_Ensemble_model_perf_local_PRS_MCL1.png", width = 10, height = 5, device='png', dpi=600)

