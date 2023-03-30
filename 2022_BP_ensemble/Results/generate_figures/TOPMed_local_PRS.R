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

dbp_lprs <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/Results/Local_PRS_output/dbp_local_prs_lasso_results_20230330.csv", stringsAsFactors = TRUE)
sbp_lprs <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/Results/Local_PRS_output/sbp_local_prs_lasso_results_20230330.csv", stringsAsFactors = TRUE)
dbp_sbp_lr <- rbind(dbp_lr, sbp_lr)

results <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/Results/Local_PRS_output/dbp_local_prs_genetic_results_20230323.csv", stringsAsFactors = TRUE)



results <- results %>% mutate(phenotype=recode(phenotype, 'dbp'='DBP', 'sbp'='SBP'))
#results <- results %>% mutate(model=recode(model, 'model_1'='Level 1', 'model_2'='Level 2', 'model_3'='Level 3'))
results <- results %>% mutate(group=recode(group, 'aa'='Black', 'asa'='Asian', 'ea'='White', 'ha'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(Model.Type=recode(Model.Type, 'XGB'='XGBoost'))

#results$full_result_test <- percent(results$full_result_test, accuracy = 0.01)


percent_value_string <- results$test_evr
numerical_value_string <- substr(percent_value_string, 1,3)
percent_value_numerical <- as.numeric(numerical_value_string)

results$test_evr <- percent_value_numerical



#In the Genetic model we only use XGBoost alg because from the Baseline model we saw that XGB performs better

#first separate df to only use XGB results
#XGB_data <- results[results$genetic_model_type == 'XGBoost', ]

h <- ggplot(data = results, aes(group, test_evr, group = Model.Type, fill=Model.Type)) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=test_evr, y=test_evr), size=2.5, position=position_dodge2(width=1.0, preserve='single'), vjust=-0.25) + 
  labs(title = "Genetic model", y = "PVE (%)", x = "", fill = "Model type") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,10.0) +
  #scale_y_continuous(expand = c(0, 0), breaks = seq(0.0,40.0, by = 5)) +
  scale_fill_viridis_d(option = "viridis") +
  facet_wrap(~ phenotype)
h
#note:expand = c(0, 0) gives extra space at the bottom

ggsave(filename  = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/figures/TOPMed_Ensemble_model_perf_full_PRS.png", width = 12, height = 7, device='png', dpi=300)

