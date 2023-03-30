library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(formattable)



results <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/copy_results_BP_ensembl_TopMed/full_prs_results_20230302_modified_to_percent.csv", stringsAsFactors = TRUE)
results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))
results <- results %>% mutate(model=recode(model, 'model_1'='Level 1', 'model_2'='Level 2', 'model_3'='Level 3'))
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))


YY <- results$full_result_test
YY_test <- substr(YY, 1,4)
test2 <- as.numeric(YY_test)

results$full_result_test <- test2

#In the Genetic model we only use XGBoos alg because from the Baseline model we saw that XGB performs better

#first separate df to only use XGB results
XGB_data <- results[results$genetic_model_type == 'XGBoost', ]

h <- ggplot(data = XGB_data, aes(group, full_result_test, group = model, fill=model)) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=full_result_test, y=full_result_test), size=2.5, position=position_dodge2(width=1.0, preserve='single'), vjust=-0.25) + 
  labs(title = "Ensemble model", y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,45.0) +
  #scale_y_continuous(expand = c(0, 0), breaks = seq(0.0,40.0, by = 5)) +
  scale_fill_viridis_d(option = "viridis") +
  facet_wrap(~ phenotype)
h
#note:expand = c(0, 0) gives extra space at the bottom

ggsave(filename  = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/figures/Ensemble_model_perf_TOPMed_full_PRS.png", width = 12, height = 7, device='png', dpi=300)

