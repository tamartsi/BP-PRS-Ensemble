library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(formattable)

#good facet plot tutorial: http://zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/

results <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/copy_results_BP_ensembl_TopMed/mgbb_full_prs_results_20230209.csv", stringsAsFactors = TRUE)
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))
results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))



resuts_copy <- results[!results$Model.Complexity == "Model 1",]

results_copy_2 <- resuts_copy[!resuts_copy$Model.Complexity == "Model 2",]

results_copy_3 <- results_copy_2[!results_copy_2$group == "Hispanic/Latino",]



YY <- results_copy_3$test_result
YY_test <- substr(YY, 1,3)
test2 <- as.numeric(YY_test)

results_copy_3$test_result <- test2

#In the Genetic model we only use XGBoos alg because from the Baseline model we saw that XGB performs better

#first separate df to only use XGB results
XGB_data <- results_copy_3[results_copy_3$genetic_model_type == 'XGBoost', ]

h <- ggplot(data = XGB_data, aes(group, test_result, group = model, fill=model)) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=test_result, y=test_result), size=2.5, position=position_dodge2(width=1.0, preserve='single'), vjust=-0.25) + 
  labs(title = "Genetic model", y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,8.5) +
  #scale_y_continuous(expand = c(0, 0), breaks = seq(0.0,15.0, by = 1)) +
  scale_fill_viridis_d(option = "viridis") +
  facet_wrap(~ phenotype)
h
#note:expand = c(0, 0) gives extra space at the bottom

ggsave(filename  = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/figures/Genetic_model_perf_TOPMed_full_PRS.png", width = 12, height = 7, device='png', dpi=300)

