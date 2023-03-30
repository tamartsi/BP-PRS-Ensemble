library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)
library(gridExtra)
library(ggplot2)
library(ggpubr)

results <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/copy_results_BP_ensembl_TopMed/mgbb_full_prs_results_20230209.csv", stringsAsFactors = TRUE)
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))
results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))


YY <- results$test_result
YY_test <- substr(YY, 1,5)
test2 <- as.numeric(YY_test)

results$test_result <- test2

h <- ggplot(data = results, aes(group, test_result, group = model_type, fill=model_type)) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=test_result, y=test_result), size=2.5, position=position_dodge2(width=1.0, preserve='single'), vjust=-0.25) + 
  labs(title = "Baseline model", y = "PVE (%)", x = "", fill = "Model type") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,35.0) +
  #scale_y_continuous(expand = c(0, 0), breaks = seq(0,35.0, by = 5.0)) +
  scale_fill_viridis_d(option = "viridis") +
  facet_wrap(~ phenotype)
h
#note:expand = c(0, 0) gives extra space at the bottom

ggsave(filename  = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/figures/Baseline_model_perf_TOPMed_full_PRS.png", width = 12, height = 7, device='png', dpi=300)


# #########################################
# #split data by BP
# SBP_data <- results[results$phenotype == 'SBP', ]
# DBP_data <- results[results$phenotype == 'DBP', ]
# 
# 
# 
# p <- ggplot(SBP_data, aes(x=group, y=test_result, group = model_type, fill=model_type))+
#   geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
#   geom_text(aes(label=test_result, y=test_result), size=2.5, position=position_dodge2(width=1.0, preserve='single'), vjust=-0.25) + 
#   labs(x = "", fill = "Model type") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_viridis_d() +
#   ggtitle("SBP")  +
#   ylab("PVE") +
#   xlab("Race/Ethnicity") + 
#   theme(axis.title.x = element_blank()) 
#   
# 
# #p 
# 
# 
# 
# j <- ggplot(DBP_data, aes(x=group, y=test_result, group = model_type, fill=model_type))+
#   geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
#   geom_text(aes(label=test_result, y=test_result), size=2.5, position=position_dodge2(width=1.0, preserve='single'), vjust=-0.25) + 
#   labs(x = "", fill = "Model type") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_viridis_d() +
#   ggtitle("DBP")  +
#   ylab("PVE") +
#   xlab("Race/Ethnicity") + 
#   theme(axis.title.x = element_blank()) 
# 
# #j
# 
# figure <- ggarrange(p, j, labels = c("A", "B"), ncol = 1, nrow = 2)
# figure
# 
