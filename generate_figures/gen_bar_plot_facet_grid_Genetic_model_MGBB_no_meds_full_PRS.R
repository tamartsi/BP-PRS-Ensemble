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


MGBB_results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/COMBINED_full_prs_results_20230413.csv", stringsAsFactors = TRUE)


## select MGB results
results <- MGBB_results[ ,(which(colnames(MGBB_results) == "X") + 1) : dim(MGBB_results)[2]]
## change colnames
colnames(results) <- c("Dataset", "phenotype", "genetic_model_type", "group", "n_test", "test_result", "full_result_test", "model")

with_out_Medication <- results[1:49,]
## because we saved the colnames as the first row
with_out_Medication <- with_out_Medication[-1, ]


#with_Medication <- results[53:dim(results)[1],]
#colnames(with_Medication) <- colnames(with_out_Medication)
## delete first row (without medication)



results <- with_out_Medication

results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))
results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))

percent_value_string <- as.numeric(sub("%", "", results$test_result)) 
numerical_value_string <- round(as.numeric(percent_value_string), 1)

results$test_result <- numerical_value_string

#In the Genetic model we only use XGBoost alg because from the Baseline model we saw that XGB performs better

#first separate df to only use XGB results
XGB_data <- results[results$genetic_model_type == 'XGBoost', ]



h <- ggplot(data = XGB_data, aes(group, test_result, group = model, fill=model)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=test_result, y=test_result), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -0.5) +  
  labs(y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,6.0) +
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(3,4,5)])  + 
  facet_wrap(~ phenotype,ncol=1) + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                        panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                        strip.background = element_rect(color = "black", size = 0.1))
h
#note:expand = c(0, 0) gives extra space at the bottom

ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/MGBB_no_meds_XGB_Genetic_model_perf_full_PRS.png", width = 10, height = 5, device='png', dpi=600)

