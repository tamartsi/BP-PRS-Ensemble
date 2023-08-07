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
library("wesanderson")

#good facet plot tutorial: http://zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/

TOPMed_results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/COMBINED_full_prs_results_20230413.csv", stringsAsFactors = TRUE)
results <- TOPMed_results[1:11]

#drop rows that had NAs (do not contain any info)
results <- results [-(61:100),]
results <- data.frame(results)
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

LR_data <- results[results$genetic_model_type == 'Linear Regression', ]


XGB_data <- XGB_data %>% mutate(model=recode(model, 'Level 1'='Level 1 XGBoost', 'Level 2'='Level 2 XGBoost', 'Level 3'='Level 3 XGBoost'))

LR_data <- LR_data %>% mutate(model=recode(model, 'Level 1'='Level 1 Linear Regression', 'Level 2'='Level 2 Linear Regression', 'Level 3'='Level 3 Linear Regression'))


results_2 <- rbind(XGB_data, LR_data)


h <- ggplot(data = results_2, aes(group, test_result_xgb, group = model, fill= model)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=test_result_xgb, y=test_result_xgb), size=3, position=position_dodge2(width=0.7, preserve='single'), vjust= -0.5,) +  
  labs(y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,8.5) +
  scale_fill_manual(values=c("plum4", "plum3", "plum1", "yellow4", "yellow3", "yellow1")) +
  #scale_fill_manual(values=c("palegreen4", "palegreen3", "palegreen1", "yellow4", "yellow3", "yellow1")) +
  facet_wrap("Genetic model" ~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                      panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                      strip.background = element_rect(color = "black", size = 0.1))
                                                      
                                                    
h + theme(legend.position="top")


#h + coord_flip() + theme(legend.position="top")


#ggsave(filename  = "/Users/yana/Desktop/test_plots/pink_yellow.png", width = 8, height = 10, device='png', dpi=600)

ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/TOPMed_Genetic_model_perf_full_PRS_XGB_vs_LR_vertical_strip_title.png", width = 10, height = 5, device='png', dpi=600)

