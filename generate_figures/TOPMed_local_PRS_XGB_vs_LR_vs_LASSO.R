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
library(forcats)
library(wesanderson)

#good facet plot tutorial: http://zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/



results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/TOPMED_local_prs_results_20230413.csv", stringsAsFactors = TRUE)

results <- results %>% mutate(group=recode(group,'Hispanic'='Hispanic/Latino'))

percent_value_string <- results$test_evr
numerical_value_string <- round(as.numeric(substr(percent_value_string, 1,4)), 1)

results$test_evr <- numerical_value_string



XGB_data <- results[results$Model == 'XGBoost', ]
LR_data <- results[results$Model == 'Linear Regression', ]
Lasso_data <- results[results$Model == 'Lasso', ]

results_2 <- rbind(XGB_data, LR_data, Lasso_data)



h <- ggplot(data = results_2, aes(x = fct_relevel(group, "Overall", after = 4), test_evr, group = Model, fill=Model)) +
  geom_col(width = 0.8, position = position_dodge(0.8),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=test_evr, y=test_evr), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -0.5) +  
  labs(y = "PVE (%)", x = "", fill = "Model") + 
  #theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,8.0) +
  scale_fill_manual(values = wes_palette("IsleofDogs1")) +
  facet_wrap(~ Phenotype,ncol=1) + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                        panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                        strip.background = element_rect(color = "black", size = 0.1))
h + theme(legend.position = "top")





# now add the Ensemble model local PRS

results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/TOPMED_local_prs_results_20230413.csv", stringsAsFactors = TRUE)

results <- results %>% mutate(group=recode(group,'Hispanic'='Hispanic/Latino'))

percent_value_string <- as.numeric(sub("%", "", results$full_test_evr)) 
numerical_value_string <- round(as.numeric(percent_value_string), 1)

results$full_test_evr <- numerical_value_string

XGB_data <- results[results$Model == 'XGBoost', ]
LR_data <- results[results$Model == 'Linear Regression', ]
Lasso_data <- results[results$Model == 'Lasso', ]

results_2 <- rbind(XGB_data, LR_data, Lasso_data)


q <- ggplot(data = results_2, aes(x = fct_relevel(group, "Overall", after = 4), full_test_evr, group = Model, fill=Model)) +
  geom_col(width = 0.8, position = position_dodge(0.8),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=full_test_evr, y=full_test_evr), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -0.5) +  
  labs(y = "PVE (%)", x = "", fill = "Model") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,40.0) +
  scale_fill_manual(values = wes_palette("GrandBudapest1")) +
  facet_wrap(~ Phenotype,ncol=1) + theme(legend.position = "none") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                        panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                        strip.background = element_rect(color = "black", size = 0.1))


q + theme(legend.position="top")

k <- grid.arrange(h, q, ncol = 1) 

k

ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/TOPMed_combined_Ensemble_and_Genetic_plots_model_perf_local_PRS_compare_XGB_LR_LASSO.png", plot = k, width = 10, height = 5, device='png', dpi=600)

