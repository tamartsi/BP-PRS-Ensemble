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
library(ggfittext)

#good facet plot tutorial: http://zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/


results <- read.csv(file = "/Users/yana/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/2022_BP_ensemble/Results/20230705/local_prs/combined_into_single_file/local_prs_combined_results.csv", stringsAsFactors = TRUE)
#results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/TOPMED_local_prs_results_20230413.csv", stringsAsFactors = TRUE)

#results <- results %>% mutate(group=recode(group,'Hispanic'='Hispanic/Latino'))

percent_value_string <- results$test_evr
numerical_value_string <- round(as.numeric(substr(percent_value_string, 1,4)), 1)
results$test_evr <- numerical_value_string

percent_value_string <- as.numeric(sub("%", "", results$full_test_evr)) 
numerical_value_string <- round(as.numeric(percent_value_string), 1)
results$full_test_evr <- numerical_value_string


Genetic_model_PVE <- c("Genetic model")
Ensemble_model_PVE <- c("Ensemble model")


results$Genetic_model_PVE <- Genetic_model_PVE 

results$Ensemble_model_PVE <- Ensemble_model_PVE 




h <- ggplot(data = results, aes(x = fct_relevel(group, "Overall", after = 4), test_evr, group = model, fill=model)) +
  geom_col(width = 0.9, position = position_dodge(0.9),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=test_evr, y=test_evr), size=2.5, position=position_dodge2(width=0.9, preserve='single'), vjust= -0.5) + 
  labs(y = "PVE (%)", x = "", fill = "Model") + 
  ylim(0.0,8.0) +
  scale_fill_manual(values = wes_palette("Rushmore1")[c(5,3,4)]) + 
  #facet_wrap("PVE: Genetic model" ~ Phenotype,ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
  facet_wrap("Genetic model" ~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                 panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                 strip.background = element_rect(color = "black", size = 0.1))
h + theme(legend.position = "top") 


h


###################

q <- ggplot(data = results, aes(x = fct_relevel(group, "Overall", after = 4), full_test_evr, group = model, fill=model)) +
  geom_col(width = 0.9, position = position_dodge(0.9),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=full_test_evr, y=full_test_evr), size=2.5, position=position_dodge2(width=0.9, preserve='single'), vjust= -0.5) + 
  labs(y = "PVE (%)", x = "", fill = "Model") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,40.0) +
  scale_fill_manual(values = wes_palette("BottleRocket2")[c(1,4,5)]) +
  facet_wrap("Ensemble model" ~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                 panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                 strip.background = element_rect(color = "black", size = 0.1))


q + theme(legend.position="top") 

k <- grid.arrange(h, q, ncol = 1) 



k



ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/TOPMed_combined_Ensemble_and_Genetic_plots_model_perf_local_PRS_compare_XGB_LR_LASSO_PVE_strip_title_vertical_strip_title.png", plot = k, width = 10, height = 5, device='png', dpi=600)



