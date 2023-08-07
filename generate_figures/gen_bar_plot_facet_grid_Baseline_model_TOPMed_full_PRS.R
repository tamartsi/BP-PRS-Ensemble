library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(viridis)  
library(RColorBrewer)
library("wesanderson")

results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/TOPMed_baseline_results_20230316.csv", stringsAsFactors = TRUE)
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(model_type=recode(model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))


percent_value_string <- results$test_result
numerical_value_string <- round(as.numeric(substr(percent_value_string, 1,5)), 1)


results$test_result <- numerical_value_string
# color schemes: https://github.com/karthik/wesanderson#bottle-rocket-1996
# color schemes: https://coolors.co/e5f9e0-a3f7b5-40c9a2-2f9c95-664147


write.csv(results, "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/TOPMed_baseline_results_20230316_formatted_for_SuppMat.csv", row.names=FALSE)

h <- ggplot(data = results, aes(group, test_result, group = model_type, fill=model_type)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = TRUE) +
  geom_text(aes(label=test_result, y=test_result), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -0.5) + 
  labs(y = "PVE (%)", x = "", fill = "Model type") +
  ylim(0.0,35.0) +
  scale_fill_manual(values = wes_palette("Rushmore1")[c(4,5)]) + 
  facet_wrap("Baseline model"~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
  #facet_wrap(~ phenotype,ncol=1) + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                    panel.border = element_rect(color = "black", fill = NA, size = 0.1), 
                                                    strip.background = element_rect(color = "black", size = 0.1))
h 


ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/TOPMed_Baseline_model_perf_vertical_strip_text.png", width = 10, height = 5, device='png', dpi=600)


