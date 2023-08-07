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


#good facet plot tutorial: http://zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/



results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/TOPMED_local_prs_results_20230413.csv", stringsAsFactors = TRUE)

results <- results %>% mutate(group=recode(group,'Hispanic'='Hispanic/Latino'))

percent_value_string <- results$test_evr
numerical_value_string <- round(as.numeric(substr(percent_value_string, 1,4)), 1)

results$test_evr <- numerical_value_string


#In the Genetic model we only use XGBoost alg because from the Baseline model we saw that XGB performs better

#all results are for Level 3? - means we do not need to group by model complexity level? And all are XGB


XGB_data <- results[results$Model == 'XGBoost', ]

h <- ggplot(data = XGB_data, aes(x = fct_relevel(group, "Overall", after = 4), test_evr, group = group, fill=group)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = FALSE) +
  geom_text(aes(label=test_evr, y=test_evr), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -0.5) +  
  labs(y = "PVE (%)", x = "", fill = "Population") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,6.0) +
  
  scale_fill_manual(values = wes_palette("Zissou1")) +
  facet_wrap(~ Phenotype,ncol=1) + theme(legend.position = "none") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                    panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                    strip.background = element_rect(color = "black", size = 0.1))
h 



ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/TOPMed_Genetic_model_perf_local_PRS.png", width = 10, height = 5, device='png', dpi=600)

