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



results <- read.csv(file = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/TOPMED_local_prs_results_20230413.csv", stringsAsFactors = TRUE)

results <- results %>% mutate(group=recode(group,'Hispanic'='Hispanic/Latino'))

percent_value_string <- as.numeric(sub("%", "", results$full_test_evr)) 
numerical_value_string <- round(as.numeric(percent_value_string), 1)

results$full_test_evr <- numerical_value_string

XGB_data <- results[results$Model == 'XGBoost', ]


h <- ggplot(data = XGB_data, aes(x = fct_relevel(group, "Overall", after = 4), full_test_evr, group = group, fill=group)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = FALSE) +
  geom_text(aes(label=full_test_evr, y=full_test_evr), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -0.5) +  
  labs(y = "PVE (%)", x = "", fill = "Population") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,35.0) +
  scale_fill_manual(values = wes_palette("Zissou1")) +
  facet_wrap(~ Phenotype,ncol=1) + theme(legend.position = "none") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                                                        panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                                                        strip.background = element_rect(color = "black", size = 0.1))
h 



ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/TOPMed_Ensemble_model_perf_local_PRS.png", width = 10, height = 5, device='png', dpi=600)

