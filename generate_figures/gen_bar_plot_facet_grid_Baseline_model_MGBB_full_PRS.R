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
library(tidyverse)

#good facet plot tutorial: http://zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/

results <- read.csv("/Users/yana/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/2022_BP_ensemble/Results/20230705/MGBB_results/Baseline\ Results/MGBB_baseline_results_20230705.csv")

#results <- read.csv("/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/20230413/20230406/MGBB_baseline_results_20230330.csv")
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))

results$results <- percent(results$results, accuracy = 0.1)

percent_value_string <- as.numeric(sub("%", "", results$results)) 
numerical_value_string <- as.numeric(percent_value_string)

results$results <- numerical_value_string

#drop Hispanic/Latino due to low sample size
results <- results[-(which(results$group=="Hispanic/Latino")),]


h <- ggplot(data = results, aes(x = fct_relevel(group, "Overall", after = 4), results, group = group, fill=group)) +
  geom_col(width = 0.7, position = position_dodge(0.7),  alpha=1, show.legend = FALSE) +
  geom_text(aes(label=results, y=results), size=3.0, position=position_dodge2(width=0.7, preserve='single'), vjust= -0.5) + 
  labs(y = "PVE (%)", x = "", fill = "Model complexity") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.0,25.0) +
  scale_fill_manual(values = wes_palette("Darjeeling1")) + 
  facet_wrap("Baseline model" ~ phenotype, ncol=1, strip.position="right") + theme(legend.position = "none") + theme_bw() + theme(panel.spacing = unit(.05, "lines"),
                                                      panel.border = element_rect(color = "black", fill = NA, size = 0.1),
                                                      strip.background = element_rect(color = "black", size = 0.1))


h

ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/MGBB_Baseline_model_perf_vertical_strip_title.png", width = 10, height = 5, device='png', dpi=600)


