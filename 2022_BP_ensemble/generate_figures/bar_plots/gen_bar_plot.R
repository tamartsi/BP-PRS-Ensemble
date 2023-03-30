library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)
#results <- read.csv(file = "~/Dropbox (Partners HealthCare)/2022_BP_ensemble/Results/20230126_aggregate_full_prs_results.csv")
results <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/copy_results_BP_ensembl_TopMed/baseline_results_20230302.csv")

results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino'))
results <- results %>% mutate(model=recode(model, 'model_3'='Model 1', 'model_4'='Model 2', 'model_5'='Model 3'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))

#names(results)[names(results) == "genetic_model_type"] <- "Genetic model type"

# split data by race/ethnicity
Whites_data <- results[which(results$group == "White"),]
Blacks_data <- results[which(results$group == "Black"),]
Asians_data <- results[which(results$group == "Asian"),]
Hispanic_Latinos_data <- results[which(results$group == "Hispanic/Latino"),]


# Sort by BP alphabetically
Whites_data <- Whites_data[order(Whites_data$model),]

#Whites_data <- results
# with grey background
p <- ggplot(Whites_data, aes(x=model, y=test_result, group=phenotype, fill = genetic_model_type )) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=phenotype, y=test_result+0.003), size=3, position=position_dodge2(width=1.0, preserve='single')) + 
  ylim(0.0, 0.1) +
  xlab("Model complexity") +
  ylab("EVR") +
  scale_fill_discrete(name="Genetic model type") + theme_light()
  
  
  #theme(panel.background = element_blank()) 
p 


Blacks_data <- Blacks_data[order(Blacks_data$model),]

q <- ggplot(Blacks_data, aes(x=model, y=test_result, group=phenotype, fill = genetic_model_type )) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=phenotype, y=test_result+0.003), size=3, position=position_dodge2(width=1.0, preserve='single')) + 
  ylim(0.0, 0.027) +
  xlab("Model complexity") +
  ylab("EVR") +
  scale_fill_discrete(name="Genetic model type") + theme_light()


#theme(panel.background = element_blank()) 
q

Asians_data <- Asians_data[order(Asians_data$model),]

j <- ggplot(Asians_data, aes(x=model, y=test_result, group=phenotype, fill = genetic_model_type )) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=phenotype, y=test_result+0.002), size=3, position=position_dodge2(width=1.0, preserve='single')) + 
  ylim(0.0, 0.06) +
  xlab("Model complexity") +
  ylab("EVR") +
  scale_fill_discrete(name="Genetic model type") + theme_light()


#theme(panel.background = element_blank()) 
j


Hispanic_Latinos_data <- Hispanic_Latinos_data[order(Hispanic_Latinos_data$model),]

k <- ggplot(Hispanic_Latinos_data, aes(x=model, y=test_result, group=phenotype, fill = genetic_model_type )) +
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=phenotype, y=test_result+0.002), size=3, position=position_dodge2(width=1.0, preserve='single')) + 
  ylim(0.0, 0.065) +
  xlab("Model complexity") +
  ylab("EVR") +
  scale_fill_discrete(name="Genetic model type") + theme_light()


#theme(panel.background = element_blank()) 
k

# ggsave(
#   filename = "~/Dropbox (Partners HealthCare)/2022_BP_ensemble/bar_plots/genetic_model_full_PRS_test_results.png",
#   plot = last_plot(),
#   width = 12,
#   height = 15,
#   units = "in",
#   dpi = 300)



