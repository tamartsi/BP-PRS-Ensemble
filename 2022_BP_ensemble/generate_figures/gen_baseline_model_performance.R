library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)
results <- read.csv(file = "~/Dropbox (Partners HealthCare)/2022_BP_ensemble/Results/20230126_aggregate_full_prs_results.csv")

results <- results %>% mutate(phenotype=recode(phenotype, 'DBP_V1'='DBP', 'SBP_V1'='SBP'))
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino'))
results <- results %>% mutate(model=recode(model, 'model_1'='Model 1', 'model_2'='Model 2', 'model_3'='Model 3', 'model_4'='Model 4', 'model_5'='Model 5', 'model_6'='Model 6'))
results <- results %>% mutate(genetic_model_type=recode(genetic_model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))




colnames(results)[1] <- c("Dataset")
colnames(results)[2] <- c("Phenotype")
colnames(results)[3] <- c("Genetic model type")
colnames(results)[4] <- c("Race/Ethnicity")
colnames(results)[5] <- c("N training")
colnames(results)[6] <- c("N testing") 
colnames(results)[7] <- c("Genetic model training results") 
colnames(results)[8] <- c("Genetic model testing results") 
colnames(results)[9] <- c("Ensemble model training results") 
colnames(results)[10] <- c("Ensemble model testing results") 
colnames(results)[11] <- c("Model complexity level")




