library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)

results <- read.csv(file = "/Users/mr447/Documents/yana_hrytsenko_bwh_projects/2022_BP_ensemble/copy_results_BP_ensembl_TopMed/baseline_results_20230302.csv")
results <- results %>% mutate(group=recode(group, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'overall' = 'Overall'))
results <- results %>% mutate(model_type=recode(model_type, 'xgboost'='XGBoost', 'linear regression'='Linear Regression'))


#split data by BP
SBP_data <- results[results$phenotype == 'SBP', ]
DBP_data <- results[results$phenotype == 'DBP', ]

#further split data by model type
SBP_data_LR <- SBP_data[SBP_data$model_type == 'Linear Regression', ]
SBP_data_XGB <- SBP_data[SBP_data$model_type == 'XGBoost', ]



p <- ggplot(SBP_data, aes(x=group, y=test_result, group = model_type, fill=model_type))+
  geom_col(position=position_dodge2(width=0.8, preserve='single'),  alpha=0.9, show.legend = TRUE) +
  geom_text(aes(label=test_result, y=test_result), size=3, position=position_dodge2(width=1.0, preserve='single')) + 
  scale_fill_viridis_d() +
  #ylim(0.0, 35.0) +
  ylab("Test PVE") +
  xlab("Race/Ethnicity") 
p







#####################################

#SBP XGB
ggplot(SBP_data_XGB, aes(x=group, y=test_result))+
  geom_bar(stat='identity', fill="forest green")+
  ylab("Test PVE") +
  xlab("XGBoost")



# #SBP LR
# p <- ggplot(SBP_data_LR, aes(x=group, y=test_result, fill=group))+
#   geom_bar(stat='identity', show.legend = FALSE)+
#   scale_fill_(palette="PuBuGn") +
#   ylab("Test PVE") +
#   xlab("Linear regression") 
# p + theme(legend.position = "none")
# p


# #SBP LR
# ggplot(SBP_data_LR, aes(x=SBP_data_LR$group, y=SBP_data_LR$test_result))+
#   geom_bar(stat='identity', fill="forest green")+
#   ylab("Test PVE") +
#   xlab("Linear regression")
# 
# #SBP XGB
# ggplot(SBP_data_XGB, aes(x=SBP_data_XGB$group, y=SBP_data_XGB$test_result))+
#   geom_bar(stat='identity', fill="forest green")+
#   ylab("Test PVE") +
#   xlab("XGBoost")


#SBP
ggplot(results[results$phenotype == 'SBP', ], aes(x=results[results$phenotype == 'SBP', ]$model_type, y=results[results$phenotype == 'SBP', ]$test_result, group=results[results$phenotype == 'SBP', ]$group, fill = x=results[results$phenotype == 'SBP', ]$model_type ))+
  geom_bar(stat='identity', fill="forest green")+
  ylab("Test PVE") +
  xlab("Model type")

SBP_data <- results[results$phenotype == 'SBP', ]
DBP_data <- results[results$phenotype == 'DBP', ]


names(SBP_data)[names(SBP_data) == "phenotype"] <- "SBP"
names(DBP_data)[names(DBP_data) == "phenotype"] <- "DBP"



#split data into SBP and DBP data

#SBP_data <- results[results$phenotype == 'SBP', ]
#DBP_data <- results[results$phenotype == 'DBP', ]



ggplot(SBP_data, aes(x=SBP_data$group, y=SBP_data$test_result))+
  geom_bar(stat='identity', fill="forest green")+
  ylab("SBP test PVE Baseline model") +
  xlab("Model type")+
  scale_fill_discrete(name="Genetic model type") + theme_light()





# split data by groups (race/ethnicity)
Whites_data <- results[which(results$group == "White"),]
Blacks_data <- results[which(results$group == "Black"),]
Asians_data <- results[which(results$group == "Asian"),]
Hispanic_Latinos_data <- results[which(results$group == "Hispanic/Latino"),]
Overall <- results[which(results$group == "Overall"),]

#Whites_data_SBP <- Whites_data[Whites_data$phenotype == 'SBP', ]


ggplot(Whites_data_SBP, aes(x=Whites_data_SBP$model_type, y=signif(Whites_data_SBP$test_result, digits = 2)))+
  geom_bar(stat='identity', fill="forest green")+
  ylab("PVE") +
  xlab("Model type")




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



