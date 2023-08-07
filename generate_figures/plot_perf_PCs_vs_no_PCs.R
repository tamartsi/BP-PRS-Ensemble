library(ggplot2)

folds <- c(1,2,3,4,5,1,2,3,4,5)

group_names <- c("PCs included","PCs included","PCs included", "PCs included","PCs included", "PCs not included", "PCs not included", "PCs not included", "PCs not included", "PCs not included")

phenotype_SBP <- rep(c("SBP"),each=10)

phenotype_DBP <- rep(c("DBP"),each=10)



SBP_file <- read.csv("/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/TOPMed_Cross_Validation_baseline_results_PCs_vs_no_PCs_20230330_SBP.csv")


SBP_PVE_PC_NoPC <- SBP_file[,c(3, 7)]
mean_SBP_PVE_PC <- round(SBP_PVE_PC_NoPC[6,1] * 100, digits = 1)
mean_SBP_PVE_no_PC <- round(SBP_PVE_PC_NoPC[6,2] * 100, digits = 1)

SBP_PVE_PC <- SBP_PVE_PC_NoPC[1:5,1]
SBP_PVE_no_PC <- SBP_PVE_PC_NoPC[1:5,2]
SBP_PVE_PC_and_no_PC <- c(SBP_PVE_PC, SBP_PVE_no_PC) 
SBP_percent_PVE_PC_and_no_PC <- round(SBP_PVE_PC_and_no_PC * 100, digits = 1)

#put a df together 
#df_SBP <- data.frame(folds, SBP_percent_PVE_PC_and_no_PC, group_names, phenotype_SBP)


# same steps for the DBP

DBP_file <- read.csv("/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/Results/TOPMed_Cross_Validation_baseline_results_PCs_vs_no_PCs_20230330_DBP.csv")


DBP_PVE_PC_NoPC <- DBP_file[,c(3, 7)]
mean_DBP_PVE_PC <- round(DBP_PVE_PC_NoPC[6,1] * 100, digits = 1)
mean_DBP_PVE_no_PC <- round(DBP_PVE_PC_NoPC[6,2] * 100, digits = 1)

DBP_PVE_PC <- DBP_PVE_PC_NoPC[1:5,1]
DBP_PVE_no_PC <- DBP_PVE_PC_NoPC[1:5,2]
DBP_PVE_PC_and_no_PC <- c(DBP_PVE_PC, DBP_PVE_no_PC) 
DBP_percent_PVE_PC_and_no_PC <- round(DBP_PVE_PC_and_no_PC * 100, digits = 1)

#put a df together 
#df_DBP <- data.frame(folds, DBP_percent_PVE_PC_and_no_PC, group_names, phenotype_DBP)




columns = c("folds", "percent_PVE_PC_and_no_PC", "group_names", "phenotype") 
df_SBP_DBP <- data.frame(matrix(nrow = 20, ncol = length(columns))) 
colnames(df_SBP_DBP) <-  columns

df_SBP_DBP$folds <- folds
df_SBP_DBP$percent_PVE_PC_and_no_PC <- c(SBP_percent_PVE_PC_and_no_PC, DBP_percent_PVE_PC_and_no_PC)
df_SBP_DBP$group_names <- group_names
df_SBP_DBP$phenotype <- c(phenotype_SBP, phenotype_DBP)








q <- ggplot(data=df_SBP_DBP, aes(x=folds, y=percent_PVE_PC_and_no_PC, group=group_names)) +
  geom_line(aes(color=group_names)) +
  geom_point(aes(color=group_names)) +
  scale_color_manual(values = c("purple2", "orangered2")) +
  labs(x="Folds", y = "PVE (%)") +
  
  geom_hline(data = df_SBP_DBP %>% filter(phenotype == "DBP"),
             aes(yintercept = mean_DBP_PVE_PC, linetype = "mean PVE with PCs"), col = "purple2", show.legend = TRUE) +
  
  geom_hline(data = df_SBP_DBP %>% filter(phenotype == "DBP"),
             aes(yintercept = mean_DBP_PVE_no_PC, linetype = "mean PVE without PCs"), col = "orangered2", show.legend = TRUE) +
  
  
  geom_hline(data = df_SBP_DBP %>% filter(phenotype == "SBP"),
             aes(yintercept = mean_SBP_PVE_PC, linetype = "mean PVE with PCs"), col = "purple2", show.legend = TRUE) +
  
  geom_hline(data = df_SBP_DBP %>% filter(phenotype == "SBP"),
             aes(yintercept = mean_SBP_PVE_no_PC, linetype = "mean PVE without PCs"), col = "orangered2", show.legend = TRUE) +
  
  
  facet_wrap("Baseline model" ~ phenotype, ncol=1, strip.position="right") + theme_bw() + theme(legend.title=element_blank()) +  theme(panel.spacing = unit(.05, "lines"),
                                                      panel.border = element_rect(color = "black", fill = NA, size = 0.1), 
                                                      strip.background = element_rect(color = "black", size = 0.1)) +
  scale_linetype_manual(name = "limit", values = c(2, 2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("purple2", "orangered2"))))
  

q + theme(legend.position="top") 

ggsave(filename  = "/Users/yana/Documents/GitHub/BWH_projects/2022_BP_ensemble/figures_final/larger_fonts/TOPMed_model_perf_PCs_vs_no_PCs.png", width = 8, height = 5, device='png', dpi=600)








