library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)
#pheno_file <- read.csv(file = "~/Dropbox (Partners HealthCare)/2022_BP_ensemble/Data/csv_for_figures/MGBB_full_dataset_20230316.csv")

pheno_file <- read.csv(file = "/Volumes/linkage/Integration/Projects/2022_BP_ensemble/Results/MGBB_full_dataset_20230316.csv")


race_clean <- c()

for(i in 1:nrow(pheno_file)) {
  
  #extract a single row
  row <- pheno_file[i,]

  col_idx <- which(row[1:6] == 1)
  
  #get the name of that column
  race_ethnicity_name <- colnames(pheno_file)[col_idx]
  
  #append the name to the vector of names
  race_clean <- c(race_clean, race_ethnicity_name)

  
}


pheno_file["race_clean"] <- race_clean





#table(pheno_file$HTN_V1) 

pheno_file <- pheno_file %>% mutate(race_clean=recode(race_clean, 'race_clean_AA'='Black', 'race_clean_AsA'='Asian', 'race_clean_EA'='White', 'race_clean_HA'='Hispanic/Latino', 'race_clean_Other.Unknown'='Other/Unknown'))

pheno_file <- pheno_file %>% mutate(GENDER=recode(GENDER, '1'='Female', '0'='Male')) #check if 1 is Female

#pheno_file <- pheno_file %>% mutate(HTN_V1=recode(HTN_V1, '0'='No', '1'='Yes')) #check if 0 is No, what is 2?

which(pheno_file$race_clean == 'Hispanic/Latino')

#remove 2 Hispanic/Latino individuals
pheno_file <- pheno_file[-c(which(pheno_file$race_clean == 'Hispanic/Latino')),]

table(pheno_file$race_clean)

colnames(pheno_file)[which(colnames(pheno_file) == "race_clean")] <- c("Race/Ethnicity")
colnames(pheno_file)[which(colnames(pheno_file) == "GENDER")] <- c("Gender") 
colnames(pheno_file)[which(colnames(pheno_file) == "AGE_V1")] <- c("Age")
colnames(pheno_file)[which(colnames(pheno_file) == "BMI_V1")] <- c("BMI")
colnames(pheno_file)[which(colnames(pheno_file) == "DBP_V1")] <- c("DBP")
colnames(pheno_file)[which(colnames(pheno_file) == "SBP_V1")] <- c("SBP")
colnames(pheno_file)[which(colnames(pheno_file) == "HTN_V1")] <- c("Hypertensive")
#colnames(pheno_file)[19] <- c("Using ANTHTN medications")


mean(pheno_file$Age) # 60.49857

#trial2 <- pheno_file %>% select(Gender, Age, "Race/Ethnicity", SBP, DBP, 'Using ANTHTN medications', BMI)

trial2 <- pheno_file %>% select(Gender, Age, "Race/Ethnicity", SBP, DBP, BMI, Hypertensive)

table2 <- 
  tbl_summary(
    trial2,
    by = "Race/Ethnicity", # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**Characteristic**") %>% # update the column header
  bold_labels() 

table(pheno_file$Hypertensive == "Yes") 
#FALSE  TRUE 
#10333  4684 

table(pheno_file$Gender) 

table2
sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 6, height = 8
  ),
  type = "continuous",
  page_margins = page_mar()
)
table2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "~/Dropbox (Partners HealthCare)/2022_BP_ensemble/Figures/MGBB/Figure1_landscape_MGBB_data.docx", pr_section = sect_properties)
  #flextable::save_as_image(path = "~/Dropbox (Partners HealthCare)/2022_BP_ensemble/Figures/MGBB/Figure1_portrait_MGBB_data.png", webshot = "webshot")

