library(gtsummary)
library(magrittr)
library(dplyr)
library(officer)
library(webshot)
#pheno_file <- read.csv(file = "/Volumes/linkage/Integration/Projects/2022_BP_ensemble/Data/full_and_test_train_splits_TOPMed/full_data_20230126.csv")

#pheno_file <- read.csv(file = "/Volumes/linkage/Integration/Projects/2022_BP_ensemble/Data/full_and_test_train_splits_TOPMed/train_data_20230126.csv")

pheno_file <- read.csv(file = "/Volumes/linkage/Integration/Projects/2022_BP_ensemble/Data/full_and_test_train_splits_TOPMed/test_data_20230126.csv")


table(pheno_file$race_clean) 
table(pheno_file$GENDER) 
table(pheno_file$HTN_V1) 

pheno_file <- pheno_file %>% mutate(race_clean=recode(race_clean, 'AA'='Black', 'AsA'='Asian', 'EA'='White', 'HA'='Hispanic/Latino', 'Other/Unknown'='Other/Unknown'))

pheno_file <- pheno_file %>% mutate(GENDER=recode(GENDER, '0'='Female', '1'='Male')) #check if 0 is Female

pheno_file <- pheno_file %>% mutate(HTN_V1=recode(HTN_V1, '0'='No', '1'='Yes')) #check if 0 is No, what is 2?


colnames(pheno_file)[which(colnames(pheno_file) == "race_clean")] <- c("Race/Ethnicity")
colnames(pheno_file)[which(colnames(pheno_file) == "GENDER")] <- c("Gender") 
colnames(pheno_file)[which(colnames(pheno_file) == "AGE_V1")] <- c("Age")
colnames(pheno_file)[which(colnames(pheno_file) == "BMI_V1")] <- c("BMI")
colnames(pheno_file)[which(colnames(pheno_file) == "DBP_V1")] <- c("DBP")
colnames(pheno_file)[which(colnames(pheno_file) == "SBP_V1")] <- c("SBP")
colnames(pheno_file)[which(colnames(pheno_file) == "HTN_V1")] <- c("Hypertensive")


table(pheno_file$Hypertensive == "Yes") 
#FALSE  TRUE 
#25481 36814


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



table2
sect_properties <- prop_section(
  page_size = page_size(
    orient = "portrait",
    width = 6, height = 8
  ),
  type = "continuous",
  page_margins = page_mar()
)
table2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "~/Dropbox (Partners HealthCare)/2022_BP_ensemble/Figures/TOPMed/Figure1_portrait_TOPMed_testing_data.docx", pr_section = sect_properties)
  #flextable::save_as_image(path = "~/Dropbox (Partners HealthCare)/2022_BP_ensemble/Figures/Figure1_portrait_testing_data.png", webshot = "webshot")

