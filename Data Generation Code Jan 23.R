#Final file to merge all 4 surveys
library(readxl)
library(writexl)

#Main Survey Data Generation ####
ys_responses_1 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Andhra_Pradesh_Translated_30thNov_-_all_versions_-_English_en_-_2023-01-23-17-02-49.xlsx")
ys_responses_2 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-01-23-16-59-37.xlsx")

ys_responses_3 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_AU_21stDec_Translated_-_all_versions_-_English_en_-_2023-01-23-17-03-35.xlsx")

ys1 <- colnames(ys_responses_1)
ys2 <- colnames(ys_responses_2)
ys3 <- colnames(ys_responses_3)

match(ys2,ys1) %in% c(1:213)

ys_responses_1 <- ys_responses_1[,-205]

youth_survey_responses <- rbind(ys_responses_1, ys_responses_2, ys_responses_3)

ys4 <- colnames(youth_survey_responses)

#Add Clone file Responses
ys_responses_5 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Clone_of_Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-01-23-17-06-10.xlsx")

ys5 <- colnames(ys_responses_5)

match(ys5,ys4) %in% c(1:213)

youth_survey_responses <- rbind(youth_survey_responses, ys_responses_5)

write_xlsx(youth_survey_responses, "youth_survey_responses (23rd Jan).xlsx")



#Roster Data Generation ####

#Household Roster
ys_responses_1 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Andhra_Pradesh_Translated_30thNov_-_all_versions_-_English_en_-_2023-01-23-17-02-49.xlsx", sheet = 2)
ys_responses_2 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-01-23-16-59-37.xlsx", sheet = 2)

ys_responses_3 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_AU_21stDec_Translated_-_all_versions_-_English_en_-_2023-01-23-17-03-35.xlsx", sheet = 2)

ys1 <- colnames(ys_responses_1)
ys2 <- colnames(ys_responses_2)
ys3 <- colnames(ys_responses_3)

match(ys2,ys1) %in% c(1:38)

Household_Roster <- rbind(ys_responses_1, ys_responses_2, ys_responses_3)

ys4 <- colnames(Household_Roster)


#Clone File Rosters
ys_responses_5 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Clone_of_Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-01-23-17-06-10.xlsx", sheet = 2)

ys5 <- colnames(ys_responses_5)

match(ys5,ys4) %in% c(1:38)

Household_Roster <- rbind(Household_Roster, ys_responses_5)


write_xlsx(Household_Roster, "Household Roster Youth Survey (23rd Jan).xlsx")



#Outmigration Roster ####
ys_responses_1 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Andhra_Pradesh_Translated_30thNov_-_all_versions_-_English_en_-_2023-01-23-17-02-49.xlsx", sheet = 3)
ys_responses_2 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-01-23-16-59-37.xlsx", sheet = 3)

ys_responses_3 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_AU_21stDec_Translated_-_all_versions_-_English_en_-_2023-01-23-17-03-35.xlsx", sheet = 3)

ys1 <- colnames(ys_responses_1)
ys2 <- colnames(ys_responses_2)
ys3 <- colnames(ys_responses_3)

match(ys2,ys1) %in% c(1:38)

Outmigration_Roster <- rbind(ys_responses_1, ys_responses_2, ys_responses_3)

ys4 <- colnames(Outmigration_Roster)


#Clone File Rosters
ys_responses_5 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Clone_of_Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-01-23-17-06-10.xlsx", sheet = 3)

ys5 <- colnames(ys_responses_5)

match(ys5,ys4) %in% c(1:38)

Outmigration_Roster <- rbind(Outmigration_Roster, ys_responses_5)

write_xlsx(Outmigration_Roster, "Outmigration Roster Youth Survey (23rd Jan).xlsx")
