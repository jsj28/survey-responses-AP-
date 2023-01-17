#Merging Rosters
library(readxl)
library(writexl)


#Household Roster
ys_responses_1 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Andhra_Pradesh_Translated_30thNov_-_all_versions_-_English_en_-_2023-01-13-19-13-07.xlsx", sheet = 2)
ys_responses_2 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-01-13-19-12-20.xlsx", sheet = 2)

ys_responses_3 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_AU_21stDec_Translated_-_all_versions_-_English_en_-_2023-01-13-19-13-35.xlsx", sheet = 2)

ys1 <- colnames(ys_responses_1)
ys2 <- colnames(ys_responses_2)
ys3 <- colnames(ys_responses_3)

match(ys2,ys1)

Household_Roster <- rbind(ys_responses_1, ys_responses_2, ys_responses_3)

ys4 <- colnames(Household_Roster)

write_xlsx(Household_Roster, "Household Roster Youth Survey (Final 16th Jan).xlsx")

#Outmigration Roster
ys_responses_1 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Andhra_Pradesh_Translated_30thNov_-_all_versions_-_English_en_-_2023-01-13-19-13-07.xlsx", sheet = 3)
ys_responses_2 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-01-13-19-12-20.xlsx", sheet = 3)

ys_responses_3 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_AU_21stDec_Translated_-_all_versions_-_English_en_-_2023-01-13-19-13-35.xlsx", sheet = 3)

ys1 <- colnames(ys_responses_1)
ys2 <- colnames(ys_responses_2)
ys3 <- colnames(ys_responses_3)

match(ys2,ys1)

Outmigration_Roster <- rbind(ys_responses_1, ys_responses_2, ys_responses_3)

ys4 <- colnames(Outmigration_Roster)

write_xlsx(Outmigration_Roster, "Outmigration Roster Youth Survey (Final 14th Jan).xlsx")
