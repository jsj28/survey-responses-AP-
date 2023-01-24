#New City Name Fixer File - 23rd Jan
library(data.table)
library(readxl)
library(writexl)

#Main Files ####
AP_Youth_Survey <- read_excel("youth_survey_responses (23rd Jan).xlsx")
AP_Household_Roster <- read_excel("Household Roster Youth Survey (23rd Jan).xlsx")
AP_Outmigration_Roster <- read_excel("Outmigration Roster Youth Survey (23rd Jan).xlsx")


#Codebooks ####
AP_Youth_Survey_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx")
colnames(AP_Youth_Survey) <- AP_Youth_Survey_Codebook$Variable_Name
attr(AP_Youth_Survey, "variable.labels") <- AP_Youth_Survey_Codebook$Column_Name

AP_Household_Roster_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx", sheet = 2)
colnames(AP_Household_Roster) <- AP_Household_Roster_Codebook$Variable_Name
attr(AP_Household_Roster, "variable.labels") <- AP_Household_Roster_Codebook$Column_Name

AP_Outmigration_Roster_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx", sheet = 3)
colnames(AP_Outmigration_Roster) <- AP_Outmigration_Roster_Codebook$Variable_Name
attr(AP_Outmigration_Roster, "variable.labels") <- AP_Outmigration_Roster_Codebook$Column_Name


#Fixing City Name for City Tables ####
v <- c("VIJAYAWADA", "VIJAYAWADa")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v, 
                                      "Vijayawada", AP_Youth_Survey$`City Name`)


v <- c("GVMC(VISAKHAPATNAM)", "Gvmc visakhapatnam", "GVMC (VISAKHAPATNAM)", "Gvmc Visakhapatnam",
       "GVMC visakhapatnam", "GVMC VISAKHAPATNAM", "GVMC VISHAKAPATNAM", "Gvmc(Visakhapatnam)",
       "GVMC(VISAKHAPATNAM)", "VISAKHAPATNAM")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Visakhapatnam", AP_Youth_Survey$`City Name`)

v <- c("Tirupathi", "tirupati", "Tirupati", "TIRUPATI", "1012")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Tirupati", AP_Youth_Survey$`City Name`)



v <- c("Tadipatri", "TADIPATRI", "TADIPARI", "Tadipari", "1007")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Tadipatri", AP_Youth_Survey$`City Name`)


v <- c("KURNOOL", "Kurnool", "kurnool", "Kurnnol")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Kurnool", AP_Youth_Survey$`City Name`)

v <- c("Vaddilapeta", "Kakinada", "KAKINADA", "Kakinda")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Kakinada", AP_Youth_Survey$`City Name`)


v <- c("NELLORE", "Nellore", "21031019", "NELLLORE", "1031")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Nellore", AP_Youth_Survey$`City Name`)


v <- c("Rayadurgh", "Rayadurg", "Rayadurgham")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Rayadurga", AP_Youth_Survey$`City Name`)


v <- c("KADIRI", "Kadiri", "\nKadiri", "1005")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Kadiri", AP_Youth_Survey$`City Name`)


v <- c("Hindupur", "Hindhupur", "Himdupur", "Hi", "Hinduput")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Hindupur", AP_Youth_Survey$`City Name`)


v <- c("GUNTUR", "Guntur", "Nalandanagar", "1024")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Guntur", AP_Youth_Survey$`City Name`)

v <- c("ELURU", "Eluru")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Eluru", AP_Youth_Survey$`City Name`)

v <- c("Adoni", "ADONI", "ADINI")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Adoni", AP_Youth_Survey$`City Name`)

v <- c("Rajahmundry", "Rajamhundry", "Rajamundry", "RAJAHMUNDRY")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Rajahmundry", AP_Youth_Survey$`City Name`)

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` == "KADAPA",
                                      "Kadapa", AP_Youth_Survey$`City Name`)


write_xlsx(AP_Youth_Survey, "youth_survey_responses (23rd Jan).xlsx")

City_Wise_Numbers <- as.data.frame(table(AP_Youth_Survey$`City Name`))

City_Wise_Numbers <- City_Wise_Numbers[City_Wise_Numbers$Var1 != "N",]; colnames(City_Wise_Numbers) <- c("City Name", "Responses")

write_xlsx(City_Wise_Numbers, "AP Citywise Survey Responses.xlsx")


#Roster and Main Survey merge ####
#Merge only for those respondents who have a self
#Non Self data file
v <- AP_Youth_Survey$`_uuid`[(AP_Youth_Survey$`_uuid` %in% AP_Household_Roster$`_submission__uuid`[AP_Household_Roster$H_1 == "Self"]) == F]

AP_Household_Roster$H_1[AP_Household_Roster$`_submission__uuid` %in% v]

AP_Household_NonSelf <- AP_Household_Roster[AP_Household_Roster$`_submission__uuid` %in% v,]
AP_Household_NonSelf <- AP_Household_NonSelf[AP_Household_NonSelf$Age > 18,]


for(i in 1:nrow(AP_Household_NonSelf)) {
  
  AP_Household_NonSelf[i,c(39:50)] = AP_Youth_Survey[AP_Youth_Survey$`_uuid` == AP_Household_NonSelf$`_submission__uuid`[i], c(1:12)]
  
}


write_xlsx(AP_Household_NonSelf, "AP_Household_NonSelf_Responses.xlsx")


#Main Survey and Roster merged
AP_Youth_Survey_Merged <- merge(AP_Youth_Survey, AP_Household_Roster[AP_Household_Roster$H_1 == "Self",], by.x = c("_uuid"), by.y = c("_submission__uuid"))
attr(AP_Youth_Survey_Merged, "variable.labels") <- c(AP_Youth_Survey_Codebook$Column_Name, AP_Household_Roster_Codebook$Column_Name[AP_Household_Roster_Codebook$Column_Name != "_submission__uuid"])

write_xlsx(AP_Youth_Survey_Merged, "AP_YouthSurvey_Roster_Merged (Jan 23rd).xlsx")
