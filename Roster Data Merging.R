#Merging Rosters
library(readxl)
library(writexl)

#Roster Data Generation ####

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

write_xlsx(Household_Roster, "Household Roster Youth Survey (Final 14th Jan).xlsx")

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

#Read In Files
Household_Roster <- read_excel("Household Roster Youth Survey (Final 14th Jan).xlsx")
Outmigration_Roster <- read_excel("Outmigration Roster Youth Survey (Final 14th Jan).xlsx")

table((AP_Youth_Survey$`_uuid` %in% Household_Roster$`_submission__uuid`[Household_Roster$`Relationship to the Youth Respondent` == "Self"]))


#Households with no self responses####
Household_Roster$`Relationship to the Youth Respondent`[Household_Roster$`_submission__uuid` %in% v]

v <- AP_Youth_Survey$`_uuid`[(AP_Youth_Survey$`_uuid` %in% Household_Roster$`_submission__uuid`[Household_Roster$`Relationship to the Youth Respondent` == "Self"]) == F]
Household_NonSelf <- Household_Roster[Household_Roster$`_submission__uuid` %in% v,]
Household_NonSelf <- Household_NonSelf[Household_NonSelf$Age > 18,]

1:12

for(i in 1:nrow(Household_NonSelf)) {
  
  Household_NonSelf[i,c(39:50)] = AP_Youth_Survey[AP_Youth_Survey$`_uuid` == Household_NonSelf$`_submission__uuid`[i], c(1:12)]
  
}


for(i in 1:nrow(Household_NonSelf)) {
  
  Household_NonSelf$Name_Resp[i] = AP_Youth_Survey$Y_2[AP_Youth_Survey$`_uuid` == Household_NonSelf$`_submission__uuid`[i]]
  
  
  
}


write_xlsx(Household_NonSelf, "Household_Non_Self_Responses.xlsx")


#Analysis
AP_Youth_Survey_R <- merge(AP_Youth_Survey, Household_Roster[Household_Roster$`Relationship to the Youth Respondent` == "Self",], by.x = c("_uuid"), by.y = c("_submission__uuid"))

table(AP_Youth_Survey_R$Y_F_81)

table(AP_Youth_Survey_R$`Primary Activity`, AP_Youth_Survey_R$Y_F_81)

table(AP_Youth_Survey_R$`Marital Status`)


#Scheme-Wise Receipts
Neg_Resp <- c("No", "Don't Know", "Don't know")

vec <- c(62, 67, 72, 77, 82) + 1

c <- cbind.data.frame(Col_No = as.numeric(vec), Var_Codes = colnames(AP_Youth_Survey_R)[vec], Q_Name = AP_Youth_Survey_Codebook$Column_Name[vec])

Yes_Resp <- Tot_Resp <- rep(NA, length(vec))

for (i in 1:nrow(c)) {

  t <- as.data.frame(table(AP_Youth_Survey_R[,c$Col_No[i]] %in% Neg_Resp))
  Yes_Resp[i] <- t$Freq[t$Var1 == F]
  Tot_Resp[i] <- sum(t$Freq, na.rm = T)
  
  
}

c <- cbind.data.frame(c, Yes_Resp, Tot_Resp)
c$Benefit_Rate = round(100*c$Yes_Resp/c$Tot_Resp, 2)



AP_Youth_Survey_R$Health_Ben <- ifelse(AP_Youth_Survey_R$Y_E_60 %in% Neg_Resp, 0, 1)

Emp <- c("Casual/Daily Labour", "Gig work", "Regular Salaried", "Self-Employed", "Unpaid Family Worker")

AP_Youth_Survey_R$Primary_Rec <- ifelse(AP_Youth_Survey_R$`Primary Activity` %in% Emp, "Employed",
                                               ifelse(AP_Youth_Survey_R$`Primary Activity` == "Student", "Student", "Unemployed"))

#We see that there's a large chunk of population that has been classified as employed based on the roster,
#But is unemployed in the main survey
#Most of these responses seem to be from those engaged in casual/daily labour or self-employed, with unpaid family worker next in line


#Problems to check for - 
#Occupation vs Industry
#Household Child Problem - Check children by age
#Marital status by age
#Education level and activity match between main survey and rosters

#19-29, Female vs Male, Employment vs. Data 


#Group by response on skilling questions - for student

#Household Size and Immigration



#Impact of father's education, income and other variables on self education, income and skilling status


