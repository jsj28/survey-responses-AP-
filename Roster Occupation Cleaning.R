library(data.table)
library(readxl)
library(writexl)
library(dplyr)
library(MASS)
library(kableExtra)
library(foreign)

AP_Household_Roster <- read_excel("Household Roster Youth Survey (12th Mar).xlsx")

AP_Household_Roster_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx", sheet = 2)
colnames(AP_Household_Roster) <- c(AP_Household_Roster_Codebook$Variable_Name,"NAN")
attr(AP_Household_Roster, "variable.labels") <- AP_Household_Roster_Codebook$Column_Name

table(AP_Household_Roster$H_7[AP_Household_Roster$H_6 == "Unpaid Family Worker"])


AP_Youth_Survey <- read_excel("youth_survey_responses (12th Mar).xlsx")
AP_Youth_Survey_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx")
colnames(AP_Youth_Survey) <- c(AP_Youth_Survey_Codebook$Variable_Name, "NAN")
attr(AP_Youth_Survey, "variable.labels") <- AP_Youth_Survey_Codebook$Column_Name

#Cleaning Primary Activity H_6, Reclassifying Unpaid Family Workers on the basis of response to
#Description of Occupation H_11


AP_Household_Roster$HR_6 <- ifelse(is.na(AP_Household_Roster$H_6) == F, AP_Household_Roster$H_6, 0)
AP_Household_Roster$HR_8 <- ifelse(is.na(AP_Household_Roster$H_8) == F, AP_Household_Roster$H_8, 0)


#Domestic, Home, House

Home_Vec <- c("Home|Domestic|House|Hiuse|Hiusewife|Wife|Maker|Hous")

for (i in 1:nrow(AP_Household_Roster)) {
  
  if(AP_Household_Roster$HR_6[i] == "Unpaid Family Worker") {
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Home_Vec, AP_Household_Roster$H_11[i], ignore.case = T, fixed = F) == T, 
                                          "Attending Domestic Duties", AP_Household_Roster$HR_6[i])
    
    
  } 
  
}


A <- as.data.frame(table(AP_Household_Roster$H_11[AP_Household_Roster$HR_6 == "Unpaid Family Worker"]))

AP_Household_Roster$HR_10 <- ifelse(is.na(AP_Household_Roster$H_10) == F, AP_Household_Roster$H_10, 0)


#Cleaning H_10 others on the basis of H_11 description

Doctor  <- c(0)
Engineer <- c("Software|Soft ware|Web developed")
Freelancer  <- c("Swiggy|Zomato")
Manager  <- c("manager")
Nurse  <- c("Ward boy|Working in hospital")
Officer  <- c("officer|GVMC")
Other_service_workers <- c("Security|Cleark|Sanitary|Sanitory|Sanation|Sanitation|Sweeper|Watchman|Watchmen|Sales|Sells|delivery|cook|coocking|food|buty|anganwadi|asha|shop worker|security|driver|sachivalayam|attender|assistant")
Owner_Home_based <- c("home|maker|house|Housr")
Owner_Non_home_based <- c("Tea shop|Tea.stall|Tea maker|Tailor|Tailer|Tailar|Taylor|Tylor|Weaver|Sweet|auto driving|auto driver|auto rickshaw|autodriver|autu|dry cleaner|barber|barbar|barbr|dhobhi|b dhobi|busines|bussiness|marchant|vendor|vender|Chiken shop|Fancy store|Fansy store|Owner|Flower shhop|Fruit Sales|Footwear shop|farming|farm|agriculture|dry cleaner|barber|barbar|barbr|Business|dhobhi|b dhobi|busines|laundry|Laundry|merchant|bussiness|marchant|vendor|vender")
Professionals <- c("account|acount|advocate|finance|scientist|book keeper|ca|constable|Lecturer|manager|journalist|Journalist")
Skilled_Construction_workers <- c(0)
Supervisors <- c("Supervisor|Admin|Valanteer|Valunter|Vaulanteer|Volunteer|Volenteer|Voulanter|Voulenter|valunteer|vollenters")
Teacher <- c("Teacher|Teaching")
Technician <- c("electri|data|computer|mechanic|contractor|carpenter|corpenter|clerk|clarke|technician|electri|data|craft|smith|Gold|factory|computer|mechanic|Mechanic|Mechanical|mechanical|contractor|carpenter|corpenter|repair|Repair|clerk|clarke|technician|operator")
Unskilled_labourer <- c("daily|labour|cooli")

Vec <- rbind(Doctor, Engineer, Freelancer, Manager, Nurse, Officer, Other_service_workers, Owner_Home_based, Owner_Non_home_based,
             Professionals, Skilled_Construction_workers, Supervisors, Teacher, Technician, Unskilled_labourer)

names <- c("Doctor" , "Engineer","Freelancer","Manager","Nurse","Officer","Other service workers (clerks, shop assistants etc.)",
           "Owner: Home based","Owner: Non-home based","Professionals","Skilled Construction workers", "Supervisors (service workers like ANMs)",
           "Teacher","Technician","Unskilled labourer")



for (i in 1:nrow(AP_Household_Roster)) {
  
  if(AP_Household_Roster$HR_10[i] == "Others") {
    
    for (j in 1:length(Vec)) {
      
      AP_Household_Roster$HR_10[i] <- ifelse(grepl(Vec[j], AP_Household_Roster$H_11[i], ignore.case = T, fixed = F) == T, 
                                             names[j], AP_Household_Roster$HR_10[i])
      
    }
    
  } 
  
}




AP_Household_Roster$HR_6 = ifelse(AP_Household_Roster$HR_6 == 0, NA, AP_Household_Roster$HR_6)
AP_Household_Roster$HR_10 = ifelse(AP_Household_Roster$HR_10 == 0, NA, AP_Household_Roster$HR_10)


#Replace 0s with NAs when done
