#Code for Logit Models
#Required - Merged files and added variables, plus invlogit function

library(readxl)
library(writexl)
library(data.table)
library(MASS)

invlogit <- function(x){1/(1 + exp(-x))}

#Main Files ####
AP_Youth_Survey <- read_excel("youth_survey_responses (7th Feb).xlsx")
AP_Household_Roster <- read_excel("Household Roster Youth Survey (7th Feb).xlsx")

#Codebooks ####
AP_Youth_Survey_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx")
colnames(AP_Youth_Survey) <- AP_Youth_Survey_Codebook$Variable_Name
attr(AP_Youth_Survey, "variable.labels") <- AP_Youth_Survey_Codebook$Column_Name

AP_Household_Roster_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx", sheet = 2)
colnames(AP_Household_Roster) <- AP_Household_Roster_Codebook$Variable_Name
attr(AP_Household_Roster, "variable.labels") <- AP_Household_Roster_Codebook$Column_Name


#Merging dataset and combining skilling variables####
AP_Youth_Survey_Merged <- merge(AP_Youth_Survey, AP_Household_Roster[AP_Household_Roster$H_1 == "Self",], by.x = c("_uuid"), by.y = c("_submission__uuid"))
attr(AP_Youth_Survey_Merged, "variable.labels") <- c(AP_Youth_Survey_Codebook$Column_Name, AP_Household_Roster_Codebook$Column_Name[AP_Household_Roster_Codebook$Column_Name != "_submission__uuid"])

AP_Youth_Survey_Merged <- AP_Youth_Survey_Merged[AP_Youth_Survey_Merged$`City Name` != "N",]

AP_Youth_Survey_Merged$YR_F_87 <- ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Student", AP_Youth_Survey_Merged$Y_F_87,
                                         ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Employed", AP_Youth_Survey_Merged$Y_F_123, AP_Youth_Survey_Merged$Y_F_160))

AP_Youth_Survey_Merged$YR_F_92 <- ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Student", AP_Youth_Survey_Merged$Y_F_92,
                                         ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Employed", AP_Youth_Survey_Merged$Y_F_128, AP_Youth_Survey_Merged$Y_F_165))

AP_Youth_Survey_Merged$YR_F_94 <- ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Student", AP_Youth_Survey_Merged$Y_F_94,
                                         ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Employed", AP_Youth_Survey_Merged$Y_F_130, AP_Youth_Survey_Merged$Y_F_167))


Small <- c("Kadiri", "Peddapuram", "Rayadurga") #Less than 1 Lakh
Medium <- c("Adoni", "Eluru", "Hindupur", "Kadapa", "Kakinada", "Narasaraopet", "Rajahmundry",
            "Tadipatri", "Tenali", "Tirupati")#1-4 Lakhs
Large <- c("Guntur", "Visakhapatnam", "Kurnool", "Nellore", "Vijayawada") #More than 4 Lakhs



AP_Youth_Survey_Merged$City_Size_Class <- ifelse(AP_Youth_Survey_Merged$`City Name`%in% Small, "Small",
                                                 ifelse(AP_Youth_Survey_Merged$`City Name` %in% Medium, "Medium",
                                                        ifelse(AP_Youth_Survey_Merged$`City Name` %in% Large, "Large",
                                                               "NAN")))



#Optional - Can Drop Small City Size Class
AP_Youth_Survey_Merged <- AP_Youth_Survey_Merged[AP_Youth_Survey_Merged$City_Size_Class != "Small", ]
#If you do include small city responses, make sure to set your baseline afresh


#Models for Skilling####
#Predictors to include

#Age
#Religion of household - Y_A_13 - Christianity, buddh, dont know, hindu, islam, none, other
#Caste - Y_A_15 (Gen, SC, ST, BC)
#City Size Class
#Primary Activity
#Gender
#Educational Background - H_5

#Baseline - Male, hindu, large city, employed (typical age 25)


#Organising Variables
y <- ifelse(AP_Youth_Survey_Merged$YR_F_87 == "No", 0, 1)

age <- AP_Youth_Survey_Merged$Age

male <- ifelse(AP_Youth_Survey_Merged$Gender == "Male", 1, 0)
female <- ifelse(AP_Youth_Survey_Merged$Gender == "Female", 1, 0)

rel_hindu <- ifelse(AP_Youth_Survey_Merged$Y_A_13 == "Hinduism", 1, 0)
rel_muslim <- ifelse(AP_Youth_Survey_Merged$Y_A_13 == "Islam", 1, 0)
rel_christ <- ifelse(AP_Youth_Survey_Merged$Y_A_13 == "Christianity", 1, 0)
rel_others <- ifelse((AP_Youth_Survey_Merged$Y_A_13 %in% c("Christianity", "Hinduism", "Islam")) == F, 1, 0)


caste_gen <- ifelse(AP_Youth_Survey_Merged$Y_A_15 == "General (OC)", 1, 0)
caste_sc <- ifelse(AP_Youth_Survey_Merged$Y_A_15 == "Scheduled Caste (SC)", 1, 0)
caste_st <- ifelse(AP_Youth_Survey_Merged$Y_A_15 == "Scheduled Tribe (ST)", 1, 0)
caste_bc <- ifelse(AP_Youth_Survey_Merged$Y_A_15 == "Backward Caste (BC)", 1, 0)
caste_others <- ifelse((AP_Youth_Survey_Merged$Y_A_15 %in% c("Backward Caste (BC)", "Scheduled Tribe (ST)", "Scheduled Caste (SC)", "General (OC)")) == F, 1, 0)

prim_act <- AP_Youth_Survey_Merged$Y_F_81

city_size <- AP_Youth_Survey_Merged$City_Size_Class


#Running the model and getting a probability estimate
logit.fit <- glm(y ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + caste_sc + caste_st + caste_bc, family=binomial(link = "logit")) 
summary(logit.fit)

logit.fit$coefficients

x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0)  ## OC, Hindu, Employed, Big City, Male

invlogit(logit.fit$coefficients %*% x)


#Simulations to generate predicted values
x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0)

set.seed(1234)
sim.coef <- mvrnorm(15000, logit.fit$coefficients, vcov(logit.fit))

predvals.sim.0 <- invlogit(sim.coef %*% x)

quantile(predvals.sim.0, seq(0,1,0.01))

#Determine 95% confidence intervals

#To be done - additional analysis beyond baseline for
#Schedule caste, ST, muslim, women, student, unemployed


