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

#Creating Skilling Variables by Sector
AP_Youth_Survey_Merged$IT_F87 <- grepl(c("Information Technology"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T) | grepl(c("Computer"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)
AP_Youth_Survey_Merged$English_F87 <- grepl(c("English"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T) | grepl(c("Communication"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)
AP_Youth_Survey_Merged$CompExam_F87 <- grepl(c("Competitive Exam"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)
AP_Youth_Survey_Merged$SciEng_F87 <- grepl(c("Engineering"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T) | grepl(c("Science"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)
AP_Youth_Survey_Merged$Repair_F87 <- grepl(c("Technical"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T) | grepl(c("Repair"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)
AP_Youth_Survey_Merged$Craft_F87 <- grepl(c("Craftwork"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)

#Interest
AP_Youth_Survey_Merged$IT_F92 <- grepl(c("Information Technology"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T) | grepl(c("Computer"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)
AP_Youth_Survey_Merged$English_F92 <- grepl(c("English"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T) | grepl(c("Communication"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)
AP_Youth_Survey_Merged$CompExam_F92 <- grepl(c("Competitive Exam"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)
AP_Youth_Survey_Merged$SciEng_F92 <- grepl(c("Engineering"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T) | grepl(c("Science"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)
AP_Youth_Survey_Merged$Repair_F92 <- grepl(c("Technical"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T) | grepl(c("Repair"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)
AP_Youth_Survey_Merged$Craft_F92 <- grepl(c("Craftwork"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)



#Optional - Can Drop Small City Size Class
#AP_Youth_Survey_Merged <- AP_Youth_Survey_Merged[AP_Youth_Survey_Merged$City_Size_Class != "Small", ]
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
#For Enrollment in Skilling
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

x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)  ## OC, Hindu, Employed, Big City, Male

invlogit(logit.fit$coefficients %*% x)


#Simulations to generate predicted values
x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)

set.seed(1234)
sim.coef <- mvrnorm(15000, logit.fit$coefficients, vcov(logit.fit))

predval.sim <- invlogit(sim.coef %*% x)

quantile(predvals.sim.0, seq(0,1,0.01))

#Determine 95% confidence intervals

#To be done - additional analysis beyond baseline for
#Schedule caste, ST, muslim, women, student, unemployed


#Loop to determine confidence intervals

sim.coef <- mvrnorm(15000, logit.fit$coefficients, vcov(logit.fit))

bounds <- matrix(NA, nrow = 13, ncol = 2)
mean_prob <- rep(NA, 13)

var_names <- c('Baseline', 'Female', 'Student', 'Unemployed', 'Medium City', 'Small City', 
               'Other Religions', 'Muslims', 'Christians', 'Other Castes', 'SC', 'ST', 'Backward Castes')

for (i in 1:13) {
  
  x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)

  if(i > 1) {
    
    x[i+1] = 1
    
  }
  
  predval.sim <- invlogit(sim.coef %*% x)
  
  mean_prob[i] <- mean(predval.sim)
  
  bounds[i,] <- quantile(predval.sim, c(0.025,0.975))
  
}


Conf_Int_Probs1 = cbind.data.frame(Variable = var_names, Lower_Bound = bounds[,1], Mean = mean_prob, Upper_Bound = bounds[,2])

Conf_Int_Probs1[,c(2:4)] <-  round(Conf_Int_Probs1[,c(2:4)], 2)



#Model 2 - For interest in skilling####
y <- ifelse(AP_Youth_Survey_Merged$YR_F_92 == "No", 0, 1)

logit.fit <- glm(y ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + caste_sc + caste_st + caste_bc, family=binomial(link = "logit")) 
summary(logit.fit)

logit.fit$coefficients

x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)  ## OC, Hindu, Employed, Big City, Male

invlogit(logit.fit$coefficients %*% x)

#Loop to determine confidence intervals
set.seed(1234)
sim.coef <- mvrnorm(15000, logit.fit$coefficients, vcov(logit.fit))

bounds <- matrix(NA, nrow = 13, ncol = 2)
mean_prob <- rep(NA, 13)


var_names <- c('Baseline', 'Female', 'Student', 'Unemployed', 'Medium City', 'Small City', 
               'Other Religions', 'Muslims', 'Christians', 'Other Castes', 'SC', 'ST', 'Backward Castes')

for (i in 1:13) {
  
  x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
  
  if(i > 1) {
    
    x[i+1] = 1
    
  }
  
  predval.sim <- invlogit(sim.coef %*% x)
  
  mean_prob[i] <- mean(predval.sim)
  
  bounds[i,] <- quantile(predval.sim, c(0.025,0.975))
  
}


Conf_Int_Probs2 = cbind.data.frame(Variable = var_names, Lower_Bound = bounds[,1], Mean = mean_prob, Upper_Bound = bounds[,2])

Conf_Int_Probs2[,c(2:4)] <-  round(Conf_Int_Probs2[,c(2:4)], 2)

rm(predval.sim, bounds,sim.coef,i)




#Outcome 3 - Mapping Labour Sector Interest to Skilling Course Interest ####
#YF_170
l1 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$IT_F87), 1),1))
l2 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$English_F87), 2),1))
l3 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$CompExam_F87), 2),1))
l4 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$SciEng_F87), 2),1))
l5 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$Repair_F87), 2),1))
l6 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$Craft_F87), 2),1))


l1 <- l1[l1$Var2 == T,]; l2 <- l2[l2$Var2 == T,]; l3 <- l3[l3$Var2 == T,]; l4 <- l4[l4$Var2 == T,]; l5 <- l5[l5$Var2 == T,]; l6 <- l6[l6$Var2 == T,]

Labour_Skill <- cbind.data.frame(Labour_Pref = l1$Var1, l1$Freq, l2$Freq, l3$Freq, l4$Freq, l5$Freq, l6$Freq )
colnames(Labour_Skill) <- c("Labour Preference","IT", "English", "Competitive Exam", "Engineering/Science", "Technical/Repair", "Craftwork")


#Interest in Skilling
l1 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$IT_F92), 1),1))
l2 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$English_F92), 2),1))
l3 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$CompExam_F92), 2),1))
l4 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$SciEng_F92), 2),1))
l5 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$Repair_F92), 2),1))
l6 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$Craft_F92), 2),1))

l1 <- l1[l1$Var2 == T,]; l2 <- l2[l2$Var2 == T,]; l3 <- l3[l3$Var2 == T,]; l4 <- l4[l4$Var2 == T,]; l5 <- l5[l5$Var2 == T,]; l6 <- l6[l6$Var2 == T,]

Labour_Skill_Int <- cbind.data.frame(Labour_Pref = l1$Var1, l1$Freq, l2$Freq, l3$Freq, l4$Freq, l5$Freq, l6$Freq )
colnames(Labour_Skill_Int) <- c("Labour Preference","IT", "English", "Competitive Exam", "Engineering/Science", "Technical/Repair", "Craftwork")


rm(l1,l2,l3,l4,l5,l6)

