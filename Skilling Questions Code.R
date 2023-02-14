library(data.table)
library(readxl)
library(writexl)
library(dplyr)
library(MASS)
library(kableExtra)
library(foreign)

#Main Files ####
AP_Youth_Survey <- read_excel("youth_survey_responses (27th Jan).xlsx")
AP_Household_Roster <- read_excel("Household Roster Youth Survey (27th Jan).xlsx")
AP_Outmigration_Roster <- read_excel("Outmigration Roster Youth Survey (27th Jan).xlsx")


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


AP_Youth_Survey_Merged <- merge(AP_Youth_Survey, AP_Household_Roster[AP_Household_Roster$H_1 == "Self",], by.x = c("_uuid"), by.y = c("_submission__uuid"))
attr(AP_Youth_Survey_Merged, "variable.labels") <- c(AP_Youth_Survey_Codebook$Column_Name, AP_Household_Roster_Codebook$Column_Name[AP_Household_Roster_Codebook$Column_Name != "_submission__uuid"])

#Wrapping functions
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}

wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}


#Combining Skill Question Columns#### 
#In main survey
AP_Youth_Survey$YR_F_87 <- ifelse(AP_Youth_Survey$Y_F_81 == "Student", AP_Youth_Survey$Y_F_87,
                                  ifelse(AP_Youth_Survey$Y_F_81 == "Employed", AP_Youth_Survey$Y_F_123, AP_Youth_Survey$Y_F_160))

AP_Youth_Survey$YR_F_92 <- ifelse(AP_Youth_Survey$Y_F_81 == "Student", AP_Youth_Survey$Y_F_92,
                                  ifelse(AP_Youth_Survey$Y_F_81 == "Employed", AP_Youth_Survey$Y_F_128, AP_Youth_Survey$Y_F_165))

AP_Youth_Survey$YR_F_94 <- ifelse(AP_Youth_Survey$Y_F_81 == "Student", AP_Youth_Survey$Y_F_94,
                                  ifelse(AP_Youth_Survey$Y_F_81 == "Employed", AP_Youth_Survey$Y_F_130, AP_Youth_Survey$Y_F_167))

#Analysing Skilling Course Responses####
#Course Categories
#Information Technology, Computer
#Engineering, Science
#English, Communication
#Competitive Exam
#Technical Repair
#Craftwork

#Creating Variables
AP_Youth_Survey$IT_F87 <- grepl(c("Information Technology"), AP_Youth_Survey$YR_F_87, ignore.case = T) | grepl(c("Computer"), AP_Youth_Survey$YR_F_87, ignore.case = T)
AP_Youth_Survey$English_F87 <- grepl(c("English"), AP_Youth_Survey$YR_F_87, ignore.case = T) | grepl(c("Communication"), AP_Youth_Survey$YR_F_87, ignore.case = T)
AP_Youth_Survey$CompExam_F87 <- grepl(c("Competitive Exam"), AP_Youth_Survey$YR_F_87, ignore.case = T)
AP_Youth_Survey$SciEng_F87 <- grepl(c("Engineering"), AP_Youth_Survey$YR_F_87, ignore.case = T) | grepl(c("Science"), AP_Youth_Survey$YR_F_87, ignore.case = T)
AP_Youth_Survey$Repair_F87 <- grepl(c("Technical"), AP_Youth_Survey$YR_F_87, ignore.case = T) | grepl(c("Repair"), AP_Youth_Survey$YR_F_87, ignore.case = T)
AP_Youth_Survey$Craft_F87 <- grepl(c("Craftwork"), AP_Youth_Survey$YR_F_87, ignore.case = T)


#Interest
AP_Youth_Survey$IT_F92 <- grepl(c("Information Technology"), AP_Youth_Survey$YR_F_92, ignore.case = T) | grepl(c("Computer"), AP_Youth_Survey$YR_F_92, ignore.case = T)
AP_Youth_Survey$English_F92 <- grepl(c("English"), AP_Youth_Survey$YR_F_92, ignore.case = T) | grepl(c("Communication"), AP_Youth_Survey$YR_F_92, ignore.case = T)
AP_Youth_Survey$CompExam_F92 <- grepl(c("Competitive Exam"), AP_Youth_Survey$YR_F_92, ignore.case = T)
AP_Youth_Survey$SciEng_F92 <- grepl(c("Engineering"), AP_Youth_Survey$YR_F_92, ignore.case = T) | grepl(c("Science"), AP_Youth_Survey$YR_F_92, ignore.case = T)
AP_Youth_Survey$Repair_F92 <- grepl(c("Technical"), AP_Youth_Survey$YR_F_92, ignore.case = T) | grepl(c("Repair"), AP_Youth_Survey$YR_F_92, ignore.case = T)
AP_Youth_Survey$Craft_F92 <- grepl(c("Craftwork"), AP_Youth_Survey$YR_F_92, ignore.case = T)


AP_Youth_Survey$Total_Enr_F87 <- rowSums(AP_Youth_Survey[,c(217:222)])
AP_Youth_Survey$Total_Int_F92 <- rowSums(AP_Youth_Survey[,c(223:228)])


#Enrollment
IT <- table(AP_Youth_Survey$IT_F87)
English <- table(AP_Youth_Survey$English_F87)
Competitive_Exam <- table(AP_Youth_Survey$CompExam_F87)
Eng_Science <- table(AP_Youth_Survey$SciEng_F87)
Repair <- table(AP_Youth_Survey$Repair_F87)
Craftwork <- table(AP_Youth_Survey$Craft_F87)

cbind(IT, English, Competitive_Exam, Eng_Science, Repair, Craftwork)

#Interest to take
IT <- table(AP_Youth_Survey$IT_F92)
English <- table(AP_Youth_Survey$English_F92)
Competitive_Exam <- table(AP_Youth_Survey$CompExam_F92)
Eng_Science <- table(AP_Youth_Survey$SciEng_F92)
Repair <- table(AP_Youth_Survey$Repair_F92)
Craftwork <- table(AP_Youth_Survey$Craft_F92)

cbind(IT, English, Competitive_Exam, Eng_Science, Repair, Craftwork)



#Category Wise Enrolment
IT <- table(AP_Youth_Survey$IT_F87, AP_Youth_Survey$Y_F_81)
English <- table(AP_Youth_Survey$English_F87, AP_Youth_Survey$Y_F_81)
Competitive_Exam <- table(AP_Youth_Survey$CompExam_F87, AP_Youth_Survey$Y_F_81)
Eng_Science <- table(AP_Youth_Survey$SciEng_F87, AP_Youth_Survey$Y_F_81)
Repair <- table(AP_Youth_Survey$Repair_F87, AP_Youth_Survey$Y_F_81)
Craftwork <- table(AP_Youth_Survey$Craft_F87, AP_Youth_Survey$Y_F_81)

cbl <- cbind(IT[2,], English[2,], Competitive_Exam[2,], Eng_Science[2,], Repair[2,], Craftwork[2,])
colnames(cbl) <- c("IT", "English", "Competitive Exam", "Engineering/Science", "Technical/Repair", "Craftwork")

cbl <- rbind(cbl, Total = colSums(cbl))

cbl %>%
  kbl(caption = "Distribution of Enrolment in Skilling Courses", align = "l")%>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 11) %>%
  row_spec(4, bold = T)



#Category wise Interest
IT <- table(AP_Youth_Survey$IT_F92, AP_Youth_Survey$Y_F_81)
English <- table(AP_Youth_Survey$English_F92, AP_Youth_Survey$Y_F_81)
Competitive_Exam <- table(AP_Youth_Survey$CompExam_F92, AP_Youth_Survey$Y_F_81)
Eng_Science <- table(AP_Youth_Survey$SciEng_F92, AP_Youth_Survey$Y_F_81)
Repair <- table(AP_Youth_Survey$Repair_F92, AP_Youth_Survey$Y_F_81)
Craftwork <- table(AP_Youth_Survey$Craft_F92, AP_Youth_Survey$Y_F_81)

cbl <- cbind(IT[2,], English[2,], Competitive_Exam[2,], Eng_Science[2,], Repair[2,], Craftwork[2,])
colnames(cbl) <- c("IT", "English", "Competitive Exam", "Engineering/Science", "Technical/Repair", "Craftwork")

cbl <- rbind(cbl, Total = colSums(cbl))

cbl %>%
  kbl(caption = "Distribution of Interest in Skilling Courses", align = "l")%>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 11) %>%
  row_spec(4, bold = T)



#

