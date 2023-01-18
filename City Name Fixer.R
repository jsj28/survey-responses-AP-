library(data.table)
library(readxl)
library(writexl)


#Updated
AP_Youth_Survey <- read_excel("/Users/gurkirat/Downloads/Archive/youth_survey_responses (final 14th Jan).xlsx")
ap_outmigration_roster <- read_excel("/Users/gurkirat/Downloads/Archive/Outmigration Roster Youth Survey (Final 14th Jan).xlsx")
ap_household_roster  <- read_excel("/Users/gurkirat/Downloads/Archive/Household Roster Youth Survey (Final 14th Jan).xlsx")

#Add Codebook: youth questions
x <- colnames(AP_Youth_Survey)
y <- as.data.frame(sapply(AP_Youth_Survey, class))

y <- as.data.frame(t(y))

rownames(y) <- NULL

y <- cbind.data.frame(x,y); y <- y[,c(1:2)]

write_xlsx(y, "AP_Youth_Survey_Codebook.xlsx")

#household roster

h <- colnames(ap_household_roster)
hh <- as.data.frame(sapply(ap_household_roster, class))

hh <- as.data.frame(t(hh))

rownames(hh) <- NULL

hh <- cbind.data.frame(h,hh); hh <- hh[,c(1:2)]

write_xlsx(hh, "AP_household_roster codebook.xlsx")

#outmigration roster
o <- colnames(ap_outmigration_roster)
oo <- as.data.frame(sapply(ap_outmigration_roster, class))

oo <- as.data.frame(t(oo))

rownames(oo) <- NULL

oo <- cbind.data.frame(o,oo); oo <- oo[,c(1:2)]

write_xlsx(oo, "AP_outmigration_roster codebook.xlsx")

#Adding Colnames and labels from Codebook
AP_Youth_Survey_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx")
colnames(AP_Youth_Survey) <- AP_Youth_Survey_Codebook$Variable_Name
attr(AP_Youth_Survey, "variable.labels") <- AP_Youth_Survey_Codebook$Column_Name

AP_household_roster_codebook <- read_excel("/Users/gurkirat/Downloads/Archive/AP_household_roster codebook.xlsx")
colnames(ap_household_roster) <- AP_household_roster_codebook$Variable_Name
attr(ap_household_roster, "variable.labels") <- AP_household_roster_codebook$Column_Name

AP_outmigration_roster_codebook <- read_excel("/Users/gurkirat/Downloads/Archive/AP_outmigration_roster codebook.xlsx")
colnames(ap_outmigration_roster) <- AP_outmigration_roster_codebook$Variable_Name
attr(ap_outmigration_roster, "variable.labels") <- AP_outmigration_roster_codebook$Column_Name

#AP_Youth_Survey <- youth_survey_responses

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


v <- c("KURNOOL", "Kurnool", "kurnool")

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

v <- c("Adoni", "ADONI")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Adoni", AP_Youth_Survey$`City Name`)

v <- c("Rajahmundry", "Rajamhundry", "Rajamundry")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Rajahmundry", AP_Youth_Survey$`City Name`)

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` == "KADAPA",
                                      "Kadapa", AP_Youth_Survey$`City Name`)




##Analysis
as.data.frame(table(AP_Youth_Survey$`City Name`))
#Analysis
AP_Youth_Survey$`Ideally, which of the following type of work would you prefer?`


#Average Household size - 4
#Average remitances per household - median is 15000, mean is 1.06L
#Average Rent paid - 4150, rentals comprise 662 responses
#Average income demands - 17000

table(AP_Youth_Survey$`Do you pay rent to live in this house?`)

#NA Fixer Code Only if there are NAs manually written
for (i in 1:nrow(AP_Youth_Survey)) {
  
  for (j in 1:ncol(AP_Youth_Survey)) {
    
    AP_Youth_Survey[i,j] <- ifelse(AP_Youth_Survey[i,j] == "NA", NA, AP_Youth_Survey[i,j])
    
  }
  
}


#3 tables - students, employed, unemployed

#93 - Identifier
#Student Skilling Questions - 99-112*, No no responses for students
#Employed - No No Response *118, 120, 124, 126, 128, 131,
#Yes No Response - 119,121, 122, 123, 125, 127, 129, 130, 132, 133, 137, 138, 139

#Unemployed - No No Response - 155, 181 ending
#Yes No Response - 150,151,152,153, 154, 156,158, 160, 162, 163, 165, 166, 167, 168, 169, 170, 174, 
#175, 176, 

#Question wise non responses on skilling questions ####


vec <- c(119,121, 122, 123, 125, 127, 129, 130, 132, 133, 137, 138, 139, 150,151,152,153, 154, 156,158, 160, 162, 163, 165, 166, 167, 168, 169, 170, 174, 175, 176)

a <- cbind.data.frame(Col_No = as.numeric(vec), Var_Codes = colnames(AP_Youth_Survey)[vec], Q_Name = AP_Youth_Survey_Codebook$Column_Name[vec])

No_Resp <- Tot_Resp <- rep(NA, nrow(a))




for (i in 1:nrow(a)) {
  
  t <- as.data.frame(table(AP_Youth_Survey[,a$Col_No[i]] == "No Response"))
  No_Resp[i] <- sum(t$Freq, na.rm = T) - t$Freq[t$Var1 == F]
  Tot_Resp[i] <- sum(t$Freq, na.rm = T)
  
}


a <- cbind.data.frame(a, No_Resp, Tot_Resp)
a$Non_Resp_Rate = round(100*a$No_Resp/a$Tot_Resp, 2)

#Youth section non-responses - ####

vec <- c(182:201)

b <- cbind.data.frame(Col_No = as.numeric(vec), Var_Codes = colnames(AP_Youth_Survey)[vec], Q_Name = AP_Youth_Survey_Codebook$Column_Name[vec])

No_Resp <- Tot_Resp <- rep(NA, nrow(b))




for (i in 1:nrow(b)) {
  
  t <- as.data.frame(table(AP_Youth_Survey[,b$Col_No[i]] == "No Response"))
  No_Resp[i] <- sum(t$Freq, na.rm = T) - t$Freq[t$Var1 == F]
  Tot_Resp[i] <- sum(t$Freq, na.rm = T)
  
}


b <- cbind.data.frame(b, No_Resp, Tot_Resp)
b$Non_Resp_Rate = round(100*b$No_Resp/b$Tot_Resp, 2)

b <- b[b$Tot_Resp >= 1630,]


##Enumerator-wise non responses



v <- prop.table(table(AP_Youth_Survey$`Enumerator Code`, AP_Youth_Survey$`Andhra Pradesh now has many cash schemes to give money to its citizens, like Ammavodi and YSR Cheyutha. Do you believe the amount given in cash schemes should be more or less?` == "No Response"), 1)
v <- as.data.frame(v); v <- v[v$Var2 != FALSE,]; v[,3] <- round(100*v[,3], 2)

colnames(v)[3] <- "Cash_Support"



v <- prop.table(table(AP_Youth_Survey[,vec[1]], AP_Youth_Survey$`Enumerator Code`))





