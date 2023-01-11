library(data.table)


AP_Youth_Survey <- youth_survey_responses

v <- c("VIJAYAWADA", "VIJAYAWADa")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v, 
                                      "Vijayawada", AP_Youth_Survey$`City Name`)


v <- c("GVMC(VISAKHAPATNAM)", "Gvmc visakhapatnam", "GVMC (VISAKHAPATNAM)", "Gvmc Visakhapatnam",
  "GVMC visakhapatnam", "GVMC VISAKHAPATNAM", "GVMC VISHAKAPATNAM", "Gvmc(Visakhapatnam)",
  "GVMC(VISAKHAPATNAM)", "VISAKHAPATNAM")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Visakhapatnam", AP_Youth_Survey$`City Name`)

v <- c("Tirupathi", "tirupati", "Tirupati", "TIRUPATI")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Tirupati", AP_Youth_Survey$`City Name`)



v <- c("Tadipatri", "TADIPATRI", "TADIPARI", "Tadipari", "1007")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Tadipatri", AP_Youth_Survey$`City Name`)


v <- c("KURNOOL", "Kurnool")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Kurnool", AP_Youth_Survey$`City Name`)

v <- c("Vaddilapeta", "Kakinada", "KAKINADA", "Kakinda")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Kakinada", AP_Youth_Survey$`City Name`)


v <- c("NELLORE", "Nellore", "21031019")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Nellore", AP_Youth_Survey$`City Name`)


v <- c("Rayadurgh", "Rayadurg", "Rayadurgham")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Rayadurga", AP_Youth_Survey$`City Name`)


v <- c("KADIRI", "Kadiri", "\nKadiri", "1005")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Kadiri", AP_Youth_Survey$`City Name`)


v <- c("Hindupur", "Hindhupur", "Himdupur", "Hi")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Hindupur", AP_Youth_Survey$`City Name`)


v <- c("GUNTUR", "Guntur", "Nalandanagar", "1024")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Guntur", AP_Youth_Survey$`City Name`)

v <- c("ELURU", "Eluru")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Eluru", AP_Youth_Survey$`City Name`)


table(AP_Youth_Survey$`City Name`)
#Analysis
AP_Youth_Survey$`Ideally, which of the following type of work would you prefer?`


#Average Household size - 4
#Average remitances per household - median is 15000, mean is 1.06L
#Average Rent paid - 4150, rentals comprise 662 responses
#Average income demands - 17000

table(AP_Youth_Survey$`Do you pay rent to live in this house?`)

#3 tables - students, employed, unemployed

#93 - Identifier
#Student Skilling Questions - 99-112*, No no responses for students
#Employed - No No Response *118, 120, 124, 126, 128, 131,
#Yes No Response - 119,121, 122, 123, 125, 127, 129, 130, 132, 133, 137, 138, 139

#Unemployed - No No Response - 155, 181 ending
#Yes No Response - 150,151,152,153, 154, 156,158, 160, 162, 163, 165, 166, 167, 168, 169, 170, 174, 
#175, 176, 

#Question wise non responses####
vec <- c(119,121, 122, 123, 125, 127, 129, 130, 132, 133, 137, 138, 139, 150,151,152,153, 154, 156,158, 160, 162, 163, 165, 166, 167, 168, 169, 170, 174, 175, 176)

a <- cbind.data.frame(Col_No = vec, Q_Name = colnames(AP_Youth_Survey)[vec])

No_Resp <- Tot_Resp <- rep(NA, nrow(a))

for (i in 1:nrow(a)) {
  t <- as.data.frame(table(AP_Youth_Survey[,a$Col_No[i]] == "No Response"))
  No_Resp[i] <- sum(t$Freq, na.rm = T) - t$Freq[t$Var1 == F]
  Tot_Resp[i] <- sum(t$Freq, na.rm = T)
}


a <- cbind.data.frame(a, No_Resp, Tot_Resp)
a$Non_Resp_Rate = round(100*a$No_Resp/a$Tot_Resp, 2)




##Enumerator-wise non responses

v <- prop.table(table(AP_Youth_Survey$`Enumerator Code`, AP_Youth_Survey$`Andhra Pradesh now has many cash schemes to give money to its citizens, like Ammavodi and YSR Cheyutha. Do you believe the amount given in cash schemes should be more or less?` == "No Response"), 1)
v <- as.data.frame(v); v <- v[v$Var2 != FALSE,]; v[,3] <- round(100*v[,3], 2)

colnames(v)[3] <- "Cash_Support"



v <- prop.table(table(AP_Youth_Survey[,vec[1]], AP_Youth_Survey$`Enumerator Code`))
