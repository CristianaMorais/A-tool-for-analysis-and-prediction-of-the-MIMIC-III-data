## app.R ##
library(DBI)
library(bigrquery)
library(readr)
library(shiny)
library(dplyr)
library(shinyBS)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(eeptools)
library(tibble)
library(DT)
library(ggplot2)
library(data.table)
#install.packages("mice")
library(mice)
#install.packages("gtsummary")
library(gtsummary)
library(gt)
library(caret)
#install.packages("caTools")
library(caTools)
#install.packages("heatmaply")
library(heatmaply)
library(ggcorrplot)
library(xgboost)
library(randomForest)
library(glmnet)
library(pROC)

bq_auth(email = "up201505454@g.uporto.pt")

# Create the BigQuery connection
con <- dbConnect(
  bigrquery::bigquery(),
  project = "physionet-data",
  dataset = "mimiciii_clinical",
  billing = "clinical-entity-extraction"
)

# WINDOWS
#setwd("~/GitHub/MIMIC-III")
setwd("~/Github/A-tool-for-analysis-and-prediction-of-the-MIMIC-III-data")
source("~/Github/A-tool-for-analysis-and-prediction-of-the-MIMIC-III-data/src/sepsis3.R")
ADMISSIONS <- "SELECT * FROM `physionet-data.mimiciii_clinical.admissions`"
ICUSTAYS <- "SELECT * FROM `physionet-data.mimiciii_clinical.icustays`"
PATIENTS <- "SELECT * FROM `physionet-data.mimiciii_clinical.patients`"
dfmerge <- read.csv("~/GitHub/MIMIC-III/dfmerge.csv")
DIAGNOSES_ICD <- "SELECT * FROM `physionet-data.mimiciii_clinical.diagnoses_icd`"
D_ICD_DIAGNOSES <- "SELECT * FROM `physionet-data.mimiciii_clinical.d_icd_diagnoses`"
# INPUTEVENTS_CV <- fread("~/GitHub/MIMIC-III/INPUTEVENTS_CV.csv", data.table=FALSE)

ADMISSIONS <- dbGetQuery(con, ADMISSIONS)
ICUSTAYS <- dbGetQuery(con, ICUSTAYS)
PATIENTS <- dbGetQuery(con, PATIENTS)
D_ICD_DIAGNOSES <- dbGetQuery(con, D_ICD_DIAGNOSES)

#LINUX

# setwd("~/Documents/Github/MIMIC-III")
# ADMISSIONS <- read_csv("~/Documents/Github/MIMIC-III/ADMISSIONS.csv")
# ICUSTAYS <- read_csv("~/Documents/Github/MIMIC-III/ICUSTAYS.csv")
# PATIENTS <- read_csv("~/Documents/Github/MIMIC-III/PATIENTS.csv")
# dfmerge <- read_csv("~/Documents/Github/MIMIC-III/dfmerge.csv")
# DIAGNOSES_ICD <- read_csv("~/Documents/Github/MIMIC-III/DIAGNOSES_ICD.csv")

DIAGNOSES_ICD <- "SELECT
d_icd.ICD9_CODE AS ICD9_CODE,
d_icd.SHORT_TITLE AS SHORT_TITLE,
d_icd.LONG_TITLE AS LONG_TITLE,
diagnoses_icd.*
  FROM
`physionet-data.mimiciii_clinical.d_icd_diagnoses` AS d_icd
LEFT JOIN
`physionet-data.mimiciii_clinical.diagnoses_icd` AS diagnoses_icd
ON
d_icd.ICD9_CODE = diagnoses_icd.ICD9_CODE"
DIAGNOSES_ICD <- dbGetQuery(con, DIAGNOSES_ICD)

################################## Predictions

sepsis3_basic <- sepsis3
columns_to_remove <- c("excluded", "suspected_infection_time_poe")
sepsis3_basic <- sepsis3[, !(names(sepsis3) %in% columns_to_remove)]


# Impute the mean
imputar_media <- function(sepsis3_basic) {
  for (coluna in names(sepsis3_basic)) {
    if (is.numeric(sepsis3_basic[[coluna]])) {  # Verifica se a coluna é numérica
      media_coluna <- mean(sepsis3_basic[[coluna]], na.rm = TRUE)  # Calcula a média excluindo valores ausentes
      sepsis3_basic[[coluna]][is.na(sepsis3_basic[[coluna]])] <- media_coluna  # Imputa a média
    }
  }
  return(sepsis3_basic)
}


dados_imputados <- imputar_media(sepsis3_basic)
sepsis3_basic <- dados_imputados
#summary(sepsis3_basic)

sepsis_predf <- sepsis3_basic

stats <- sepsis_predf
stats <- stats[, !names(stats) %in% c('icustay_id', 'hadm_id', 'ethnicity', 'gender')]
selectF <- stats[, !names(stats) %in% c('HOSPITAL_EXPIRE_FLAG', 'THIRTYDAY_EXPIRE_FLAG')]
##########################################################################################

colnames(dfmerge)[colnames(dfmerge) == 'SUBJECT_ID.x'] <- 'SUBJECT_ID'
dfmerge <- dfmerge[-c(29,28)]

colnames(dfmerge)[2] <- "ROW_ID"

n_adm <- n_distinct(ADMISSIONS$SUBJECT_ID)

n_hospitalizacoes <- length(ADMISSIONS$SUBJECT_ID)


n_distdoentes <- length(PATIENTS$SUBJECT_ID) # PODEMOS OBTER TAMBEM CONTANDO ROWS OF PATIENTS


n_icudist <- n_distinct(ICUSTAYS$HADM_ID)

#GENDER
males <- sum(PATIENTS$GENDER == 'M')
females<- sum(PATIENTS$GENDER == 'F')

percMales <- males/length(PATIENTS$GENDER)
percMales <- round( percMales, 3) *100

#numero de mortes 
n_deaths <- sum( is.na(PATIENTS$DOD))

PATIENTS$DOB <- gsub(PATIENTS$DOB,pattern=" 00:00:00",replacement="",fixed=T)
PATIENTS$DOD <- gsub(PATIENTS$DOD,pattern=" 00:00:00",replacement="",fixed=T)
PATIENTS$DOD_HOSP <- gsub(PATIENTS$DOD_HOSP,pattern=" 00:00:00",replacement="",fixed=T)
PATIENTS$DOD_SSN <- gsub(PATIENTS$DOD_SSN,pattern=" 00:00:00",replacement="",fixed=T)


ADMISSIONS_unique <- ADMISSIONS[!rev(duplicated(rev(ADMISSIONS$SUBJECT_ID))),] ## remover rows com duplicados baseado no id
df = merge(x=ADMISSIONS_unique,y=PATIENTS,by="SUBJECT_ID")
#View(df)

df$DOB <- as.Date(df$DOB)
df$ADMITTIME <- as.Date(df$ADMITTIME)
df$EXPIRE_FLAG <- as.factor(df$EXPIRE_FLAG)

#df <- df %>% distinct(SUBJECT_ID, .keep_all =  TRUE)
#df$age <- age_calc(df$DOB, df$ADMITTIME, units = "years",precise = FALSE)

# Assuming DOB and ADMITTIME columns are already in Date format
df$age <- as.integer(difftime(df$ADMITTIME, df$DOB, units = "days") / 365.25)

# Correct for cases where the admission date hasn't occurred yet in the year of birth
df$age[df$ADMITTIME < as.Date(paste0(year(df$DOB), "-01-01"))] <- NA

# df <- subset(df, select = -c(ROW_ID.x, ROW_ID.y))

#gender <- data.frame(unclass(table(dfmerge$GENDER)))
#n_males <-gender[2,] / sum(gender) 


dfmerge$ADMITTIME <- as.Date(dfmerge$ADMITTIME)
dfmerge$DISCHTIME <- as.Date(dfmerge$DISCHTIME)
dfmerge$DEATHTIME <- as.Date(dfmerge$DEATHTIME)
dfmerge$DEATHTIME <- as.Date(dfmerge$DEATHTIME)
dfmerge$ADMISSION_TYPE <- as.factor(dfmerge$ADMISSION_TYPE)
dfmerge$ADMISSION_LOCATION <- as.factor(dfmerge$ADMISSION_LOCATION)
dfmerge$INSURANCE <- as.factor(dfmerge$INSURANCE)
dfmerge$RELIGION <- as.factor(dfmerge$RELIGION)
dfmerge$MARITAL_STATUS <- as.factor(dfmerge$MARITAL_STATUS)
dfmerge$ETHNICITY <- as.factor(dfmerge$ETHNICITY)
dfmerge$EDREGTIME <- as.Date(dfmerge$EDREGTIME)
dfmerge$EDOUTTIME <- as.Date(dfmerge$EDOUTTIME)
dfmerge$DIAGNOSIS <- as.factor(dfmerge$DIAGNOSIS)
dfmerge$DOB <- as.Date(dfmerge$DOB)
dfmerge$DOD <- as.Date(dfmerge$DOD)
dfmerge$DOD_HOSP <- as.Date(dfmerge$DOD_HOSP)
dfmerge$DOD_SSN <- as.Date(dfmerge$DOD_SSN)
dfmerge$LANGUAGE <- as.factor(dfmerge$LANGUAGE)


ADMISSIONS$DIAGNOSIS <- as.factor(ADMISSIONS$DIAGNOSIS)

admission_temp <- data.frame(unclass(summary(as.factor(ADMISSIONS$ADMISSION_TYPE))), check.names = FALSE, stringsAsFactors = TRUE)
colnames(admission_temp) <- c("Admission type frequency")
admission_loc <- data.frame(unclass(summary(as.factor(ADMISSIONS$ADMISSION_LOCATION))), check.names = FALSE, stringsAsFactors = TRUE)
colnames(admission_loc) <- c("Admission location frequency")
discharge_loc <- data.frame(unclass(summary(as.factor(ADMISSIONS$DISCHARGE_LOCATION))), check.names = FALSE, stringsAsFactors = TRUE)
colnames(discharge_loc) <- c("Discharge location frequency")
insurance_temp <- data.frame(unclass(summary(as.factor(ADMISSIONS$INSURANCE))), check.names = FALSE, stringsAsFactors = TRUE)
colnames(insurance_temp) <- c("Insurance frequency")
diagnosis_temp <- as.data.frame(unclass(summary(as.factor(ADMISSIONS$DIAGNOSIS))), check.names = FALSE, stringsAsFactors = TRUE)
diagnosis_temp <- tibble::rownames_to_column(diagnosis_temp, "Diagnose")
colnames(diagnosis_temp) <- c("Diagnose","D")

# data <- data.frame(ADMISSIONS$DIAGNOSIS)
# data$ADMISSIONS.DIAGNOSIS <- factor(data$ADMISSIONS.DIAGNOSIS, levels = unique(data$ADMISSIONS.DIAGNOSIS)[order(data$Count, decreasing = TRUE)])



dfmerge$X <- NULL
dfmerge$ROW_ID.x <- NULL
dfmerge$...1 <- NULL
dfmerge$ROW_ID.y <- NULL


xpto <- unique(as.character(dfmerge$GENDER))
xpto <- c("ALL",xpto)

xpto2 <- unique(as.character(dfmerge$ETHNICITY))
xpto2 <- c("ALL",xpto2)

xpto3 <- unique( as.character(dfmerge$RELIGION))
xpto3 <- c("ALL", xpto3)

xpto4 <- unique( as.character(dfmerge$INSURANCE))
xpto4 <- c("ALL", xpto4)

xpto5 <- unique( as.character(dfmerge$ADMISSION_TYPE))
xpto5 <- c("ALL", xpto5)

xpto6 <- unique( as.character(dfmerge$ADMISSION_LOCATION))
xpto6 <- c("ALL", xpto6)

xpto7 <- unique( as.character(dfmerge$DISCHARGE_LOCATION))
xpto7 <- c("ALL", xpto7)

xpto8 <- unique( as.character(dfmerge$HOSPITAL_EXPIRE_FLAG))
xpto8 <- c("ALL", xpto8)

xpto9 <- unique( as.character(dfmerge$DIAGNOSIS))
xpto9 <- c("ALL", xpto9)

xpto10 <- unique( as.character(dfmerge$EXPIRE_FLAG))
xpto10 <- c("ALL", xpto10)

xpto11 <- unique( as.character(ICUSTAYS$FIRST_CAREUNIT))
xpto11 <- c("ALL", xpto11)

xpto12 <- unique( as.character(ICUSTAYS$LAST_CAREUNIT))
xpto12 <- c("ALL", xpto12)

xpto13 <- unique( as.character(ICUSTAYS$FIRST_WARDID))
xpto13 <- c("ALL", xpto13)

xpto14 <- unique( as.character(ICUSTAYS$LAST_WARDID))
xpto14 <- c("ALL", xpto14)


#dfmerge$len_stay <- age_calc(dfmerge$ADMITTIME, dfmerge$DISCHTIME, units = "days",precise = FALSE)
#dfmerge$len_stay <- gsub(dfmerge$len_stay,pattern=" days",replacement="",fixed=T)
#dfmerge$len_stay <- as.numeric(dfmerge$len_stay)

#mortalitysum(!is.na(dfmerge$DOD))

tempage <- filter(df, age <300 | age>= 0 )
average_age <- mean(tempage$age)

dfmerge$GENDER <- as.factor(dfmerge$GENDER)
dfmerge$DIAGNOSIS <- as.factor(dfmerge$DIAGNOSIS)


#### DIAGNOSES ####
v <- DIAGNOSES_ICD  %>% filter(str_detect(ICD9_CODE, "^V"))
e <- DIAGNOSES_ICD  %>% filter(str_detect(ICD9_CODE, "^E"))
na <- sum(is.na(DIAGNOSES_ICD$ICD9_CODE))
infections139 <-filter(DIAGNOSES_ICD, ICD9_CODE <= 1398)
neoplasms239 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 1400 , ICD9_CODE <= 2399)
endocrine279 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 2400 , ICD9_CODE <= 2799)
blood289 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 2800 , ICD9_CODE <= 2899)
mental319 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 2900 , ICD9_CODE <= 319)
nervous389 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 3200 , ICD9_CODE <= 3899)
circulatory459 <-filter(DIAGNOSES_ICD, ICD9_CODE >= 3900 , ICD9_CODE <= 4599)
respiratory519 <-filter(DIAGNOSES_ICD, ICD9_CODE >= 460 , ICD9_CODE <= 5199)
digestive579 <-filter(DIAGNOSES_ICD, ICD9_CODE >= 5200 , ICD9_CODE <= 5799)
genitourinary629 <-filter(DIAGNOSES_ICD, ICD9_CODE >= 5800 , ICD9_CODE <= 6299)
pregnancy679 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 6300 , ICD9_CODE <= 67914)
skin709 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 6800 , ICD9_CODE <= 7099)
muscle739 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 7100 , ICD9_CODE <= 7399)
congenital759 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 7400 , ICD9_CODE <= 7599)
perinatal779 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 7600 , ICD9_CODE <= 7799)
symptoms799 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 7800 , ICD9_CODE <= 7999)
injury999 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 8000 , ICD9_CODE <= 9999)

ICD9CODE <- c("0-139", "140-239", "240-279", "280-289", "290-319", "320-389",
              "390-459", "460-519", "520-579","580-629","630-679","680-709","710-739",
              "740-759","760-779", "780-799", "800-999","V01-V091","E000-E999"
)



Frequency <- c(nrow(infections139), nrow(neoplasms239), nrow(endocrine279), nrow(blood289), nrow(mental319),
               nrow(nervous389), nrow(circulatory459), nrow(respiratory519), nrow(digestive579), nrow(genitourinary629),
               nrow(pregnancy679), nrow(skin709), nrow(muscle739), nrow(congenital759), nrow(perinatal779), nrow(symptoms799),
               nrow(injury999), nrow(v), nrow(e))

a <- c("0-139", "140-239", "240-279", "280-289", "290-319", "320-389",
       "390-459", "460-519", "520-579","580-629","630-679","680-709","710-739",
       "740-759","760-779", "780-799", "800-999","V01-V091","E000-E999"
)

Frequency2 <- c(nrow(infections139_2), nrow(neoplasms239_2), nrow(endocrine279_2), nrow(blood289_2), nrow(mental319_2),
                nrow(nervous389_2), nrow(circulatory459_2), nrow(respiratory519_2), nrow(digestive579_2), nrow(genitourinary629_2),
                nrow(pregnancy679_2), nrow(skin709_2), nrow(muscle739_2), nrow(congenital759_2), nrow(perinatal779_2), nrow(symptoms799_2),
                nrow(injury999_2), nrow(v_2), nrow(e_2)
)



diagnosesPlot <- data.frame(ICD9CODE, Frequency)



diagnosesPlot2 <- data.frame(a, Frequency2)
diagnosesPlot<- diagnosesPlot[order(diagnosesPlot$ICD9CODE),]
diagnosesPlot2<- diagnosesPlot2[order(diagnosesPlot2$a),]
diagnosesPlot$name <- name
diagnosesPlot2$name <- name



name <- c(
  "INFECTIOUS AND PARASITIC DISEASES (001-139)" ,
  "NEOPLASMS (140-239)" ,
  "ENDOCRINE, NUTRITIONAL AND METABOLIC DISEASES, AND IMMUNITY DISORDERS (240-279)",
  "DISEASES OF THE BLOOD AND BLOOD-FORMING ORGANS (280-289)",
  "MENTAL DISORDERS (290-319)" ,
  "DISEASES OF THE NERVOUS SYSTEM AND SENSE ORGANS (320-389)" ,
  "DISEASES OF THE CIRCULATORY SYSTEM (390-459)" ,
  "DISEASES OF THE RESPIRATORY SYSTEM (460-519)" ,
  "DISEASES OF THE DIGESTIVE SYSTEM (520-579)" ,
  "DISEASES OF THE GENITOURINARY SYSTEM (580-629)" ,
  "COMPLICATIONS OF PREGNANCY, CHILDBIRTH, AND THE PUERPERIUM (630-679)",
  "DISEASES OF THE SKIN AND SUBCUTANEOUS TISSUE (680-709)",
  "DISEASES OF THE MUSCULOSKELETAL SYSTEM AND CONNECTIVE TISSUE (710-739)",
  "CONGENITAL ANOMALIES (740-759)" ,
  "CERTAIN CONDITIONS ORIGINATING IN THE PERINATAL PERIOD (760-779)" ,
  "SYMPTOMS, SIGNS, AND ILL-DEFINED CONDITIONS (780-799)" ,
  "INJURY AND POISONING (800-999)" ,
  "SUPPLEMENTARY CLASSIFICATION OF FACTORS INFLUENCING HEALTH STATUS AND CONTACT WITH HEALTH SERVICES (V01-V89)" ,
  "SUPPLEMENTARY CLASSIFICATION OF EXTERNAL CAUSES OF INJURY AND POISONING (E800-E999)"
  
  
) 


# print (df)

#dfmerge2 <- left_join(DIAGNOSES_ICD, ICUSTAYS ,by= c("SUBJECT_ID","HADM_ID")) ## VER ISTO TENHO MUITAS DUVIDAS AQUI
#acho que nao posso dar merge a isto pq o icustays ? unico, ver caso do subject_id = 7
# dfmerge <- subset(dfmerge, select = -c(SUBJECT_ID.y))
#dfmerge2 <- merge(dfmerge2, dfmerge[,c("age","SUBJECT_ID")], by = "SUBJECT_ID", all = TRUE)

dfmerge2 <- left_join(DIAGNOSES_ICD, df[,c("GENDER","INSURANCE","age","SUBJECT_ID")], by = "SUBJECT_ID")
clean2 <- aggregate(LOS~HADM_ID+SUBJECT_ID, data=ICUSTAYS, FUN=sum)
dfmerge2 <- left_join(dfmerge2 , clean2, by = c("SUBJECT_ID", "HADM_ID"))
# dfmerg3 <- merge(dfmerge,DIAGNOSES_ICD,by="SUBJECT_ID")





# dfmerge2 <- dfmerge2[ -c(2,4,5,6,11,15,16,18,19,21,22,23,24,28,29,30) ]
# colnames(dfmerge2)[colnames(dfmerge2) == 'HADM_ID.x'] <- 'HADM_ID'



dfmerge2$GENDER <- as.factor(dfmerge2$GENDER)
dfmerge2$INSURANCE <- as.factor(dfmerge2$INSURANCE)
dfmerge2$SHORT_TITLE <- as.factor(dfmerge2$SHORT_TITLE)
dfmerge2$LONG_TITLE <- as.factor(dfmerge2$LONG_TITLE)
dfmerge2 <- filter(dfmerge2, LOS < 176 )

temp1 <-filter(dfmerge2, ICD9_CODE <= 1398)
temp2 <- filter(dfmerge2, ICD9_CODE >= 1400 , ICD9_CODE <= 2399)
temp3 <- filter(dfmerge2, ICD9_CODE >= 2400 , ICD9_CODE <= 2799)
temp4 <- filter(dfmerge2, ICD9_CODE >= 2800 , ICD9_CODE <= 2899)
temp5 <- filter(dfmerge2, ICD9_CODE >= 2900 , ICD9_CODE <= 319)
temp6 <- filter(dfmerge2, ICD9_CODE >= 3200 , ICD9_CODE <= 3899)
temp7 <-filter(dfmerge2, ICD9_CODE >= 3900 , ICD9_CODE <= 4599)
temp8 <-filter(dfmerge2, ICD9_CODE >= 460 , ICD9_CODE <= 5199)
temp9 <-filter(dfmerge2, ICD9_CODE >= 5200 , ICD9_CODE <= 5799)
temp10 <-filter(dfmerge2, ICD9_CODE >= 5800 , ICD9_CODE <= 6299)
temp11 <- filter(dfmerge2, ICD9_CODE >= 6300 , ICD9_CODE <= 67914)
temp12 <- filter(dfmerge2, ICD9_CODE >= 6800 , ICD9_CODE <= 7099)
temp13 <- filter(dfmerge2, ICD9_CODE >= 7100 , ICD9_CODE <= 7399)
temp14 <- filter(dfmerge2, ICD9_CODE >= 7400 , ICD9_CODE <= 7599)
temp15 <- filter(dfmerge2, ICD9_CODE >= 7600 , ICD9_CODE <= 7799)
temp16 <- filter(dfmerge2, ICD9_CODE >= 7800 , ICD9_CODE <= 7999)
temp17 <- filter(dfmerge2, ICD9_CODE >= 8000 , ICD9_CODE <= 9999)
temp18<- DIAGNOSES_ICD  %>% filter(str_detect(ICD9_CODE, "^V"))
temp19 <- DIAGNOSES_ICD  %>% filter(str_detect(ICD9_CODE, "^E"))

temp18 <- merge(temp18,ICUSTAYS, by = "ROW_ID", all.x  =TRUE) #LEFT OUTER JOIN
temp19 <- merge(temp19,ICUSTAYS, by = "ROW_ID", all.x  =TRUE) #LEFT OUTER JOIN

firstseq_num <- filter(DIAGNOSES_ICD, SEQ_NUM == "1")

infections139_2 <-filter(firstseq_num, ICD9_CODE <= 1398)
neoplasms239_2 <- filter(firstseq_num, ICD9_CODE >= 1400 , ICD9_CODE <= 2399)
endocrine279_2 <- filter(firstseq_num, ICD9_CODE >= 2400 , ICD9_CODE <= 2799)
blood289_2 <- filter(firstseq_num, ICD9_CODE >= 2800 , ICD9_CODE <= 2899)
mental319_2 <- filter(firstseq_num, ICD9_CODE >= 2900 , ICD9_CODE <= 319)
nervous389_2 <- filter(firstseq_num, ICD9_CODE >= 3200 , ICD9_CODE <= 3899)
circulatory459_2 <-filter(firstseq_num, ICD9_CODE >= 3900 , ICD9_CODE <= 4599)
respiratory519_2 <-filter(firstseq_num, ICD9_CODE >= 460 , ICD9_CODE <= 5199)
digestive579_2 <-filter(firstseq_num, ICD9_CODE >= 5200 , ICD9_CODE <= 5799)
genitourinary629_2 <-filter(firstseq_num, ICD9_CODE >= 5800 , ICD9_CODE <= 6299)
pregnancy679_2 <- filter(firstseq_num, ICD9_CODE >= 6300 , ICD9_CODE <= 67914)
skin709_2 <- filter(firstseq_num, ICD9_CODE >= 6800 , ICD9_CODE <= 7099)
muscle739_2 <- filter(firstseq_num, ICD9_CODE >= 7100 , ICD9_CODE <= 7399)
congenital759_2 <- filter(firstseq_num, ICD9_CODE >= 7400 , ICD9_CODE <= 7599)
perinatal779_2 <- filter(firstseq_num, ICD9_CODE >= 7600 , ICD9_CODE <= 7799)
symptoms799_2 <- filter(firstseq_num, ICD9_CODE >= 7800 , ICD9_CODE <= 7999)
injury999_2 <- filter(firstseq_num, ICD9_CODE >= 8000 , ICD9_CODE <= 9999)
v_2 <- firstseq_num  %>% filter(str_detect(ICD9_CODE, "^V"))
e_2 <- firstseq_num %>% filter(str_detect(ICD9_CODE, "^E"))


x1 <- count(filter(ADMISSIONS_unique, HOSPITAL_EXPIRE_FLAG == "1"))/46520

# t.first <- ADMISSIONS[match(unique(ADMISSIONS$SUBJECT_ID), ADMISSIONS$SUBJECT_ID),]

# t.first2 <- dfmerge[match(unique(dfmerge$ROW_ID), dfmerge$ROW_ID),]

# dfmerge_tfirst_patientes <- merge(PATIENTS, t.first, by = "SUBJECT_ID")
# 
# dfmerge_tfirst_patientes <- subset(dfmerge_tfirst_patientes, select = -c(DOD_SSN, DOD_HOSP,ROW_ID.x ,ROW_ID.y, EXPIRE_FLAG, DEATHTIME, HOSPITAL_EXPIRE_FLAG, HAS_CHARTEVENTS_DATA))

dfmerge3 <- subset(df, select = -c(ROW_ID.x, ROW_ID.y,HAS_CHARTEVENTS_DATA))


dfmerge3 <- dfmerge3 %>% mutate(across(c(ADMISSION_TYPE, DISCHARGE_LOCATION,ADMISSION_LOCATION, INSURANCE, LANGUAGE, RELIGION, MARITAL_STATUS, ETHNICITY, EDREGTIME, EDOUTTIME, DIAGNOSIS, GENDER, DOB, DOD, DOD_HOSP, DOD_SSN, HOSPITAL_EXPIRE_FLAG ,ADMITTIME, DISCHTIME, DEATHTIME), as.factor))

dfmerge3 <- dfmerge3 %>% relocate(age, .before=HADM_ID)
dfmerge3 <- dfmerge3 %>% relocate(GENDER, .before = HADM_ID)
dfmerge3 <- dfmerge3 %>% relocate(ETHNICITY, .before = HADM_ID)

# diagnoses_with_description <- merge( DIAGNOSES_ICD, D_ICD_DIAGNOSES , by = "ICD9_CODE", all.x = TRUE)

# diagnoses_with_description <- subset(diagnoses_with_description, select = -c(ROW_ID.y, ROW_ID.x))

clean <- subset(ICUSTAYS, select = -c(1,2,4,5,6,7,8,9,10,11))
clean <- aggregate(LOS~HADM_ID, data=clean, FUN=sum) # soma os LOS dos HADM_ID iguais

diagnoses_with_LOS <- merge(DIAGNOSES_ICD, clean, by = "HADM_ID", all.x = TRUE)

firstseq_compare <- left_join(firstseq_num, ADMISSIONS, by = "HADM_ID")
firstseq_compare <- subset(firstseq_compare, select = -c(1,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25))
colnames(firstseq_compare)[1] <- "SUBJECT_ID"

# 
# INPUTEVENTS_CV <- INPUTEVENTS_CV[order(INPUTEVENTS_CV$SUBJECT_ID),] 
# gato1 <- INPUTEVENTS_CV[1:3000000,]
# write.csv(gato1,"gato1.csv")
# 
# gato2 <- INPUTEVENTS_CV[3000001:6000000,]
# write.csv(gato2,"gato2.csv")
# 
# gato3 <- INPUTEVENTS_CV[6000001:10000000,]
# write.csv(gato1,"gato3.csv")
# 
# gato4 <- INPUTEVENTS_CV[10000001:13000000,]
# write.csv(gato1,"gato4.csv")
# 
# gato5 <- INPUTEVENTS_CV[13000001:17527935,]
# write.csv(gato1,"gato5.csv")

dfmerge2$SHORT_TITLE <- as.factor(dfmerge2$SHORT_TITLE)
dfmerge2$LONG_TITLE <- as.factor(dfmerge2$LONG_TITLE)

bug_solver <- summary(temp1)


ICUcomplete <- left_join(ICUSTAYS, df[,c("age","SUBJECT_ID", "GENDER","ETHNICITY","EXPIRE_FLAG","HOSPITAL_EXPIRE_FLAG")], 
                  by = "SUBJECT_ID")

ICUcomplete <- subset(ICUcomplete, select= -c(1))

ICUcomplete$GENDER <- as.factor(ICUcomplete$GENDER)
ICUcomplete$ETHNICITY   <- as.factor(ICUcomplete$ETHNICITY)
ICUcomplete$FIRST_CAREUNIT   <- as.factor(ICUcomplete$FIRST_CAREUNIT)
ICUcomplete$LAST_CAREUNIT   <- as.factor(ICUcomplete$LAST_CAREUNIT)
ICUcomplete$DBSOURCE   <- as.factor(ICUcomplete$DBSOURCE)
ICUcomplete$INTIME   <- as.Date(ICUcomplete$INTIME)
ICUcomplete$OUTTIME   <- as.Date(ICUcomplete$OUTTIME)



most_freq_icd <- count(DIAGNOSES_ICD, ICD9_CODE)
most_freq_icd <- most_freq_icd[order(-most_freq_icd$n),]
most_freq_icd <- head(most_freq_icd,20)
most_freq_icd <- left_join(most_freq_icd, D_ICD_DIAGNOSES, by = "ICD9_CODE")

days_before_death <- filter(df, EXPIRE_FLAG == "1"  & HOSPITAL_EXPIRE_FLAG == "0") #para casos onde hospital expire flag esta mal. NOP ANULADO
days_before_death <-subset(days_before_death, select = -c(2,20))
days_before_death$days<- difftime( days_before_death$DOD, days_before_death$DISCHTIME , units = c("days"))
days_before_death$days <- as.numeric(days_before_death$days)

days_before_death <- days_before_death %>% relocate(days, .before=HADM_ID)
days_before_death$ADMISSION_TYPE <- as.factor(days_before_death$ADMISSION_TYPE)
days_before_death$ADMISSION_LOCATION <- as.factor(days_before_death$ADMISSION_LOCATION)
days_before_death$DISCHARGE_LOCATION <- as.factor(days_before_death$DISCHARGE_LOCATION)
days_before_death$INSURANCE <- as.factor(days_before_death$INSURANCE)
days_before_death$LANGUAGE<- as.factor(days_before_death$LANGUAGE)
days_before_death$RELIGION <- as.factor(days_before_death$RELIGION)
days_before_death$MARITAL_STATUS <- as.factor(days_before_death$MARITAL_STATUS)
days_before_death$ETHNICITY <- as.factor(days_before_death$ETHNICITY)
days_before_death$DIAGNOSIS <- as.factor(days_before_death$DIAGNOSIS)
days_before_death$GENDER <- as.factor(days_before_death$GENDER)
days_before_death$DOD<- as.Date(days_before_death$DOD)
days_before_death$DOD_HOSP<- as.Date(days_before_death$DOD_HOSP)
days_before_death$DOD_SSN<- as.Date(days_before_death$DOD_SSN)
days_before_death$EDREGTIME<- as.Date(days_before_death$EDREGTIME)
days_before_death$EDOUTTIME<- as.Date(days_before_death$EDOUTTIME)
# days_before_death$SHORT_TITLE<- as.Date(days_before_death$SHORT_TITLE)
# days_before_death$LONG_TITLE<- as.Date(days_before_death$LONG_TITLE)

days_before_death <- left_join(days_before_death, firstseq_num[,c("ICD9_CODE","SHORT_TITLE","LONG_TITLE","HADM_ID")], by = "HADM_ID")





header <- dashboardHeader(title="MIMIC-III",
                          tags$li(
                            class = "dropdown",
                            style = "float: right;",  # Float to the right
                            actionButton("info_button", label = icon("info-circle"), 
                                         style = "font-size: 27px; color: #0047AB; background-color: transparent; border: none;"),
                            dropdownMenuOutput("info_menu")
                          )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "home", icon = icon("home"),
             badgeLabel = "Home", badgeColor = "green"),
    # menuItem("Pesquisa", icon = icon("search"), tabName = "search",
    #          badgeLabel = "beta", badgeColor = "green"),
    menuItem("Patients", tabName = "patientss", icon = icon("hospital-user"),
             startExpanded = FALSE,
             menuSubItem(" General Patient Info",
                         tabName = "patients"),
             menuSubItem(" Patient Search by ID",
                         tabName = "patients3")
             # menuSubItem("Especific Patient Search",
             #             tabName = "patients2")
             
    ),
    
    menuItem("Admissions", tabName = "admissions", icon = icon("book-medical"),
             startExpanded = FALSE,
             menuSubItem("General Admissions info",
                         tabName = "admissions2"),
             
             menuSubItem("Search by admission ID",
                         tabName = "admissions1")
             
    ),
    
    menuItem("Diagnoses", tabName = "diagnoses", icon = icon("stethoscope"),
             startExpanded = FALSE,
             menuSubItem("General diagnoses info",
                         tabName = "diagnoses1"),
             menuSubItem("Specific ICD9 Group details",
                         tabName = "diagnoses2"),
             menuSubItem("Diagnoses search by ICD",
                         tabName = "diagnoses5"),
             menuSubItem("First diagnoses - comparison",
                         tabName = "diagnoses3"),
             menuSubItem("Diagnoses search by patient ID",
                         tabName = "diagnoses4")
    ),
    
    menuItem("ICU", tabName = "icu", icon = icon("bed"),
             startExpanded = FALSE,
             menuSubItem(" General ICU stays Info",
                         tabName = "icu1"),
             menuSubItem(" Patient Mortality",
                         tabName = "icu2")


    ),
    
    menuItem("Predictions", tabName = "predictions", icon = icon("magnifying-glass-chart"))
  )
)

body <- dashboardBody(
  # Define the modal dialog box content as before
  tags$div(
    bsModal("info_modal", "Information about the Tool", "info_button", size = "large",
            selectInput("info_select", "Select a menu for more information",
                        choices = c("", "Dashboard", "Patients", "Admissions", "Diagnoses", "ICU", "Predictions")),
            uiOutput("sub_option_ui"),
            uiOutput("info_text_ui")),
  ),
  tabItems(
    tabItem(tabName = "home",
            h4("Welcome to the home page"),
            fluidRow(
              box(
                title = "Basic MIMIC-III statistics between 2001-2012", width = 7, solidHeader = TRUE,
                status = "warning",
                #colocar dia e mes
                #cat("OLA"),
                HTML('<b> Number of distinct ICU stays:</b>' ),print(n_icudist), HTML('</br>'),
                
                
                HTML('<b> Number of hospital admissions:</b>'), print(n_hospitalizacoes), HTML( '</br>'),
                HTML('<b> Number of distinct patients:</b>' ), print(n_distdoentes), HTML( '</br>'),
                HTML('<b> Gender, Male %:</b>'), print(percMales), HTML('</br>'),
                HTML('<b> Average age, years:</b> '),print( round(average_age,3) ), HTML('</br>'),
                HTML('<b> ICU length of stay, average days:</b>'), print( round(mean(ICUSTAYS$LOS, na.rm =  TRUE),3) ), HTML('</br>'),
                #HTML('<b> Hospital length of stay, average days:</b> 6.9 <br>'),
                #HTML('<b> ICU Mortality, %:</b> 8.5 <br>'),
                HTML('<b> Hospital mortality,% :</b>'), print(round(x1*100,3)), HTML('</br>'),
              ),
              
              box(
                title = "Welcome to the MIMIC Explorer Tool", width = 5, solidHeader = TRUE,
                status="success",
                
                br(),
               HTML("This dashboard allows to explore the MIMIC-III Database without needing some sort of programming skills. 

The tool is divided on 5 side main menus, each one giving you different functions.  The data is displayed with the help of tables, graphs and text. In menus where data is filtered and subseted by the user, the option to download it to the user machine is available."),
                
                
                
                
                
                
                
              ),
              
              box(
                
                status = "primary",
                plotlyOutput("los_graph")
              ),
              
              box(
                
                status = "primary",
                plotlyOutput("age_graph")
              ),
              
              box(
               
                status = "primary",
                plotlyOutput("piegender")
              ),
              
              box(
                
                status = "primary",
                plotlyOutput("pieethnicity")
              ),
              
              box(
                
                status = "primary",
                plotlyOutput("agemortality")
              ),
              
              box(
                
                status = "primary",
                plotlyOutput("etmortality")
              )
            )
            
            
            
            
    ),
    
    tabItem(tabName = "patients",
            h4("Filter to find general info about patients"),
            
            
            sidebarLayout(
              
              
              sidebarPanel(
                
                
                
                selectInput(inputId = "in_gender",
                            label = "Choose a gender:",
                            choices = xpto),
                
                selectInput(inputId = "in_ethnicity",
                            label = "Choose an ethnicity:",
                            choices = xpto2 ),
                selectInput(inputId = "in_religion",
                            label = "Choose a religion:",
                            choices = xpto3),
                selectInput(inputId = "in_insurance",
                            label = "Choose an insurance type:",
                            choices = xpto4 ),
                selectInput(inputId = "in_admtype",
                            label = "Choose an admission type:",
                            choices = xpto5 ),
                
                selectInput(inputId = "in_admloc",
                            label = "Choose an admission location:",
                            choices = xpto6 ),
                
                selectInput(inputId = "in_disloc",
                            label = "Choose an discharge location:",
                            choices = xpto7 ),
                
                selectInput(inputId = "in_hospex",
                            label = "Choose if alive(1) or dead(0) [Hospital expire flag]:",
                            choices = xpto8 ),
                selectInput(inputId = "in_dead",
                            label = "Choose if alive(1) or dead(0) [Expire flag]:",
                            choices = xpto10 ),
                selectInput(inputId = "in_diag",
                            label = "Choose the first diagnose:",
                            choices = xpto9 ),
                
                sliderInput( inputId = "in_age",
                             label = "Age", min = -1, max = 300,
                             value = c(-1,-1)
                ),
                print("Select -1 to not select age"),
                
                actionButton('select', 'Select'),
                width = 3,
                
                downloadButton('download',"Download the data"),
                
              ),
              mainPanel(
                
                
                verbatimTextOutput("summary"),
                width = 9,
                
                box(
                  status = "primary",
                  DT::dataTableOutput("tabelinha"),
                  width = 14,
                ),
                
                box(
                  status = "primary",
                  plotlyOutput("patgender"),
                  width = 6,
                ),
                
                box(
                  status = "primary",
                  plotlyOutput("patage"),
                  width = 6,
                ),
                
                box(
                  status = "primary",
                  plotlyOutput("patadmitype"),
                  width = 6,
                ),
                
                box(
                  status = "primary",
                  plotlyOutput("patinsurance"),
                  width = 6,
                ),
                
                
                box(
                  status = "primary",
                  plotlyOutput("patadmloc"),
                  width = 6,
                ),
                
                box(
                  status = "primary",
                  plotlyOutput("patdisloc"),
                  width = 6,
                )
                
                
              ),
              
              
              
              
            ), 
            
    ), #fimpatients
    
   
    
    tabItem(tabName = "patients3",
            h4("Search by patient ID"),
            
            
            fluidRow(
              
              
              box(
                status = "warning",
                #selectInput("patid", "Patient ID: ", choices = PATIENTS$SUBJECT_ID, selected = NULL ),
                #actionButton('buttonid2','Select'),
                width = 2,
                selectizeInput("patid", "Choose or type patient ID:", choices = NULL)
              ),
              
             column(12,
                box(
                  status = "primary",
                  title = "Patient info",
                  DT::dataTableOutput("patientid"),
                  #tableOutput("patient_id"),
                  width = 12,
                  
                ),
                
                box(
                  
                  status = "primary",
                  title = "All admissions of patient",
                  DT::dataTableOutput("adms_each_patient"),
                  width=12,
                )
             )
              
            ), 
            
    ),
    
    
 
    
    tabItem(tabName = "admissions2",
            h3("Details of every patient admission on the hospital"),
            br(),
            h4("There's a total of 58976 admissions registered on the database. Each one has a diferent ID , called HADM_ID. "),
            br(),
            fluidRow(
              box(
                width = 12,status = "primary",
                tabsetPanel(
                  id = 'dataset',
                  tabPanel("Admission type", DT::dataTableOutput("mytable1"), plotlyOutput("grafico1")),
                  tabPanel("Admission location", DT::dataTableOutput("mytable2"), plotlyOutput("grafico2")),
                  tabPanel("Discharge location", DT::dataTableOutput("mytable3"),  plotlyOutput("grafico3")),
                  tabPanel("Insurance", DT::dataTableOutput("mytable4"),  plotlyOutput("grafico4")),
                  tabPanel("Diagnosis", DT::dataTableOutput("mytable5"),  plotlyOutput("grafico5")),
                ),
              )
              
            )
            
            
            
            
    ), #fim admissions
    
    
    
    tabItem(tabName = "admissions1",
            h5("Search by admission ID to see more information about that particular admission:"),
            fluidRow(
              box(
                status = "warning",
                width = 2,
                selectizeInput("admission_id_input", "Choose or type admission ID:", choices = NULL)
                
              ), #fim box
              
              
              box(
                status = "primary",
                DT::dataTableOutput("admission_table"),
                #tableOutput("patient_id"),
                width = 10,
              ),
              
              
              
            )
    ),
    
    

    
    tabItem(tabName = "diagnoses1",
            h3("General view of the ICD-9 Codes"),
            fluidRow(
              column(  width = 12, 
                       box(
                         status = "primary",
                         plotlyOutput("graficoICDS"),
                         # height = 10,
                       ),
                       box(
                         status = "primary",
                         plotlyOutput("boxplotcompare"),
                         # height = 10,
                       ),
                       
                       box(
                         status = "primary",
                         plotlyOutput("mostfreqicd"),
                       ),
                       
                       box(
                         status = "warning",
                         h3("Select ICD9 code to see a summary:"),
                         # width = 3,
                         selectInput( "code92", "Select the ICD9 code", choices = c(
                           "-",
                           "INFECTIOUS AND PARASITIC DISEASES (001-139)" ="parasit",
                           "NEOPLASMS (140-239)" = "neoplasm",
                           "ENDOCRINE, NUTRITIONAL AND METABOLIC DISEASES, AND IMMUNITY DISORDERS (240-279)" = "endocrine",
                           "DISEASES OF THE BLOOD AND BLOOD-FORMING ORGANS (280-289)" = "blood",
                           "MENTAL DISORDERS (290-319)" = "mental",
                           "DISEASES OF THE NERVOUS SYSTEM AND SENSE ORGANS (320-389)" = "nervous",
                           "DISEASES OF THE CIRCULATORY SYSTEM (390-459)" = "circulatory",
                           "DISEASES OF THE RESPIRATORY SYSTEM (460-519)" = "respiratory",
                           "DISEASES OF THE DIGESTIVE SYSTEM (520-579)" = "digestive",
                           "DISEASES OF THE GENITOURINARY SYSTEM (580-629)" = "genitourinary",
                           "COMPLICATIONS OF PREGNANCY, CHILDBIRTH, AND THE PUERPERIUM (630-679)" = "pregnancy",
                           "DISEASES OF THE SKIN AND SUBCUTANEOUS TISSUE (680-709)" = "skin",
                           "DISEASES OF THE MUSCULOSKELETAL SYSTEM AND CONNECTIVE TISSUE (710-739)" ="muscle",
                           "CONGENITAL ANOMALIES (740-759)" = "anomalies",
                           "CERTAIN CONDITIONS ORIGINATING IN THE PERINATAL PERIOD (760-779)" = "perinatal",
                           "SYMPTOMS, SIGNS, AND ILL-DEFINED CONDITIONS (780-799)" ="signs",
                           "INJURY AND POISONING (800-999)" ="poison",
                           "SUPPLEMENTARY CLASSIFICATION OF FACTORS INFLUENCING HEALTH STATUS AND CONTACT WITH HEALTH SERVICES (V01-V89)" ="v1",
                           "SUPPLEMENTARY CLASSIFICATION OF EXTERNAL CAUSES OF INJURY AND POISONING (E800-E999)" = "v2"
                           
                           
                         ) 
                         )
                       ),
           
              ),
              
              
              
              box(
                status = "primary",
                verbatimTextOutput("summary2"), width = 12,
              ),
              
              
            )
    ),
    
    #***************************
    tabItem(tabName = "diagnoses2",
            h5("ICD-9 Codes -Diseases and Parasitic Diseases"),
            fluidRow(
              box(
                status = "warning",
                width = 3,
                selectInput( "code9", "Select the ICD9 code", choices = c(
                  "INFECTIOUS AND PARASITIC DISEASES (001-139)" ="parasit",
                  "NEOPLASMS (140-239)" = "neoplasm",
                  "ENDOCRINE, NUTRITIONAL AND METABOLIC DISEASES, AND IMMUNITY DISORDERS (240-279)" = "endocrine",
                  "DISEASES OF THE BLOOD AND BLOOD-FORMING ORGANS (280-289)" = "blood",
                  "MENTAL DISORDERS (290-319)" = "mental",
                  "DISEASES OF THE NERVOUS SYSTEM AND SENSE ORGANS (320-389)" = "nervous",
                  "DISEASES OF THE CIRCULATORY SYSTEM (390-459)" = "circulatory",
                  "DISEASES OF THE RESPIRATORY SYSTEM (460-519)" = "respiratory",
                  "DISEASES OF THE DIGESTIVE SYSTEM (520-579)" = "digestive",
                  "DISEASES OF THE GENITOURINARY SYSTEM (580-629)" = "genitourinary",
                  "COMPLICATIONS OF PREGNANCY, CHILDBIRTH, AND THE PUERPERIUM (630-679)" = "pregnancy",
                  "DISEASES OF THE SKIN AND SUBCUTANEOUS TISSUE (680-709)" = "skin",
                  "DISEASES OF THE MUSCULOSKELETAL SYSTEM AND CONNECTIVE TISSUE (710-739)" ="muscle", 
                  "CONGENITAL ANOMALIES (740-759)" = "anomalies",
                  "CERTAIN CONDITIONS ORIGINATING IN THE PERINATAL PERIOD (760-779)" = "perinatal",
                  "SYMPTOMS, SIGNS, AND ILL-DEFINED CONDITIONS (780-799)" ="signs",
                  "INJURY AND POISONING (800-999)" ="poison",
                  "SUPPLEMENTARY CLASSIFICATION OF FACTORS INFLUENCING HEALTH STATUS AND CONTACT WITH HEALTH SERVICES (V01-V89)" ="v1",
                  "SUPPLEMENTARY CLASSIFICATION OF EXTERNAL CAUSES OF INJURY AND POISONING (E800-E999)" = "v2"
                  
                  
                ) 
                )
              ), #fim box
              
              
              box(
                status = "primary",
                width = 12,
                plotlyOutput("icddiagnoses")
              ),
              
              box(
                status = "primary",
                width = 12,
                
                plotlyOutput("boxplot")
              )
              
            )
    ),
    
    tabItem(tabName = "diagnoses3",
            h3("First Diagnoses  "),
            fluidRow(
              box(
                status = "primary",
                plotlyOutput("graficoICDS2"),
                
              ),
              
              box(
                title = "ICD-9 Codes Description", width = 6, solidHeader = TRUE,
                status="success",
                
                br(),
                
                HTML("<p>&nbsp;Infectious and parasitic diseases <strong>(001-139)</strong><br>Neoplasms <strong>(140-239)&nbsp;</strong><br>Endocrine, nutritional and metabolic diseases, and immunity disorders <strong>(240-279)</strong><br>Diseases of the blood and blood-forming organs <strong>(280-289)</strong></p>

<p>Diseases of the nervous system and sense organs <strong>(320-389)</strong><br>Diseases of the circulatory system <strong>(390-459)</strong><br>Diseases of the respiratory system <strong>(460-519)</strong><br>Diseases of the genitourinary system<strong>&nbsp;(580-629)</strong><br>Complications of pregnancy, childbirth, and the puerperium <strong>(630-679)</strong><br> Diseases of the skin and subcutaneous tissue <strong>(680-709)</strong><br> Diseases of the musculoskeletal system and connective tissue<strong>&nbsp;(710-739)</strong><br>Congenital anomalies <strong>
  
  (740-759)</strong><br> Certain conditions originating in the perinatal period <strong>(760-779)</strong><br>Symptoms, signs, and ill-defined conditions <strong>(780-799)</strong><br>Injury and poisoning <strong>(800-999)</strong><br>Supplementary classification of factors influencing health status and contact with health services<strong>&nbsp;(V01-V89)</strong><br> Supplementary classification of external causes of injury and poisoning <strong>(E800-E999)</strong></p>")
                
                
                
                
                
                
                
              ),
              
              
              box( 
                status = "primary",
                DT::dataTableOutput("firstseqcompare"),
                width = 12,
                
              )
              
            )
    ),
    
    
    
    tabItem(tabName = "diagnoses4",
            h5("Search by patient ID to see all diagnoses of that patient:"),
            fluidRow(
              box(
                status = "warning",
                width = 2,
                selectizeInput("diagnose_patid", "Choose or type patient ID:", choices = NULL)
                
              ), #fim box
              
              
              box(
                status = "primary",
                DT::dataTableOutput("diagnose_patientid"),
                #tableOutput("patient_id"),
                width = 10,
              ),
              
              
              
            )
    ),
    
    tabItem(tabName = "diagnoses5",
            h5("Search by ICD-9 code:"),
            fluidRow(
              box(
                status = "warning",
                width = 2,
                selectizeInput("diagnose_icd", "Choose or type ICD:", choices = NULL)
                
              ), #fim box
              
              
              box(
                status = "primary",
                DT::dataTableOutput("diagnose_icdcode"),
                #tableOutput("patient_id"),
                width = 10,
              ),
              
              box(
                # status = "primary",
                verbatimTextOutput("summarydiagnoses5"),
                width = 12,
                
              )
              
              
              
            )
    ),
    
    tabItem(tabName ="icu1",
            h4("Filter to find general icu info:"),
            
            
            sidebarLayout(
              
              
              sidebarPanel(
                
                
                
                selectInput(inputId = "in_gender2",
                            label = "Choose a gender:",
                            choices = xpto),
                
                selectInput(inputId = "in_ethnicity2",
                            label = "Choose an ethnicity:",
                            choices = xpto2 ),
                selectInput(inputId = "in_hospex2",
                            label = "Choose if alive(1) or dead(0) [Hospital expire flag]:",
                            choices = xpto8 ),
                selectInput(inputId = "in_dead2",
                            label = "Choose if alive(1) or dead(0) [Expire flag]:",
                            choices = xpto10 ),
                selectInput(inputId = "in_funit",
                            label = "Choose the first care unit:",
                            choices = xpto11 ),
                selectInput(inputId = "in_lunit",
                            label = "Choose the last care unit:",
                            choices = xpto12),
                selectInput(inputId = "in_fward",
                            label = "Choose the first ward id:",
                            choices = xpto13 ),
                selectInput(inputId = "in_lward",
                            label = "Choose the last ward id:",
                            choices = xpto14 ),
                sliderInput( inputId = "in_los",
                             label = "LOS", min = -1, max = 174,
                             value = c(-1,-1)
                ),
                sliderInput( inputId = "in_age2",
                             label = "Age", min = -1, max = 300,
                             value = c(-1,-1)
                ),
                print("Select -1 to not select age"),
                br(),
                
                print("Select -1 to not filter by LOS"),
                
                actionButton('select2', 'Select'),
                width = 3,
                
                downloadButton('download2',"Download the data"),
                
              ),
              mainPanel(
                
                verbatimTextOutput("summary3"),
                
                width = 9,
                box(
                  status = "primary",
                  DT::dataTableOutput("icutable"),
                  width = 14,
                ),
                
                box(
                  status = "primary",
                  plotlyOutput("icugrafico"),
                  width = 6,
                ),
                
                box(
                  status = "primary",
                  plotlyOutput("icuidade"),
                  width = 6,
                ),
                
                box(
                  status = "primary",
                  plotlyOutput("icufunit"),
                  width = 6,
                ),
                
                box(
                  status = "primary",
                  plotlyOutput("iculunit"),
                  width = 6,
                ),
                
                
              ),
              
              
              
              
            ), 
            
    ),
    
    
    
    tabItem(tabName = "icu2",
            h5("Patient mortality after Discharge:"),
            fluidRow(
              box(
                title = "Patient mortality X days after Discharge", width = 6, solidHeader = TRUE,
                status="success",
                HTML("On this submenu we can get information of patients that died after discharge. That information is given by the attribute days.
                     The ICD9_Code associated to each perished patient correspondes to the first ICD diagnosed on that admission (SEQ_NUM=1).")
                
               
                
              ), #fim box
              
             box(
               width = 6,
               sliderInput( inputId = "in_days",
                              label = "Days", min = -1, max = 365,
                              value = c(-1,-1)
              ),
              print("Select -1 to not filter by days"),
             
             actionButton('select3', 'Select'),
             ),
              
              box(
                status = "primary",
                DT::dataTableOutput("death_days"),
                #tableOutput("patient_id"),
                width = 12,
                downloadButton('download3',"Download this data"),
              ),
              
              box(
                status = "primary",
                verbatimTextOutput("summary4"),
                width = 12
                
              ),
              
              
              box(
                status = "primary",
                plotlyOutput("death_eth"),
                width = 6,
              ),
              
              box(
                status = "primary",
                plotlyOutput("death_age"),
                width = 6,
              ),
              
              box(
                status = "primary",
                plotlyOutput("death_icd"),
                width = 12,
              )
              
              
              
            )
    ),
    
    tabItem(tabName = "predictions",
            tabsetPanel(
              tabPanel("Data Extraction", 
                       fluidRow(
                         box(
                           title = "Select variable of Prediction",
                           width = 12,
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           fluidRow(
                             column(width = 6, selectInput("selecPredType", "Select the type of outcome factor", choices = c("", "Mortality", "30-days mortality"))),
                             column(width = 6, selectInput("selecTypeDise", "Select the Type of Disease", choices = c("", "sepsis-3")))
                           )
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "!(input.selecPredType === '' || input.selecTypeDise === '')",
                         fluidRow(
                           box(
                             width = 12,
                             status = "primary",
                             DT::dataTableOutput("sepsis3_table")
                           )
                         )
                       )
              ),
              
              tabPanel("Summary",
                       tabsetPanel(
                         tabPanel("Baseline statistics",
                                  fluidRow(
                                    div(
                                      style = "overflow-y: auto; max-height: 500px;",
                                      box(
                                        width = 12,
                                        status = "primary",
                                        gt_output('table_summary')
                                      )
                                    )
                                  )
                         ),
                         
                         tabPanel("Baseline Statistics with Range and Quartiles", 
                                  fluidRow(
                                    div(
                                      style = "overflow-y: auto; max-height: 500px;",  # Add CSS for scroll and set a max-height
                                      box(
                                        width = 12,
                                        status = "primary",
                                        gt_output('table_summary2')
                                      )
                                    )
                                  )
                         )
                       )
                       
              ),
              
              tabPanel("Features",
                       
                       tabsetPanel(
                         tabPanel("Features Selection",
                                  fluidRow(
                                    div(
                                      box(
                                        style = "max-height: 500px; overflow-y: auto;",
                                        width = 4,
                                        status = "primary",
                                        checkboxGroupInput("features", "Select Features:",
                                                           choices = colnames(selectF), selected = c("urineoutput", "lactate_min", "bun_mean", "Sysbp_min", 
                                                                                                     "metastatic_cancer", "inr_max", "age", "sodium_max", 
                                                                                                     "aniongap_max", "creatinine_min", "SpO2_mean")),
                                      ),
                                      
                                      #box(verbatimTextOutput("selectedFeaturesOutput")),
                                      
                                      box(
                                        title = "Feature Correlation",
                                        width = 8,
                                        status = "primary",
                                        fluidRow(
                                          box(
                                            DTOutput("correlation_table"),
                                            width = 12
                                          )
                                        ),
                                        
                                        box(
                                          plotlyOutput("correlation_plot"),
                                          width = 12
                                        )
                                        
                                      )
                                    )
                                  )
                         ),
                         
                         tabPanel("Plots",
                                  
                                  fluidRow(
                                    box(
                                      selectInput("variableHist", "Select variable to visualize", choices = colnames(selectF)),
                                      plotlyOutput("histogramPlot"),
                                      width = 6,
                                      status = "primary"
                                      
                                    ),
                                    
                                    box(
                                      selectInput("variablePieChar", "Select variable to visualize", choices = colnames(selectF)),
                                      plotlyOutput("pie_chart"),
                                      width = 6,
                                      status = "primary"
                                      
                                    ),
                                    
                                    box(
                                      selectInput("variableBarplot", "Select variable to visualize", choices = colnames(selectF)),
                                      plotlyOutput("barplot"),
                                      width = 6,
                                      status = "primary"
                                    ),
                                    
                                    box(
                                      selectInput("variableScatterplot", "Select variable to visualize", choices = colnames(selectF)),
                                      plotlyOutput("scatter_plot"),
                                      width = 6,
                                      status = "primary"
                                    )
                                  )
                         )
                       )
              ),
              
              
              
              tabPanel("Train Model",
                       # Split Data, model selection and model training, model Evaluation
                       fluidRow(
                         box(
                           title = "Select a model to use:",
                           selectInput("Select_model", 
                                       label = NULL, 
                                       choices = c("","XGBoost", "Random Forest", "Logistic Regression"),
                                       
                           ),
                           
                           actionButton("train_button", "Train Model"),
                           solidHeader = TRUE,
                           width = 6,
                           height = "175",
                           status = "primary"
                         ),
                         
                         box(
                           title = "Train/Test Split %",
                           sliderInput(
                             "Slider1",
                             label = NULL,
                             min = 0,
                             max = 100,
                             value = 70
                           ),
                           textOutput("cntTrain"),
                           textOutput("cntTest"),
                           solidHeader = TRUE,
                           width = 6,
                           height = "175",
                           status = "primary"
                         ),
                         
                         conditionalPanel(condition = "input.train_button > 0",
                                          
                                          box(
                                            title = "Model Performance",
                                            solidHeader = TRUE,
                                            width = 12,
                                            status = "primary",
                                            fluidRow(
                                              box(
                                                textOutput("accuracy_text"),
                                                textOutput("precision_text"),
                                                textOutput("recall_text"),
                                                textOutput("f1_text"),
                                                textOutput("roc_auc_text"),
                                                plotOutput("roc_plot")
                                                #solidHeader = TRUE,
                                                #status = "primary"
                                              ),
                                              
                                              box(
                                                verbatimTextOutput("confusion_matrix")
                                              )
                                            )
                                          )
                         )
                       )
              ),
              tabPanel("Model Comparison",
                       fluidRow(
                         box(title = "Selected model:" ,
                             textOutput("selectedModel"),
                             solidHeader = TRUE,
                             width = 6,
                             status = "primary"
                         ),
                         
                         box(
                           selectInput("compare", "Select a model to compare with:", choices = ""),
                           solidHeader = TRUE,
                           width = 6,
                           status = "primary"
                         )
                       ),
                       
                       fluidRow(
                         
                         box(title = "Already trained model Evaluation",
                             solidHeader = TRUE,
                             width = 6,
                             status = "primary",
                             box(
                               textOutput("accuracy_trained"),
                               textOutput("precision_trained"),
                               textOutput("recall_trained"),
                               textOutput("f1_trained"),
                               textOutput("roc_auc_trained"),
                               plotOutput("roc_plot_trained"),
                               width = 12
                             ),
                             
                             box(
                               verbatimTextOutput("confusion_matrix_trained"),
                               width = 12
                             )
                         ),
                         
                         box(title = "Selected model Evaluation" ,
                             
                             solidHeader = TRUE,
                             width = 6,
                             status = "primary",
                             box(
                               textOutput("accuracy_compare"),
                               textOutput("precision_compare"),
                               textOutput("recall_compare"),
                               textOutput("f1_compare"),
                               textOutput("roc_auc_compare"),
                               plotOutput("roc_plot_compare"),
                               width = 12
                             ),
                             
                             box(
                               verbatimTextOutput("confusion_matrix_compare"),
                               width = 12
                             )
                         )
                       )
              ),
              tabPanel("Make Prediction",
                       fluidRow(
                         box(
                           uiOutput("numeric_inputs"),
                           actionButton("predict_button", "Predict"),
                           style = "max-height: 500px; overflow-y: auto;",
                           solidHeader = TRUE,
                           width = 6,
                           status = "primary"
                         ),
                         
                         box(
                           title = "Prediction Result:",
                           textOutput("prediction_result_box"),
                           solidHeader = TRUE,
                           width = 6,
                           status = "primary"
                         )
                       )
              )
            )
    )
    
  
  ) #fimtab items
  
  
  
)

ui <- dashboardPage(header, sidebar, body)

# ********************************************* SERVER *****************************************************
server <- (function(input, output,session) {
  
  # output$grafico <- renderPlot({
  #   if( is.factor(dfmerge[,input$inState]) || is.character(dfmerge[,input$inState])) {
  #     
  #     barplot(summary(as.factor(dfmerge[,input$inState])),col = "#75AADB")
  #   }
  #   else
  #     
  #     hist((dfmerge[,input$inState]),col = "#75AADB")
  # })
  
  
  
  
  # output$age_graph <- renderPlot({
  #   barplot(table(dfmerge$age),main = "Age frequency", 
  #           xlab = "Frequency", 
  #           ylab = "Age")
  # })
  
  # output$los_graph <- renderPlot({
  #   hist(ICUSTAYS$LOS,breaks= "FD",xlim = c(0,50),main = "Length of Stay frequency", 
  #           xlab = "Frequency", 
  #           ylab = "LOS")
  # })
  
  
  
  output$los_graph <- renderPlotly({
    plot_ly(
      data = ICUSTAYS,
      x = ~LOS,
      type = "histogram"
    ) %>%
      layout(title= "Histogram of Length of Stay", 
             xaxis= list(title = "Days" ),
             yaxis= list(title = "Frequency")
      )
    
    
  })
  
  output$age_graph <- renderPlotly({
    plot_ly(
      data = filter(df, age <300 & age > 0),
      x = ~age,
      type="histogram"
    ) %>%
      layout(title= "Histogram of age", 
             xaxis= list(title = "Years" ),
             yaxis= list(title = "Frequency")
             
      )
  })
  
  output$diseases <- renderDataTable(
    admission_temp
    
  )
  
  
  ## PATIENTS ######################################################################
  
  filtered_gender <- reactive({
    if(input$in_gender == "ALL"){
      dfmerge3
    } else {
      dfmerge3 %>% 
        filter(GENDER == input$in_gender)
    }
  })
  
  filtered_ethnicity <- reactive({
    if(input$in_ethnicity == "ALL"){
      filtered_gender()
    } else {
      filtered_gender() %>% 
        filter(ETHNICITY == input$in_ethnicity)
    }
  })
  
  
  filtered_religion <- reactive({
    if(input$in_religion == "ALL"){
      filtered_ethnicity()
    } else {
      filtered_ethnicity() %>% 
        filter(RELIGION == input$in_religion)
    }
  })
  
  
  
  filtered_age <- reactive({
    if(input$in_age == "-1" ){
      filtered_religion()
    }
    else{
      filtered_religion() %>% 
        filter( age >= input$in_age[1] & age <= input$in_age[2])
    }
  })
  
  filtered_insurance <- reactive({
    if(input$in_insurance == "ALL"){
      filtered_age()
    } else {
      filtered_age() %>% 
        filter(INSURANCE == input$in_insurance)
    }
  })
  
  filtered_admtype <- reactive({
    if(input$in_admtype == "ALL"){
      filtered_insurance()
    } else {
      filtered_insurance() %>% 
        filter(ADMISSION_TYPE == input$in_admtype)
    }
  })
  
  filtered_admloc <- reactive({
    if(input$in_admloc == "ALL"){
      filtered_admtype()
    } else {
      filtered_admtype() %>% 
        filter(ADMISSION_LOCATION == input$in_admloc)
    }
  })
  
  filtered_disloc <- reactive({
    if(input$in_disloc == "ALL"){
      filtered_admloc()
    } else {
      filtered_admloc() %>% 
        filter(DISCHARGE_LOCATION == input$in_disloc)
    }
  })
  
  filtered_hospex <- reactive({
    if(input$in_hospex == "ALL"){
      filtered_disloc()
    } else {
      filtered_disloc() %>% 
        filter(HOSPITAL_EXPIRE_FLAG == input$in_hospex)
    }
  })
  
  filtered_diag <- reactive({
    if(input$in_diag == "ALL"){
      filtered_hospex()
    } else {
      filtered_hospex() %>% 
        filter(DIAGNOSIS == input$in_diag)
    }
  })
  filtered_dead <- reactive({
    if(input$in_dead == "ALL"){
      filtered_diag()
    } else {
      filtered_diag() %>% 
        filter(EXPIRE_FLAG == input$in_dead)
    }
  })
  
  
  
  fully_filtered <- eventReactive(input$select, {
    filtered_dead()
  })
  
  output$download <- downloadHandler(
    filename = function(){"data.csv"}, 
    content = function(fname){
      write.csv(fully_filtered(), fname)
    }
  )
  
  
  output$summary <- renderPrint({
    
    summary(fully_filtered())
  })
  
  
  
  
  
  ## FIM PATIENTS #################################################
  
  
  ## INICIO ICU ###################################################
  
  
  filtered_gender2 <- reactive({
    if(input$in_gender2 == "ALL"){
      ICUcomplete
    } else {
      ICUcomplete %>% 
        filter(GENDER == input$in_gender2)
    }
  })
  
  filtered_ethnicity2 <- reactive({
    if(input$in_ethnicity2 == "ALL"){
      filtered_gender2()
    } else {
      filtered_gender2() %>% 
        filter(ETHNICITY == input$in_ethnicity2)
    }
  })
 
  
  filtered_age2 <- reactive({
    if(input$in_age2 == "-1" ){
      filtered_ethnicity2()
    }
    else{
      filtered_ethnicity2() %>% 
        filter( age >= input$in_age2[1] & age <= input$in_age2[2])
    }
  })
  
  
  
  
  
  filtered_los <- reactive({
    if(input$in_los == "-1" ){
      filtered_age2()
    }
    else{
      filtered_age2() %>%
        filter( LOS >= input$in_los[1] & LOS <= input$in_los[2])
    }
  })


  
  filtered_hospex2 <- reactive({
    if(input$in_hospex2 == "ALL"){
      filtered_los()
    } else {
      filtered_los() %>% 
        filter(HOSPITAL_EXPIRE_FLAG == input$in_hospex2)
    }
  })
  

  filtered_dead2 <- reactive({
    if(input$in_dead2 == "ALL"){
      filtered_hospex2()
    } else {
      filtered_hospex2() %>% 
        filter(EXPIRE_FLAG == input$in_dead2)
    }
  })
  
  filtered_funit <- reactive({
    if(input$in_funit == "ALL"){
      filtered_dead2()
    } else {
      filtered_dead2() %>% 
        filter(FIRST_CAREUNIT == input$in_funit)
    }
  })
  
  
  filtered_lunit <- reactive({
    if(input$in_lunit == "ALL"){
      filtered_funit()
    } else {
      filtered_funit() %>% 
        filter(LAST_CAREUNIT == input$in_lunit)
    }
  })
  
  filtered_fward <- reactive({
    if(input$in_fward == "ALL"){
      filtered_lunit()
    } else {
      filtered_lunit() %>% 
        filter(FIRST_WARDID == input$in_fward)
    }
  })
  
  filtered_lward <- reactive({
    if(input$in_lward == "ALL"){
      filtered_fward()
    } else {
      filtered_fward() %>% 
        filter(LAST_WARDID == input$in_lward)
    }
  })
  
  
  
  
  fully_filtered2 <- eventReactive(input$select2, {
    filtered_lward()
  })
  
  
  output$download2 <- downloadHandler(
    filename = function(){"data.csv"}, 
    content = function(fname){
      write.csv(fully_filtered2(), fname)
    }
  )
  
  
  output$icutable <- DT::renderDataTable({
    DT::datatable(fully_filtered2(),options = list(scrollX = TRUE) )
  })
  
  
  
  output$summary3 <- renderPrint({
    
    summary(fully_filtered2())
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## FIM ICU #######################################################
  
  
  output$tabelinha <- DT::renderDataTable({
    DT::datatable(fully_filtered(),options = list(scrollX = TRUE) )
  })
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(admission_temp)
  })
  
  output$grafico1 <- renderPlotly({
    plot_ly(
      data = ADMISSIONS,
      x = ~ADMISSION_TYPE,
      type="histogram"
    ) %>%
      layout(title= "Admission Type " ,
             xaxis= list(title = "Type" ),
             yaxis= list(title = "Frequency")
             
      )
  })
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(admission_loc)
  })
  
  output$grafico2 <- renderPlotly({
    plot_ly(
      data = ADMISSIONS,
      x = ~ADMISSION_LOCATION,
      type="histogram"
    ) %>%
      layout(title= "Admission Location " ,
             xaxis= list(title = "Location" ),
             yaxis= list(title = "Frequency")
             
             
      )
  })
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(discharge_loc)
  })
  
  output$grafico3 <- renderPlotly({
    plot_ly(
      data = ADMISSIONS,
      x = ~DISCHARGE_LOCATION,
      type="histogram"
    ) %>%
      layout(title= "Discharge Location " ,
             xaxis= list(title = "Location" ),
             yaxis= list(title = "Frequency")
             
      ) 
  })
  
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(insurance_temp)
  })
  
  output$grafico4 <- renderPlotly({
    plot_ly(
      data = ADMISSIONS,
      x = ~INSURANCE,
      type="histogram"
    ) %>%
      layout(title= "Insurance " ,
             xaxis= list(title = "Insurance" ,
                         categoryorder = "total descending"),
             yaxis= list(title = "Frequency")
             
      )
  })
  
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(diagnosis_temp)
  })
  
  output$grafico5 <- renderPlotly({
    plot_ly(
      data = diagnosis_temp,
      x = ~D,
      type="bar"
    ) %>%
      layout(title= "Diagnosis " ,
             xaxis= list(title = "Diagnosis" )
             
      )
  })
  output$graficoICDS <-  renderPlotly({
    plot_ly(
      data = diagnosesPlot,
      x = ~ICD9CODE,
      y = ~Frequency,
      type = "bar",
      text = ~Frequency,
      textposition = "auto",
      hoverinfo = "text",
      hovertext = paste("IC9-Code:", diagnosesPlot$name)
    ) %>%
      layout(title= "Frequency of each ICD9 code"
             
      )
    
    
  })
  
  output$boxplot <- renderPlotly({
    
    
    if (input$code9 == "parasit"){
      
      plot_ly(
        data = temp1,
        y = ~LOS,
        type = "box",
        name = "001-139"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
      
    } 
    else if (input$code9 == "neoplasm") {
      plot_ly(
        data = temp2,
        y = ~LOS,
        type = "box",
        name = "140-239"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
      
    }
    else if (input$code9 == "endocrine") {
      plot_ly(
        data = temp3,
        y = ~LOS,
        type = "box",
        name = "240-279"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
      
    }
    
    else if ( input$code9 == "blood" )
      
      plot_ly(
        data = temp4,
        y = ~LOS,
        type = "box",
        name = "280-289"
      ) %>%
      layout(title= "Length of stay",
             yaxis= list(title = "Days"  )
             
      )
    
    
    
    else if (input$code9 == "mental") {
      plot_ly(
        data = temp5,
        y = ~LOS,
        type = "box",
        name = "290-319"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
      
    }
    
    else if (input$code9 == "nervous") {
      plot_ly(
        data = temp6,
        y = ~LOS,
        type = "box",
        name = "320-389"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
      
    }
    
    else if (input$code9 == "circulatory") {
      plot_ly(
        data = temp7,
        y = ~LOS,
        type = "box",
        name = "390-459"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
      
    }
    else if (input$code9 == "respiratory") {
      plot_ly(
        data = temp8,
        y = ~LOS,
        type = "box",
        name = "460-519"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
    }
    
    else if (input$code9 == "digestive") {
      plot_ly(
        data = temp9,
        y = ~LOS,
        type = "box",
        name = "520-579"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
    }
    
    else if (input$code9 == "genitourinary") {
      plot_ly(
        data = temp10,
        y = ~LOS,
        type = "box",
        name = "580-629"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
    }
    
    else if (input$code9 == "pregnancy") {
      plot_ly(
        data = temp11,
        y = ~LOS,
        type = "box",
        name = "630-679"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
      
    }
    
    else if (input$code9 == "skin") {
      plot_ly(
        data = temp12,
        y = ~LOS,
        type = "box",
        name = "680-709"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
      
    }
    
    else if (input$code9 == "muscle") {
      plot_ly(
        data = temp13,
        y = ~LOS,
        type = "box",
        name = "710-739"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
    }
    
    else if (input$code9 == "anomalies") {
      plot_ly(
        data = temp14,
        y = ~LOS,
        type = "box",
        name = "740-759"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
    }
    
    else if (input$code9 == "perinatal") {
      plot_ly(
        data = temp15,
        y = ~LOS,
        type = "box",
        name = "760-779"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
    }
    
    else if (input$code9 == "signs") {
      plot_ly(
        data = temp16,
        y = ~LOS,
        type = "box",
        name = "780-799"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
    }
    
    else if (input$code9 == "poison") {
      plot_ly(
        data = temp17,
        y = ~LOS,
        type = "box",
        name = "800-999"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
      
    }
    
    
    else if (input$code9 == "v1") {
      plot_ly(
        data = temp18,
        y = ~LOS,
        type = "box",
        name = "V01-V89"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
    }
    
    else if (input$code9 == "v2") {
      plot_ly(
        data = temp19,
        y = ~LOS,
        type = "box",
        name = "E800-E999"
      ) %>%
        layout(title= "Length of stay",
               yaxis= list(title = "Days"  )
               
        )
    }
    
    
    
    
  })
  
  output$icddiagnoses <- renderPlotly({
    
    if(input$code9 == "blood"){
      counts <- as.data.frame(table(blood289$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
    } 
    else if (input$code9 == "neoplasm") {
      counts <- as.data.frame(table(neoplasms239$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    else if (input$code9 == "parasit") {
      counts <- as.data.frame(table(infections139$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    else if (input$code9 == "endocrine") {
      counts <- as.data.frame(table(endocrine279$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    
    else if (input$code9 == "mental") {
      counts <- as.data.frame(table(mental319$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "nervous") {
      counts <- as.data.frame(table(nervous389$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "circulatory") {
      counts <- as.data.frame(table(circulatory459$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    else if (input$code9 == "respiratory") {
      counts <- as.data.frame(table(respiratory519$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "digestive") {
      counts <- as.data.frame(table(digestive579$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "genitourinary") {
      counts <- as.data.frame(table(genitourinary629$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "pregnancy") {
      counts <- as.data.frame(table(pregnancy679$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "skin") {
      counts <- as.data.frame(table(skin709$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "muscle") {
      counts <- as.data.frame(table(muscle739$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "anomalies") {
      counts <- as.data.frame(table(congenital759$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "perinatal") {
      counts <- as.data.frame(table(perinatal779$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "signs") {
      counts <- as.data.frame(table(symptoms799$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "poison") {
      counts <- as.data.frame(table(injury999$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    
    else if (input$code9 == "v1") {
      counts <- as.data.frame(table(v$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "v2") {
      counts <- as.data.frame(table(e$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "Range of ICD9 Codes" ,
                           categoryorder = "total descending")
        )
      
    }
    
  })
  
  output$summary2 <- renderPrint({
    
    if (input$code92 == "parasit"){
      
      bug_solver
      
    } 
    else if (input$code92 == "neoplasm") {
      summary(temp2)
      
    }
    else if (input$code92 == "endocrine") {
      summary(temp3)
      
    }
    
    else if ( input$code92 == "blood" )
      
      summary(temp4)
    
    
    
    else if (input$code92 == "mental") {
      summary(temp5)
      
    }
    
    else if (input$code92 == "nervous") {
      summary(temp6)
      
    }
    
    else if (input$code92 == "circulatory") {
      summary(temp7)
      
    }
    else if (input$code92 == "respiratory") {
      summary(temp8)
    }
    
    else if (input$code92 == "digestive") {
      summary(temp9)
    }
    
    else if (input$code92 == "genitourinary") {
      summary(temp10)
    }
    
    else if (input$code92 == "pregnancy") {
      summary(temp11)
      
    }
    
    else if (input$code92 == "skin") {
      summary(temp12)
      
    }
    
    else if (input$code92 == "muscle") {
      summary(temp13)
    }
    
    else if (input$code92== "anomalies") {
      summary(temp14)
    }
    
    else if (input$code92 == "perinatal") {
      summary(temp15)
    }
    
    else if (input$code92 == "signs") {
      summary(temp16)
    }
    
    else if (input$code92 == "poison") {
      summary(temp17)
      
    }
    
    
    else if (input$code92 == "v1") {
      summary(temp18)
    }
    
    else if (input$code92 == "v2") {
      summary(temp19)
    }
    
  })
  
  
  output$doentes <- DT::renderDataTable({
    
    if ( input$code93 == "parasit")
      DT::datatable(subset(infections139, select = -c(ROW_ID) ) )
    
    else if (input$code93 == "neoplasm") {
      DT::datatable(subset(neoplasms239, select = -c(ROW_ID) ))
      
    }
    else if (input$code93 == "endocrine") {
      DT::datatable(subset(endocrine279, select = -c(ROW_ID) ))
      
    }
    
    else if ( input$code93 == "blood" )
      
      DT::datatable(subset(blood289, select = -c(ROW_ID) ))
    
    
    
    else if (input$code93 == "mental") {
      DT::datatable(subset(mental319, select = -c(ROW_ID) ))
      
    }
    
    else if (input$code93 == "nervous") {
      DT::datatable(subset(nervous389, select = -c(ROW_ID) ))
      
    }
    
    else if (input$code93 == "circulatory") {
      DT::datatable(subset(circulatory459, select = -c(ROW_ID) ))
      
    }
    else if (input$code93 == "respiratory") {
      DT::datatable(subset(respiratory519, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "digestive") {
      DT::datatable(subset(digestive579, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "genitourinary") {
      DT::datatable(subset(genitourinary629, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "pregnancy") {
      DT::datatable(subset(pregnancy679, select = -c(ROW_ID) ))
      
    }
    
    else if (input$code93 == "skin") {
      DT::datatable(subset(skin709, select = -c(ROW_ID) ))
      
    }
    
    else if (input$code93 == "muscle") {
      DT::datatable(subset(muscle739, select = -c(ROW_ID) ))
    }
    
    else if (input$code93== "anomalies") {
      DT::datatable(subset(congenital759, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "perinatal") {
      DT::datatable(subset(perinatal779, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "signs") {
      DT::datatable(subset(symptoms799, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "poison") {
      DT::datatable(subset(injury999, select = -c(ROW_ID) ))
      
    }
    
    
    else if (input$code93 == "v1") {
      DT::datatable(subset(v, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "v2") {
      DT::datatable(subset(e, select = -c(ROW_ID) ))
    }
    
    
  })
  
  output$boxplotcompare <- renderPlotly({
    fig <- plot_ly(
      data = temp1,
      y = ~LOS,
      type = "box",
      name= "001-139"
      
    )
    fig <- fig %>% add_trace(
      data = temp2,
      y = ~LOS,
      type = "box",
      name="140-239"
      
    )
    
    
    fig <- fig %>% add_trace(
      data = temp3,
      y = ~LOS,
      type = "box",
      name="240-279"
      
    )
    fig <- fig %>% add_trace(
      data = temp4,
      y = ~LOS,
      type = "box",
      name="280-289"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp5,
      y = ~LOS,
      type = "box",
      name="290-319"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp6,
      y = ~LOS,
      type = "box",
      name="320-389"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp7,
      y = ~LOS,
      type = "box",
      name="390-459"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp8,
      y = ~LOS,
      type = "box",
      name="460-519"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp9,
      y = ~LOS,
      type = "box",
      name="520-579"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp10,
      y = ~LOS,
      type = "box",
      name="580-629"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp11,
      y = ~LOS,
      type = "box",
      name="630-679"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp12,
      y = ~LOS,
      type = "box",
      name="680-709"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp13,
      y = ~LOS,
      type = "box",
      name="710-739"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp14,
      y = ~LOS,
      type = "box",
      name="740-759"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp15,
      y = ~LOS,
      type = "box",
      name="760-779"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp16,
      y = ~LOS,
      type = "box",
      name="780-799"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp17,
      y = ~LOS,
      type = "box",
      name="800-999"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp18,
      y = ~LOS,
      type = "box",
      name="V01-V89"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp19,
      y = ~LOS,
      type = "box",
      name="E800-E999"
      
    )%>%
      layout(yaxis= list(title = "Days"))
    
  })
  
  output$graficoICDS2 <-  renderPlotly({
    plot_ly(
      data = diagnosesPlot2,
      x = ~ICD9CODE,
      y = ~Frequency2,
      type = "bar",
      text = ~Frequency2,
      textposition = "auto",
      hoverinfo = "text",
      hovertext = paste("IC9-Code:", diagnosesPlot2$a)
    ) %>%
      layout(title= "Frequency of each ICD9 code",
             yaxis= list(title = "Days")
             
      )
    
    
  })
  
  updateSelectizeInput(session, "patid", choices = PATIENTS$SUBJECT_ID, server = T, selected = "2")
  
  
  
  
  
  # observeEvent(input$buttonid,
  #              {
  #                updateSelectizeInput(session, "listid", selected = "Item 1234")
  #              })
  
  # output$patient_id <- renderTable({
  #   filter( PATIENTS, SUBJECT_ID == "input$patid")
  # })
  
  
  
  
  
  output$patientid <- DT::renderDataTable({
    
    
    DT::datatable( filter( dfmerge3, SUBJECT_ID == input$patid), options = list(scrollX = TRUE) )
  })
  
  
  updateSelectizeInput(session, "diagnose_patid", choices = PATIENTS$SUBJECT_ID, server = T, selected = "2")
  
  
  updateSelectizeInput(session, "diagnose_icd", choices = D_ICD_DIAGNOSES$ICD9_CODE, server = T, selected = "0010")
  
  output$diagnose_patientid <- DT::renderDataTable({
    
   temporario3 <- filter( diagnoses_with_LOS, SUBJECT_ID == input$diagnose_patid)
   temporario3 <- subset (temporario3, select= -c(2))
    
    DT::datatable( temporario3, options = list(scrollX = TRUE) )
  })
  
  output$diagnose_icdcode <- DT::renderDataTable({
    
   temporario2 <-  filter( diagnoses_with_LOS, ICD9_CODE == input$diagnose_icd)
   temporario2 <- subset (temporario2, select= -c(2))
   
    DT::datatable( temporario2, options = list(scrollX = TRUE) )
  })
  
  
  output$summarydiagnoses5 <- renderPrint({
    
    temporario <- left_join(diagnoses_with_LOS, dfmerge3, by = "SUBJECT_ID")
    temporario <- subset (temporario, select= -c(HADM_ID.y))
    summary(filter(temporario,ICD9_CODE == input$diagnose_icd) )
    
  })
  
  output$firstseqcompare <- DT::renderDataTable({
    
    
    DT::datatable( firstseq_compare, options = list(scrollX = TRUE) )
  })
  
  updateSelectizeInput(session, "admission_id_input", choices = ADMISSIONS$HADM_ID, server = T)
  
  output$admission_table <- DT::renderDataTable({
    
    
    DT::datatable(  filter( subset(ADMISSIONS, select = -c(ROW_ID)) , HADM_ID == input$admission_id_input), options = list(scrollX = TRUE) )
  })
  
  output$icugrafico <-  renderPlotly({
    plot_ly(
      data = fully_filtered2(),
      y = ~LOS,
      type = "box",
      name = "Length of stay"
    ) %>%
      layout(title= "BoxPlot of LOS selected"
             
      )
    
    
    
  })
  
  output$piegender <-  renderPlotly({
    plot_ly(
      count(df, GENDER),values=~n,labels=~factor(GENDER),
            marker=list(colors=c("#89CFF0","#1B6AA1")),type="pie") %>%
      layout(title= "Gender distribuiton"
             
             
      )
    
    
  })
  output$pieethnicity <-  renderPlotly({
    plot_ly(
      count(df, ETHNICITY),values=~n,labels=~factor(ETHNICITY)
      ,type="pie") %>%
      layout(title= "Ethnicity distribuiton"
             
             
      )
    
    
  })

  output$mostfreqicd <-  renderPlotly({
    plot_ly(
      data = most_freq_icd,
      x = ~ICD9_CODE,
      y = ~n,
      type = "bar",
      text = ~n,
      textposition = "auto",
      hoverinfo = "text",
      hovertext = paste("IC9-Code:", most_freq_icd$SHORT_TITLE)
    ) %>%
      layout(title= "Top ten most frequent ICD-9 codes",
             yaxis= list(title = "Frequency"  )
             
             )
      
    
    
  })
  
  output$adms_each_patient <- DT::renderDataTable({
    temporario <- filter( ADMISSIONS, SUBJECT_ID == input$patid)
    temporario <- subset(temporario, select = -c(1))
    DT::datatable( temporario, options = list(scrollX = TRUE) )
  })
  
  
  
  output$patgender <-  renderPlotly({
    plot_ly(
      count(fully_filtered(), ETHNICITY),values=~n,labels=~factor(ETHNICITY)
      ,type="pie") %>%
      layout(title= "Ethnicity distribuiton", showlegend = F
             
      )

  })
  
  output$patage <-  renderPlotly({
    plot_ly(
      data = fully_filtered(),
      x = ~age,
      type = "histogram"
    ) %>%
      layout(title= "Histogram of Age", 
             xaxis= list(title = "Years" ),
             yaxis= list(title = "Frequency")
      )
    
  })
  
  output$icuidade <-  renderPlotly({
    plot_ly(
      data = fully_filtered2(),
      x = ~age,
      type = "histogram"
    ) %>%
      layout(title= "Histogram of Age", 
             xaxis= list(title = "Years" ),
             yaxis= list(title = "Frequency")
      )
    
  })
  
  
  output$icufunit <-  renderPlotly({
    plot_ly(
      count(fully_filtered2(), FIRST_CAREUNIT),values=~n,labels=~factor(FIRST_CAREUNIT)
      ,type="pie") %>%
      layout(title= "First Care Unit"
             
      )
    
  })
  
  output$iculunit <-  renderPlotly({
    plot_ly(
      count(fully_filtered2(), LAST_CAREUNIT),values=~n,labels=~factor(LAST_CAREUNIT)
      ,type="pie") %>%
      layout(title= "Last Care Unit"
             
      )
    
  })
  
  output$patadmitype <-  renderPlotly({
    plot_ly(
      count(fully_filtered(), ADMISSION_TYPE),values=~n,labels=~factor(ADMISSION_TYPE)
      ,type="pie") %>%
      layout(title= "Admission type"
             
      )
    
  })
  
  output$patinsurance <-  renderPlotly({
    plot_ly(
      count(fully_filtered(), INSURANCE),values=~n,labels=~factor(INSURANCE)
      ,type="pie") %>%
      layout(title= "Insurance type"
             
      )
    
  })
  
  output$patadmloc <-  renderPlotly({
    plot_ly(
      count(fully_filtered(), ADMISSION_LOCATION),values=~n,labels=~factor(ADMISSION_LOCATION)
      ,type="pie") %>%
      layout(title= "Admission Location"
             
      )
    
  })
 
  output$patdisloc <-  renderPlotly({
    plot_ly(
      count(fully_filtered(), DISCHARGE_LOCATION),values=~n,labels=~factor(DISCHARGE_LOCATION)
      ,type="pie") %>%
      layout(title= "Discharge Location"
             
      )
    
  })
  
  
  output$agemortality <- renderPlotly({
    plot_ly(
      data = filter(df, HOSPITAL_EXPIRE_FLAG == "1"),
      x = ~age,
      type = "histogram"
    ) %>%
      layout(title= "Histogram of Mortality in hospital by age, including people over 89 years", 
             xaxis= list(title = "Age Range" ),
             yaxis= list(title = "Frequency")
      )
    
    
  })
  
  
  output$etmortality <-  renderPlotly({
    
    
    plot_ly(
      count(filter(df, HOSPITAL_EXPIRE_FLAG == "1"), ETHNICITY),values=~n,labels=~factor(ETHNICITY)
      ,type="pie") %>%
      layout(title= "Mortality in hospital by Ethnicity"
             
      )
    
  })
  # rdays_before_death <- reactive(days_before_death)
  
  
  
  
  lost <- reactive({
    if(input$in_days == "-1" ){
      days_before_death
    }
    else{
      days_before_death %>% 
        filter( days >= input$in_days[1] & days <= input$in_days[2])
    }
  
  })
  
  fully_filtered3 <- eventReactive(input$select3, {
    lost()
  })
  
  output$death_days <- DT::renderDataTable({
    
    
    DT::datatable(  fully_filtered3(), options = list(scrollX = TRUE) )
  })
  
  
  output$download3 <- downloadHandler(
    filename = function(){"deathsdischarge.csv"}, 
    content = function(fname){
      write.csv(fully_filtered3(), fname)
    }
  )
  
  output$summary4 <- renderPrint({
    
    summary(fully_filtered3())
  })
  
  output$death_eth <-  renderPlotly({
    
    
    plot_ly(
      count(fully_filtered3(), ETHNICITY),values=~n,labels=~factor(ETHNICITY)
      ,type="pie") %>%
      layout(title= "Mortality  by Ethnicity"
             
      )
    
  })
  
  output$death_age <- renderPlotly({
    plot_ly(
      data = fully_filtered3(),
      x = ~age,
      type = "histogram"
    ) %>%
      layout(title= "Histogram of Mortality , including people over 89 years", 
             xaxis= list(title = "Age Range" ),
             yaxis= list(title = "Frequency")
      )
    
    
  })
  
  output$death_icd <- renderPlotly({
    plot_ly(
      data = fully_filtered3(),
      x = ~ICD9_CODE,
      type = "histogram"
      # hoverinfo = "text",
      # hovertext = paste("IC9-Code:", fully_filtered3$SHORT_TITLE)
    ) %>%
      layout(title= "Histogram of ICD Mortality ", 
             xaxis= list(title = "ICD9" ),
             yaxis= list(title = "Frequency")
      )
    
    
  })
  
  ################# Predicitions #################
  var <- ""
  trained_model <- NULL
  
  selected_features <- reactive({
    input$features
  })
  
  # Data Preparation
  
  output$sepsis3_table <- DT::renderDataTable({
    
    selectedPredType <- input$selecPredType
    selectedTypeDise <- input$selecTypeDise
    
    if(selectedPredType == "Mortality" && selectedTypeDise == "sepsis-3") {
      sepsis_predf <- sepsis_predf %>% select(-THIRTYDAY_EXPIRE_FLAG)  
    }
    
    else if(selectedPredType == "30-days mortality" && selectedTypeDise == "sepsis-3") {
      sepsis_predf <- sepsis_predf %>% select(-HOSPITAL_EXPIRE_FLAG)  
    }
    
    DT::datatable(sepsis_predf, options = list(scrollX = TRUE)) %>%
      formatStyle(names(sepsis_predf), textAlign = 'center')
  })
  
  # Data analysis
  
  #exclude <- c("icustay_id", "hadm_id", "is_male", "THIRTYDAY_EXPIRE_FLAG")
  #sepsis3_statistics <- sepsis3_statistics[, !(names(sepsis3_statistics) %in% exclude)]
  #column_names_c <- as.character(names(sepsis3_statistics))
  #table1 <- 
  #  sepsis3_statistics %>%
  #  tbl_summary(include = column_names_c)
  
  output$table_summary <- render_gt({
    
    selectedPredType <- input$selecPredType
    selectedTypeDise <- input$selecTypeDise
    
    stats_aux <- stats
    
    if(selectedPredType == "Mortality" && selectedTypeDise == "sepsis-3") {
      stats_aux$HOSPITAL_EXPIRE_FLAG <- ifelse(stats_aux$HOSPITAL_EXPIRE_FLAG == 1, "Death", "Alive")
      stats_aux <- stats_aux[, !names(stats_aux) %in% c('THIRTYDAY_EXPIRE_FLAG')]
      var = "HOSPITAL_EXPIRE_FLAG"
    }
    
    else {
      stats_aux$THIRTYDAY_EXPIRE_FLAG <- ifelse(stats_aux$THIRTYDAY_EXPIRE_FLAG == 1, "Death", "Alive")
      stats_aux <- stats_aux[, !names(stats_aux) %in% c('HOSPITAL_EXPIRE_FLAG')]
      var = "THIRTYDAY_EXPIRE_FLAG"
    }
    
    # Increase workspace size
    # options(expressions = 10000)
    
    table_summary <- 
      tbl_summary(
        data = stats_aux,
        by = all_of(var),
        statistic = all_continuous() ~ "{mean} ({sd})"
      ) %>%
      add_p() %>% as_gt()
  })
  
  output$table_summary2 <- render_gt({
    
    selectedPredType <- input$selecPredType
    selectedTypeDise <- input$selecTypeDise
    stats_aux <- stats
    
    if(selectedPredType == "Mortality" && selectedTypeDise == "sepsis-3") {
      stats_aux$HOSPITAL_EXPIRE_FLAG <- ifelse(stats_aux$HOSPITAL_EXPIRE_FLAG == 1, "Death", "Alive")
      var = "HOSPITAL_EXPIRE_FLAG"
    }
    
    else {
      stats_aux$THIRTYDAY_EXPIRE_FLAG <- ifelse(stats_aux$THIRTYDAY_EXPIRE_FLAG == 1, "Death", "Alive")
      var = "THIRTYDAY_EXPIRE_FLAG"
    }
    
    # Increase workspace size
    # options(expressions = 10000)
    
    table_summary <- 
      tbl_summary(
        data = stats_aux,
        by = all_of(var),
        statistic = all_continuous() ~ "{median} ({p25}, {p75}), {min}, {max}"
      ) %>%
      add_p() %>% as_gt()
  })
  
  # Feature Selection
  
  observe({
    features_s <- input$features
    stats_feat <- stats[, features_s]
    numeric_data <- stats_feat[sapply(stats_feat, is.numeric)]
    
    correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    correlation_matrix <- round(correlation_matrix, 2)
    correlation_df <- as.data.frame(correlation_matrix)
    
    output$correlation_table <- renderDT({
      datatable(
        correlation_df,
        options = list(scrollX = TRUE),
        callback = JS(
          "table.columns().every(function() {
          var column = this;
          var columnIndex = column.index();
          var colData = column.data();
          var colName = colData[0];
          var cellIdx = table.column(columnIndex).data().indexOf(colName);
          table.cell(cellIdx, columnIndex).nodes().to$().css({ 'text-align': 'center' });
        });"
        )
      )
    })
    
    cor_matrix <- cor(numeric_data)
    
    output$correlation_plot <- renderPlotly({
      heatmap <- plot_ly(
        x = colnames(cor_matrix),
        y = colnames(cor_matrix),
        z = cor_matrix,
        type = "heatmap",
        colorscale = "Viridis"
      )
      
      heatmap <- heatmap %>% layout(
        title = "Correlation Matrix",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
      
      heatmap
    })
  })
  
  ################## Plots ################
  
  selected_variable_hist <- reactive({
    input$variableHist
  })
  
  selected_variable_pie <- reactive({
    input$variablePieChar
  })
  
  selected_variable_barplot <- reactive({
    input$variableBarplot
  })
  
  selected_variable_Scatterplot <- reactive({
    input$variableScatterplot
  })
  
  output$histogramPlot <- renderPlotly({
    selected_col <- input$variableHist 
    hist_data <- selectF[[selected_col]]
    selectedTypeDise <- input$selecTypeDise
    
    title_text <- paste("Histogram of MIMIC-III patients with", selectedTypeDise, "by", selected_col)
    xaxis_label <- paste("Value of", selected_col)
    
    plot_ly(x = ~hist_data, type = "histogram", name = "Histogram") %>%
      layout(
        title = title_text,
        xaxis = list(title = xaxis_label),
        barmode = "group",  # Add this line to group bars
        bargap = 0.2  # Adjust the gap as needed (0.2 for 20% gap)
      )
  })
  
  output$pie_chart <- renderPlotly({
    selected_col <- input$variablePieChar
    data <- selectF
    selectedTypeDise <- input$selecTypeDise
    
    # Define the number of age groups you want for numeric variables
    num_groups_numeric <- 7 
    
    # Check if the variable contains only 0s and 1s
    if (is.numeric(data[[selected_col]]) && all(data[[selected_col]] %in% c(0, 1))) {
      # For binary numeric variables, treat them as categorical
      unique_values <- as.character(unique(data[[selected_col]]))
      
      breaks <- seq_along(unique_values)
      labels <- unique_values
      
      data$group <- factor(as.character(data[[selected_col]]), levels = unique_values)
    } 
    
    else if (is.numeric(data[[selected_col]])) {
      # For other numeric variables, create breaks and labels based on range
      min_val <- min(data[[selected_col]], na.rm = TRUE)
      max_val <- max(data[[selected_col]], na.rm = TRUE)
      
      # Check if min_val and max_val are finite
      if (is.finite(min_val) && is.finite(max_val)) {
        breaks <- seq(min_val, max_val, length.out = num_groups_numeric + 1)
        
        labels <- character(num_groups_numeric)
        for (i in 1:num_groups_numeric) {
          labels[i] <- paste(round(breaks[i], 2), round(breaks[i + 1], 2), sep = "-")
        }
        
        data$group <- cut(data[[selected_col]], breaks = breaks, labels = labels)
      }
    } 
    
    else {
      # For categorical variables, create breaks and labels based on unique values
      unique_values <- unique(data[[selected_col]])
      
      breaks <- seq_along(unique_values)
      labels <- unique_values
      
      data$group <- factor(data[[selected_col]], levels = unique_values)
    }
    
    # Create a table of counts
    var_counts <- table(data$group)
    
    pie_chart_data <- data.frame(Group = as.character(names(var_counts)), Count = as.numeric(var_counts))
    
    title_text <- paste("Pie Chart of MIMIC-III patients with", selectedTypeDise, "by", selected_col)
    
    plot_ly(pie_chart_data, labels = ~Group, values = ~Count, type = "pie") %>%
      layout(title = title_text)
  })
  
  
  output$barplot <- renderPlotly({
    selected_col <- input$variableBarplot
    data <- selectF
    selectedTypeDise <- input$selecTypeDise
    
    # Define the number of age groups you want for numeric variables
    num_groups_numeric <- 7 
    
    # Check if the variable contains only 0s and 1s
    if (is.numeric(data[[selected_col]]) && all(data[[selected_col]] %in% c(0, 1))) {
      # For binary numeric variables, treat them as categorical
      unique_values <- as.character(unique(data[[selected_col]]))
      
      breaks <- seq_along(unique_values)
      labels <- unique_values
      
      data$group <- factor(as.character(data[[selected_col]]), levels = unique_values)
    } 
    
    else if (is.numeric(data[[selected_col]])) {
      # For other numeric variables, create breaks and labels based on range
      min_val <- min(data[[selected_col]], na.rm = TRUE)
      max_val <- max(data[[selected_col]], na.rm = TRUE)
      
      # Check if min_val and max_val are finite
      if (is.finite(min_val) && is.finite(max_val)) {
        breaks <- seq(min_val, max_val, length.out = num_groups_numeric + 1)
        
        labels <- character(num_groups_numeric)
        for (i in 1:num_groups_numeric) {
          labels[i] <- paste(round(breaks[i], 2), round(breaks[i + 1], 2), sep = "-")
        }
        
        data$group <- cut(data[[selected_col]], breaks = breaks, labels = labels)
      }
    } 
    
    else {
      # For categorical variables, create breaks and labels based on unique values
      unique_values <- unique(data[[selected_col]])
      
      breaks <- seq_along(unique_values)
      labels <- unique_values
      
      data$group <- factor(data[[selected_col]], levels = unique_values)
    }
    
    # Create a table of counts
    var_counts <- table(data$group)
    
    barplot_data <- data.frame(Group = as.character(names(var_counts)), Count = as.numeric(var_counts))
    
    title_text <- paste("Bar Plot of MIMIC-III patients with", selectedTypeDise, "by", selected_col)
    
    plot_ly(barplot_data, x = ~Group, y = ~Count, type = "bar", marker = list(color = "blue")) %>%
      layout(
        title = title_text,
        xaxis = list(title = selected_col),
        yaxis = list(title = "Frequency")
      )
  })
  
  output$scatter_plot <- renderPlotly({
    selected_col <- input$variableScatterplot
    selectedTypeDise <- input$selecTypeDise
    data <- selectF
    bar_data <- data[[selected_col]]
    
    title_text <- paste("Scatter Plot of MIMIC-III patients with", selectedTypeDise, "by", selected_col)
    
    plot_ly(x = ~bar_data, type = "scatter", mode = "markers", marker = list(color = "blue")) %>%
      layout(
        title = title_text,
        xaxis = list(title = selected_col),
        yaxis = list(title = "Frequency")
      )
  })
  
  #observe({
  #  features_selected <- input$features
  #})
  
  # Observe the changes in selected features
  #output$selectedFeaturesOutput <- renderText({
  #  selected_features <- input$features
  #  if (length(selected_features) > 0) {
  # 
  #    selected_features_string <- paste(selected_features, collapse = ", ")
  #    return(paste("Selected Features: ", selected_features_string))
  #  } else {
  #    return("No features selected")
  #  }
  #})
  
  # Train Model
  
  output$cntTrain <- renderText({
    selected_value <- input$Slider1 # Calculate the train percentage
    paste("Selected Train Percentage:", selected_value, "%")
  })
  
  output$cntTest <- renderText({
    selected_value <- 100 - input$Slider1  # Calculate the test percentage
    paste("Selected Test Percentage:", selected_value, "%")
  })
  
  selected_features <- reactive({
    # Convert selected features to a character vector
    selected_feature_names <- as.character(input$features)
    
    # Create a data frame with only the selected features
    selected_data <- stats[, c(selected_feature_names)]
    
    return(selected_data)
  })
  
  trainedModel <- NULL
  
  # Define a function to train the model
  train_model <- function(model_to_train, selectedF_aux, split_percentage) {
    
    set.seed(1502)
    
    selectedPredType <- input$selecPredType
    selectedTypeDise <- input$selecTypeDise
    trained_model <- NULL
    dtrain <- NULL
    dteste <- NULL
    test_labels <- NULL
    selected_data <- selected_features()
    
    if (selectedPredType == "Mortality" && selectedTypeDise == "sepsis-3") {
      
      selectedF_aux$HOSPITAL_EXPIRE_FLAG <- stats$HOSPITAL_EXPIRE_FLAG
      target_variable <- selectedF_aux$HOSPITAL_EXPIRE_FLAG
      
      if (model_to_train == "XGBoost") {
        
        numberOfTrainingSamples <- round(length(target_variable) * split_percentage)
        dataMdoel <- data.matrix(selected_data)
        
        # training data
        train_data <- dataMdoel[1:numberOfTrainingSamples,]
        train_labels <- target_variable[1:numberOfTrainingSamples]
        
        # testing data
        test_data <- dataMdoel[-(1:numberOfTrainingSamples),]
        test_labels <- target_variable[-(1:numberOfTrainingSamples)]
        
        dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
        dtest <- xgb.DMatrix(data = test_data, label= test_labels)
        
        xgb_model <- xgboost(data = dtrain, nround = 10, objective = "binary:logistic")
        
        trained_model <- xgb_model
      } 
      
      else if (model_to_train == "Random Forest") {
        
        target_variable <- as.factor(target_variable)
        
        selectedF_aux$HOSPITAL_EXPIRE_FLAG = factor(selectedF_aux$HOSPITAL_EXPIRE_FLAG, levels = c(0, 1))
        
        num_samples <- nrow(selectedF_aux)
        num_train_samples <- round(num_samples * split_percentage)
        permuted_indices <- sample(num_samples)
        
        train_indices <- permuted_indices[1:num_train_samples]
        test_indices <- permuted_indices[(num_train_samples + 1):num_samples]
        
        training_set <- selectedF_aux[train_indices, ]
        test_set <- selectedF_aux[test_indices, ]
        
        rf <- randomForest(x = training_set[-ncol(training_set)], 
                           y = training_set$HOSPITAL_EXPIRE_FLAG,
                           ntree = 500, 
                           random_state = 0)
        
        trained_model <- rf
        dtest <- test_set
        
      } 
      
      else if (model_to_train == "Logistic Regression") {
        target_variable <- as.factor(target_variable)
        
        selectedF_aux$HOSPITAL_EXPIRE_FLAG = factor(selectedF_aux$HOSPITAL_EXPIRE_FLAG, levels = c(0, 1))
        
        num_samples <- nrow(selectedF_aux)
        num_train_samples <- round(num_samples * split_percentage)
        permuted_indices <- sample(num_samples)
        
        train_indices <- permuted_indices[1:num_train_samples]
        test_indices <- permuted_indices[(num_train_samples + 1):num_samples]
        
        training_set <- selectedF_aux[train_indices, ]
        test_set <- selectedF_aux[test_indices, ]
        
        model <- glm(HOSPITAL_EXPIRE_FLAG ~ ., data = training_set, family = binomial(link = "logit"))
        
        trained_model <- model
        dtest <- test_set
      } 
      
      
      predictions <- predict(trained_model, dtest, type = "response")
      
      if (model_to_train == "XGBoost") {
        confusion_matrix <- confusionMatrix(data = as.factor(ifelse(predictions > 0.5, 1, 0)), reference = as.factor(test_labels))
        roc <- roc(test_labels, predictions)
      }
      
      else if(model_to_train == "Random Forest") {
        
        confusion_matrix <- confusionMatrix(predictions, dtest$HOSPITAL_EXPIRE_FLAG)
        true_labels <- ifelse(dtest$HOSPITAL_EXPIRE_FLAG == "1", 1, 0)
        roc <- roc(true_labels, as.numeric(predictions))
        
      }
      
      else if (model_to_train == "Logistic Regression") {
        threshold <- 0.5
        binary_predictions <- ifelse(predictions > threshold, 1, 0)
        
        confusion_matrix <- confusionMatrix(data = factor(binary_predictions, levels = c(0, 1)), 
                                            reference = dtest$HOSPITAL_EXPIRE_FLAG)
        
        true_labels <- ifelse(dtest$HOSPITAL_EXPIRE_FLAG == "1", 1, 0)
        roc <- roc(true_labels, as.numeric(predictions))
      }
      
      return(list(
        trained_model = trained_model,
        dtest = dtest,
        test_labels = test_labels,
        confusion_matrix = confusion_matrix,
        predictions = predictions,
        roc = roc
      ))
    } 
    
    else if (selectedPredType == "30-days mortality" && selectedTypeDise == "sepsis-3") {
      target_variable <- stats$THIRTYDAY_EXPIRE_FLAG
      selectedF_aux$THIRTYDAY_EXPIRE_FLAG <- stats$THIRTYDAY_EXPIRE_FLAG
      
      if (model_to_train == "XGBoost") {
        
        numberOfTrainingSamples <- round(length(target_variable) * split_percentage)
        dataMdoel <- data.matrix(selected_data)
        
        # training data
        train_data <- dataMdoel[1:numberOfTrainingSamples,]
        train_labels <- target_variable[1:numberOfTrainingSamples]
        
        # testing data
        test_data <- dataMdoel[-(1:numberOfTrainingSamples),]
        test_labels <- target_variable[-(1:numberOfTrainingSamples)]
        
        dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
        dtest <- xgb.DMatrix(data = test_data, label= test_labels)
        
        xgb_model <- xgboost(data = dtrain, nround = 10, objective = "binary:logistic")
        
        trained_model <- xgb_model
      } 
      
      else if (model_to_train == "Random Forest") {
        target_variable <- as.factor(target_variable)
        selectedF_aux$THIRTYDAY_EXPIRE_FLAG = factor(selectedF_aux$THIRTYDAY_EXPIRE_FLAG, levels = c(0, 1))
        
        num_samples <- nrow(selectedF_aux)
        num_train_samples <- round(num_samples * split_percentage)
        permuted_indices <- sample(num_samples)
        
        train_indices <- permuted_indices[1:num_train_samples]
        test_indices <- permuted_indices[(num_train_samples + 1):num_samples]
        
        training_set <- selectedF_aux[train_indices, ]
        test_set <- selectedF_aux[test_indices, ]
        
        rf <- randomForest(x = training_set[-ncol(training_set)], 
                           y = training_set$THIRTYDAY_EXPIRE_FLAG,
                           ntree = 500, 
                           random_state = 0)
        
        trained_model <- rf
        dtest <- test_set
        
      } 
      
      else if (model_to_train == "Logistic Regression") {
        target_variable <- as.factor(target_variable)
        
        selectedF_aux$THIRTYDAY_EXPIRE_FLAG = factor(selectedF_aux$THIRTYDAY_EXPIRE_FLAG, levels = c(0, 1))
        
        num_samples <- nrow(selectedF_aux)
        num_train_samples <- round(num_samples * split_percentage)
        permuted_indices <- sample(num_samples)
        
        train_indices <- permuted_indices[1:num_train_samples]
        test_indices <- permuted_indices[(num_train_samples + 1):num_samples]
        
        training_set <- selectedF_aux[train_indices, ]
        test_set <- selectedF_aux[test_indices, ]
        
        model <- glm(THIRTYDAY_EXPIRE_FLAG ~ ., data = training_set, family = binomial(link = "logit"))
        
        trained_model <- model
        dtest <- test_set
        
      } 
      
      
      predictions <- predict(trained_model, dtest, type = "response")
      
      if (model_to_train == "XGBoost") {
        confusion_matrix <- confusionMatrix(data = as.factor(ifelse(predictions > 0.5, 1, 0)), reference = as.factor(test_labels))
        roc <- roc(test_labels, predictions)
      }
      
      else if(model_to_train == "Random Forest") {
        
        confusion_matrix <- confusionMatrix(predictions, dtest$THIRTYDAY_EXPIRE_FLAG)
        true_labels <- ifelse(dtest$THIRTYDAY_EXPIRE_FLAG == "1", 1, 0)
        roc <- roc(true_labels, as.numeric(predictions))
      }
      
      else if (model_to_train == "Logistic Regression") {
        threshold <- 0.5
        binary_predictions <- ifelse(predictions > threshold, 1, 0)
        
        confusion_matrix <- confusionMatrix(data = factor(binary_predictions, levels = c(0, 1)), 
                                            reference = dtest$THIRTYDAY_EXPIRE_FLAG)
        
        true_labels <- ifelse(dtest$THIRTYDAY_EXPIRE_FLAG == "1", 1, 0)
        roc <- roc(true_labels, as.numeric(predictions))
      }
      
      return(list(
        trained_model = trained_model,
        dtest = dtest,
        test_labels = test_labels,
        confusion_matrix = confusion_matrix,
        predictions = predictions,
        roc = roc
      ))
      
    }
    
    return(NULL)
  }
  
  evaluateModel <- function(confusion_matrix, roc) {
    accuracy <- confusion_matrix$overall["Accuracy"]
    precision <- confusion_matrix$byClass["Pos Pred Value"]
    recall <- confusion_matrix$byClass["Sensitivity"]
    f1_score <- confusion_matrix$byClass["F1"]
    
    
    output$accuracy_text <- renderText({
      paste("Accuracy:", round(accuracy, 4))
    })
    
    output$precision_text <- renderText({
      paste("Precision:", round(precision, 4))
    })
    
    output$recall_text <- renderText({
      paste("Recall:", round(recall, 4))
    })
    
    output$f1_text <- renderText({
      paste("F1-Score:", round(f1_score, 4))
    })
    
    auc_score <- auc(roc)  # Calculate ROC-AUC
    
    output$roc_auc_text <- renderText({
      paste("ROC-AUC:", round(auc_score, 4))
    })
    
    output$roc_plot <- renderPlot({
      plot(roc, main = "Receiver Operating Characteristic (ROC) Curve")
    })
    
    output$confusion_matrix <- renderPrint({
      confusion_matrix
    })
    
    output$accuracy_trained <- renderText({
      paste("Accuracy:", round(accuracy, 4))
    })
    
    output$precision_trained <- renderText({
      paste("Precision:", round(precision, 4))
    })
    
    output$recall_trained <- renderText({
      paste("Recall:", round(recall, 4))
    })
    
    output$f1_trained <- renderText({
      paste("F1-Score:", round(f1_score, 4))
    })
    
    auc_score <- auc(roc)  # Calculate ROC-AUC
    
    output$roc_auc_trained <- renderText({
      paste("ROC-AUC:", round(auc_score, 4))
    })
    
    output$roc_plot_trained <- renderPlot({
      plot(roc, main = "Receiver Operating Characteristic (ROC) Curve")
    })
    
    output$confusion_matrix_trained <- renderPrint({
      confusion_matrix
    })
    
  }
  
  observeEvent(input$train_button, {
    
    selected_model <- input$Select_model
    selectedF_aux <- selected_features()
    #split_percentage <- input$Slider1 / 100
    split_percentage <- as.numeric(input$Slider1) / 100
    
    training_results <- train_model(selected_model, selectedF_aux, split_percentage)
    trainedModel <<- training_results
    
    if (!is.null(training_results)) {
      trained_model <- training_results$trained_model
      dtest <- training_results$dtest
      test_labels <- training_results$test_labels
      confusion_matrix <- training_results$confusion_matrix
      predictions <- training_results$predictions
      roc <- training_results$roc
      
      if (!is.null(trained_model)) {
        evaluateModel(confusion_matrix, roc)
        
      }
    }
  })
  
  
  # Model Comparison
  
  observe({
    selected_model <- input$Select_model
    
    output$selectedModel <- renderText({
      if (selected_model == "") {
        "No model trained"
      } 
      
      else {
        paste("Selected model:", selected_model)
      }
    })
    
    updateSelectInput(session, "compare", choices = setdiff(c("XGBoost", "Random Forest", "Logistic Regression"), selected_model))
  })
  
  observeEvent(input$compare, {
    selected_model <- input$compare
    selectedF_aux <- selected_features()
    split_percentage <- as.numeric(input$Slider1) / 100
    
    training <- train_model(selected_model, selectedF_aux, split_percentage)
    if (!is.null(training)) {
      # Calculate ROC for the compared model
      roc_compare <- training$roc
      confusion_matrix <- training$confusion_matrix
      
      accuracy <- confusion_matrix$overall["Accuracy"]
      precision <- confusion_matrix$byClass["Pos Pred Value"]
      recall <- confusion_matrix$byClass["Sensitivity"]
      f1_score <- confusion_matrix$byClass["F1"]
      
      # Render metrics for the compared model
      output$accuracy_compare <- renderText({
        paste("Accuracy for", selected_model, ":", round(accuracy, 4))
      })
      
      output$precision_compare <- renderText({
        paste("Precision for", selected_model, ":", round(precision, 4))
      })
      
      output$recall_compare <- renderText({
        paste("Recall for", selected_model, ":", round(recall, 4))
      })
      
      output$f1_compare <- renderText({
        paste("F1-Score for", selected_model, ":", round(f1_score, 4))
      })
      
      # Calculate ROC-AUC for compared model
      auc_score1 <- auc(roc_compare)
      output$roc_auc_compare <- renderText({
        paste("ROC-AUC for", selected_model, ":", round(auc_score1, 4))
      })
      
      roc_main <- paste("ROC Curve for", selected_model)
      
      # Render ROC plot for compared model
      output$roc_plot_compare <- renderPlot({
        plot(roc_compare, main = roc_main)
      })
      
      # Render confusion matrix for compared model
      output$confusion_matrix_compare <- renderPrint({
        confusion_matrix
      })
    }
    
  })
  
  # Model Comparison
  
  observe({
    selected_model <- input$Select_model
    
    output$selectedModel <- renderText({
      if (selected_model == "") {
        "No model trained"
      } 
      
      else {
        paste("Selected model:", selected_model)
      }
    })
    
    updateSelectInput(session, "compare", choices = setdiff(c("XGBoost", "Random Forest", "Logistic Regression"), selected_model))
  })
  
  observeEvent(input$compare, {
    selected_model <- input$compare
    selectedF_aux <- selected_features()
    split_percentage <- as.numeric(input$Slider1) / 100
    
    training <- train_model(selected_model, selectedF_aux, split_percentage)
    if (!is.null(training)) {
      # Calculate ROC for the compared model
      roc_compare <- training$roc
      confusion_matrix <- training$confusion_matrix
      
      accuracy <- confusion_matrix$overall["Accuracy"]
      precision <- confusion_matrix$byClass["Pos Pred Value"]
      recall <- confusion_matrix$byClass["Sensitivity"]
      f1_score <- confusion_matrix$byClass["F1"]
      
      # Render metrics for the compared model
      output$accuracy_compare <- renderText({
        paste("Accuracy for", selected_model, ":", round(accuracy, 4))
      })
      
      output$precision_compare <- renderText({
        paste("Precision for", selected_model, ":", round(precision, 4))
      })
      
      output$recall_compare <- renderText({
        paste("Recall for", selected_model, ":", round(recall, 4))
      })
      
      output$f1_compare <- renderText({
        paste("F1-Score for", selected_model, ":", round(f1_score, 4))
      })
      
      # Calculate ROC-AUC for compared model
      auc_score1 <- auc(roc_compare)
      output$roc_auc_compare <- renderText({
        paste("ROC-AUC for", selected_model, ":", round(auc_score1, 4))
      })
      
      roc_main <- paste("ROC Curve for", selected_model)
      
      # Render ROC plot for compared model
      output$roc_plot_compare <- renderPlot({
        plot(roc_compare, main = roc_main)
      })
      
      # Render confusion matrix for compared model
      output$confusion_matrix_compare <- renderPrint({
        confusion_matrix
      })
    }
    
  })
  
  # Make predictions
  
  output$numeric_inputs <- renderUI({
    selected_features_list <- input$features
    
    if (is.null(selected_features_list) || length(selected_features_list) == 0) {
      return(NULL)
    }
    
    numeric_inputs <- lapply(selected_features_list, function(feature_name) {
      
      feature_values <- stats[[feature_name]]
      feature_min <- min(feature_values)
      feature_max <- max(feature_values)
      
      numericInput(
        inputId = paste0("input_", feature_name),
        label = feature_name,
        value = feature_min,
        min = feature_min,
        max = feature_max
      )
    })
    
    do.call(tagList, numeric_inputs)
  })
  
  observeEvent(input$predict_button, {
    
    selectedPredType <- input$selecPredType
    selectedTypeDise <- input$selecTypeDise
    pred <- NULL
    
    if (!is.null(trainedModel$trained_model)) {
      input_values <- list()
      
      # Retrieve the values from the numeric inputs
      selected_features_list <- input$features
      for (feature_name in selected_features_list) {
        input_id <- paste0("input_", feature_name)
        input_values[[feature_name]] <- as.numeric(input[[input_id]])
      }
      
      if (input$Select_model == "XGBoost") {
        input_matrix <- do.call(cbind, input_values)
        
        input_data <- xgb.DMatrix(data = input_matrix)
        
        predictions <- predict(trainedModel$trained_model, input_data, type = "response")
        pred <- predictions
        
      } 
      
      else {
        input_val <- as.data.frame(input_values)
        input_tibble <- as_tibble(input_val)
        predictions <- predict(trainedModel$trained_model, input_tibble, type = "response")
        pred <- predictions
      }
      
      
      output$prediction_result_box <- renderText({
        
        if (is.null(trainedModel) || is.null(trainedModel$trained_model)) {
          return("Please train a model before making predictions.")
        }
        
        if (selectedPredType == "Mortality" && selectedTypeDise == "sepsis-3") {
          result <- ifelse(predictions >= 0.5, "The model predicts sepsis mortality. Positive (1).", "The model predicts no sepsis mortality. Negative (0).")
        } 
        
        else if (selectedPredType == "30-days mortality" && selectedTypeDise == "sepsis-3") {
          result <- ifelse(predictions >= 0.5, "The model predicts sepsis mortality within 30 days. Positive (1).", "The model predicts no sepsis mortality within 30 days. Negative (0).")
        }
        
        return(result)
      })
      
    }
    
    else {
      output$prediction_result_box <- renderText({
        "Please train a model before making predictions."
      })
    }
  })

  
  ################ Information Button #################
  
  output$sub_option_ui <- renderUI({
    req(input$info_select)  # Check that input$info_select is not empty or missing
    
    if (input$info_select %in% c("Dashboard")) {
      return("Explicação da dashboard")
    } 
    
    else if (input$info_select %in% c("Predictions")) {
      return("Explicação de Predictions")
    } 
    
    else if (input$info_select == "Patients") {
      selectInput("sub_option_select", "Select a submenu item",
                  choices = c("", "General Patient Info", "Pacient Search by ID"))
    } 
    
    else if (input$info_select == "Admissions") {
      selectInput("sub_option_select", "Select a submenu item",
                  choices = c("", "General Admissions info", "Search by admission ID"))
    } 
    
    else if (input$info_select == "Diagnoses") {
      selectInput("sub_option_select", "Select a submenu item",
                  choices = c("", "General Diagnoses info", "Specific ICD9 Group details", "Diagnoses search by ICD", "First Diagnoses - comparison", "Diagnoses search by patient ID"))
    } 
    
    else if (input$info_select == "ICU") {
      selectInput("sub_option_select", "Select a submenu item",
                  choices = c("", "General ICU stays Info", "Patient Mortality"))
    } 
    
    else {
      NULL
    }
  })
  
  # Render the text output UI conditionally based on the selected sub-option
  output$info_text_ui <- renderUI({
    if (!is.null(input$sub_option_select) && input$sub_option_select != "") {
      textOutput("info_text")
    } else {
      NULL
    }
  })
  
  # Define the app information text based on the selected sub-option
  output$info_text <- renderText({
    selected_sub_option <- input$sub_option_select
    app_info_text <- switch(selected_sub_option,
                            "General Patient Info" = "General Patient Info information" , 
                            "Pacient Search by ID" = "Pacient Search by ID information",
                            
                            "General Admissions info" = "General Admissions info information", 
                            "Search by admission ID" = "Search by admission ID information",
                            
                            "General Diagnoses info" = "General Diagnoses info information", 
                            "Specific ICD9 Group details" = "Specific ICD9 Group details information", 
                            "Diagnoses search by ICD" = "Diagnoses search by ICD information", 
                            "First Diagnoses - comparison" = "First Diagnoses - comparison information", 
                            "Diagnoses search by patient ID" = "Diagnoses search by patient ID information",
                            
                            "General ICU stays Info" = "General ICU stays Info information", 
                            "Patient Mortality" = "Patient Mortality information",
                            "")
    return(app_info_text)
  })
  
  
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })
  
})

shinyApp(ui, server)

#TODO 18/7/2022

#COMPARAR PERCURSO DO DOENTE PARA UMA DOEN???A ( onde tenha muitos outliers). Um outlier vs poucos dias. 1 aleatorio. 
# + grafico caregivers . Mostrar itens do paciente internado + numero de caregivers + exames? 


#METER AGE NO SEARCH BY PATIENT ID