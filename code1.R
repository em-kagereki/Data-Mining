#https://github.com/umbertogriffo/Predictive-Maintenance-using-LSTM
library(dplyr)
library(stringr)
library(tidyverse)
library(tidyr)
library(dlookr)
library(table1)
library(alphaOutlier)
library(boot) 
library(htmlTable)
library(ggplot2)
library(scales)
library(factoextra)
library(lubridate)
library(gtsummary)
library(tm)
library(tidytext)
library(data.table)
#suppressWarnings(dlookr)

## 
##Management of Acute Coronary Syndromes
##
##https://www.frontiersin.org/articles/10.3389/fcvm.2020.593496/full
##



setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
admin<-read.csv("ADMISSIONS.csv")

admin <- admin %>% select(SUBJECT_ID,HADM_ID,ADMITTIME,DISCHTIME,ADMISSION_TYPE,
                          ADMISSION_LOCATION,DISCHARGE_LOCATION,INSURANCE,
                          LANGUAGE,RELIGION,MARITAL_STATUS,ETHNICITY,DIAGNOSIS) # The hospital flag variable is duplicated in the patient table


text <- tibble(line = 1:nrow(admin), text = admin$DIAGNOSIS)
text<-text %>%
  unnest_tokens(word, text)
text<-data.frame(text)
text2<-text %>%
  group_by(line) %>%
  mutate(unique_types = n_distinct(word))
commonWords<-data.frame(table(unique(text)$word)[as.character(text$word)])
candidateKeyWords<-commonWords[!duplicated(commonWords$Var1), ]
names(candidateKeyWords)[names(candidateKeyWords) == "Var1"] <- "Word"



pt<-read.csv("PATIENTS.csv")
pt <- pt %>% select(SUBJECT_ID, GENDER, DOB,EXPIRE_FLAG)

data<-merge(x = admin, y = pt, by = "SUBJECT_ID", all.x = TRUE)

 


data$ADMITTIME <- as.Date(data$ADMITTIME)
data$DISCHTIME <- as.Date(data$DISCHTIME)
data$DOB <- as.Date(data$DOB)
data$AGE<-round(as.numeric(difftime(data$ADMITTIME,data$DOB, units = "days")/365),0)
data$LOS1<-as.numeric(difftime(data$DISCHTIME,data$ADMITTIME, units = "hours"))


## Convert into the year

# If the age age was 89 or more, they added 300! We factor that in
## We then get the admission period
data<-data %>%
  mutate(AGE = ifelse(AGE<300, AGE, AGE-211)) %>% 
  mutate(Period = ntile(as.numeric(data$ADMITTIME),12))


## Create a column on the number of admissions 
## This must be removed from the model - Data leakage!!
data<-data %>%
  group_by(SUBJECT_ID) %>%
  arrange(DISCHTIME) %>%
  mutate(nAdmissions = n_distinct(HADM_ID)) 


## Get the admission cycle

data <- data %>%
  group_by(SUBJECT_ID) %>%
  mutate(admissionCycle = 1:n())

## The EXPIRE_FLAG should only be there when the patient actually dies!!!! Otherwise even if the patient has
## Multiple admissions and then dies in the last one, ALL the admissions carry the death tag!! This needs to be changed

# https://stackoverflow.com/questions/30606360/subtract-value-from-previous-row-by-group
## This is not correct - To come back!!!!!
data <- data %>%
  group_by(SUBJECT_ID) %>%
  arrange(admissionCycle) %>%
  mutate(timeToNextAdmission = as.numeric(difftime(DISCHTIME, lag(ADMITTIME, default = first(DISCHTIME)), units = "hours")))
data <- select(data, -DISCHTIME,-DOB)
## Data cleaning with the diagnosis
icustay<-read.csv("ICUSTAYS.csv")
icustay <- icustay %>%
  group_by(HADM_ID) %>%
  mutate(stayTransfers = 1:n())%>%
  mutate(LOS = sum(LOS))
## We select the number
icustay<-icustay %>%
  group_by(HADM_ID) %>%
  slice(which.max(stayTransfers))
icustay <- icustay %>% select(HADM_ID, LOS, stayTransfers)
data<-merge(x = data, y = icustay, by = "HADM_ID", all.x = TRUE)
rm(admin)
rm(pt)
rm(icustay)
## Encoding:

data$admissionCycle<-as.numeric(data$admissionCycle)


data <- data %>% mutate_all(na_if,"")
data$MARITAL_STATUS<-ifelse(data$MARITAL_STATUS=="UNKNOWN (DEFAULT)",'UNKNOWN', data$MARITAL_STATUS)
data$MARITAL_STATUS <-data$MARITAL_STATUS %>% replace_na("UNKNOWN")


data$EXPIRE_FLAG<-as.numeric(data$EXPIRE_FLAG)
data2<-data %>% 
  dplyr::mutate(Readmission = EXPIRE_FLAG) %>% 
  filter(admissionCycle==1)


###
##It also follows that the last admission could either lead to loss to follow-up if the person does not die in hospital
##

dat1<-data %>%
  dplyr::select(SUBJECT_ID,HADM_ID,admissionCycle) %>% 
  group_by(SUBJECT_ID) %>%
  slice(which.max(admissionCycle)) %>% 
  mutate(Max = 'True') %>% 
  dplyr::select(-admissionCycle)

data<-merge(data,dat1, by='HADM_ID', all.x=TRUE) %>% 
    dplyr::select(-SUBJECT_ID.y)

data$Max<-data$Max %>% replace_na("False")

names(data)[names(data) == "SUBJECT_ID.x"] <- "SUBJECT_ID"

data$Outcome <- ifelse(data$Max=="True" & data$EXPIRE_FLAG==1,"Deceased", "Readmitted")
data$Outcome <- ifelse(data$Max=="True" & data$EXPIRE_FLAG==0,"Loss to followup", data$Outcome)

data$Outcome <- ifelse(data$Max=="False" & data$EXPIRE_FLAG==1 & data$nAdmissions==1,"Deceased", data$Outcome)
data$Outcome <- ifelse(data$Max=="False" & data$EXPIRE_FLAG==0 & data$nAdmissions==1,"Loss to followup", data$Outcome)

data<-data %>% 
  mutate(year = year(ADMITTIME),month=month(ADMITTIME),day = mday(ADMITTIME),PeriodtoYear = (2011+Period)) %>% 
  mutate(decodedAdminDate = make_datetime(PeriodtoYear, month, day)) %>% 
  select(-year,-month,-day,-PeriodtoYear,-Period)
