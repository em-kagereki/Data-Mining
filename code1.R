#https://github.com/umbertogriffo/Predictive-Maintenance-using-LSTM
library(dplyr)
library(stringr)
library(tidyverse)
library(tidyr)
setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
admin<-read.csv("ADMISSIONS.csv")
admin <- admin %>% select(SUBJECT_ID,HADM_ID,ADMITTIME,DISCHTIME,ADMISSION_TYPE,
                          ADMISSION_LOCATION,DISCHARGE_LOCATION,INSURANCE,
                          LANGUAGE,RELIGION,MARITAL_STATUS,ETHNICITY,DIAGNOSIS) # The hospital flag variable is duplicated in the patient table

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
data <- select(data,  -ADMITTIME, -DISCHTIME,-DOB)
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


library(alphaOutlier)
data$Outlier<- aout.pois(data = data$admissionCycle, param = median(data$admissionCycle), alpha = 0.01)

# https://www.tandfonline.com/doi/abs/10.1080/01621459.1993.10476339


#data<- data %>% 
#  dplyr::filter(admissionCycle <5 ) 




