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

#knitr::write_bib(c(.packages(), "data.table"), "packages.bib")



setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
# The following tables are used to define and track patient stays:
admin<-read.csv("ADMISSIONS.csv") 

pt<-read.csv("PATIENTS.csv")
pt <- pt %>% select(SUBJECT_ID, GENDER, DOB,EXPIRE_FLAG)

admin<-merge(x = admin, y = pt, by = "SUBJECT_ID", all.x = TRUE) %>% 
  mutate(ADMITTIME = ymd_hms(ADMITTIME)) %>% 
  mutate(DISCHTIME = ymd_hms(DISCHTIME)) %>% 
  mutate(DEATHTIME = ymd_hms(DEATHTIME)) %>% 
  mutate(DOB = ymd_hms(DOB)) %>%
  mutate(AGE = round(as.numeric(difftime(ADMITTIME,DOB, units = "days")/365),0)) %>% 
  mutate(LOS2 = as.numeric(difftime(DISCHTIME,ADMITTIME, units = "hours"))) %>% 
  mutate(AGE = ifelse(AGE<300, AGE, AGE-211)) %>% 
  mutate(Period = ntile(as.numeric(ADMITTIME),12)) %>% 
  mutate(endGoldenHour = ADMITTIME + minutes(60)) #dhours

## The admission cycle
admin <- admin %>%
  group_by(SUBJECT_ID) %>%
  mutate(admissionCycle = 1:n())

## Total number of admissions
admin<-admin %>%
  group_by(SUBJECT_ID) %>%
  arrange(DISCHTIME) %>%
  mutate(nAdmissions = n_distinct(HADM_ID)) 


admin1 <- admin %>%
  mutate(deadBefore = as.numeric(DEATHTIME-endGoldenHour)) %>% 
  mutate(deadBefore = ifelse(HOSPITAL_EXPIRE_FLAG==0, 0, deadBefore)) %>%  ## Remove all the patients who died before the cut-off
  filter(deadBefore>=0)


deadBefore<-admin %>% 
  mutate(deadBefore = as.numeric(DEATHTIME-endGoldenHour)) %>% 
  mutate(deadBefore = ifelse(HOSPITAL_EXPIRE_FLAG==1, 0, deadBefore)) %>%  ## Remove all the patients who died before the cut-off
  filter(deadBefore==0)


admin1$DIAGNOSIS2<-tolower(admin1$DIAGNOSIS)
keyword<- c("stemi","acute coronary syndrome","angina","tachycardia","aortic aneurysm","pericardi","ortic dissection",
            "coronary artery dissection","cardiomyopathy","heart failure","mitral valve disease","mitral stenosis",
            "coronary artery disease","chf","congestive heart failure","heart failure","telemetry","myocardial infaction",
            "cardiac arrest","myocardial infarction","aortic stenosis","st elevated","pericardial effusion", "cardiomyopathy",
            "cath lab","tamponade","tamponede")

cardiacSyndromes <- admin1 %>% 
  filter( grepl(paste(keyword, collapse="|"),DIAGNOSIS2)) %>% 
  select(-DIAGNOSIS2,-ROW_ID) %>% 
  mutate(EXPIRE_FLAG = ifelse(EXPIRE_FLAG==1 & nAdmissions>admissionCycle, 0, EXPIRE_FLAG)) ## To pick out exactly the cycle the patient died!



cardiacSyndromes2<-cardiacSyndromes %>% select(SUBJECT_ID,HADM_ID,endGoldenHour)


Loinc <- read.csv("Loinc.csv") %>% 
  mutate(LOINC_CODE = LOINC_NUM) %>% 
  select(LOINC_CODE,COMPONENT,CLASS)
#write.csv(cardiacSyndromes,"cardiacSyndromes.csv")

#lab<-read.csv("LABEVENTS.csv") %>% 
#  select(HADM_ID,SUBJECT_ID,FLUID,LABEL,FLAG,CHARTTIME)

#data2<-data[data$SUBJECT_ID %in% cardiac$SUBJECT_ID, ]

#lab %>%
 # filter(gene_ID %in% accessions40$V1)


#labiTem<-read.csv("D_LABITEMS.csv")
#lab2<-merge(x=lab, y=labiTem, by="ITEMID", all.x = TRUE) %>% 
#  filter(ROW_ID>1) %>% ## The first row contains the test lable and not the actual test
#  unite(combined, FLUID, LABEL, sep = "-", remove = FALSE) %>% 
#  select(HADM_ID,CHARTTIME,FLAG,combined) %>% 
#  mutate_if(is.character, list(~na_if(.,""))) %>%   ## Replace the blanks
#  mutate(CHARTTIME = ymd_hms(CHARTTIME))

#lab3<-merge(x=cardiacSyndromes2,y=lab2,by='HADM_ID',x.all=TRUE) %>% 
#  mutate(Checktime = ifelse(endGoldenHour>=CHARTTIME, "After", "Before")) %>% 
#  filter(Checktime=="Before",FLAG=="abnormal") %>% 
 # select(-Checktime) %>% 
 # select(HADM_ID,combined,FLAG) %>% 
 # unite(combined, combined, FLAG, sep = "_", remove = FALSE) %>% 
 # count(HADM_ID, combined, sort = TRUE)


#labWide <- lab3 %>% 
#  spread(combined, n) %>% 
#  replace(is.na(.), 0)


#labMerge<-merge(x=cardiacSyndromes2, y=labWide, by="HADM_ID", all.x = TRUE)

labNew<-read.csv("newdata.csv") %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  drop_na(LOINC_CODE) %>% 
  unite(combined, LOINC_CODE,FLAG, sep = "-", remove = FALSE) %>% 
  select(-FLAG,-LABEL,-FLUID,LOINC_CODE,-X)
  
lab10<-merge(x=labNew, y = Loinc, by = "LOINC_CODE", x.ll=TRUE)

## The labs in those missing the HADM were done as outpatients
missingHADM<-labNew %>% 
  filter(is.na(HADM_ID)) 

missingHADM2<- merge(x=missingHADM,y=admin,by="SUBJECT_ID", x.all=TRUE) %>% 
  select(SUBJECT_ID,HADM_ID.x,HADM_ID.y,CHARTTIME,ADMITTIME,DISCHTIME,combined,LOINC_CODE) %>% 
  mutate(Checktime = ifelse(CHARTTIME>=ADMITTIME & CHARTTIME<=DISCHTIME, "After", "Before")) %>% 
  filter(Checktime=="Before") %>% 
  mutate(HADM_ID = HADM_ID.y) %>% 
  select(SUBJECT_ID,HADM_ID,CHARTTIME,combined,LOINC_CODE)

## These had admission numbers - Labs were done in patient
presentHADM<-labNew %>% 
  drop_na(HADM_ID)

allLAbs<-bind_rows(presentHADM, missingHADM2)

lab3<-merge(x=cardiacSyndromes2,y=allLAbs,by='HADM_ID',x.all=TRUE) %>% 
  mutate(Checktime = ifelse(endGoldenHour>=CHARTTIME, "After", "Before")) %>% 
  filter(Checktime=="Before") %>% 
  select(-Checktime) %>% 
  select(HADM_ID,combined) %>% 
  count(HADM_ID, combined, sort = TRUE)

## Convert to wide
labWide <- lab3 %>% 
  spread(combined, n) %>% 
  replace(is.na(.), 0)

services<-read.csv("SERVICES.csv")
services2<-merge(x=cardiacSyndromes2,y=services,by='HADM_ID',x.all=TRUE) %>% 
  mutate(TRANSFERTIME2 = ymd_hms(TRANSFERTIME)) %>% 
  mutate(Checktime = ifelse(endGoldenHour>=TRANSFERTIME2, "After", "Before")) %>% 
  filter(Checktime=="Before") %>% 
  select(HADM_ID,PREV_SERVICE,CURR_SERVICE) %>% 
  unite(cService, PREV_SERVICE, CURR_SERVICE, sep = "_", remove = FALSE) %>% 
  select(HADM_ID,cService)

services3<-services2 %>% count(HADM_ID, cService, sort = TRUE)

servicesWide <- services3 %>% 
  spread(cService, n) %>% 
  replace(is.na(.), 0)


servicesMerge<-merge(x=cardiacSyndromes2, y=servicesWide, by="HADM_ID", all.x = TRUE) %>% 
  replace(is.na(.), 0)

checkServicesBeforeAdmin<-servicesMerge %>% filter(HADM_ID==0) 
nrow(checkServicesBeforeAdmin)


procedures<-read.csv("PROCEDUREEVENTS_MV.csv")
D_ITEM<-read.csv("D_ITEMS.csv")
procedures<-merge(x=procedures,y=D_ITEM, by = "ITEMID",x.all=TRUE) %>% 
  select(SUBJECT_ID,HADM_ID,STARTTIME,ENDTIME,LABEL)

proceduresForProcess<-procedures
procedures2<-merge(x=procedures,y=cardiacSyndromes2, by = "HADM_ID",x.all=TRUE) %>% 
  mutate(STARTTIME2 = ymd_hms(STARTTIME)) %>% 
  mutate(Checktime = ifelse(endGoldenHour>=STARTTIME2, "After", "Before")) %>% 
  filter(Checktime=="Before") %>% 
  select(HADM_ID,LABEL) 

procedures3<-procedures2 %>% count(HADM_ID, LABEL, sort = TRUE)

procWide <- procedures3 %>% 
  spread(LABEL, n) %>% 
  replace(is.na(.), 0)

procMerge<-merge(x=cardiacSyndromes2, y=procWide, by="HADM_ID", all.x = TRUE) %>% 
  replace(is.na(.), 0)

microb<-read.csv("MICROBIOLOGYEVENTS.csv") %>% 
  select(-ROW_ID,-SUBJECT_ID)

microb2<-merge(x=cardiacSyndromes2,y=microb,by='HADM_ID',x.all=TRUE) %>% 
  mutate(CHARTTIME = ymd_hms(CHARTTIME)) %>% 
  mutate(Checktime = ifelse(endGoldenHour>=CHARTTIME, "After", "Before")) %>% 
  filter(Checktime=="Before") %>% 
  unite(combined, SPEC_TYPE_DESC, ORG_ITEMID, sep = "_", remove = FALSE) %>% 
  unite(combined, combined, AB_NAME, sep = "_", remove = FALSE) %>% 
  unite(combined, combined, INTERPRETATION, sep = "_", remove = FALSE) %>% 
  select(HADM_ID,combined) %>% 
  count(HADM_ID, combined, sort = TRUE)

microbWide <- microb2 %>% 
  spread(combined, n) %>% 
  replace(is.na(.), 0)


#write.csv(microbWide,"microbWide.csv")

#microbMerge<-merge(x=cardiacSyndromes2, y=microbWide, by="HADM_ID", all.x = TRUE)


#write.csv(med2,"med2.csv")

#target <- c(cardiacSyndromes$HADM_ID)
#med2<-medication %>% 
#  filter(HADM_ID %in% target)
#write.csv(med2,"med2.csv")




library(bupaR)

