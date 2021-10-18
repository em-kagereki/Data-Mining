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
library(BurStMisc)


setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
# The following tables are used to define and track patient stays:
admin<-read.csv("ADMISSIONS.csv") 

pt<-read.csv("PATIENTS.csv")
#pt <- pt %>% select(SUBJECT_ID, GENDER, DOB,EXPIRE_FLAG)

admin<-merge(x = admin, y = pt, by = "SUBJECT_ID", all.x = TRUE) %>% 
  mutate(ADMITTIME = ymd_hms(ADMITTIME)) %>% 
  mutate(DISCHTIME = ymd_hms(DISCHTIME)) %>% 
  mutate(DEATHTIME = ymd_hms(DEATHTIME)) %>% 
  mutate(DOB = ymd_hms(DOB)) %>%
  mutate(AGE = round(as.numeric(difftime(ADMITTIME,DOB, units = "days")/365),0)) %>% 
  mutate(LOS2 = as.numeric(difftime(DISCHTIME,ADMITTIME, units = "hours"))) %>% 
  mutate(AGE = ifelse(AGE<300, AGE, AGE-211)) %>% 
  mutate(endGoldenHour = ADMITTIME + minutes(60)) #dhours

## The admission cycle
admin <- admin %>%
  dplyr::group_by(SUBJECT_ID) %>%
  dplyr::mutate(admissionCycle = 1:n())

## Total number of admissions
admin<-admin %>%
  dplyr::group_by(SUBJECT_ID) %>%
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
  select(-DIAGNOSIS2) %>% 
  mutate(EXPIRE_FLAG = ifelse(EXPIRE_FLAG==1 & nAdmissions>admissionCycle, 0, EXPIRE_FLAG)) 
  mutate(admissionDiagnosis = "") %>% 
  select(-DEATHTIME,-EDREGTIME,-EDOUTTIME,-HOSPITAL_EXPIRE_FLAG,-HAS_CHARTEVENTS_DATA,-DOB,-endGoldenHour,-nAdmissions,-deadBefore)
  ## To pick out exactly the cycle the patient died!
## To pick out exactly the cycle the patient died!

NotcardiacSyndromes <- admin1 %>% 
  filter( !grepl(paste(keyword, collapse="|"),DIAGNOSIS2)) %>% 
  select(-DIAGNOSIS2) %>% 
  mutate(EXPIRE_FLAG = ifelse(EXPIRE_FLAG==1 & nAdmissions>admissionCycle, 0, EXPIRE_FLAG)) %>% 
  mutate(admissionDiagnosis = "Other Conditions") %>% 
  select(-DEATHTIME,-EDREGTIME,-EDOUTTIME,-HOSPITAL_EXPIRE_FLAG,-HAS_CHARTEVENTS_DATA,-DOB,-endGoldenHour,-nAdmissions,-deadBefore)
data<-rbind(NotcardiacSyndromes,cardiacSyndromes)




data$ETHNICITY<-ifelse(grepl("UNKNOWN",data$ETHNICITY),"UNKNOWN",data$ETHNICITY)
data$ETHNICITY<-ifelse(grepl("PATIENT DECLINED",data$ETHNICITY),"UNKNOWN",data$ETHNICITY)

data$Outcome <- ifelse(data$EXPIRE_FLAG==1 & data$nAdmissions==1,"Deceased", "Readmited")

data$ETHNICITY2<-ifelse(grepl("ASIAN",data$ETHNICITY),"ASIAN",data$ETHNICITY)
data$ETHNICITY2<-ifelse(grepl("MIDDLE EASTERN",data$ETHNICITY2),"ASIAN",data$ETHNICITY2)


data$ETHNICITY2<-ifelse(grepl("HISPANIC",data$ETHNICITY2),"HISPANIC",data$ETHNICITY2)
data$ETHNICITY2<-ifelse(grepl("SOUTH AMERICAN",data$ETHNICITY2),"HISPANIC",data$ETHNICITY2)
data$ETHNICITY2<-ifelse(grepl("WHITE - BRAZILIAN",data$ETHNICITY2),"HISPANIC",data$ETHNICITY2)
data$ETHNICITY2<-ifelse(grepl("CARIBBEAN ISLAND",data$ETHNICITY2),"HISPANIC",data$ETHNICITY2)


data$ETHNICITY2<-ifelse(grepl("ALASKA NATIVE",data$ETHNICITY2),"CAUCASIAN",data$ETHNICITY2)
data$ETHNICITY2<-ifelse(grepl("NATIVE HAWAIIAN",data$ETHNICITY2),"CAUCASIAN",data$ETHNICITY2)
data$ETHNICITY2<-ifelse(grepl("PORTUGUESE",data$ETHNICITY2),"CAUCASIAN",data$ETHNICITY2)
data$ETHNICITY2<-ifelse(grepl("WHITE",data$ETHNICITY2),"CAUCASIAN",data$ETHNICITY2)

data$ETHNICITY2<-ifelse(grepl("BLACK",data$ETHNICITY2),"BLACK",data$ETHNICITY2)

data$ETHNICITY2<-ifelse(grepl("MULTI RACE ETHNICITY",data$ETHNICITY2),"OTHER",data$ETHNICITY2)

data$ETHNICITY2<-ifelse(grepl("UNABLE TO OBTAIN",data$ETHNICITY2),"UNKNOWN",data$ETHNICITY2)

theme_gtsummary_journal(journal = "jama")
#> Setting theme `JAMA`
theme_gtsummary_compact()
#> Setting theme `Compact

trial2<-data %>%
  select(GENDER,AGE,INSURANCE,MARITAL_STATUS,ETHNICITY2,Outcome) %>%
  mutate(INSURANCE = recode(INSURANCE, Medicaid = "Public",Medicare="Public",Government="Public")) %>% 
  mutate(MARITAL_STATUS = recode(MARITAL_STATUS,DIVORCED = "Living alone",SEPARATED="Living alone",
                                 SINGLE="Living alone",WIDOWED="Living alone",'LIFE PARTNER'="Living with Partner",
                                 MARRIED="Living with Partner")) %>% 
  
  tbl_summary(
    by = Outcome,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    label = ETHNICITY2 ~ "Ethinicity",
    missing_text = "(Missing)"
  ) %>%
  modify_caption("**Table 1. Patient Characteristics**") %>%
  bold_labels()


