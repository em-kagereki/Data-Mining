
#knitr::write_bib(c(.packages(), "data.table"), "packages.bib")

setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
admin<-read.csv("ADMISSIONS.csv") 
pt<-read.csv("PATIENTS.csv") %>% 
  select(SUBJECT_ID, GENDER, DOB,EXPIRE_FLAG)

admin<-merge(x = admin, y = pt, by = "SUBJECT_ID", all.x = TRUE) %>% 
  mutate(ADMITTIME = ymd_hms(ADMITTIME)) %>% 
  mutate(DISCHTIME = ymd_hms(DISCHTIME)) %>% 
  mutate(DEATHTIME = ymd_hms(DEATHTIME)) %>% 
  mutate(DOB = ymd_hms(DOB)) %>%
  mutate(AGE = round(as.numeric(difftime(ADMITTIME,DOB, units = "days")/365),0)) %>% 
  mutate(LOS2 = as.numeric(difftime(DISCHTIME,ADMITTIME, units = "hours"))) %>% 
  mutate(AGE = ifelse(AGE<300, AGE, AGE-211)) %>% 
  mutate(Period = ntile(as.numeric(ADMITTIME),12)) %>% 
  mutate(endGoldenHour = ADMITTIME + minutes(60)) %>% 
  group_by(SUBJECT_ID) %>%
  mutate(admissionCycle = 1:n()) %>% ## The admission cycle
  group_by(SUBJECT_ID) %>%
  arrange(DISCHTIME) %>%
  mutate(nAdmissions = n_distinct(HADM_ID)) %>% 
  mutate(deadBefore = as.numeric(DEATHTIME-endGoldenHour)) %>% 
  mutate(deadBefore = ifelse(HOSPITAL_EXPIRE_FLAG==0, 0, deadBefore)) %>%  ## Remove all the patients who died before the cut-off
  filter(deadBefore>=0) %>% 
  mutate(DIAGNOSIS2 = tolower(DIAGNOSIS)) %>% 
  mutate(dayOfYear = yday(ADMITTIME)) %>% 
  mutate(Month = month(ADMITTIME)) %>% 
  mutate(week = week(ADMITTIME)) %>% 
  mutate(weekday = wday(ADMITTIME)) %>% 
  mutate(year = year(ADMITTIME)) %>% 
  mutate(hour = hour(ADMITTIME))

deadBefore<-admin %>% 
  mutate(deadBefore = as.numeric(DEATHTIME-endGoldenHour)) %>% 
  mutate(deadBefore = ifelse(HOSPITAL_EXPIRE_FLAG==1, 0, deadBefore)) %>%  ## Remove all the patients who died before the cut-off
  filter(deadBefore==0)

## Patients with suspected acute cardiac syndrome
## Patients who died before end of one hour
cardiacSyndromes <- admin %>% 
  filter( grepl(paste(keyword, collapse="|"),DIAGNOSIS2)) %>% 
  select(-ROW_ID) %>% 
  mutate(EXPIRE_FLAG = ifelse(EXPIRE_FLAG==1 & nAdmissions>admissionCycle, 0, EXPIRE_FLAG)) ## To pick out exactly the cycle the patient died!

#Exclude some accidental key word : ludwig angina

cardiacSyndromes <- cardiacSyndromes[!grepl("ludwigs",cardiacSyndromes$DIAGNOSIS2),]
 

cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS2, "&", "and")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3, "[[:punct:]]", "/")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3, "//remote west//", "")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3, "//", "/")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3, "\\+", "")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"chf", "congestive heart failure")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"aaa", "ascending aortic aneurysm")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"catheterization", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"left heart cath with percutareous coronary intervention to the left anterior descending artery", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"catherization", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"right and left cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"right and left heart cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"left heart cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"left heart cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"left and right heart cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"left heart cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cath lab room 1", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cath with intravascular ultrasound", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cath with brachy standby", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cath with intervention", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cath with intervention", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"catherterization", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cath right and left", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cath and l sfa intervention", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cath with brachy s/b", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cath lab", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"complete heart cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"right heart cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"r heart cath with swan placement", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"right and left cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"right heart cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"right heart cath/swan line", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cath/cathetrization", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"/catheter", "/cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cardiac carth", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cardiac cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cardiac cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cardiac cath", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"caridac cathl", "cath")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"left haeart cath", "cath")


#cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"", "cath")

cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non/st segment elevation myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non st elevation myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non s/t elevation myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non st elevation mi", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non/st segment elevation myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non st elevation myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non/st elevation myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non s/t elevation mi", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non st elevated mi", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non st elevated myocardial infarc", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non st elevated myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non st segment elevation myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non/st elevated myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non/st/elevation myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non/st/elevation myocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"n stem", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"n/ stemi", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non stemi mi", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"nstemition", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"nstemyocardial infarction", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"nstemit", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"nonstemii mi", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"nstemii", "nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"with nstemi", "/nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non st myocardial infarction", "/nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non st segment myocardial infarction", "/nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non ste myocardial infarction", "/nstemi")

cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non sten electrical myocardial infarction", "/nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"", "/nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"", "/nstemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"", "/nstemi")


cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"acute st elevation myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"code stemi", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"elevated stemi", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"end stemi", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"instem", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"end st elevated myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st segment elevation myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"inferior st/segment elevation myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"s t elevation myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"s/t elevation myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"segment elevation myocardial infaction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st elevated mi", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st segment elevation myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st/segment elevation myocardial infarction anterior", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st elevated myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st elevation mi", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st elevation myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"anterior stemi", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"with nstemi", "/stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"inferior stemi", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"s/t elevations/myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st elevated myocardial infarct", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st elevated myocrdial infarct", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st elevation/myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st/elevation myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st elevetaed myocardial infarction//", "stemi/")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st/segment elevation myocardial infarction//", "stemi/")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st/segment elevation myocardial infarction", "stemi")
#cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st/segment elevation/myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st segment myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"st wave myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"ste myocardial infarction", "stemi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"stemyocardial infarction", "stemi")

cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"new onset angina", "angina")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"ongoing angina", "angina")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"persistent angina", "angina")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"unsstable angina", "unstable angina")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"post infarct angina", "angina")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"recurrent angina pectoris", "recurrent angina")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"unstabel angina", "unstable angina")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"worsening angina", "progressive angina")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"new angina", "angina")

cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"rule out", "/")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"rule/out", "/")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"r/o", "/")

cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"rule in", "")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"rule otu", "")


cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"coronary artery bypass graft", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"needs urgent bypass surgery", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"aortic carotid subclavian bypass graft", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cornary artery bypass", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"coronoary artery bypass graft with mvr", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"minimally invasive coronary artery bypass", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"off pump coronary artery bypass", "cabg")

cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"off pump coronary artery/ bypass", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"acending aorta/coronary artery bypass", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cabg withradial artery/ / off pump coronary artery bypass with radial artery left", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"cabg/ / off pump coronary artery bypas", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"coroanry artery bypass graft with mitral valve regurgitation", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"innominate artery bypass", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"off pump coronary artery bypass endoscopic left chest", "cabg")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"off pump coronary artery bypass", "cabg")
#cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"", "cabg")


cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"inferior myocardial infarction", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non q wave myocardial infaction", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non/q wave myocardial infarction", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non/q/wave myocardial infarction", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non/q/wave myocardial infarction", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"non0q0wave myocardial infarction", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"posterior myocardial infarction", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"acute miion", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"miion", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"anterior wall mi", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"anterior mi", "mi")


cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"3 vessel cad", "3VD")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"3 vessel coronary artery disease", "3VD")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"3/vessel coronary artery disease", "3VD")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"3 vessel diagnosis", "3VD")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"3 vessel disease", "3VD")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"severe 3vessels disease", "3VD")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"three vessel coronary artery disease", "3VD")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"3 vessel disease", "3VD")


cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"new a/fib", "afib")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"a/fib", "afib")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"atrial fibrillation", "afib")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"atrial flutter/fibrillation", "afib")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"atrial fib", "afib")

cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"s/p", "post")
table<-table(cardiacSyndromes$DIAGNOSIS3)


cardiacSyndromes2<-cardiacSyndromes %>% select(SUBJECT_ID,HADM_ID,endGoldenHour)
Loinc <- read.csv("Loinc.csv") %>% 
  mutate(LOINC_CODE = LOINC_NUM) %>% 
  select(LOINC_CODE,RELATEDNAMES2)

## Source("PrepareLabData.R") ## The lab data is 1.72GB, manipulations were limited, therefore it was run in the server 

labNew<-read.csv("newdata.csv") 
labNew<- merge(x=labNew, y = Loinc, by = "LOINC_CODE", x.all=TRUE) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  unite(combined, LOINC_CODE,FLAG, sep = "-", remove = FALSE) %>% 
  select(-FLAG,-LABEL,-FLUID,LOINC_CODE,-X)
  
## The labs in those missing the HADM were done as outpatients
missingHADM<-labNew %>% 
  filter(is.na(HADM_ID)) 
## We check teh admission of the patient!
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

## Convert to wide
labWide<-merge(x=cardiacSyndromes2,y=allLAbs,by='HADM_ID',x.all=TRUE) %>% 
  mutate(Checktime = ifelse(endGoldenHour>=CHARTTIME, "After", "Before")) %>% 
  filter(Checktime=="Before") %>% 
  select(-Checktime) %>% 
  select(HADM_ID,combined) %>% 
  count(HADM_ID, combined, sort = TRUE) %>% 
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

servicesWide<-merge(x=cardiacSyndromes2, y=servicesWide, by="HADM_ID", all.x = TRUE) %>% 
  replace(is.na(.), 0)
checkServicesBeforeAdmin<-servicesWide %>% filter(HADM_ID==0) 
nrow(checkServicesBeforeAdmin)

## Procedures
procedures<-read.csv("PROCEDUREEVENTS_MV.csv")
D_ITEM<-read.csv("D_ITEMS.csv")
procedures<-merge(x=procedures,y=D_ITEM, by = "ITEMID",x.all=TRUE) %>% 
  select(SUBJECT_ID,HADM_ID,STARTTIME,ENDTIME,LABEL) %>% 
  unite(combined, HADM_ID, LABEL, sep = "_", remove = FALSE)

procedures<-procedures[!duplicated(procedures$combined), ] # Here we remove the duplicated procedures
group<-aggregate(procedures$LABEL, list(procedures$HADM_ID), paste, collapse="/")
names(group)[1] <- "HADM_ID"
procW<-merge(x=group,y=cardiacSyndromes2, by = "HADM_ID",x.all=TRUE)

logProdecures<-procedures
procWide<-merge(x=procedures,y=cardiacSyndromes2, by = "HADM_ID",x.all=TRUE) %>% 
  mutate(STARTTIME2 = ymd_hms(STARTTIME)) %>% 
  mutate(Checktime = ifelse(endGoldenHour>=STARTTIME2, "After", "Before")) %>% 
  filter(Checktime=="Before") %>% 
  select(HADM_ID,LABEL) %>% count(HADM_ID, LABEL, sort = TRUE)%>% 
  spread(LABEL, n) %>% 
  replace(is.na(.), 0)
procWide<-merge(x=cardiacSyndromes2, y=procWide, by="HADM_ID", all.x = TRUE) %>% 
  replace(is.na(.), 0)

microb<-read.csv("MICROBIOLOGYEVENTS.csv") %>% 
  select(-ROW_ID,-SUBJECT_ID)
microb<-merge(x=cardiacSyndromes2,y=microb,by='HADM_ID',x.all=TRUE) %>% 
  mutate(CHARTTIME = ymd_hms(CHARTTIME)) %>% 
  unite(combined, SPEC_TYPE_DESC, ORG_NAME,AB_NAME,INTERPRETATION, sep = "_", remove = FALSE)
#Logmicrob<-microb
#write.csv(Logmicrob,"Logmicrob")
microbWide<-microb%>% 
  mutate(Checktime = ifelse(endGoldenHour>=CHARTTIME, "After", "Before")) %>% 
  filter(Checktime=="Before") %>% 
  select(HADM_ID,combined) %>% 
  count(HADM_ID, combined, sort = TRUE) %>% 
  spread(combined, n) %>% 
  replace(is.na(.), 0)


## Medication
meds<-read.csv("PRESCRIPTIONS.csv") %>% 
  mutate(drug2 = tolower(DRUG)) %>% 
  filter( grepl(paste(subset, collapse="|"),drug2)) %>% 
  mutate(DRUG = case_when(grepl("aspirin", drug2) ~ "Aspirin",
                          grepl("morphine", drug2) ~ "Morphine",
                          grepl((paste(HMGCoA, collapse="|")), drug2) ~ "HMGCoA",
                          grepl((paste(ACE, collapse="|")), drug2) ~ "ACE Inhibitors",
                          grepl((paste(betaBlockers, collapse="|")), drug2) ~ "Beta blockers",
                          grepl((paste(glycoproteinInhibitors, collapse="|")), drug2) ~ "GpIIb/IIIa inhibitors",
                          grepl("nitroglycerin", drug2, ignore.case = TRUE) ~"Nitroglycerine")) %>% 
  na.omit() %>%
  mutate(STARTDATE=ymd_hms(STARTDATE,tz="Europe/London")) %>% 
  mutate(ENDDATE=ymd_hms(ENDDATE,tz="Europe/London")) %>% 
  unite(case_id, SUBJECT_ID,HADM_ID,DRUG, sep = "-", remove = FALSE) %>% 
  group_by(case_id) %>% 
  mutate(start = min(STARTDATE),complete = max(ENDDATE)) %>% 
  select(-X.1,-X,-STARTDATE,-ENDDATE,-drug2) %>% 
  distinct(case_id, .keep_all= TRUE) %>% 
  unite(case_id, SUBJECT_ID,HADM_ID, sep = "-", remove = FALSE) %>% 
  group_by(case_id) %>% 
  arrange(start) %>% 
  mutate(activity_instance = 1:n())


## Write flat files:
# Classification:
write.csv(cardiacSyndromes,"cardiacSyndromes.csv")
write.csv(servicesWide,"servicesWide.csv")
write.csv(labWide,"labWide.csv")
write.csv(procWide,"procWide.csv")
write.csv(microbWide,"microbWide.csv")

# Process mining:
write.csv(logProdecures,"logProdecures.csv")
write.csv(microbWide,"microbWide.csv")
write.csv(medication, "medication.csv")



