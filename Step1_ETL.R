
#knitr::write_bib(c(.packages(), "data.table"), "packages.bib")

setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
source("Global.R")
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

#patientOutcome<-admin %>% 
 # select(SUBJECT_ID,HADM_ID,EXPIRE_FLAG) %>% 
  #unite(case_id, SUBJECT_ID,HADM_ID, sep = "-", remove = FALSE) 
#write.csv(patientOutcome,"patientOutcome.csv")

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
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"abd pain", "abdominal pain")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"abd aortic", "abdominal aortic")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"thoraua", "thoracic")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"aortic valve replacement redo|redo aortic valve replacement", "avr")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"mitral valve replacement redo", "mvr")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"mitral valve replacement", "mvr")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"aortic valve replacement", "avr")

cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"thoraco/abdominal", "thoracoabdominal")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"myocardial infarction", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"acute mi", "mi")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"re/do", "redo")

cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"gastrointestinal bleed", "gi bleed")



cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"90/", "")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"afibrialltion", "afib")
cardiacSyndromes$DIAGNOSIS3 <- str_replace_all(cardiacSyndromes$DIAGNOSIS3,"afibuation", "afib")


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
## We check the admission of the patient!
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
serviceMerge<-services %>% 
  unite(group,PREV_SERVICE, CURR_SERVICE, sep = "_", remove = FALSE) 
serviceN<-data.frame(prop.table(table(serviceMerge$group)))
names(serviceN)[names(serviceN) == "Var1"] <- "group"
serviceMerge<-merge(x=serviceMerge,y=serviceN, by = 'group', all.x = TRUE)
services2<-merge(x=cardiacSyndromes2,y=serviceMerge,by='HADM_ID',x.all=TRUE) %>% 
  select(HADM_ID,Freq)

group <- services2 %>%
  group_by(HADM_ID) %>%
  summarise(all_names = paste(Freq, collapse = "/"))
group$count <- sapply(strsplit(group$all_names,'/'), uniqueN)
serviceGroup<-data.frame(str_split_fixed(group$all_names, "/", 7))%>% 
  replace(is.na(.), 0)
serviceGroup$X1 <- ifelse(nchar(serviceGroup$X1)==0, 0, serviceGroup$X1)
serviceGroup$X2 <- ifelse(nchar(serviceGroup$X2)==0, 0, serviceGroup$X2)
serviceGroup$X3 <- ifelse(nchar(serviceGroup$X3)==0, 0, serviceGroup$X3)
serviceGroup$X4 <- ifelse(nchar(serviceGroup$X4)==0, 0, serviceGroup$X4)
serviceGroup$X5 <- ifelse(nchar(serviceGroup$X5)==0, 0, serviceGroup$X5)
serviceGroup$X6 <- ifelse(nchar(serviceGroup$X6)==0, 0, serviceGroup$X6)
serviceGroup$X7 <- ifelse(nchar(serviceGroup$X7)==0, 0, serviceGroup$X7)
serviceGroup<-cbind(group$HADM_ID,serviceGroup)
names(serviceGroup)[1] <- "HADM_ID"


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
procW$count <- sapply(strsplit(procW$x,'/'), uniqueN)
proc<-data.frame(str_split_fixed(procW$x, "/", max(procW$count)))

source("Recoding.R")

microb<-read.csv("MICROBIOLOGYEVENTS.csv") %>% 
  select(-ROW_ID,-SUBJECT_ID)
microb<-merge(x=cardiacSyndromes2,y=microb,by='HADM_ID',x.all=TRUE) %>% 
  mutate(CHARTTIME = ymd_hms(CHARTTIME)) %>% 
  unite(combined, SPEC_TYPE_DESC, ORG_NAME,AB_NAME,INTERPRETATION, sep = "_", remove = FALSE) %>% 
  select(HADM_ID,combined)
microbN<-data.frame(prop.table(table(microb$combined)))
names(microbN)[names(microbN) == "Var1"] <- "group"
names(microb)[names(microb) == "combined"] <- "group"
microb<-merge(x=microb,y=microbN, by = 'group', all.x = TRUE)
microb<-merge(x=cardiacSyndromes2,y=microb,by='HADM_ID',x.all=TRUE) %>% 
  select(HADM_ID,Freq)
miGroup <- microb %>%
  group_by(HADM_ID) %>%
  summarise(all_names = paste(Freq, collapse = "/"))
miGroup$count <- sapply(strsplit(miGroup$all_names,'/'), uniqueN)
microbGroup<-data.frame(str_split_fixed(miGroup$all_names, "/", 75))%>% 
  replace(is.na(.), 0)
microGroup<-cbind(miGroup$HADM_ID,microbGroup)
names(microGroup)[1] <- "HADM_ID"
#m<-microGroup %>% 
#  mutate_all(microGroup, list(~na_if(.,"")))
for(i in 1:ncol(microGroup)) {    
  microGroup[ , i][microGroup[ , i]==""]<-0
  # for-loop over columns
}
microGroup<-microGroup %>% 
  replace(is.na(.), 0)

## Medication
meds<-read.csv("PRESCRIPTIONS.csv")
meds<-merge(x=cardiacSyndromes2,y=meds,by='HADM_ID',x.all=TRUE) %>% 
      mutate(drug2 = tolower(DRUG)) %>% 
      select(HADM_ID,DRUG,drug2) 
meds$drug2 <- str_replace(meds$drug2,"aspirin","Aspirin")  
meds$drug2 <- str_replace(meds$drug2,"morphine","Morphine")  
meds$drug2 <- str_replace(meds$drug2,(paste(HMGCoA, collapse="|")),"HMGCoA")  
meds$drug2 <- str_replace(meds$drug2,(paste(ACE, collapse="|")),"ACE Inhibitors")  
meds$drug2 <- str_replace(meds$drug2,(paste(betaBlockers, collapse="|")),"Beta blockers")  
meds$drug2 <- str_replace(meds$drug2,(paste(glycoproteinInhibitors, collapse="|")),"GpIIb/IIIa inhibitors")  
meds$drug2 <- str_replace(meds$drug2,"nitroglycerin","Nitroglycerine")  
med2<-meds[!duplicated(meds$drug2),] %>% 
  select(drug2)
med.hash<-hashed.model.matrix(c("drug2"),med2,hash.size=2^20,
                              create.mapping = TRUE)
Medmapping<-hash.mapping(med.hash)
datMed<-as.data.frame(hashed.value(Medmapping))
mean(duplicated(Medmapping))
names<-data.frame(names(Medmapping))
medMap<-cbind(datMed,names)
names(medMap)[1]<-"value"
names(medMap)[2]<-"drug2"
medMap$drug2 <- gsub("drug2","",medMap$drug2)  
medMap$drug2<-trimws(medMap$drug2)
med2<-merge(x=meds, y = medMap, by ="drug2", all.x = TRUE ) %>% 
  select(-drug2,-DRUG)
medGroup <- med2 %>%
  group_by(HADM_ID) %>%
  summarise(all_names = paste(value, collapse = "/"))
medGroup$count <- sapply(strsplit(medGroup$all_names,'/'), uniqueN)
mediGroup<-data.frame(str_split_fixed(medGroup$all_names, "/", 144))%>% 
  replace(is.na(.), 0)
mediGroup<-cbind(medGroup$HADM_ID,mediGroup)
names(mediGroup)[1] <- "HADM_ID"
for(i in 1:ncol(mediGroup)) {    
  mediGroup[ , i][mediGroup[ , i]==""]<-0
  # for-loop over columns
}






## Write flat files:
# Classification:
#write.csv(cardiacSyndromes,"cardiacSyndromes.csv")
#write.csv(serviceGroup,"serviceGroup.csv")
#write.csv(labWide,"labGroup.csv")
#write.csv(proc,"procGroup.csv")
#write.csv(microGroup,"microGroup.csv")
#write.csv(mediGroup, "mediGroup.csv")


# Process mining:
#write.csv(logProdecures,"logProdecures.csv")
#write.csv(microbWide,"microbWide.csv")



