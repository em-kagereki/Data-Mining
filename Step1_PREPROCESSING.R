

setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")

cardiacSyndromes<-read.csv("cardiacSyndromes.csv")
labWide<-read.csv("labWide.csv") %>% 
  select(-X)
#View(labWide)
servicesWide<-read.csv("servicesWide.csv") %>% 
  select(-X)
#View(servicesWide)
procWide<-read.csv("procWide.csv") %>% 
  select(-X)
#View(procWide)
microbWide<-read.csv("microbWide.csv") %>% 
  select(-X)
#View(microbWide)



cardiacSyndromesModel<- cardiacSyndromes %>% 
  select(-X,-ADMITTIME,-DISCHTIME,-DISCHARGE_LOCATION,-HOSPITAL_EXPIRE_FLAG,-HAS_CHARTEVENTS_DATA,
         -EDOUTTIME,-EDREGTIME,-DEATHTIME,-DOB,-LOS2,-endGoldenHour,-nAdmissions,-deadBefore,-SUBJECT_ID) %>% 
  mutate_all(list(~na_if(.,"")))


## Missingness of the data
##Step 1. Drop all the variables with more that 30% missing entries

missing<-cardiacSyndromesModel %>%
  diagnose() %>%
  select(-unique_count, -unique_rate) %>% 
  #filter(missing_count > 0) %>% 
  arrange(desc(missing_count))

cardiacSyndromesModel<-cardiacSyndromesModel %>% 
  select(-LANGUAGE)

cardiacSyndromesModel %>% group_by(MARITAL_STATUS) %>% summarize(count=n())

## Here we replace the missing values to UNKNOWN

cardiacSyndromesModel<-cardiacSyndromesModel %>% 
  mutate(MARITAL_STATUS = ifelse(is.na(MARITAL_STATUS), "UNKNOWN (DEFAULT)", MARITAL_STATUS))

## Then we check the Religion
cardiacSyndromesModel %>% group_by(RELIGION) %>% summarize(count=n())

cardiacSyndromesModel<-cardiacSyndromesModel %>% 
  mutate(RELIGION = ifelse(is.na(RELIGION), "UNOBTAINABLE", RELIGION)) %>% 
  mutate(RELIGION = ifelse(RELIGION=="NOT SPECIFIED", "UNOBTAINABLE", RELIGION)) %>% 
  select(-DIAGNOSIS)

#write.csv(cardiacSyndromesModel,"cardiacSyndromesModel.csv")

cardiacSyndromesModel %>% group_by(EXPIRE_FLAG) %>% summarize(count=n())


data<-merge(x=cardiacSyndromesModel,y=labWide, by="HADM_ID", all=TRUE) %>% 
  replace(is.na(.), 0)
data<-merge(x=data,y=servicesWide, by="HADM_ID",all=TRUE) %>% 
  replace(is.na(.), 0)
data<-merge(x=data,y=procWide, by="HADM_ID", all=TRUE) %>% 
  replace(is.na(.), 0)
data<-merge(x=data,y=microbWide, by="HADM_ID", all=TRUE) %>% 
  replace(is.na(.), 0)

data<-data %>% replace(is.na(.), 0)

library(caret)
## One hot encoding
dummies<-dummyVars(EXPIRE_FLAG~.,data=data)
data<-predict(dummies,newdata=data)

 rm(labWide)
 rm(pt)
 rm(missing)
 rm(microbWide)
 rm(cardiacSyndromesModel)
 rm(admin1)
 rm(admin)
 rm(procWide)
rm(servicesWide)
 rm(cardiacSyndromes)
rm(cardiacSyndromes2)
rm(dummies)

trans = preProcess(data,method=c("BoxCox","center","scale","pca"))
PC = predict(trans,data)
head(PC,5)
