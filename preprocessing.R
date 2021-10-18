adminTYpe <- data %>% select(HADM_ID,ADMISSION_TYPE)
adminTYpe<-adminTYpe %>% drop_na()
data <- data %>% select(-ADMISSION_TYPE)

adminTYpe$Present<- 1
adminTYpe <- adminTYpe %>% 
  spread(ADMISSION_TYPE, Present)
adminTYpe<-adminTYpe %>% 
  mutate(across(everything(), .fns=~replace_na(.,0)))
data<-merge(x = data, y = adminTYpe, by = "HADM_ID", all.x = TRUE)

rm(adminTYpe)
## Discharge Type
dischargeTYpe <- data %>% select(HADM_ID,DISCHARGE_LOCATION)
dischargeTYpe<-dischargeTYpe %>% drop_na()
data <- data %>% select(-DISCHARGE_LOCATION)
dischargeTYpe$Present<- 1
dischargeTYpe <- dischargeTYpe %>% 
  spread(DISCHARGE_LOCATION, Present)
dischargeTYpe<-dischargeTYpe %>% 
  mutate(across(everything(), .fns=~replace_na(.,0)))
data<-merge(x = data, y = dischargeTYpe, by = "HADM_ID", all.x = TRUE)
rm(dischargeTYpe)

## 
adminLocationTYpe <- data %>% select(HADM_ID,ADMISSION_LOCATION)
adminLocationTYpe<-adminLocationTYpe %>% drop_na()
data <- data %>% select(-ADMISSION_LOCATION)
adminLocationTYpe$Present<- 1
adminLocationTYpe <- adminLocationTYpe %>% 
  spread(ADMISSION_LOCATION, Present)
adminLocationTYpe<-adminLocationTYpe %>% 
  mutate(across(everything(), .fns=~replace_na(.,0)))
data<-merge(x = data, y = adminLocationTYpe, by = "HADM_ID", all.x = TRUE)
rm(adminLocationTYpe)

### INSURANCE
insuranceTYpe <- data %>% select(HADM_ID,INSURANCE)
insuranceTYpe<-insuranceTYpe %>% drop_na()
data <- data %>% select(-INSURANCE)
insuranceTYpe$Present<- 1
insuranceTYpe <- insuranceTYpe %>% 
  spread(INSURANCE, Present)
insuranceTYpe<-insuranceTYpe %>% 
  mutate(across(everything(), .fns=~replace_na(.,0)))
data<-merge(x = data, y = insuranceTYpe, by = "HADM_ID", all.x = TRUE)
rm(insuranceTYpe)

## RELIGION
### Change to NOT SPECIFIED for the missing values
religionTYpe <- data %>% select(HADM_ID,RELIGION)
religionTYpe<-religionTYpe %>% drop_na()
data <- data %>% select(-RELIGION)
religionTYpe$Present<- 1
religionTYpe <- religionTYpe %>% 
  spread(RELIGION, Present)
religionTYpe<-religionTYpe %>% 
  mutate(across(everything(), .fns=~replace_na(.,0)))
data<-merge(x = data, y = religionTYpe, by = "HADM_ID", all.x = TRUE)
rm(religionTYpe)

##MARITAL_STATUS
maritalTYpe <- data %>% select(HADM_ID,MARITAL_STATUS)
maritalTYpe<-maritalTYpe %>% drop_na()
data <- data %>% select(-MARITAL_STATUS)
maritalTYpe$Present<- 1
maritalTYpe <- maritalTYpe %>% 
  spread(MARITAL_STATUS, Present)
maritalTYpe<-maritalTYpe %>% 
  mutate(across(everything(), .fns=~replace_na(.,0)))
data<-merge(x = data, y = maritalTYpe, by = "HADM_ID", all.x = TRUE)
rm(maritalTYpe)

## ETHNICITY
ethnicityTYpe <- data %>% select(HADM_ID,ETHNICITY)
ethnicityTYpe<-ethnicityTYpe %>% drop_na()
data <- data %>% select(-ETHNICITY)
ethnicityTYpe$Present<- 1
ethnicityTYpe <- ethnicityTYpe %>% 
  spread(ETHNICITY, Present)
ethnicityTYpe<-ethnicityTYpe %>% 
  mutate(across(everything(), .fns=~replace_na(.,0)))
data<-merge(x = data, y = ethnicityTYpe, by = "HADM_ID", all.x = TRUE)
rm(ethnicityTYpe)
## GENDER
genderTYpe <- data %>% select(HADM_ID,GENDER)
genderTYpe<-genderTYpe %>% drop_na()
data <- data %>% select(-GENDER)
genderTYpe$Present<- 1
genderTYpe <- genderTYpe %>% 
  spread(GENDER, Present)
genderTYpe<-genderTYpe %>% 
  mutate(across(everything(), .fns=~replace_na(.,0)))
data<-merge(x = data, y = genderTYpe, by = "HADM_ID", all.x = TRUE)
rm(genderTYpe)


data <- data %>% select(-LANGUAGE,-DIAGNOSIS)


write.csv(data,"dataEDA.csv")

DRGReshaped<-read.csv("DRGReshaped.csv")
data<-merge(x = data, y = DRGReshaped, by = "HADM_ID", all.x = TRUE)
rm(DRGReshaped)
## Then we run the PCA

write.csv(data,"dataBeforeEDA.csv")

library(factoextra)

data.pca <-prcomp(data,scale=TRUE)


ls()
gc()
memory.size(max=F)




### THis code is meant  to augment the data with 0's
data2 <-df <- data.frame(matrix(ncol = ncol(data), nrow = 0))
x<-colnames(data3)
df <- data.frame(matrix(ncol = ncol(data), nrow = 42))
colnames(df) <- x
df[is.na(df)] <- 0

data2 <-df <- data.frame(matrix(ncol = ncol(data), nrow = 1))
data2[is.na(data2)] <- 0
colnames(data2) <- x


dfList <- split( data3 , f = data$SUBJECT_ID )
for(i in 1:length(dfList))
{
  miniData <-data.frame(dfList[i])
  colnames(miniData) <- x
  
  if (nrow(miniData)<4){
    df$SUBJECT_ID<-miniData$SUBJECT_ID  
    df2 <-df[1:(4-nrow(miniData)),]
    miniData1<-cbind(df2,miniData)  
    data2 <-cbind(data2,miniData)
  }else{
    data2 <-cbind(data2,miniData)
  }
}




