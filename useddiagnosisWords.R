library(dplyr)
setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4/codes")
data<-read.csv("diagnosisWords.csv")
data$DIAGNOSIS3<-stringr::str_trim(data$DIAGNOSIS3) 
data<-data%>%
  group_by(DIAGNOSIS3,EXPIRE_FLAG) %>%
  tally()

