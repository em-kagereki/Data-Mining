setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")

library(tidyverse)
library(tidyr)

####1. DRGReshaped


## DRG
DRGReshaped<-read.csv("DRGCODES.csv")
DRGReshaped <- DRGReshaped %>% select(HADM_ID,DRG_CODE)
### By removing the duplicates - DRGs that have multiple classifications are removed
DRGReshaped = DRGReshaped[!duplicated(DRGReshaped$HADM_ID),]
DRGReshaped$DRG_CODE <-paste("DRG", DRGReshaped$DRG_CODE, sep="")

DRGReshaped$Present<- 1
DRGReshaped <- DRGReshaped %>% 
  spread(DRG_CODE, Present)

DRGReshaped<-DRGReshaped %>% 
  mutate(across(everything(), .fns=~replace_na(.,0)))
# View(DRGReshaped)
#write.csv(DRGReshaped, "DRGReshaped.csv")










PrescriptionReshaped<-read.csv("PRESCRIPTIONS.csv")
PrescriptionReshaped <- PrescriptionReshaped %>% select(HADM_ID,DRUG)

PrescriptionReshaped$combined<-paste(PrescriptionReshaped$HADM_ID,PrescriptionReshaped$DRUG,sep="")

PrescriptionReshaped<-PrescriptionReshaped %>%
  group_by(combined) %>%
  mutate(nPresc = n_distinct(DRUG))

PrescriptionReshaped<-PrescriptionReshaped %>% 
  group_by(combined) %>%
  filter(nPresc == max(nPresc))