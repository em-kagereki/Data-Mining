library(bupaR)
library(dplyr)
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

library(processcheckR)

setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
patientOutcome<-read.csv("patientOutcome.csv")

medication<-read.csv("medication.csv")  
  #mutate(start = as.Date.character(start)) %>% 
  #mutate(complete = as.Date.character(complete))

medicationStart<-medication %>% 
  select(-complete) %>% 
  mutate(status = "start",timestamp=start,Item=DRUG) %>% 
  select(-SUBJECT_ID,-HADM_ID,-start,-DRUG) %>% 
  mutate(activity	= Item)
medicationStart$timestamp<-ymd(medicationStart$timestamp)
medicationStart$timestamp<-update(medicationStart$timestamp, hour = 1)
medicationStart$timestamp<-ymd_hms(medicationStart$timestamp)
medicationStartOnly<-merge(x=medicationStart, y=patientOutcome, by="case_id")

died<-medicationStartOnly %>% 
  filter(EXPIRE_FLAG== '1')
died$activity_instance <- seq.int(nrow(died)) 

died_XES <-died %>%
  mutate(resource = NA) %>%
  filter(!is.na(timestamp)) %>%
  eventlog(
    case_id = "case_id",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "resource"
  )
died_Map<-died_XES %>%
  process_map()

alive<-medicationStartOnly %>% 
  filter(EXPIRE_FLAG== '0')
alive$activity_instance <- seq.int(nrow(alive)) 
alive_XES <-alive %>%
  mutate(resource = NA) %>%
  filter(!is.na(timestamp)) %>%
  eventlog(
    case_id = "case_id",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "resource"
  )
alive_Map<-alive_XES %>%
  process_map()


alive_XES %>%
  check_rules(
    r1 = starts("Aspirin"),
    r2 = and("Nitroglycerine","Morphine")) %>%
  group_by(r1, r2) %>%
  n_cases() 



died_XES %>%
  check_rules(
    r1 = starts("Aspirin"),
    r2 = and("Nitroglycerine","Morphine")) %>%
  group_by(r1, r2) %>%
  n_cases() 

d<-died_XES %>%
  check_rules(
    r1 = starts("Nitroglycerine")) %>% 
    #r2 = and("Nitroglycerine","Morphine")) %>%
  group_by(r1) %>%
  n_cases() 

a<-alive_XES %>%
  check_rules(
    r1 = starts("Nitroglycerine")) %>% 
  #r2 = and("Nitroglycerine","Morphine")) %>%
  group_by(r1) %>%
  n_cases() 
m<-data.frame(merge(x=d,y=a,by="r1"))

library(epiR)

q2.m = matrix(c(m[1,3],m[1,2],m[2,3],m[2,2]), nrow=2, byrow=T, 
              dimnames = list(c("Alive", "Dead"),c("Asprin", "No")))
epi.2by2(q2.m)

library(bupaR)
library(pm4py)
library(petrinetR)

patients_complete <- alive_XES %>% filter_lifecycle("complete")
model <- discovery_inductive(patients_complete)




proc<-read.csv("logProdecures.csv") %>% 
  unite(case_id, SUBJECT_ID,HADM_ID, sep = "-", remove = FALSE) %>%  
  mutate(Item = LABEL, status= "start",timestamp = STARTTIME)  
proc2<- proc[grepl("CT scan|Cardiac Cath|EKG|Chest X-Ray",proc$LABEL),]
proc <-proc2 %>% 
  mutate(activity = Item)%>%
  group_by(case_id) %>%
  mutate(activity_instance = 1:n()) %>% 
  select(-SUBJECT_ID,-HADM_ID,-STARTTIME,-ENDTIME,-LABEL)  
proc$timestamp<-ymd_hms(proc$timestamp)
log <- rbind(x=proc,y=medicationStart) %>% 
  select(-X)
log<-merge(x=log, y=patientOutcome, by="case_id")%>% 
  select(-X)


log$activity_instance <- seq.int(nrow(log)) 
log_XES <-log %>%
  mutate(resource = NA) %>%
  filter(!is.na(timestamp)) %>%
  eventlog(
    case_id = "case_id",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "resource"
  )
Process_Map<-log_XES %>%
  process_map()

## Lab can be added the same way

log_XES %>%
  filter_activity_presence("EKG") %>%
  traces

log_XES %>%
  filter_trim(start_activities = "Aspirin", end_activities =  c("Chest X-Ray","EKG")) %>%
  process_map(type = performance())

log_XES %>%
  filter_resource_frequency(perc = 0.80) %>%
  resources()







# medicationComplete<-medication %>% 
#  select(-start) %>% 
#   mutate(status = "complete",timestamp=complete) %>% 
#   select(-SUBJECT_ID,-HADM_ID,-complete) %>% 
#   mutate(activity	= DRUG) 
# 
# medication2<-rbind(medicationStart,medicationComplete) 
# medication2$activity_instance <- seq.int(nrow(medication2)) 
# 
# example_log_4 <-medication2 %>%
#   mutate(resource = NA) %>%
#   filter(!is.na(timestamp)) %>%
#   eventlog(
#     case_id = "case_id",
#     activity_id = "activity",
#     activity_instance_id = "activity_instance",
#     lifecycle_id = "status",
#     timestamp = "timestamp",
#     resource_id = "resource"
#   )
# 
# medicationComplete$activity_instance <- seq.int(nrow(medicationComplete)) 
# example_log_5 <-medicationComplete %>%
#   mutate(resource = NA) %>%
#   filter(!is.na(timestamp)) %>%
#   eventlog(
#     case_id = "case_id",
#     activity_id = "activity",
#     activity_instance_id = "activity_instance",
#     lifecycle_id = "status",
#     timestamp = "timestamp",
#     resource_id = "resource"
#   )
# 
# ## Map the process
# example_log<-example_log_5 %>%
#   process_map()
# 
# library(pm4py)
# library(petrinetR)
# 
# lab<-read.csv("labsForLog.csv")
# patients<-read.csv("ADMISSIONS.csv")
# 
# 
# 
# #processmapR::example_log 
# #patients_completes <- patients[patients$registration_type == "complete", ]
# #pn <- discovery_inductive(patients_completes)
# #render_PN(pn$petrinet)
# 
# example_log_4 %>%
#   trace_explorer(coverage = 0.8)
# 
# 
# ## http://www.bupar.net/subsetting.html
# example_log_4 %>%
#   filter_activity_presence(c("betaBlockers", "ACE"), method = "all")  %>%
#   traces
