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

setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")

medication<-read.csv("medication.csv") %>% 
  mutate(start = as.Date.character(start)) %>% 
  mutate(complete = as.Date.character(complete))

medicationStart<-medication %>% 
  select(-complete) %>% 
  mutate(status = "start",timestamp=start) %>% 
  select(-SUBJECT_ID,-HADM_ID,-start) %>% 
  mutate(activity	= DRUG)

medicationComplete<-medication %>% 
  select(-start) %>% 
  mutate(status = "complete",timestamp=complete) %>% 
  select(-SUBJECT_ID,-HADM_ID,-complete) %>% 
  mutate(activity	= DRUG) 

medication2<-rbind(medicationStart,medicationComplete) 
medication2$activity_instance <- seq.int(nrow(medication2)) 

example_log_4 <-medication2 %>%
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

medicationComplete$activity_instance <- seq.int(nrow(medicationComplete)) 
example_log_5 <-medicationComplete %>%
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

## Map the process
example_log<-example_log_5 %>%
  process_map()

library(pm4py)
library(petrinetR)

lab<-read.csv("labsForLog.csv")


#processmapR::example_log 
#patients_completes <- patients[patients$registration_type == "complete", ]
#pn <- discovery_inductive(patients_completes)
#render_PN(pn$petrinet)

example_log_4 %>%
  trace_explorer(coverage = 0.8)


## http://www.bupar.net/subsetting.html
example_log_4 %>%
  filter_activity_presence(c("betaBlockers", "ACE"), method = "all")  %>%
  traces
