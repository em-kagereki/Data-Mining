

setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4/codes")
source("code1.R")

## This function will help to transform all the blank cells into NA

diagnose(data)
num<-diagnose_numeric(data)

cat<-diagnose_category(data)
diagnose_outlier(data)

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

## https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

#table1<-table1(~ GENDER + AGE +INSURANCE+MARITAL_STATUS+ETHNICITY2| Outcome, data=data)
##We could split the table to show the relationship between the Deaths in the first and the subsequent admissions
##


##
##The population of interest is those who had at leasst one admission and were discharged
##
##The person got at least one discharge !!!
##


data2 <- data[ which(data$EXPIRE_FLAG==0 & data$nAdmissions >= 1), ]



## We identify the admission numbers that are outliers

data2$Outlier<- aout.pois(data2$admissionCycle, param = median(data2$admissionCycle), alpha = 0.01)

ggplot(data2, aes(x = factor(admissionCycle))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent) +
  geom_vline(data=data2, aes(xintercept=5, color="red"),
             linetype="dashed")


##
##Here we remove the outlier values
##


data3 <- data2[ which(data2$admissionCycle<5), ]

#data3<-data3 %>% 
#  dplyr::select(HADM_ID,SUBJECT_ID,ADMISSION_TYPE,ADMISSION_LOCATION,DISCHARGE_LOCATION,INSURANCE,LANGUAGE,RELIGION,MARITAL_STATUS,
#                     ETHNICITY,DIAGNOSIS,GENDER,AGE,LOS,Period,admissionCycle,timeToNextAdmission,stayTransfers,Outcome)

ggplot(data3, aes(x = factor(admissionCycle))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent) +
  geom_vline(data=data2, aes(xintercept=5, color="red"),
             linetype="dashed")











