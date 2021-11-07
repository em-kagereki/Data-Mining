setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4/codes")

source("Global.R")
setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
data<-read.csv("cardiacSyndromes.csv")%>% 
  mutate_all(na_if,"") %>% 
  mutate(MARITAL_STATUS = if_else(is.na(MARITAL_STATUS), "UNKNOWN", MARITAL_STATUS)) 


data$MARITAL_STATUS<-str_replace_all(data$MARITAL_STATUS, "UNKNOWN (DEFAULT)", "UNKNOWN")

#data$MARITAL_STATUS<-ifelse(grepl("UNKNOWN (DEFAULT)",data$MARITAL_STATUS),"UNKNOWN",data$MARITAL_STATUS)

data$ETHNICITY<-ifelse(grepl("UNKNOWN",data$ETHNICITY),"UNKNOWN",data$ETHNICITY)
data$ETHNICITY<-ifelse(grepl("PATIENT DECLINED",data$ETHNICITY),"UNKNOWN",data$ETHNICITY)
data$Outcome <- ifelse(data$EXPIRE_FLAG==1,"Deceased", "Survived")
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
#theme_gtsummary_compact()
#> Setting theme `Compact
table1<-data %>%
  select(GENDER,AGE,INSURANCE,MARITAL_STATUS,ETHNICITY2,Outcome) %>%
  mutate(INSURANCE = recode(INSURANCE, Medicaid = "Public",Medicare="Public",Government="Public")) %>% 
  mutate(MARITAL_STATUS = recode(MARITAL_STATUS,DIVORCED = "Living alone",SEPARATED="Living alone",
                                 SINGLE="Living alone",WIDOWED="Living alone",'LIFE PARTNER'="Living with Partner",
                                 MARRIED="Living with Partner")) 


table1<-data.frame(table1)

#table1 <- gtsummary::tbl_summary(table1)


date<-data 
date$ADMITTIME<-as.Date(date$ADMITTIME)
date<-date%>%
  select(ADMITTIME) %>% 
  count(ADMITTIME)

# Time series
ggplot(date, aes(x = ADMITTIME, y = n)) + 
  theme_minimal()









