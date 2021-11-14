#setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
source("Global.R")
pt <-read.csv("PATIENTS.csv")
biodata <-read.csv("cardiacSyndromes.csv")
service <- read.csv("serviceGroup.csv") %>% 
  select(where(is.numeric) ,-X)
lab <- read.csv("labGroup.csv") %>% 
  select(where(is.numeric),-X)
med <- read.csv("mediGroup.csv") %>% 
  select(where(is.numeric),-X)
microb <- read.csv("microGroup.csv") %>% 
  select(where(is.numeric),-X)
proc <- read.csv("procGroup.csv") %>% 
  select(where(is.numeric),-X)

data <-biodata %>% 
  mutate_all(na_if,"") %>% 
  mutate(MARITAL_STATUS = if_else(is.na(MARITAL_STATUS), "UNKNOWN", MARITAL_STATUS)) 
data$MARITAL_STATUS<-trimws(data$MARITAL_STATUS)
data$MARITAL_STATUS<-ifelse(grepl("(DEFAULT)",data$MARITAL_STATUS),"UNKNOWN",data$MARITAL_STATUS)

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
theme_gtsummary_compact()
#> Setting theme `Compact
data<-as.data.frame(data)
#trial2<-data %>%
 # select(GENDER,AGE,INSURANCE,MARITAL_STATUS,ETHNICITY2,Outcome) %>%
  #mutate(INSURANCE = recode(INSURANCE, Medicaid = "Public",Medicare="Public",Government="Public")) %>% 
  #mutate(MARITAL_STATUS = recode(MARITAL_STATUS,DIVORCED = "Living alone",SEPARATED="Living alone",
   #                             SINGLE="Living alone",WIDOWED="Living alone",'LIFE PARTNER'="Living with Partner",
    #                             MARRIED="Living with Partner")) %>% 
 # tbl_summary(
  #  by = Outcome,
   # statistic = list(all_continuous() ~ "{mean} ({sd})",
   #                  all_categorical() ~ "{n} ({p}%)"),
   # digits = all_continuous() ~ 2
    #label = ETHNICITY2 ~ "Ethinicity",
  #  missing_text = "(Missing)"
 # ) %>%
  #add_p() %>% 
  #modify_caption("**Patient Characteristics**") %>%
  #bold_labels()
#theme_gtsummary_journal(journal = "jama")
#> Setting theme `JAMA`
#theme_gtsummary_compact()
#> Setting theme `Compact`
#trial2


## Time PLot

dataPlot <-biodata %>% 
  select(ADMITTIME,EXPIRE_FLAG) %>% 
  mutate(year = lubridate::year(ADMITTIME)) %>% 
  select(-ADMITTIME)

Dead<-dataPlot %>% 
  filter(EXPIRE_FLAG==0) %>% 
  count(year, sort = TRUE) %>% 
  mutate(status = "Dead")

survive<-dataPlot %>% 
  filter(EXPIRE_FLAG== 1) %>% 
  count(year, sort = TRUE) %>% 
  mutate(status = "Survived")

dataPlot<-rbind(Dead,survive)

dataPlot<-ggplot(dataPlot, aes(x = year, y = n)) + 
  geom_line(aes(color = status), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()


