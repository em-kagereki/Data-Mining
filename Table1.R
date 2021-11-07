# https://stefvanbuuren.name/fimd/sec-MCAR.html

setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
library(gtsummary)
library(tidyverse)
library(tidyr)
data <-read.csv("cardiacSyndromes.csv") %>% 
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
  add_p() %>% 
  modify_caption("**Patient Characteristics**") %>%
  bold_labels()

trial2



library(FSelector)
library(FSelectorRcpp)

information.gain(Outcome~., data)


# NOT RUN {
irisX <- iris[-5]
y <- iris$Species

## data.frame interface
information_gain(x = irisX, y = y)

# formula interface
information_gain(formula = Species ~ ., data = iris)
information_gain(formula = Species ~ ., data = iris, type = "gainratio")
information_gain(formula = Species ~ ., data = iris, type = "symuncert")

# sparse matrix interface
library(Matrix)
i <- c(1, 3:8); j <- c(2, 9, 6:10); x <- 7 * (1:7)
x <- sparseMatrix(i, j, x = x)
y <- c(1, 1, 1, 1, 2, 2, 2, 2)

information_gain(x = x, y = y)
information_gain(x = x, y = y, type = "gainratio")
information_gain(x = x, y = y, type = "symuncert")

# }

library(boot)


trial2$GENDER <- 
  factor(trial2$GENDER, levels=c("F","M"),
         labels=c("Female", 
                  "Male"))
trial2$MARITAL_STATUS <- 
  factor(trial2$MARITAL_STATUS, levels=c("Living alone","Living with Partner","UNKNOWN","UNKNOWN (DEFAULT)"),
         labels=c("Living alone", 
                  "Living with Partner",
                  "UNKNOWN",
                  "UNKNOWN"))
label(trial2$GENDER)       <- "Sex"
label(trial2$AGE)       <- "Age"
label(trial2$MARITAL_STATUS)     <- "Marital Status"
label(trial2$ETHNICITY2) <- "Ethnicity"
label(trial2$INSURANCE) <- "Type of insurance"
units(trial2$AGE)       <- "years"

table1(~ GENDER + AGE + MARITAL_STATUS + ETHNICITY2+INSURANCE | Outcome, data=trial2)
