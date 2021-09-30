



library(dlookr)

suppressWarnings(dlookr)

setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4/codes")
source("code1.R")





## This function will help to transform all the blank cells into NA



diagnose(data)

num<-diagnose_numeric(data)

cat<-diagnose_category(data)
diagnose_outlier(data)



library(boot) 
library(htmlTable)
library(table1)



data <- data %>% mutate_all(na_if,"")
data$MARITAL_STATUS<-ifelse(data$MARITAL_STATUS=="UNKNOWN (DEFAULT)",'UNKNOWN', data$MARITAL_STATUS)
data$MARITAL_STATUS <-data$MARITAL_STATUS %>% replace_na("UNKNOWN")


data$EXPIRE_FLAG<-as.numeric(data$EXPIRE_FLAG)
data2<-data %>% 
  dplyr::mutate(Readmission = EXPIRE_FLAG) %>% 
  filter(admissionCycle==1)

data2$Outcome <- ifelse(data2$EXPIRE_FLAG==1 & data2$nAdmissions==1,"Deceased", "Readmited")
data2$Outcome <- ifelse(data2$EXPIRE_FLAG==0 & data2$nAdmissions==1,"Loss to followup", data2$Outcome)




data2$ETHNICITY<-ifelse(grepl("UNKNOWN",data2$ETHNICITY),"UNKNOWN",data2$ETHNICITY)
data2$ETHNICITY<-ifelse(grepl("PATIENT DECLINED",data2$ETHNICITY),"UNKNOWN",data2$ETHNICITY)

data2$Outcome <- ifelse(data2$EXPIRE_FLAG==1 & data2$nAdmissions==1,"Deceased", "Readmited")

data2$ETHNICITY2<-ifelse(grepl("ASIAN",data2$ETHNICITY),"ASIAN",data2$ETHNICITY)
data2$ETHNICITY2<-ifelse(grepl("MIDDLE EASTERN",data2$ETHNICITY2),"ASIAN",data2$ETHNICITY2)


data2$ETHNICITY2<-ifelse(grepl("HISPANIC",data2$ETHNICITY2),"HISPANIC",data2$ETHNICITY2)
data2$ETHNICITY2<-ifelse(grepl("SOUTH AMERICAN",data2$ETHNICITY2),"HISPANIC",data2$ETHNICITY2)
data2$ETHNICITY2<-ifelse(grepl("WHITE - BRAZILIAN",data2$ETHNICITY2),"HISPANIC",data2$ETHNICITY2)
data2$ETHNICITY2<-ifelse(grepl("CARIBBEAN ISLAND",data2$ETHNICITY2),"HISPANIC",data2$ETHNICITY2)


data2$ETHNICITY2<-ifelse(grepl("ALASKA NATIVE",data2$ETHNICITY2),"CAUCASIAN",data2$ETHNICITY2)
data2$ETHNICITY2<-ifelse(grepl("NATIVE HAWAIIAN",data2$ETHNICITY2),"CAUCASIAN",data2$ETHNICITY2)
data2$ETHNICITY2<-ifelse(grepl("PORTUGUESE",data2$ETHNICITY2),"CAUCASIAN",data2$ETHNICITY2)
data2$ETHNICITY2<-ifelse(grepl("WHITE",data2$ETHNICITY2),"CAUCASIAN",data2$ETHNICITY2)

data2$ETHNICITY2<-ifelse(grepl("BLACK",data2$ETHNICITY2),"BLACK",data2$ETHNICITY2)

data2$ETHNICITY2<-ifelse(grepl("MULTI RACE ETHNICITY",data2$ETHNICITY2),"OTHER",data2$ETHNICITY2)

data2$ETHNICITY2<-ifelse(grepl("UNABLE TO OBTAIN",data2$ETHNICITY2),"UNKNOWN",data2$ETHNICITY2)


pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

## https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

table1(~ GENDER + AGE +INSURANCE+MARITAL_STATUS+ETHNICITY2| Outcome, data=data2,overall=F, extra.col=list(`P-value`=pvalue))






