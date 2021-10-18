setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")

library(tidyverse)

library(lubridate)

library(dplyr)

transfer<-read.csv("TRANSFERS.csv")
transfer$INTIME2 <- as.Date(transfer$INTIME)

transfer<-transfer%>%
  mutate(Period = ntile(as.numeric(INTIME2),12))%>% 
  mutate(year = year(INTIME2),month=month(INTIME2),day = mday(INTIME2),PeriodtoYear = (2011+Period)) %>% 
  mutate(decodedAdminDate = make_datetime(PeriodtoYear, month, day)) %>% 
  select(-year,-month,-day,-PeriodtoYear,-Period)%>%
  drop_na(LOS) %>% 
  select(decodedAdminDate,LOS) %>% 
  arrange(desc(decodedAdminDate))

set.seed(20)
sample<-transfer[sample(nrow(transfer), 20), ]
first<-min(sample$decodedAdminDate)
last<-max(sample$decodedAdminDate)
days<-as.numeric(difftime(last,first, units = "days"))
days


dates<-as.data.frame(seq(ymd(first), ymd(last), by = "1 days"))
dates$occupation<-0
names(dates)[1] <- "Date"


sample$decodedAdminDate<-as.Date(sample$decodedAdminDate)
 
df <- data.frame(matrix(ncol = 2, nrow = 1))
x <- c("Date", "values")
colnames(df) <- x

for (i in 1:nrow(sample)){
  first<-sample$decodedAdminDate[i]
  period<-sample$LOS[i]
  fullDays<-period%/%24
  mod<-period-(fullDays*24)
  mod<-mod
  da<-data.frame(cbind(first,period,fullDays,mod))
  da$first<-as_date(da$first, origin = lubridate::origin)
  da$first <- as.Date(da$first)
  last<-da$first+(fullDays+1)
  dates<-as.data.frame(seq(ymd(first), ymd(last), by = "1 days"))
  names(dates)[1] <- "Date"
  dates$values<-24
  dates[nrow(dates), "values"]<- mod
  df[nrow(df) + 1, ] <- dates
  }
  
# naming the columns
colnames(df)<-c("Date", "values")

df



#CCU	Coronary care unit
#CSRU	Cardiac surgery recovery unit
#MICU	Medical intensive care unit
#NICU	Neonatal intensive care unit
#NWARD	Neonatal ward
#SICU	Surgical intensive care unit
#TSICU	Trauma/surgical intensive care unit