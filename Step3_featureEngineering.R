## PCA
source("summaryStats.R")
gender<-data.frame(prop.table(table(biodata$GENDER)))
names(gender)[names(gender) == "Var1"] <- "GENDER"
names(gender)[names(gender) == "Freq"] <- "FreqGENDER"
admintype<-data.frame(prop.table(table(biodata$ADMISSION_TYPE)))
names(admintype)[names(admintype) == "Var1"] <- "ADMISSION_TYPE"
names(admintype)[names(admintype) == "Freq"] <- "FreqADMISSION_TYPE"
adminloc<-data.frame(prop.table(table(biodata$ADMISSION_LOCATION)))
names(adminloc)[names(adminloc) == "Var1"] <- "ADMISSION_LOCATION"
names(adminloc)[names(adminloc) == "Freq"] <- "FreqADMISSION_LOCATION"
insurance<-data.frame(prop.table(table(biodata$INSURANCE)))
names(insurance)[names(insurance) == "Var1"] <- "INSURANCE"
names(insurance)[names(insurance) == "Freq"] <- "FreqINSURANCE"
religion<-data.frame(prop.table(table(biodata$RELIGION)))
names(religion)[names(religion) == "Var1"] <- "RELIGION"
names(religion)[names(religion) == "Freq"] <- "FreqRELIGION"
marital<-data.frame(prop.table(table(biodata$MARITAL_STATUS)))
names(marital)[names(marital) == "Var1"] <- "MARITAL_STATUS"
names(marital)[names(marital) == "Freq"] <- "FreqMARITAL_STATUS"
ethninicity<-data.frame(prop.table(table(biodata$ETHNICITY)))
names(ethninicity)[names(ethninicity) == "Var1"] <- "ETHNICITY"
names(ethninicity)[names(ethninicity) == "Freq"] <- "FreqETHNICITY"
language<-data.frame(prop.table(table(biodata$LANGUAGE)))
names(language)[names(language) == "Var1"] <- "LANGUAGE"
names(language)[names(language) == "Freq"] <- "FreqLANGUAGE"
dischargeLocation<-data.frame(prop.table(table(biodata$DISCHARGE_LOCATION)))
names(dischargeLocation)[names(dischargeLocation) == "Var1"] <- "DISCHARGE_LOCATION"
names(dischargeLocation)[names(dischargeLocation) == "Freq"] <- "FreqDISCHARGE_LOCATION"
data2<-merge(x=biodata, y=gender, by="GENDER", all=TRUE)
data2<-merge(x=data2, y=admintype, by="ADMISSION_TYPE", all=TRUE)
data2<-merge(x=data2, y=adminloc, by="ADMISSION_LOCATION", all=TRUE)
data2<-merge(x=data2, y=insurance, by="INSURANCE", all=TRUE)
data2<-merge(x=data2, y=religion, by="RELIGION", all=TRUE)
data2<-merge(x=data2, y=marital, by="MARITAL_STATUS", all=TRUE)
data2<-merge(x=data2, y=ethninicity, by="ETHNICITY", all=TRUE)
data2<-merge(x=data2, y=dischargeLocation, by="DISCHARGE_LOCATION", all=TRUE)
data2<-merge(x=data2, y=language, by="LANGUAGE", all=TRUE)
biodata <- data2%>% dplyr::select(where(is.numeric))
data2<-data2%>%
  select(-GENDER,-ADMISSION_TYPE,-ADMISSION_LOCATION,-INSURANCE,-RELIGION,-MARITAL_STATUS,-DISCHARGE_LOCATION,-ETHNICITY,-LANGUAGE,
         -DISCHTIME,-DEATHTIME,-EDREGTIME,-EDOUTTIME,-DIAGNOSIS,-HAS_CHARTEVENTS_DATA,-DOB,-LOS2,-Period,-endGoldenHour,-nAdmissions,-HOSPITAL_EXPIRE_FLAG,
         -deadBefore,-SUBJECT_ID,-DIAGNOSIS2,-DIAGNOSIS3,-X,-dayOfYear,-Month,-week,-weekday,-year,-hour)

data2<-merge(x=data2,y=lab, by="HADM_ID", all=TRUE)%>%
  replace(is.na(.), 0)
data2<-merge(x=data2,y=microb, by="HADM_ID", all=TRUE)%>%
  replace(is.na(.), 0)

data2<-merge(x=data2,y=proc, by="HADM_ID", all=TRUE)%>%
  replace(is.na(.), 0)
data2<-merge(x=data2,y=med, by="HADM_ID", all=TRUE)%>%
  replace(is.na(.), 0)

#data<-data2
#index = createDataPartition(data$HADM_ID, p = 0.70, list = FALSE)
#train = data[index, ]
#test = data[-index, ]

#train_y<-data.frame(train$EXPIRE_FLAG)
#train<-train%>%
#  select(-HADM_ID,-EXPIRE_FLAG)%>%
#  select_if(~!all(is.na(.))) # Remove any column with all 0s

#preObj <- preProcess(train, method=c("center", "scale"))
#train <- predict(preObj, train)
#train<-data.frame(train)

#p <- pca(train)

#trainPcaPlot<-p$loadings[,1:17]
#trainPcaPlot<-cbind(trainPcaPlot,train_y$train.EXPIRE_FLAG)
#names(trainPcaPlot)[names(trainPcaPlot) == 'train_y$train.EXPIRE_FLAG'] <- "Outcome"
#pcaPlot<-trainPcaPlot %>% 
#  mutate(Outcome = ifelse(Outcome==0, "Survived","Died"))%>%
#  ggplot(aes(PC1, PC2, color = Outcome)) + geom_point()


#library(plotly)
#fig <- plot_ly()
#trainPcaPlot$Outcome <- as.factor(trainPcaPlot$Outcome)
#fig <- plot_ly(trainPcaPlot, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Outcome, colors = c('#BF382A', '#0C4B8E'))
#fig <- fig %>% add_markers()
#fig <- fig %>% plotly::layout(scene = list(xaxis = list(title = 'Weight'),
#                                           yaxis = list(title = 'Gross horsepower'),
#                                           zaxis = list(title = '1/4 mile time')))

#fig


#Ninty<- which(cumsum(p$variance) > 90)[1]


#horn <- parallelPCA(train)
#horn$n


#elbow <- findElbowPoint(p$variance)
#elbow


#screePlot<-PCAtools::screeplot(p,
#                    components = getComponents(p, 1:20),
#                    vline = c(horn$n, elbow)) +
  
#  geom_label(aes(x = horn$n + 1, y = 50,
#                 label = 'Horn\'s', vjust = -1, size = 8)) +
#  geom_label(aes(x = elbow + 1, y = 50,
#                 label = 'Elbow method', vjust = -1, size = 8))

horn <- parallelPCA(pcaData)
elbow <- findElbowPoint(p$variance)

screePlot<-PCAtools::screeplot(p,
                    components = getComponents(p, 1:20),
                    vline = c(horn$n, elbow)) + 
  geom_label(aes(x = horn$n + 1, y = 50,
                 label = 'Horn\'s', vjust = -1, size = 8)) +
  geom_label(aes(x = elbow + 1, y = 50,
                 label = 'Elbow method', vjust = -1, size = 8))
