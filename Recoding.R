df1<-data.frame(proc$X1)
names(df1)[1] <- "string"
df2<-data.frame(proc$X2)
names(df2)[1] <- "string"
df3<-data.frame(proc$X3)
names(df3)[1] <- "string"
df4<-data.frame(proc$X4)
names(df4)[1] <- "string"
df5<-data.frame(proc$X5)
names(df5)[1] <- "string"
df6<-data.frame(proc$X6)
names(df6)[1] <- "string"
df7<-data.frame(proc$X7)
names(df7)[1] <- "string"
df8<-data.frame(proc$X8)
names(df8)[1] <- "string"
df9<-data.frame(proc$X9)
names(df9)[1] <- "string"
df10<-data.frame(proc$X10)
names(df10)[1] <- "string"

df11<-data.frame(proc$X11)
names(df11)[1] <- "string"
df12<-data.frame(proc$X12)
names(df12)[1] <- "string"
df13<-data.frame(proc$X13)
names(df13)[1] <- "string"
df14<-data.frame(proc$X14)
names(df14)[1] <- "string"
df15<-data.frame(proc$X15)
names(df15)[1] <- "string"
df16<-data.frame(proc$X16)
names(df16)[1] <- "string"
df17<-data.frame(proc$X17)
names(df17)[1] <- "string"
df18<-data.frame(proc$X18)
names(df18)[1] <- "string"
df19<-data.frame(proc$X19)
names(df19)[1] <- "string"
df20<-data.frame(proc$X20)
names(df20)[1] <- "string"

df21<-data.frame(proc$X21)
names(df21)[1] <- "string"
df22<-data.frame(proc$X22)
names(df22)[1] <- "string"
df23<-data.frame(proc$X23)
names(df23)[1] <- "string"
df24<-data.frame(proc$X24)
names(df24)[1] <- "string"
df25<-data.frame(proc$X25)
names(df25)[1] <- "string"
df26<-data.frame(proc$X26)
names(df26)[1] <- "string"
df27<-data.frame(proc$X27)
names(df27)[1] <- "string"
df28<-data.frame(proc$X28)
names(df28)[1] <- "string"
df29<-data.frame(proc$X29)
names(df29)[1] <- "string"
df30<-data.frame(proc$X30)
names(df30)[1] <- "string"

df31<-data.frame(proc$X31)
names(df31)[1] <- "string"
df32<-data.frame(proc$X32)
names(df32)[1] <- "string"
df33<-data.frame(proc$X33)
names(df33)[1] <- "string"
df34<-data.frame(proc$X34)
names(df34)[1] <- "string"
df35<-data.frame(proc$X35)
names(df35)[1] <- "string"
df36<-data.frame(proc$X36)
names(df36)[1] <- "string"
df37<-data.frame(proc$X37)
names(df37)[1] <- "string"
df38<-data.frame(proc$X38)
names(df38)[1] <- "string"
df39<-data.frame(proc$X39)
names(df39)[1] <- "string"
df40<-data.frame(proc$X40)
names(df40)[1] <- "string"


df<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,
          df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,
          df21,df22,df23,df24,df25,df26,df27,df28,df29,df30,
          df31,df32,df33,df34,df35,df36,df37,df38,df39,df40)
df$string<-trimws(df$string)
df <- distinct(df)


m.mat <- hashed.model.matrix(c("string"), df, hash.size = 2 ^ 20,
                             create.mapping = TRUE)                             
mapping <- hash.mapping(m.mat)

# Extract the mapping
mapping <- hash.mapping(m.mat)
dat<-as.data.frame(hashed.value(names(mapping)))
#View(dat)
mean(duplicated(mapping))


names<-data.frame(names(mapping))
data<-cbind(dat,names)
data$names.mapping.<-str_replace(data$names.mapping., "string", "")
names(data)[1] <- "value"
names(data)[2] <- "string"
View(data)

proc<-merge(x=proc,y=data,by.x='X1',by.y="string") %>% 
  mutate(value1= value) %>% 
  select(-X1,-value) 
proc<-merge(x=proc,y=data,by.x='X2',by.y="string") %>% 
  mutate(value2= value) %>% 
  select(-X2,-value) 
proc<-merge(x=proc,y=data,by.x='X3',by.y="string") %>% 
  mutate(value3= value) %>% 
  select(-X3,-value) 
proc<-merge(x=proc,y=data,by.x='X4',by.y="string") %>% 
  mutate(value4= value) %>% 
  select(-X4,-value) 
proc<-merge(x=proc,y=data,by.x='X5',by.y="string") %>% 
  mutate(value5= value) %>% 
  select(-X5,-value) 
proc<-merge(x=proc,y=data,by.x='X6',by.y="string") %>% 
  mutate(value6= value) %>% 
  select(-X6,-value) 
proc<-merge(x=proc,y=data,by.x='X7',by.y="string") %>% 
  mutate(value7= value) %>% 
  select(-X7,-value) 
proc<-merge(x=proc,y=data,by.x='X8',by.y="string") %>% 
  mutate(value8= value) %>% 
  select(-X8,-value) 
proc<-merge(x=proc,y=data,by.x='X9',by.y="string") %>% 
  mutate(value9= value) %>% 
  select(-X9,-value)
proc<-merge(x=proc,y=data,by.x='X10',by.y="string") %>% 
  mutate(value10= value) %>% 
  select(-X10,-value) 
proc<-merge(x=proc,y=data,by.x='X11',by.y="string") %>% 
  mutate(value11= value) %>% 
  select(-X11,-value) 
proc<-merge(x=proc,y=data,by.x='X12',by.y="string") %>% 
  mutate(value12= value) %>% 
  select(-X12,-value) 
proc<-merge(x=proc,y=data,by.x='X13',by.y="string") %>% 
  mutate(value13= value) %>% 
  select(-X13,-value) 
proc<-merge(x=proc,y=data,by.x='X14',by.y="string") %>% 
  mutate(value14= value) %>% 
  select(-X14,-value) 
proc<-merge(x=proc,y=data,by.x='X15',by.y="string") %>% 
  mutate(value15= value) %>% 
  select(-X15,-value) 
proc<-merge(x=proc,y=data,by.x='X16',by.y="string") %>% 
  mutate(value16= value) %>% 
  select(-X16,-value) 
proc<-merge(x=proc,y=data,by.x='X17',by.y="string") %>% 
  mutate(value17= value) %>% 
  select(-X17,-value) 
proc<-merge(x=proc,y=data,by.x='X18',by.y="string") %>% 
  mutate(value18= value) %>% 
  select(-X18,-value)
proc<-merge(x=proc,y=data,by.x='X19',by.y="string") %>% 
  mutate(value19= value) %>% 
  select(-X19,-value) 
proc<-merge(x=proc,y=data,by.x='X20',by.y="string") %>% 
  mutate(value20= value) %>% 
  select(-X20,-value)
proc<-merge(x=proc,y=data,by.x='X21',by.y="string") %>% 
  mutate(value21= value) %>% 
  select(-X21,-value)
proc<-merge(x=proc,y=data,by.x='X22',by.y="string") %>% 
  mutate(value22= value) %>% 
  select(-X22,-value)
proc<-merge(x=proc,y=data,by.x='X23',by.y="string") %>% 
  mutate(value23= value) %>% 
  select(-X23,-value)
proc<-merge(x=proc,y=data,by.x='X24',by.y="string") %>% 
  mutate(value24= value) %>% 
  select(-X24,-value)
proc<-merge(x=proc,y=data,by.x='X25',by.y="string") %>% 
  mutate(value25= value) %>% 
  select(-X25,-value)
proc<-merge(x=proc,y=data,by.x='X26',by.y="string") %>% 
  mutate(value26= value) %>% 
  select(-X26,-value)
proc<-merge(x=proc,y=data,by.x='X27',by.y="string") %>% 
  mutate(value27= value) %>% 
  select(-X27,-value)
proc<-merge(x=proc,y=data,by.x='X28',by.y="string") %>% 
  mutate(value28= value) %>% 
  select(-X28,-value)
proc<-merge(x=proc,y=data,by.x='X29',by.y="string") %>% 
  mutate(value29= value) %>% 
  select(-X29,-value)
proc<-merge(x=proc,y=data,by.x='X30',by.y="string") %>% 
  mutate(value30= value) %>% 
  select(-X30,-value)
proc<-merge(x=proc,y=data,by.x='X31',by.y="string") %>% 
  mutate(value31= value) %>% 
  select(-X31,-value)
proc<-merge(x=proc,y=data,by.x='X32',by.y="string") %>% 
  mutate(value32= value) %>% 
  select(-X32,-value)
proc<-merge(x=proc,y=data,by.x='X33',by.y="string") %>% 
  mutate(value33= value) %>% 
  select(-X33,-value)
proc<-merge(x=proc,y=data,by.x='X34',by.y="string") %>% 
  mutate(value34= value) %>% 
  select(-X34,-value)
proc<-merge(x=proc,y=data,by.x='X35',by.y="string") %>% 
  mutate(value35= value) %>% 
  select(-X35,-value)
proc<-merge(x=proc,y=data,by.x='X36',by.y="string") %>% 
  mutate(value36= value) %>% 
  select(-X36,-value)
proc<-merge(x=proc,y=data,by.x='X37',by.y="string") %>% 
  mutate(value37= value) %>% 
  select(-X37,-value)
proc<-merge(x=proc,y=data,by.x='X38',by.y="string") %>% 
  mutate(value38= value) %>% 
  select(-X38,-value)
proc<-merge(x=proc,y=data,by.x='X39',by.y="string") %>% 
  mutate(value39= value) %>% 
  select(-X39,-value)
proc<-merge(x=proc,y=data,by.x='X40',by.y="string") %>% 
  mutate(value40= value) %>% 
  select(-X40,-value)


proc<-cbind(procW$HADM_ID,proc)
names(proc)[1] <- "HADM_ID"
proc<-merge(x=cardiacSyndromes2, y=proc, by="HADM_ID",all.x=TRUE) %>% 
  replace(is.na(.), -962114436) %>% 
  select(-SUBJECT_ID,-endGoldenHour)
  
