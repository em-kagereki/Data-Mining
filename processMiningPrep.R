Loinc <- read.csv("Loinc.csv") %>% 
  mutate(LOINC_CODE = LOINC_NUM) %>% 
  select(LOINC_CODE,RELATEDNAMES2)

## Source("PrepareLabData.R") ## The lab data is 1.72GB, manipulations were limited, therefore it was run in the server 
labNew<-read.csv("newdata.csv") 
labNew<- merge(x=labNew, y = Loinc, by = "LOINC_CODE", x.all=TRUE) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  unite(combined, LOINC_CODE,FLAG, sep = "-", remove = FALSE) %>% 
  select(-FLAG,-LABEL,-FLUID,LOINC_CODE,-X)
labNew$RELATEDNAMES3<-tolower(labNew$RELATEDNAMES2)
LabSubset <- labNew[grep("coagulation|full blood count|hemogram|quantititive", labNew$RELATEDNAMES3), ]


## Procedures
procedures<-read.csv("PROCEDUREEVENTS_MV.csv")
D_ITEM<-read.csv("D_ITEMS.csv")
procedures<-merge(x=procedures,y=D_ITEM, by = "ITEMID",x.all=TRUE) %>% 
  select(SUBJECT_ID,HADM_ID,STARTTIME,ENDTIME,LABEL) %>% 
  unite(combined, HADM_ID, LABEL, sep = "_", remove = FALSE) %>% 
  mutate(Label2 = tolower(LABEL))
procSubset <- procedures[grep("ct scan|cardiac cath|ekg|chest x-ray", procedures$Label2), ]


## Meds
meds<-read.csv("PRESCRIPTIONS.csv")
meds<-merge(x=cardiacSyndromes2,y=meds,by='HADM_ID',x.all=TRUE) %>% 
  mutate(drug2 = tolower(DRUG)) %>% 
  select(HADM_ID,DRUG,drug2) 
meds$drug2 <- str_replace(meds$drug2,"aspirin","Aspirin")  
meds$drug2 <- str_replace(meds$drug2,"morphine","Morphine")  
meds$drug2 <- str_replace(meds$drug2,(paste(HMGCoA, collapse="|")),"HMGCoA")  
meds$drug2 <- str_replace(meds$drug2,(paste(ACE, collapse="|")),"ACE Inhibitors")  
meds$drug2 <- str_replace(meds$drug2,(paste(betaBlockers, collapse="|")),"Beta blockers")  
meds$drug2 <- str_replace(meds$drug2,(paste(glycoproteinInhibitors, collapse="|")),"GpIIb/IIIa inhibitors")  
meds$drug2 <- str_replace(meds$drug2,"nitroglycerin","Nitroglycerine")
