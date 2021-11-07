



lab<-read.csv("LABEVENTS.csv") %>% 
  select(HADM_ID,SUBJECT_ID,FLUID,LABEL,FLAG,CHARTTIME)
data2<-data[data$SUBJECT_ID %in% cardiac$SUBJECT_ID, ]
lab %>%
 filter(gene_ID %in% accessions40$V1)
  labiTem<-read.csv("D_LABITEMS.csv")
  lab2<-merge(x=lab, y=labiTem, by="ITEMID", all.x = TRUE) %>% 
  filter(ROW_ID>1) %>% ## The first row contains the test lable and not the actual test
  unite(combined, FLUID, LABEL, sep = "-", remove = FALSE) %>% 
  select(HADM_ID,CHARTTIME,FLAG,combined) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>%   ## Replace the blanks
  mutate(CHARTTIME = ymd_hms(CHARTTIME))
  lab3<-merge(x=cardiacSyndromes2,y=lab2,by='HADM_ID',x.all=TRUE) %>% 
  mutate(Checktime = ifelse(endGoldenHour>=CHARTTIME, "After", "Before")) %>% 
  filter(Checktime=="Before",FLAG=="abnormal") %>% 
 select(-Checktime) %>% 
 select(HADM_ID,combined,FLAG) %>% 
 unite(combined, combined, FLAG, sep = "_", remove = FALSE) %>% 
 count(HADM_ID, combined, sort = TRUE)

labWide <- lab3 %>% 
  spread(combined, n) %>% 
  replace(is.na(.), 0)


#labMerge<-merge(x=cardiacSyndromes2, y=labWide, by="HADM_ID", all.x = TRUE)