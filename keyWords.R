## Word analysis
diagTerms<-cardiacSyndromes %>% 
  select(HADM_ID,DIAGNOSIS) %>% 
  mutate(DIAGNOSIS = str_replace(DIAGNOSIS, "S/P", "status post")) %>% 
  mutate(DIAGNOSIS = str_replace(DIAGNOSIS, "R/O", "rule out")) %>% 
  mutate(DIAGNOSIS = str_replace(DIAGNOSIS, "\\;", "/"))  
  diagTerms2<-data.frame(str_split_fixed(diagTerms$DIAGNOSIS, "/", 7))