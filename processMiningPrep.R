
services2<-merge(x=cardiacSyndromes2,y=services,by='HADM_ID',x.all=TRUE) %>% 
  mutate(TRANSFERTIME2 = ymd_hms(TRANSFERTIME)) %>% 
  mutate(Checktime = ifelse(endGoldenHour>=TRANSFERTIME2, "After", "Before")) %>% 
  filter(Checktime=="Before") %>% 
  select(HADM_ID,PREV_SERVICE,CURR_SERVICE) %>% 
  unite(cService, PREV_SERVICE, CURR_SERVICE, sep = "_", remove = FALSE) %>% 
  select(HADM_ID,cService)
services3<-services2 %>% count(HADM_ID, cService, sort = TRUE)
servicesWide <- services3 %>% 
  spread(cService, n) %>% 
  replace(is.na(.), 0) 
servicesWide<-merge(x=cardiacSyndromes2, y=servicesWide, by="HADM_ID", all.x = TRUE) %>% 
  replace(is.na(.), 0)%>% 
  select(-SUBJECT_ID,-endGoldenHour)
checkServicesBeforeAdmin<-servicesWide %>% filter(HADM_ID==0) 
nrow(checkServicesBeforeAdmin)
