
setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")

data <-read.csv("cardiacSyndromes.csv") %>% 
  select(ADMITTIME,EXPIRE_FLAG) %>% 
  mutate(year = lubridate::year(ADMITTIME)) %>% 
  select(-ADMITTIME)

Dead<-data %>% 
  filter(EXPIRE_FLAG==0) %>% 
  count(year, sort = TRUE) %>% 
  mutate(status = "Dead")

survive<-data %>% 
  filter(EXPIRE_FLAG== 1) %>% 
  count(year, sort = TRUE) %>% 
  mutate(status = "Survived")

data<-rbind(Dead,survive)

#ggplot(data) + aes(Dead, Frequency) + geom_line()


plot<-ggplot(data, aes(x = year, y = n)) + 
  geom_line(aes(color = status), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

