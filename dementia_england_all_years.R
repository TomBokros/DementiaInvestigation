.libPaths("D:/R/Library")
library(tidyverse)
library(lubridate)

#Please be aware that the names of the read.csv files are not the names of the original data.
#They are the names I gave them for my ease of reading.


dementia19 <- read.csv('Data/dementia_england_apr2018-mar2019.csv')
dementia19[is.na(dementia19)] <- 0
dementia19 <- dementia19 %>%
  filter(Measure=="DEMENTIA_REGISTER_0_64"|Measure=="DEMENTIA_REGISTER_65_PLUS") %>% 
  rename(date=ACH_DATE,
         name=NAME,
         measure=Measure,
         value=Value,
         cc_code=COMMISSIONER_ORGANISATION_CODE) %>% 
  mutate(newdate = dmy(date)) %>% 
  dplyr::select(-date) %>% 
  rename(date=newdate)

#some cleaning, and also only taking the values we need, people on the dementia register. 
#Adding 0's instead of N/A, because NA's mess up addition.

dementia19$date <- as.character(dementia19$date)

dementia19$date <- as.character(dementia18$date)
total_england_apr2018_to_mar2019 <- dementia19 %>% 
  group_by(date, cc_code) %>% 
  summarise(total=sum(as.numeric(as.character(value)))) %>% 
  spread(date,total) %>% 
  mutate("change18-19"=(`2019-03-31`)-(`2018-04-30`)) %>% 
  select(1,2,13,14)

rm(dementia19)

#just some housekeeping to keep my environment clean.
#that's it for 2019.


dementia18 <- read_csv("Data/dementia_england_apr2017-mar2018.csv")
dementia18[is.na(dementia18)] <- 0
dementia18 <- dementia18 %>%
  filter(Measure=="DEMENTIA_REGISTER_0_64"|Measure=="DEMENTIA_REGISTER_65_PLUS") %>% 
  rename(date=ACH_DATE,
         name=NAME,
         measure=Measure,
         value=Value,
         cc_code=COMMISSIONER_ORGANISATION_CODE) %>% 
  mutate(newdate = dmy(date)) %>% 
  dplyr::select(-date) %>% 
  rename(date=newdate)
dementia18$date <- as.character(dementia18$date)
total_england_apr2017_to_mar2018 <- dementia18 %>% 
  group_by(date, cc_code) %>% 
  summarise(total=sum(as.numeric(as.character(value)))) %>% 
  spread(date,total) %>% 
  mutate("change17-18"=(`2018-03-31`)-(`2017-04-30`)) %>% 
  select(1,2,13,14)

rm(dementia18)

#same for 2017-18, just changing some functions for the different year

dementia17 <- read.csv("Data/dementia_england_apr2016-mar2017.csv")
dementia17[is.na(dementia17)] <- 0
dementia17 <- dementia17 %>%
  filter(MEASURE=="DEMENTIA_REGISTER_0_64"|MEASURE=="DEMENTIA_REGISTER_65_PLUS") %>% 
  rename(date=ACH_DATE,
         name=NAME,
         measure=MEASURE,
         value=VALUE,
         cc_code=COMMISSIONER_ORGANISATION_CODE) %>% 
  mutate(newdate = dmy(date)) %>% 
  dplyr::select(-date) %>% 
  rename(date=newdate)
dementia17$date <- as.character(dementia17$date)
total_england_apr2016_to_mar2017 <- dementia17 %>% 
  group_by(date, cc_code) %>% 
  summarise(total=sum(as.numeric(as.character(value)))) %>% 
  spread(date,total) %>% 
  mutate("change16-17"=(`2017-03-31`)-(`2016-04-30`)) %>% 
  select(1,2,13,14)

rm(change17,dementia17)

#same for 2016-2017

dementia16 <- read.csv('Data/dementia_england_apr2015-mar2016.csv')
dementia16[is.na(dementia16)] <- 0
dementia16 <- dementia16 %>%
  filter(MEASURE=="DEMENTIA_REGISTER_0_64"|MEASURE=="DEMENTIA_REGISTER_65_PLUS") %>% 
  rename(date=DATE,
         name=PRACTICE_NAME,
         measure=MEASURE,
         value=VALUE,
         cc_code=CCG_CODE) %>% 
  mutate(newdate = dmy(date)) %>% 
  dplyr::select(-date) %>% 
  rename(date=newdate)
dementia16$date <- as.character(dementia16$date)
total_england_apr2015_to_mar2016 <- dementia16 %>% 
  group_by(date, cc_code) %>% 
  summarise(total=sum(as.numeric(as.character(value)))) %>% 
  spread(date,total) %>% 
  mutate("change15-16"=(`2016-03-31`)-(`2015-04-30`)) %>% 
  select(1,2,13,14)
rm(dementia16)

#same for 2016

dementia15 <- read.csv('Data/dementia_england_apr2014-mar2015.csv')
dementia15$April.Dementia.Register <-  as.numeric(as.character(dementia15$April.Dementia.Register))
dementia15$March.Dementia.Register <-  as.numeric(as.character(dementia15$March.Dementia.Register))

dementia15[is.na(dementia15)] <- 0

total_england_apr2014_to_mar2015 <- dementia15 %>% 
  rename(name=Practice.name,
         cc_code=CCG.code,
         apr2014=April.Dementia.Register,
         mar2015=March.Dementia.Register) %>%  
  group_by(cc_code) %>% 
  summarise(apr2014=sum(apr2014),mar2015=sum(mar2015)) %>% 
  mutate("change14-15"=(mar2015-apr2014))
rm(dementia15)

#2015 was different but the principle is the same. it just broke the numbers down in columns already,
#so I could skip a bit of the code.

dementia_england_all_years <- full_join(total_england_apr2018_to_mar2019,total_england_apr2017_to_mar2018) %>% 
  full_join(total_england_apr2016_to_mar2017) %>% 
  full_join(total_england_apr2015_to_mar2016) %>% 
  full_join(total_england_apr2014_to_mar2015)

dementia_england_all_years <- dementia_england_all_years[,c(1,14,15,11,12,8,9,5,6,2,3,16,13,10,7,4)]

#compiling all the data into one data frame and ordering it nicely.

lookup19 <- read.csv("Data/name_lookup_2019.csv") %>% 
  rename(name=CCG19NM,
         cc_code=CCG19CDH) %>% 
  dplyr::select(cc_code,name) %>% 
  distinct()
lookup18 <- read.csv("Data/name_lookup_2018.csv") %>% 
  rename(name=CCG18NM,
         cc_code=CCG18CDH) %>% 
  dplyr::select(cc_code,name) %>% 
  distinct()
lookup17 <- read.csv("Data/name_lookup_2017.csv") %>% 
  rename(name=CCG17NM,
         cc_code=CCG17CDH) %>% 
  dplyr::select(cc_code,name) %>% 
  distinct()
lookup16 <- read.csv("Data/name_lookup_2016.csv") %>% 
  rename(name=CCG16NM,
         cc_code=CCG16CDH) %>% 
  dplyr::select(cc_code,name) %>% 
  distinct()
lookup15 <- read.csv("Data/name_lookup_2015.csv") %>% 
  rename(name=CCG15NM,
         cc_code=CCG15CDH) %>% 
  dplyr::select(cc_code,name) %>% 
  distinct()

lookup <- full_join(lookup19,lookup18) %>% 
  full_join(lookup17) %>% 
  full_join(lookup16) %>% 
  full_join(lookup15) 

lookup$name <- gsub("&","and",lookup$name)
rm(lookup19,lookup18,lookup17,lookup16,lookup15)
#this is a reference guide that can be used to translate ccg codes into actual ccg names

dementia_england_all_years <- left_join(dementia_england_all_years,lookup) %>% 
  distinct()
dementia_england_all_years <- dementia_england_all_years[,c(1,17,2:16)] %>% 
  select(1,2,3,5,7,9,11,12) %>% 
  mutate("Total Change April 2014 to April 2015"= (`2015-04-30`-`apr2014`), 
         "Total Change April 2015 to April 2016"= (`2016-04-30`-`2015-04-30`), 
         "Total Change April 2016 to April 2017"= (`2017-04-30`-`2016-04-30`),
         "Total Change April 2017 to April 2018"= (`2018-04-30`-`2017-04-30`),
         "Total Change April 2018 to March 2019"= (`2019-03-31`-`2018-04-30`))

write.csv(dementia_england_all_years,'dementia_england_all_years.csv')
