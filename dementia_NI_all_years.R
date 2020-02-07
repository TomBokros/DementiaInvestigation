.libPaths("D:/R/Library")
#stuff for work laptop

setwd("~/Dropbox (LOBO Distributors)/Data/R Scripts/Dementia/Dementia_England/Data/")
#stuff for mac

library("tidyverse")
library("shiny")
library('knitr')
library('lubridate')
library("reshape2")
library("plotly")
library("tools")
library(sf)
library(raster)
library(spData)
library(tmap)    
library(leaflet) 
library(mapview) 
library(shiny)   
library(splitstackshape)
library(data.table)

dementia19<- read_csv("Data/dementia_NI_apr2018-march2019.csv") %>% 
  rename(total19=Dementia,
         lcg=LCG) %>% 
  dplyr::select(lcg,total19)
dementia19 <- dementia19[-nrow(dementia19),]


#picking just the bits we need and dropping the last row because we dont really need that.

dementia18<- read_csv("Data/dementia_NI_apr2017-march2018.csv") %>% 
  rename(total18=Dementia,
         lcg=LCG) %>% 
  dplyr::select(lcg,total18)
dementia18 <- dementia18[-nrow(dementia18),]

dementia17<- read_csv("Data/dementia_NI_apr2016-march2017.csv") %>% 
  rename(total17=Dementia,
         lcg=LCG) %>% 
  dplyr::select(lcg,total17)
dementia17 <- dementia17[-nrow(dementia17),]

dementia16<- read_csv("Data/dementia_NI_apr2015-march2016.csv") %>% 
  rename(total16=Dementia,
         lcg=LCG) %>% 
  dplyr::select(lcg,total16)
dementia16 <- dementia16[-nrow(dementia16),]

dementia15<- read_csv("Data/dementia_NI_apr2014-march2015.csv") %>% 
  rename(total15=Dementia,
         lcg=LCG) %>% 
  dplyr::select(lcg,total15)
dementia15 <- dementia15[-nrow(dementia15),]
dementia15$lcg <- c("Belfast Area","South Eastern Area","Northern Area","Southern Area","Western Area")

#almost identical for next years

total <- dementia19 %>% 
  left_join(dementia18) %>% 
  left_join(dementia17) %>% 
  left_join(dementia16) %>% 
  left_join(dementia15)
rm(dementia15,dementia16,dementia17,dementia18,dementia19)

#joining all the sets and keeping environment tidy

total_change_NI <- total %>% 
  mutate("change_apr2015-mar2016"=total16-total15) %>%
  mutate("change_apr2016-mar2017"=total17-total16) %>%
  mutate("change_apr2017-mar2018"=total18-total17) %>%
  mutate("change_apr2018-mar2019"=total19-total18) 

#finding the change

coords <- read.csv("Data/NI_coordinates.csv") %>% 
  dplyr::select(-2)
coords$lcg <- c("Belfast Area","Northern Area","Southern Area","Western Area","South Eastern Area")


#introducing the coords table

total_change_NI <- left_join(total_change_NI,coords)

total_change_NI <- total_change_NI[,c(1,6,5,4,3,2,7:12)]

write.csv(total_change_NI, "dementia_NI_all_years.csv")




    