.libPaths("D:/R/Library")
#stuff for work laptop

setwd("~/Dropbox (LOBO Distributors)/Data/R Scripts/Dementia/Dementia_Wales/Data/")
#stuff for mac


#ignore the above^^



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


dementia19 <- read.csv('Data/dementia_wales_apr2018-mar2019.csv') %>% 
  group_by(Local.Health.Board) %>% 
  summarise(total19=sum(Dementia)) %>% 
  rename(lhb=Local.Health.Board)
dementia19$lhb <- gsub("&","and",dementia19$lhb)

#very simple for 2019. but this is a list of people actually on the register by the end of the period, rather than 
#the change in numbers

#plus a bit of cleanup and getting all the data in the same format

dementia18 <- read.csv('Data/dementia_wales_apr2017-mar2018.csv')%>% 
  filter(Register == 'Dementia') %>% 
  distinct() %>% 
  group_by(Local.Health.Board) %>% 
  summarise(total18=sum(Number.on.register)) %>% 
  rename(lhb=Local.Health.Board)
dementia18$lhb <- gsub(" University LHB","",dementia18$lhb)
dementia18$lhb <- gsub(" Teaching LHB","",dementia18$lhb)

  
#this is each LHB by the number of people on their dementia register

dementia17 <- read.csv('Data/dementia_wales_apr2016-mar2017.csv') %>% 
  filter(Register == 'Dementia') %>% 
  distinct() %>% 
  group_by(Local.Health.Board) %>% 
  summarise(total17=sum(Number.on.register)) %>% 
  rename(lhb=Local.Health.Board)
dementia17$lhb <- gsub(" University LHB","",dementia17$lhb)
dementia17$lhb <- gsub(" Teaching LHB","",dementia17$lhb)

#same for 2017


dementia16 <- read.csv('Data/dementia_wales_apr2015-mar2016.csv') %>% 
  group_by(Health.Board) %>% 
  summarise(total16=sum(Patients.on.register)) %>% 
  na.omit() %>% 
  rename(lhb=Health.Board)
dementia16$lhb <- gsub(" ULHB","",dementia16$lhb)
dementia16$lhb <- gsub(" Teaching LHB","",dementia16$lhb)
dementia16$lhb <- gsub("&","and",dementia16$lhb)

#same with 2016

dementia15 <- read.csv('Data/dementia_wales_apr2014-mar2015.csv') %>% 
  dplyr::select(1,4) %>% 
  rename(total15=Patients.on.register,
         lhb=Health.Board)
dementia15$lhb <- gsub(" ULHB","",dementia15$lhb)
dementia15$lhb <- gsub(" Teaching LHB","",dementia15$lhb)
dementia15$lhb <- gsub("&","and",dementia15$lhb)

#same with 2015

dementia14 <- read.csv('Data/dementia_wales_apr2013-mar2014.csv') %>% 
  dplyr::select(1,4) %>% 
  rename(total14=Patients.on.register,
         lhb=Health.Board)
dementia14$lhb <- gsub(" ULHB","",dementia14$lhb)
dementia14$lhb <- gsub(" Teaching LHB","",dementia14$lhb)
dementia14$lhb <- gsub("&","and",dementia14$lhb)
dementia14$lhb <- gsub("ABM","Abertawe Bro Morgannwg", dementia14$lhb)

#same with 2014

total <- dementia19 %>% 
  left_join(dementia18) %>% 
  left_join(dementia17) %>% 
  left_join(dementia16) %>% 
  left_join(dementia15) %>% 
  left_join(dementia14)
rm(dementia14,dementia15,dementia16,dementia17,dementia18,dementia19)
#joining all the sets and keeping environment tidy

total_change_wales <- total %>% 
  mutate("change_apr2014-mar2015"=total15-total14) %>%
  mutate("change_apr2015-mar2016"=total16-total15) %>%
  mutate("change_apr2016-mar2017"=total17-total16) %>%
  mutate("change_apr2017-mar2018"=total18-total17) %>%
  mutate("change_apr2018-mar2019"=total19-total18) 

total_change_wales <- total_change_wales[,c(1,7,6,5,4,3,2, 8:12)]

#making columns for the change between the years.

coords <- read.csv("Data/wales_coordinates.csv")

coords$lhb <- trimws(coords$lhb,"r")

total_change_wales <- left_join(total_change_wales,coords, by="lhb")

#adding in coords

write.csv(total_change_wales, "dementia_wales_all_years.csv")

