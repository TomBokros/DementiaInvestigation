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


dementia19 <- read.csv('Data/dementia_scotland_2019.csv') %>% 
  rename(lhb=LHB,
         total19=Total) %>% 
  dplyr::select(1,2)


#scotland is !!predicted!! people diagnosed in each area of all ages, split by NHS boards.

#very simple for every year. 

dementia18 <- read.csv('Data/dementia_scotland_2018.csv') %>% 
  rename(lhb=LHB,
         total18=Total) %>% 
  dplyr::select(1,2)

dementia17 <- read.csv('Data/dementia_scotland_2017.csv') %>% 
  rename(lhb=LHB,
         total17=Total) %>% 
  dplyr::select(1,2)

dementia16 <- read.csv('Data/dementia_scotland_2016.csv') %>% 
  rename(lhb=LHB,
         total16=Total) %>% 
  dplyr::select(1,2)

dementia15 <- read.csv('Data/dementia_scotland_2015.csv') %>% 
  rename(lhb=LHB,
         total15=Total) %>% 
  dplyr::select(1,2)

dementia14 <- read.csv('Data/dementia_scotland_2014.csv') %>% 
  rename(lhb=LHB,
         total14=Total) %>% 
  dplyr::select(1,2)



total <- dementia19 %>% 
  left_join(dementia18) %>% 
  left_join(dementia17) %>% 
  left_join(dementia16) %>% 
  left_join(dementia15) %>% 
  left_join(dementia14)

#joining all the sets and keeping environment tidy

total <- total[,c(1,7,6,5,4,3,2)]

#bit of reordering
total <- total %>% 
  mutate("total14-15"=(`total15`-`total14`)) %>% 
  mutate("total15-16"=(`total16`-`total15`)) %>% 
  mutate("total16-17"=(`total17`-`total16`)) %>% 
  mutate("total17-18"=(`total18`-`total17`)) %>% 
  mutate("total18-19"=(`total19`-`total18`))

rm(dementia14,dementia15,dementia16,dementia17,dementia18,dementia19)

coords <- read.csv("Data/scotland_coordinates.csv")

#introducing the coords table

total_scotland <- left_join(total,coords)

write.csv(total_scotland, "dementia_scotland_all_years.csv")
