#This uses Geocoding and Heat Maps
library(dplyr)
library(readr)
library(tidyverse)
library(ggmap)
library(ggplot2)
library(xlsx)
setwd("/Users/zachtallevast/Box Sync/Lahmans/Master/core/")
People <- read_csv("people.csv")
#install.packages("tidyverse")
#install.packages("ggmap")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("source")

#Subset for US Born Players
Hometown = People[People$birthCountry == 'USA',]
#Get Distinct Cities 
cities1 <-distinct(Hometown, birthCity)
cities1
cities1 =na.omit(cities1)
#Geocode has a 2500 limit per day. 
typeof(cities1)
#Run First 2500 cities 
cities = cities1[1:2487,]
#Run the rest
cities = cities1[2488:4103,]

cities <-as.character(cities$birthCity)
location <- geocode(cities,output = "latlon")
#location <-geocode(cities$city, output = "all")
total =cbind(cities, location)

write.xlsx(total, "CityGeocodes1.xlsx")
geocodeQueryCheck()

total = read.csv("CityGeocodes.csv", header=T)
total1 = read.csv("CityGeocodes1.csv", header=T)
total = rbind(total,total1)

PeoplewLAT <- merge(x=People, y=total, by.x='birthCity', by.y='cities')
PeoplewLAT$lon <- as.numeric(as.character(PeoplewLAT$lon))
PeoplewLAT$lat <- as.numeric(as.character(PeoplewLAT$lat))
PeoplewLAT = PeoplewLAT[c(1,26,27)]
PeoplewLAT = transform(PeoplewLAT,freq.loc = ave(seq(nrow(PeoplewLAT)),birthCity, FUN = length))
UnitedStates <- get_map(location = 'united states', zoom =4, maptype = "terrain", source='google',color='color')

ggmap(UnitedStates) + geom_point(aes(x=lon, y=lat, colour=PeoplewLAT$freq.loc), data=PeoplewLAT, alpha = .8,na.rm=T) + 
  scale_color_gradient(low="red", high="blue")


ggmap(UnitedStates) + geom_point(aes(x=lon, y=lat), data=PeoplewLAT, col = "orange",alpha=0.4,size =PeoplewLAT$freq.loc*0.05) + 
  scale_size_continuous(range=range(PeoplewLAT$freq.loc))

PeoplewLAT = distinct(PeoplewLAT)
ggmap(UnitedStates, legend = "bottomright", extent = "device") + 
  geom_point(data=PeoplewLAT, aes(x=lon, y=lat,  size = PeoplewLAT$freq.loc), color = "red")+
  scale_colour_gradient(low = "white", high="red")+
  scale_size_continuous(range = c(1,10))

ggmap(UnitedStates, legend = "bottomright", extent = "device") + 
  geom_point(data=PeoplewLAT, aes(x=lon, y=lat,  size= PeoplewLAT$freq.loc, alpha=.8), color = "red")+
  scale_fill_gradient( breaks = c(5,10,50,100,225,300))


install.packages("devtools")
library(devtools)
install_github("dkahle/ggmap")
