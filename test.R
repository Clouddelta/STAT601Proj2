if(!require('glmnet',character.only = TRUE)) install.packages('glmnet')
if(!require('dplyr',character.only = TRUE)) install.packages('dplyr')
if(!require('sf',character.only = TRUE)) install.packages('sf')
if(!require('ggplot2',character.only = TRUE)) install.packages('ggplot2')
if(!require('shiny',character.only = TRUE)) install.packages('shiny')
library(glmnet)
library(dplyr)
library(sf)
library(ggplot2)
library(shiny)
data1=read.csv("CAHousing_data.csv", header = T)[,-1]%>%
  na.omit()

long_lat=data1%>%dplyr::select(longitude,latitude)

my_sf = st_as_sf(long_lat, coords = c('longitude', 'latitude'))
my_sf=cbind(my_sf,data1$median_house_value,data1$population)
names(my_sf)[c(1,2)]=c('median_house_value','population')
p1=my_sf%>%ggplot() + 
  geom_sf(aes(color=median_house_value))+xlab('longitude')+ylab('latitude')+
  scale_color_gradientn(colours = rainbow(10))+
  ggtitle("location vs price")
p2=my_sf%>%ggplot() + 
  geom_sf(aes(color=population))+xlab('longitude')+ylab('latitude')+
  scale_color_gradientn(colours = rainbow(10))+
  ggtitle("location vs population")
p3=data1%>%group_by(housing_median_age)%>%
  summarize(mean_value=mean(median_house_value))%>%
  ggplot()+
  geom_smooth(aes(housing_median_age,mean_value),se=FALSE)+
  ggtitle("house age vs price")
p4=data1%>%ggplot()+
  geom_boxplot(aes(ocean_proximity1,median_house_value,fill=ocean_proximity1))+
  ggtitle("distance_to_ocean vs price")
