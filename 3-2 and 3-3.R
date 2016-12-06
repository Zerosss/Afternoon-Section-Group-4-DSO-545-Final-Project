setwd("C:/Users/Henry/Desktop/USC FALL2016/DSO545/final/Final Project")


data=read.csv('MyLA311_Service_Request_Data_2016.csv',header = T)
levels(data$RequestType)
library(ggplot2)
library(dplyr)
requests_summary=data%>%
  filter(!RequestType=='')%>%
  group_by(RequestType)%>%
  summarise(count=n())
ggplot(requests_summary,aes(x=reorder(RequestType,-count),y=count))+
  geom_bar(stat = 'identity')+
  ggtitle('Distribution of requests')+
  xlab('RequestType')+
  ylab('Numbers of requests')
levels(data$Owner)
Owner_summary=data%>%
  filter(!Owner=='')%>%
  group_by(Owner)%>%
  summarise(count=n())
ggplot(Owner_summary,aes(x=reorder(Owner,-count),y=count))+
  geom_bar(stat = 'identity')+
  ggtitle('Distribution Departments')+
  xlab('Departments')+
  ylab('Number of requests')
  

library(ggmap)
data_map=filter(data,!data$RequestType=='')


LAMap <- qmap("Los Angeles", zoom = 10, color = "bw", legend = "topleft")

LAMap + stat_bin2d(data = data, 
                        aes(x = Longitude, y = Latitude,
                            fill = Owner, alpha = 0.5))

              