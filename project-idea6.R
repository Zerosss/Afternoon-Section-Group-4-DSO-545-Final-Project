
data= read.csv("MyLA311_Service_Request_Data_2016.csv", header = T)
library(lubridate)
library(ggplot2)
library(dplyr)
library(stringr)

data$CreatedDate=mdy_hms(data$CreatedDate)

data$hour_request= hour(data$CreatedDate)

Call_Mobile= data %>%
  select(RequestSource,hour_request)%>%
  filter(RequestSource %in% c("Call", "Mobile App")) %>%
  group_by(hour_request, RequestSource) %>%
  summarise(count= n())

ggplot(Call_Mobile, aes(x= RequestSource, y= factor(hour_request), fill= count))+
  geom_tile()+
  scale_fill_gradient(low = "grey", high = "darkred")+
  xlab("")+
  ylab("Hour")+
  ggtitle("Change volume of requests by input type (app and call) over time")

Call= data %>%
  select(RequestSource,hour_request)%>%
  filter(RequestSource== "Call") %>%
  group_by(hour_request, RequestSource) %>%
  summarise(count= n())

ggplot(Call, aes(x= RequestSource, y= factor(hour_request), fill= count))+
  geom_tile()+
  scale_fill_gradient(low = "grey", high = "red")+
  xlab("")+
  ylab("Hour")+
  ggtitle("Change volume of requests by Phone Call over time")

app= data %>%
  select(RequestSource,hour_request)%>%
  filter(RequestSource== "Mobile App") %>%
  group_by(hour_request, RequestSource) %>%
  summarise(count= n())

ggplot(app, aes(x= RequestSource, y= factor(hour_request), fill= count))+
  geom_tile()+
  scale_fill_gradient(low = "grey", high = "blue")+
  xlab("")+
  ylab("Hour")+
  ggtitle("Change volume of requests by Mobile App over time")
