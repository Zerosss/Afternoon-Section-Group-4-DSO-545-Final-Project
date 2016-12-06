setwd("C:/Users/Henry/Desktop/USC FALL2016/DSO545/final/Final Project")
data=read.csv('MyLA311_Service_Request_Data_2016.csv',header = T)
levels(data$RequestType)
library(ggplot2)
library(dplyr)
library(lubridate)

data$CreatedDate=mdy_hms(data$CreatedDate)


#########weekday distribution
requests_wday=data%>%
  filter(RequestSource=='Call'|RequestSource=='Driver Self Report'|
           RequestSource=='Mobile App'|
           RequestSource=='Self Service'|RequestSource=='Email')%>%
  group_by(weekday=wday(CreatedDate,label=TRUE,abb=FALSE),RequestSource)%>%
  summarise(count=n())%>%
  mutate(per=count/sum(count))


requests_wday$weekday=factor(requests_wday$weekday,levels(requests_wday$weekday)[c(2:7,1)])

ggplot(requests_wday,aes(x=weekday,y=per,fill=RequestSource))+
  geom_bar(stat='identity',position = 'dodge',color='black')+
  xlab('Weekday')+
  ylab('Percentage')+
  ggtitle('Percentage of Top 5 Source')
  





library(ggmap)

LAMap <- qmap("Los Angeles", zoom = 10, color = "bw", legend = "topright")

##
alltypes.call=data%>%
  filter(RequestSource=='Call')
LAMap + stat_bin2d(data = alltypes.call, 
                   aes(x = Longitude, y = Latitude),alpha=0.8)+
  scale_fill_gradient2(low = 'white', high = 'red')+ggtitle('Call')  



alltypes.mobile=data%>%
  filter(RequestSource=='Mobile App')
LAMap + stat_bin2d(data = alltypes.mobile, 
                   aes(x = Longitude, y = Latitude),alpha=0.8)+
  scale_fill_gradient2(low = 'white', high = 'red')+ggtitle('Mobile APP')  



#################################

#####regression analysis


data2=read.csv('311_Data.csv',header = T)


line=data2%>%filter(!is.na(WaitTimesseconds))


str(data2)
library(forecast)
ggplot(line,aes(x=RequestsbyAppWeb,y=WaitTimesseconds))+geom_point()+
  stat_smooth(method = 'lm',aes(color='linear'),se=FALSE)+
  xlab('Rate of using App&web')+
  ylab('Waiting Time/second')+
  ggtitle('Regression Analysis')


regression=lm(WaitTimesseconds~RequestsbyAppWeb,data = line)
summary(regression)


