
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
requests_wday$RequestSource= factor(requests_wday$RequestSource, 
                                    levels= c("Call", "Mobile App", "Self Service","Driver Self Report", "Email"))

ggplot(requests_wday,aes(x=weekday,y=per,fill=RequestSource))+
  geom_bar(stat='identity',position = 'dodge',color='black')+
  xlab('')+
  ylab('Percentage (%)')+
  ggtitle('Daily Requests of Top 5 Sources')+
  scale_fill_discrete(guide = guide_legend(title = "Request Source")) 

  

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


line=data2%>%filter(!is.na(WaitTimesseconds))%>%
  mutate(WaitTimesmin=WaitTimesseconds/60)


str(data2)
library(forecast)
ggplot(line,aes(x=RequestsbyAppWeb,y=WaitTimesmin))+geom_point()+
  stat_smooth(method = 'lm',color= "red",se=FALSE)+
  xlab('App & Web Usage (%)')+
  ylab('Waiting Time (min)')+
  ggtitle('Relationship of Waiting Time to Digital Channels')

?stat_smooth

regression=lm(WaitTimesseconds~RequestsbyAppWeb,data = line)
summary(regression)


