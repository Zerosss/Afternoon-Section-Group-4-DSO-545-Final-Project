library(data.table)
data=fread('data.csv',header = T, sep = ',',na.strings="NA")
data=as.data.frame(data)
 
library(lubridate)
library(dplyr)
library(ggplot2)
data$CreatedDate=mdy_hms(data$CreatedDate)

Q1=data%>%
  filter(RequestSource=='Mobile App'|RequestSource=='Call')%>%
  group_by(year(CreatedDate),month(CreatedDate),RequestSource)%>%
  summarise(count=n())

names(Q1)[1]='year'
names(Q1)[2]='month'

library(zoo)

Q1$time <-  paste(Q1$year, Q1$month, sep="-")
Q1$yearmon <- as.yearmon(paste(Q1$year, Q1$month), "%Y %m")

ggplot(Q1,aes(x=factor(yearmon),y=count,fill=RequestSource))+
  geom_bar(stat='identity',position='dodge')+
  xlab('month')+
  ggtitle('Overall trends in calls / apps')


Q2=data%>%
  filter(RequestSource=='Web Form'|RequestSource=='Twitter')%>%
  group_by(year(CreatedDate),month(CreatedDate),RequestSource)%>%
  summarise(count=n())
names(Q2)[1]='year'
names(Q2)[2]='month'

library(zoo)

Q2$time <-  paste(Q2$year, Q2$month, sep="-")
Q2$yearmon <- as.yearmon(paste(Q2$year, Q2$month), "%Y %m")

ggplot(Q2,aes(x=factor(yearmon),y=count,fill=RequestSource))+
  geom_bar(stat='identity',position='dodge')+
  xlab('month')+
  ggtitle('Web Forms/Twitter')

 
