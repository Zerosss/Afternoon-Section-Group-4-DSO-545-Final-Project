
data= read.csv("MyLA311_Service_Request_Data_2016.csv", header = T)

library(lubridate)

library(dplyr)

library(ggplot2)

data$CreatedDate=mdy_hms(data$CreatedDate)

data$year= year(data$CreatedDate)
data$month= month(data$CreatedDate, label = T)

Q1=data%>%
  select(CreatedDate, RequestSource, year, month)%>%
  filter(RequestSource=='Mobile App'|RequestSource=='Call')%>%
  filter(year=="2016" & month!= "Dec") %>%
  group_by(month,RequestSource)%>%
  summarise(count=n())


ggplot(Q1,aes(x=factor(month),y=count,group= RequestSource, 
              color= RequestSource))+
  geom_line(size=2)+
  geom_point(size = 2, color= "Black")+
  xlab('')+
  ylab("Request  Volume")+
  ggtitle("2016 Monthly Changes in Call and Mobile App Request Volume")


Q2=data%>%
  filter(RequestSource=='Web Form'|RequestSource=='Twitter')%>%
  filter(year=="2016" & month!= "Dec") %>%
  group_by(year, month, RequestSource) %>%
  summarise(count=n())

Q2$time <-  paste(Q2$month, Q2$year, sep=" '")


Q2$time= factor(Q2$time, levels=c("Jan '2016", "Feb '2016","Mar '2016","Apr '2016","May '2016","Jun '2016","Jul '2016","Aug '2016", "Sep '2016","Oct '2016","Nov '2016"))

ggplot(Q2,aes(x=time,y=count,group= RequestSource, 
              color= RequestSource))+
  geom_line(size=1)+
  geom_point(size = 2, color= "Black")+
  xlab('')+
  ylab("Request  Volume")+
  ggtitle("Monthly Request Volume from Web Forms & Twitter")