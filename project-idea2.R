
library(dplyr)
library(ggplot2)

guides(fill = guide_legend(reverse = TRUE))

data= read.csv("MyLA311_Service_Request_Data_2016.csv", header = T)
summary(data$RequestSource)

referral_type = data %>%
  select(RequestType, RequestSource) %>%
  filter(RequestSource %in% c("Call", "Mobile App"))%>%
  group_by(RequestSource, RequestType) %>%
  summarise(count= n())

ggplot(referral_type,aes(x= RequestSource, y=count))+
  geom_bar(aes(fill= RequestType), stat = "identity")+
  coord_flip()

ggplot(referral_type, aes(x= RequestSource,y= count, fill= RequestType))+
  geom_bar(position = position_dodge(0.8),width = 0.75, stat = "identity")+
  geom_text(aes(label= count),size=2,vjust=-0.5, hjust= 0.5,position = position_dodge(0.8))+
  ggtitle("Phone Call VS Mobile App referrals on Total Amount and Corresponding Service Types")

alltypes= data %>%
  select(RequestType, RequestSource) %>%
  group_by(RequestSource, RequestType)%>%
  summarise(amount= n())


top_five= head(alltypes, 5)

ggplot(top_five, aes(x= reorder(RequestSource, desc(amount)), y= amount))+
  geom_bar(stat = "identity")

top5_type = data %>%
  select(RequestType, RequestSource) %>%
  filter(RequestSource %in% c("Call", "Driver Self Report", "Mobile App", "Self Service", "Email"))%>%
  group_by(RequestSource, RequestType) %>%
  summarise(count= n())

top5_type$RequestSource = factor(top5_type$RequestSource, levels =c("Call", "Driver Self Report", "Mobile App", "Self Service", "Email"))

ggplot(top5_type,aes(x= RequestSource, y=count))+
  geom_bar(aes(fill= RequestType), stat = "identity")+
  coord_flip()+
  xlab("Channels")+
  ggtitle("Service Types Distribution of Top 5 Input Channels")

ggplot(top5_type, aes(x= RequestSource,y= count, fill= RequestType))+
  geom_bar(position = position_dodge(0.8),width = 0.65, stat = "identity")+
  geom_text(aes(label= count),size=2,vjust=-0.5, hjust= 0.5,position = position_dodge(0.8))

per= data %>%
  select(RequestSource) %>%
  group_by(RequestSource) %>%
  summarise(count= n()) %>%
  mutate(percent= round(100*count/sum(count), 2)) %>%
  arrange(desc(percent))

per_top5= head(per, 5)

ggplot(per_top5,aes(x= reorder(RequestSource, percent), y= percent))+
  geom_point(size = 2)+
  geom_text(aes(label= percent),size=3,vjust=-0.7, hjust= 0.5)+
  ylab("Percent (%)")+
  xlab("")+
  theme(axis.title.y = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5))+
  ggtitle("Top 5 Input Channels Proportion")
