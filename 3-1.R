
library(dplyr)
library(ggplot2)


data= read.csv("MyLA311_Service_Request_Data_2016.csv", header = T)

per= data %>%
  select(RequestSource) %>%
  group_by(RequestSource) %>%
  summarise(count= n()) %>%
  mutate(percent= round(100*count/sum(count), 2)) %>%
  arrange(desc(percent))

per_top5= head(per, 5)

ggplot(per_top5,aes(x= reorder(RequestSource, desc(percent)), y= percent))+
  geom_bar(stat= "identity",fill= "darkred", width = 0.7)+
  geom_text(aes(label= percent),size=3,vjust=-0.7, hjust= 0.5)+
  ylab("Percent (%)")+
  xlab("")+
  theme(axis.title.y = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5))+
  ggtitle("Top 5 Request Channels by Volume")

referral_type = data %>%
  select(RequestType, RequestSource) %>%
  filter(RequestSource %in% c("Call", "Mobile App"))%>%
  group_by(RequestSource, RequestType) %>%
  summarise(count= n()/100) %>%
  group_by(RequestSource) %>%
  top_n(n=5, wt=count)

ggplot(referral_type, aes(x= RequestSource,y= count, fill= RequestType))+
  geom_bar(position = position_dodge(0.8),width = 0.7, stat = "identity")+
  geom_text(aes(label= count),size=3,vjust=-0.5, hjust= 0.5,position = position_dodge(0.8))+
  xlab("")+
  ylab("Request  Volume  ('00s)")+
  ggtitle("Request Volume by Service Type")+
  theme(axis.title.y = element_text(face = "bold"))+
  scale_fill_discrete(guide = guide_legend(title = "Request Type")) 

  
top5_type = data %>%
  select(RequestType, RequestSource) %>%
  filter(RequestSource %in% c("Call", "Driver Self Report", "Mobile App", "Self Service", "Email"))%>%
  group_by(RequestSource, RequestType) %>%
  summarise(count= n())

top5_type$RequestSource = factor(top5_type$RequestSource, levels =c("Call", "Driver Self Report", "Mobile App", "Self Service", "Email"))

ggplot(top5_type,aes(x= reorder(RequestSource, count), y=count))+
  geom_bar(aes(fill= RequestType), stat = "identity")+
  coord_flip()+
  xlab("Channels")+
  ggtitle("Service Types Distribution of Top 5 Input Channels")

top5_service = data %>%
  select(RequestType, RequestSource) %>%
  filter(RequestType %in% c("Bulky Items", "Graffiti Removal", "Metal/Household Appliances", "Illegal Dumping Pickup", "Electronic Waste"))%>%
  group_by(RequestType, RequestSource) %>%
  summarise(count= n()/100)%>%
  group_by(RequestType)%>%
  top_n(n=3, wt=count)

top5_service$RequestType = factor(top5_service$RequestType, levels = c("Electronic Waste", "Illegal Dumping Pickup", "Metal/Household Appliances","Graffiti Removal", "Bulky Items"))
ggplot(top5_service,aes(x= RequestType, y=count))+
  geom_bar(aes(fill= RequestSource), stat = "identity")+
  coord_flip()+
  xlab("")+
  ylab("Volume of Requests  ('00s)")+
  ggtitle("Top Service Types by Request Source")+
  theme(axis.title.x = element_text(face = "bold"))+
  scale_fill_discrete(guide = guide_legend(title = "Request Source")) 


