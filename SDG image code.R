#SDG image

# SET UP
library(tidyverse)
setwd() #####

# Read file
sdgs<-read.csv("SDGs goals & targets.csv") #read file with goals and targets

# Prep targets
targets<-sdgs[,4:6] #extract targets
targets$x<-as.numeric(row.names(targets)) #set x-axis values

targets$goal<-substr(targets$Target.number,1,2) #identify goal for each target
  targets$goal<-gsub("[.]","",targets$goal)
  targets$Target.number<-gsub(" ","",targets$Target.number)
  
targets$angle<-(90-360*(targets$x-0.5)/nrow(targets)) #set angle/position of target labels
  targets$hjust<-ifelse(targets$angle<(-90),1,0)
  targets$angle<-ifelse(targets$angle<(-90),targets$angle+180,targets$angle)

# Prep goals
goals<-sdgs[1:17,1:3] #extract goals

goals2 <- targets %>% 
  group_by(goal) %>% 
  summarize(start=min(x), end=max(x)) %>% #set start and end position for each goal
  rowwise() %>% 
  mutate(placement=mean(c(start, end))) #set placement of goal labels

goals2<-merge(goals,goals2,by.x="Goal.number",by.y="goal") #add goal names back in
goals2$angle<-(90-360*(goals2$placement-0.5)/nrow(targets)) #set angle/position of goal labels
  goals2$hjust<-ifelse(goals2$angle<(-90),0,1)
  goals2$angle<-ifelse(goals2$angle<(-90),goals2$angle+180,goals2$angle)
  
# Set colours for targets
for(r in 1:nrow(targets)){
  if(as.character(substr(targets$Target.number[r],1,2))=="1."){
    targets$Colour[r]<-"red2"
    targets$Line[r]<-"red4"}
  if(as.character(substr(targets$Target.number[r],1,2))=="2."){
    targets$Colour[r]<-"gold3"
    targets$Line[r]<-"gold4"}
  if(as.character(substr(targets$Target.number[r],1,2))=="3."){
    targets$Colour[r]<-"springgreen4"
    targets$Line[r]<-"darkgreen"}
  if(as.character(substr(targets$Target.number[r],1,2))=="4."){
    targets$Colour[r]<-"red3"
    targets$Line[r]<-"firebrick4"}
  if(as.character(substr(targets$Target.number[r],1,2))=="5."){
    targets$Colour[r]<-"orangered"
    targets$Line[r]<-"orangered3"}
  if(as.character(substr(targets$Target.number[r],1,2))=="6."){
    targets$Colour[r]<-"deepskyblue"
    targets$Line[r]<-"deepskyblue3"}
  if(as.character(substr(targets$Target.number[r],1,2))=="7."){
    targets$Colour[r]<-"gold2"
    targets$Line[r]<-"gold3"}
  if(as.character(substr(targets$Target.number[r],1,2))=="8."){
    targets$Colour[r]<-"darkred"
    targets$Line[r]<-"brown"}
  if(as.character(substr(targets$Target.number[r],1,2))=="9."){
    targets$Colour[r]<-"darkorange"
    targets$Line[r]<-"darkorange3"}
  if(as.character(substr(targets$Target.number[r],1,2))=="10"){
    targets$Colour[r]<-"deeppink"
    targets$Line[r]<-"deeppink3"}
  if(as.character(substr(targets$Target.number[r],1,2))=="11"){
    targets$Colour[r]<-"orange"
    targets$Line[r]<-"orange3"}
  if(as.character(substr(targets$Target.number[r],1,2))=="12"){
    targets$Colour[r]<-"gold3"
    targets$Line[r]<-"gold4"}
  if(as.character(substr(targets$Target.number[r],1,2))=="13"){
    targets$Colour[r]<-"darkgreen"
    targets$Line[r]<-"grey25"}
  if(as.character(substr(targets$Target.number[r],1,2))=="14"){
    targets$Colour[r]<-"dodgerblue"
    targets$Line[r]<-"dodgerblue4"}
  if(as.character(substr(targets$Target.number[r],1,2))=="15"){
    targets$Colour[r]<-"springgreen3"
    targets$Line[r]<-"springgreen4"}
  if(as.character(substr(targets$Target.number[r],1,2))=="16"){
    targets$Colour[r]<-"dodgerblue4"
    targets$Line[r]<-"navyblue"}
  if(as.character(substr(targets$Target.number[r],1,2))=="17"){
    targets$Colour[r]<-"darkslateblue"
    targets$Line[r]<-"blue4"}
  }

# Creat image
ggplot(targets,aes(x=x,y=1.2))+
  geom_bar(stat="identity",fill=targets$Colour,width=1,colour=targets$Line,alpha=1)+
  ylim(-0.5,1.5)+
  coord_polar(start=0)+ #make circular
  theme_minimal()+
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank())+
  geom_text(data=targets,aes(x=x,y=1.13,label=Target.number,hjust=hjust,vjust=0),
            angle=targets$angle,size=2.5,colour="white",fontface="bold")+ #target number labels
  geom_text(data=targets,aes(x=x,y=1.25,label=Target.nickname,hjust=hjust,vjust=0),
            angle=targets$angle,size=3,colour="black")+ #target name labels
  geom_segment(data=goals2,aes(x=start,xend=end,y=-0.05,yend=-0.05),size=1.5)+ #create display lines along inside of each goal
  geom_text(data=goals2,aes(x=placement,y=-0.11,label=Goal.number),size=5,fontface="bold")+ #goal number labels
  geom_text(data=goals2,aes(x=placement,y=1.08,label=Goal.nickname,hjust=hjust),
            angle=goals2$angle,size=6.5,fontface="bold",colour="white")+ #goal name labels
  geom_text(x=0,y=-0.5,label="SDGs",size=7,fontface="bold")+ #central SDG label
  ggsave("SDGs figure2.jpeg",width=18,height=18) #save image

