library(tidyverse)
library(slider)
library(patchwork)
lds<-read_csv("ldschurch.csv")
lds=lds %>% mutate(Members_per_ward=Membership/`Wards and Branches`,
               Members_per_temple = Membership/`Temples in Operation`,
               Children_Growth_Ratio = `New Children of Record`/
                 (`New Children of Record`+`Converts Baptized`),
               Yearly_Growth=Membership-lag(Membership),
               Baptisms = `New Children of Record`+`Converts Baptized`) 

ggplot(lds, aes(x=Year))+
  geom_line(aes(y=Members_per_temple, color="Members_per_temple"))
  
ggplot(lds, aes(x=Year))+
  geom_line(aes(y=Members_per_ward))+
  ylab("Members per Church Unit")

p1=ggplot(lds, 
       aes(x=Year,y=Yearly_Growth/10000))+
  geom_col()+
  geom_line()+
  ylab("Yearly Membership Growth (10,000s)")+
  theme_minimal()+scale_x_continuous(name=NULL)

p2=ggplot(lds, aes(x=Year, y=Children_Growth_Ratio))+
  geom_line()+geom_point()+
  scale_y_continuous(labels=scales::label_percent(accuracy = 1))+
  ylab("Children as a percentage of all baptisms")+
  theme_minimal()

p1/p2

p3=ggplot(lds, aes(x=Year))+
  geom_line(aes(y=`New Children of Record`/10000, color="Children"))+
  geom_point(aes(y=`New Children of Record`/10000, color="Children"))+
  geom_point(aes(y=`Converts Baptized`/10000, color="Converts"))+
  geom_line(aes(y=`Converts Baptized`/10000, color="Converts"))+
  ylab("Baptisms (10,000s)")+
  theme_minimal()+
  scale_color_discrete(name=NULL)

p4=ggplot(lds%>%filter(Year!=1995), aes(x=Year))+
  geom_line(size=3,aes(y=Baptisms/10000, color="Total Baptisms in Year"))+
   geom_ribbon(aes(ymin=Baptisms/10000, ymax=Yearly_Growth/10000, 
                   fill="Deaths or Members Lost"))+
  #geom_point(aes(y=Yearly_Growth/10000))+
  geom_area(aes(y=Yearly_Growth/10000, fill="Membership Growth"))+
  theme_minimal()+
  scale_color_discrete(direction=7, name=NULL)+
  ylab("Membership (10,000s)")+
  scale_fill_discrete(name=NULL)+
  scale_x_continuous(name=NULL)


((p1+p4))/((p2+p3))
  
