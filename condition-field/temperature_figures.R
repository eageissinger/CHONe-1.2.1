# set working directory
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/condition-field/")

#load packages
library(tidyverse)

# read in winter data
winter<-read.csv("../data/output/newman-winter-summary.csv")

# ---- plots for manuscript -----
head(winter)
Fig4<-winter%>%
  filter(cohort>1998)%>%
  ggplot()+geom_line(aes(x=cohort,y=mean_temp),size=1)+
  geom_ribbon(aes(ymin=(mean_temp-sd_temp),ymax=(mean_temp+sd_temp),x=cohort),alpha=0.3)+
  geom_text(aes(x=cohort, y=mean_temp,label=as.character(days_below_1)),vjust=2.25,size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  scale_x_continuous(breaks=seq(1999,2016,by=1))+
  theme(axis.text.x = element_text(angle=40,hjust=1,size=10))+
  ylab("Winter Temperature (°C)")+xlab("Cohort")

ggsave(file="./figures/Fig4.png",plot=Fig4,width=168,height=84,units="mm")



summary(winter)
winter2<-winter%>%filter(cohort>1999)
summary(winter2)
winter%>%filter(mean_temp>0.4)
winter%>%filter(mean_temp<(-0.4))
winter%>%filter(days_below_1==64)
winter%>%filter(days_below_1==131)
