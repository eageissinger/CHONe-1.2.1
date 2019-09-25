setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

#load data
temp17<-read.csv("./data/data-working/temperature-2017.csv")
temp<-read.csv("./data/data-working/daily-temp-corrected-newman.csv")
exp<-read.csv("./data/data-working/temperature-exp.csv")


#load packages
library(tidyverse)
library(lubridate)
library(scales)

#check data
str(temp17)
summary(temp17)
names(temp17)
names(temp17)<-c("date","julian_date","meantemp","n","deployment")

str(temp)
summary(temp)
names(temp)
names(temp)<-c("date","year","month","day","meantemp")

str(exp)
names(exp)
names(exp)<-c('year','month','day','time','tank','temp','notes')

year<-as.numeric(substr(temp17$date,start=1,stop=4))

mydata1<-cbind(temp17,year)

month<-as.numeric(substr(temp17$date,start=5,stop=6))

mydata2<-cbind(mydata1,month)

day<-as.numeric(substr(temp17$date,start=7,stop=8))     

mydata3<-cbind(mydata2,day)

temp2<-mydata3%>%
  select(julian_date,meantemp,n,deployment,year,month,day)%>%
  data.frame()
temp2$date<-with(mydata3,ymd(sprintf('%04d%02d%02d',year,month,day)))

temp$date<-ymd(paste(temp$year,temp$month,temp$day,sep="-"))

exp$date<-ymd(paste(exp$year,exp$month,exp$day,sep="-"))

#select columms
temp17<-temp2%>%
  select(date,meantemp)%>%
  data.frame()

temp<-temp%>%
  select(date,meantemp)%>%
  data.frame()

temp_all<-bind_rows(temp,temp17)

#select days for temp plot

tempwinter<-temp_all%>%
  filter(date>="2016-9-01")%>%
  filter(date<="2017-07-31")%>%
  data.frame()

ylabel<-"Temperature (°C)"

ggplot(tempwinter,aes(x=date,y=meantemp))+geom_point()+
  scale_x_date(breaks=date_breaks("months"),labels=date_format("%b-%Y"))+
  theme_classic()+
  xlab("Date")+ylab(ylabel)+
  ggtitle("Newman Sound Temperature")+
  geom_hline(yintercept=0,linetype='dashed',colour='red',size=1.25)+
  theme(axis.title=element_text(size=16))+
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(plot.title = element_text(size=18,face='bold',hjust=.5))

tempwinter%>%
  filter(date>"2016-12-20")%>%
  filter(date<"2017-04-24")%>%
  ggplot(aes(x=date,y=meantemp))+geom_point()+
  scale_x_date(breaks=date_breaks("months"),labels=date_format("%b-%Y"))+
  theme_classic()+
  xlab("Date")+ylab("Temperature (°C)\n")+
  geom_hline(yintercept=0,linetype='dashed',colour='red',size=1.25)+
  theme(axis.title=element_text(size=20))+
  theme(axis.text=element_text(size=16))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(plot.title = element_text(size=18,face='bold',hjust=.5))+
  ylim(-1.5,2)+
  theme(axis.title.x=element_text(margin=margin(t=15)))



# ---- experiment temperature ----
exp2<-exp%>%
  group_by(date)%>%
  summarise(meantemp=mean(temp))
head(exp2)

exp2%>%
  ggplot(aes(x=date,y=meantemp))+geom_point()+
  scale_x_date(breaks=date_breaks("months"),labels=date_format("%b-%Y"))+
  theme_classic()+
  xlab("Date")+ylab("Temperature (°C)\n")+
  geom_hline(yintercept=0,linetype='dashed',colour='red',size=1.25)+
  theme(axis.title=element_text(size=20))+
  theme(axis.text=element_text(size=16))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(plot.title = element_text(size=18,face='bold',hjust=.5))+
  ylim(-1.5,2)+
  theme(axis.title.x=element_text(margin=margin(t=15)))
summary(exp2)
