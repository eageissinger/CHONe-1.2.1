setwd("C:/Users/user/Dropbox/Thesis/Research/data-working")

#load data
temp17<-read.csv("temperature_2017.csv")
temp<-read.csv("daily_temp.csv")


#load packages
library(dplyr)
library(ggplot2)
library(stringr)
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


#format date
#FUUUUCKKKKKKKK

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
  filter(date>="2016-10-01")%>%
  filter(date<="2017-05-31")%>%
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
