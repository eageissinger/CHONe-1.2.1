# set working directory 
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

# ---- load packages ----
library(tidyverse)
library(lubridate)

# load data
pulserange<-read.csv("./data/data-working/age0-pulse-range.csv")
length<-read.csv("./data/data-working/newman-length.csv")

str(pulserange)

mydata<-pulserange%>%
  rename(pulse=dummy_pulse)%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 18 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 18 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 19 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 19 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 21 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 22 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 22 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 21 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 22 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 22 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 18 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 18 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 20 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 22 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 21 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 20 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 21 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 21 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 17 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 18 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 18 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 18 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 19 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 19 & pulse == 2,1))



# 2004
# take out trip 19, pulse 2; trip 20
# 2008
# take out trip 19
# 2010
# take out trip 18
# 2018
# take out trip 19

mydata2<-mydata%>%
  filter(cohort== 1999 & !trip %in% 18:21 | cohort == 2000 & !trip %in% 19:22 |
           cohort %in% 2001:2006 | cohort==2007 & !trip %in% 16:18 | cohort %in% 2008:2011 |
           cohort == 2012 & !trip %in% 17:22 | cohort==2013 & !trip %in% 21:22 |
           cohort ==2014 | cohort == 2015 & !trip %in% 16:21 | cohort == 2016 & !trip %in% 17:20)
View(mydata2)
mydata2


# as complete as it can be
write.csv(mydata,"./data/data-working/pulse_range_age0_round1.csv",row.names = FALSE)

# Format data for plotting
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
length0<-length%>%
  filter(age==0)%>%
  mutate(cohort=year)%>%
  select(-trip)%>%
  group_by(cohort,age,pulse,date)%>%
  summarise(mean=mean(mmSL),min=min(mmSL),max=max(mmSL))%>%
  filter(!is.na(pulse))
names(length0)
str(length0)
length0$pulse<-as.character(length0$pulse)

test1<-mydata2%>%
  mutate(mean=(min+max)/2)%>%
  mutate(age=1)

test<-bind_rows(length0,test1)
glimpse(test)

# plotting

test%>%
  filter(cohort==1998)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("1998 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==1999)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("1999 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2000)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2000 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2001)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2001 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2002)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2002 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2003)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2003 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2004)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2004 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2005)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2005 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2006)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2006 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2007)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2007 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2008)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2008 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2009)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2009 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2010)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2010 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2011)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2011 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2012)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2012 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2013)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2013 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2014)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2014 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2015)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2015 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2016)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2016 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

test%>%
  filter(cohort==2017)%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2017 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))
