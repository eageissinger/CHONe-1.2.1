# set working directory 
setwd("C:/Users/eageissinger/Documents/Emilie-Lab-comp/")

# ---- load packages ----
library(dplyr)
library(lubridate)

# load data
pulserange<-read.csv("./data/data-working/age1-pulse-range.csv")
length<-read.csv("./data/data-working/newman-length.csv")

str(pulserange)
pulserange<-pulserange%>%
  mutate(month=replace(month,cohort==2003 & trip == 13,7))%>%
  mutate(month=replace(month,cohort==2003 & trip ==14,7))%>%
  mutate(day=replace(day,cohort==2003 & trip ==13,16))%>%
  mutate(day=replace(day,cohort==2003 & trip == 14,31))
# fix date
pulserange$date<-ymd(paste(pulserange$year,pulserange$month,pulserange$day,sep="-"))


mydata<-pulserange%>%
  mutate(pulse=dummy_pulse)%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 16 & pulse == 2, 4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 16 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 17 & pulse == 3, "4/5"))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 17 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 17 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & month==7 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & month ==7 & pulse == 1, 2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & month == 5 & pulse == 3, 5))%>%
  mutate(pulse=replace(pulse,cohort==2001 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & month == 5 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & month == 7 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & month == 7 & pulse == 1, 3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & month ==5 & pulse == 3, 4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & month == 5 & pulse == 2, 3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & month == 5 & pulse == 1, 2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 13 & pulse == 2, "2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 13 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 14 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip ==14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 14 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 16 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 16 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 16 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 17 & pulse == 2, "2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 17 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 18 & pulse == 2, "2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & pulse == 2, "2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 20 & pulse == 5,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 20 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 20 & pulse == 3, "2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 5 & pulse == 3,5))%>%
  mutate(pulse= replace(pulse,cohort==2004 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 5 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & pulse == 3, 5))%>% # need to double check 2004
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & pulse== 2,4))%>% # send bob and email with the two possibilities
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & pulse == 1,3))%>% # either super fast growth rate or average
  mutate(pulse=replace(pulse,cohort==2005 & month == 5,3))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month ==5 & pulse == 4,6))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 5 & pulse == 3, 5))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 5 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 7 & pulse == 3, 6))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 7 & pulse == 2,5))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 7 & pulse == 1,4))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip==12 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2008 & month ==5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2008 & month == 5 & pulse ==2, 4))%>%
  mutate(pulse=replace(pulse,cohort==2008 & month == 5 & pulse ==1,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 5 & pulse==3,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 5 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 5 & pulse == 1, 2))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 7 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 7 & pulse ==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 7 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010& month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2010& month == 5 & pulse == 1,2))%>% #this one is not fully correct
  mutate(pulse=replace(pulse,cohort==2010& trip ==12 & pulse ==2,4))%>%
  mutate(pulse=replace(pulse,cohort==2010& trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 13 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 15 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 15 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 15 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2011 & month ==5 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2011 & month == 5 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & month == 5 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & month == 5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2012 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2012 & month == 5 & pulse == 1,NA))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 5 & pulse == 4,6))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 5 & pulse == 1,"2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 13 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 13 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 13 & pulse == 1,"2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 14 & pulse == 3,6))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 14 & pulse == 2,5))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 14 & pulse == 1, "2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 16 & pulse == 4,6))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 16 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 16 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & month == 5 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & month == 5 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & month ==5 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip==12 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip ==12 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip ==13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 13 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 14 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 14 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 15 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 15 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 15 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & month == 5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2016 & month == 5 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & pulse ==4,5))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse ==3,5))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 16 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 16 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 16 & pulse == 4,5))

# 2016
# take out trips 17, 18, 19, 20
# 2015
# take out trips 16-21
# 2013
# take out trips 21-22
# 2012
# take out trips 17-22
# 2007
# take out trips 16 and 18
# 2000
# take out trips 19-22
# 1999
# take out trips 18-21

mydata2<-mydata%>%
  filter(cohort== 1999 & !trip %in% 18:21 | cohort == 2000 & !trip %in% 19:22 |
           cohort %in% 2001:2006 | cohort==2007 & !trip %in% 16:18 | cohort %in% 2008:2011 |
           cohort == 2012 & !trip %in% 17:22 | cohort==2013 & !trip %in% 21:22 |
           cohort ==2014 | cohort == 2015 & !trip %in% 16:21 | cohort == 2016 & !trip %in% 17:20)

mydata2

filter(mydata,cohort==2000 & month == 7)         
filter(mydata,cohort==2001 & month == 5)
filter(mydata,cohort==2001 & month ==7)
filter(mydata,cohort==2002 & month ==5)
filter(mydata,cohort==2004 & month ==5)
filter(mydata,cohort==2004 & month ==7)
filter(mydata,cohort==2005 & month ==5)
filter(mydata,cohort==2006 & month == 5)
filter(mydata,cohort==2006 & month == 7)
filter(mydata,cohort==2007 & month == 7)
filter(mydata,cohort==2008 & month ==5)
filter(mydata,cohort==2009 & month == 5)
filter(mydata,cohort==2009 & month == 7)
# need to fix 2010 (2 is actually likely 1,2,3)
filter(mydata,cohort==2010 & month == 5)
filter(mydata,cohort==2010 & month == 7)
filter(mydata,cohort==2011 & month == 5)
filter(mydata,cohort==2011 & month == 7)
filter(mydata,cohort==2012 & month ==5)
filter(mydata,cohort==2013 & month == 5)
filter(mydata,cohort==2013 & month == 7)
filter(mydata,cohort==2015 & month ==5)
filter(mydata,cohort==2015 & month ==7)
filter(mydata,cohort==2016 & month ==5)
filter(mydata,cohort==2016 & month == 7)

# as complete as it can be
write.csv(mydata2,"./data/data-working/pulse_range_age1_final.csv")

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
