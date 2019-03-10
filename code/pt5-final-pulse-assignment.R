# set working directory 
setwd("C:/Users/eageissinger/Documents/Emilie-Lab-comp/")

# ---- load packages ----
library(dplyr)
library(lubridate)

# load data
pulserange<-read.csv("./data/data-working/age1-pulse-range.csv")

str(pulserange)
# fix date
pulserange$date<-ymd(paste(pulserange$year,pulserange$month,pulserange$day,sep="-"))

# 2000
mydata<-pulserange%>%
  mutate(pulse=dummy_pulse)%>%
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
  mutate(pulse=replace(pulse,cohort==2004 & month == 5 & pulse == 3,5))%>%
  mutate(pulse= replace(pulse,cohort==2004 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 5 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 7 & pulse == 3, 5))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 7 & pulse== 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 7 & pulse == 1,3))%>%
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
  mutate(pulse=replace(pulse,cohort==2011 & month ==5 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2011 & month == 5 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & month == 5 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & month == 5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2012 & month == 5 & pulse == 2, 4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 5 & pulse == 1,"2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 7 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 7 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 7 & pulse == 1,"2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2015 & month == 5 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & month == 5 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & month ==5 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip==12 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip ==12 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip ==13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & month == 5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2016 & month == 5 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & pulse ==4,5))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016& trip == 14 & pulse ==3,5))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse == 2,4))

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

# only updated for May and July
write.csv(mydata,"./data/data-working/pulse_range_mayjuly.csv")

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

test1<-mydata%>%
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
