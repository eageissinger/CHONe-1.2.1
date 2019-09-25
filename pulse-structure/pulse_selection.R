# ------ Pulse Selection ------

#load working directory
setwd("C:/Users/user/Dropbox/Thesis/Research/long_condition")

#load pacakges
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# load data
catch<-read.delim("catch_rate.txt",header=TRUE,sep="\t")


# check data
summary(catch)
str(catch)
head(catch)
tail(catch)
names(catch)
names(catch)<-c("notes", "year", "julian_date","hauls","pulse","abundance")

#Anova by year
#don't worry about 0's right now.  just see if initial works with the current setup

catch$pulse<-as.factor(catch$pulse)

# ----- 1996 ------

yr96<-catch%>%
  filter(year==1996)%>%
  data.frame()

plot96<-ggplot(yr96,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle("1996")
plot96

# ------ 1998------

yr98<-catch%>%
  filter(year==1998)%>%
  data.frame()

#plot 1998
plot98<-ggplot(yr98,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(1998)
plot98

# ------- 1999 -----

yr99<-catch%>%
  filter(year==1999)%>%
  data.frame()

#plot 1999
plot99<-ggplot(yr99,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(1999)
plot99

# -------2000-----
yr00<-catch%>%
  filter(year==2000)%>%
  data.frame()

#plot 2000
plot00<-ggplot(yr00,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2000)
plot00

# -------2001 ------
yr01<-catch%>%
  filter(year==2001)%>%
  data.frame()

#plot 2001
plot01<-ggplot(yr01,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2001)
plot01


# --------- 2002 ---------

yr02<-catch%>%
  filter(year==2002)%>%
  data.frame()

# plot 2002
plot02<-ggplot(yr02,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2002)
plot02

# ------ 2003 ------

yr03<-catch%>%
  filter(year==2003)%>%
  data.frame()

#plot 2003

plot03<-ggplot(yr03,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2003)
plot03

# ------ 2004 ------

yr04<-catch%>%
  filter(year==2004)%>%
  data.frame()

# plot 2004

plot04<-ggplot(yr04,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2004)
plot04


# ----- 2005 ------

yr05<-catch%>%
  filter(year==2005)%>%
  data.frame()

# plot 2005
plot05<-ggplot(yr05,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2005)
plot05

# ----- 2006 ------
yr06<-catch%>%
  filter(year==2006)%>%
  data.frame()

# plot 2006
plot06<-ggplot(yr06,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2006)
plot06

page1<-grid.arrange(plot96,plot98,plot99,plot00,plot01)
page2<-grid.arrange(plot02,plot03,plot04,plot05,plot06)

# ---- 2007 -----
yr07<-catch%>%
  filter(year==2007)%>%
  data.frame()

# plot 2007
plot07<-ggplot(yr07,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2007)
plot07

#----- 2008 ------
yr08<- catch%>%
  filter(year==2008)%>%
  data.frame()
# plot 2008
plot08<-ggplot(yr08,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2008)
plot08

# ----- 2009 -----
yr09<-catch%>%
  filter(year==2009)%>%
  data.frame()

# plot 2009
plot09<-ggplot(yr09,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2009)
plot09

# ---- 2010 -----
yr10<-catch%>%
  filter(year==2010)%>%
  data.frame()

# plot 2010
plot10<-ggplot(yr10,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2010)
plot10

# ---- 2011 ----
yr11<-catch%>%
  filter(year==2011)%>%
  data.frame()

#plot 2011
plot11<-ggplot(yr11,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2011)
plot11

# ---- 2012 -----
yr12<-catch%>%
  filter(year==2012)%>%
  data.frame()

#plot 2012
plot12<-ggplot(yr12,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2012)
plot12

# ----- 2013 -----
yr13<-catch%>%
  filter(year==2013)%>%
  data.frame()

# plot 2013
plot13<-ggplot(yr13,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2013)
plot13

# ----- 2014 ------
yr14<-catch%>%
  filter(year==2014)%>%
  data.frame()

# plot 2014
plot14<-ggplot(yr14,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2014)
plot14

# ----- 2015 -----
yr15<-catch%>%
  filter(year==2015)%>%
  data.frame()

# plot 2015
plot15<-ggplot(yr15,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2015)
plot15

# ----- 2016 ------
yr16<-catch%>%
  filter(year==2016)%>%
  data.frame()

# plot 2016
plot16<-ggplot(yr16,aes(x=pulse,y=abundance))+
  geom_bar(stat="identity")+ggtitle(2016)

page1<-grid.arrange(plot96,plot98,plot99,plot00,plot01)
page2<-grid.arrange(plot02,plot03,plot04,plot05,plot06)
page3<-grid.arrange(plot07,plot08,plot09,plot10,plot11)
page4<-grid.arrange(plot12,plot13,plot14,plot15,plot16)
