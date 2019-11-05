# Fjord Ecosystems:
# Change in pulse structure of cod in Newman Sound

#---- set working directory ----
setwd("C:/Users/Emilie/Dropbox/FHL/course-project/")

# ----load packages  ----
library(ggplot2)
library(dplyr)

# ---- load data ----
length<-read.csv("./data/newman-length-to2017.csv")

# ---- Histograms -----

#2007 cohort

length%>%
  filter(age==0 & year == 2002& month == 10)%>%
  ggplot(aes(mmSL))+geom_histogram(binwidth=3,colour='black',fill='white')+
  xlim(20,126)+ylim(0,120)+
  ggtitle("October 2002")+
  theme(plot.title=element_text(size=20,face='bold',hjust=.5))+
  xlab("Standard Length (mm)")+
  ylab("Count")+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))
length%>%
  filter(age==0 & year ==2002 & month == 11)%>%
  ggplot(aes(mmSL))+geom_histogram(binwidth=3,colour='black',fill='white')+
  xlim(20,126)+ylim(0,120)+
  ggtitle("November 2002")+
  theme(plot.title = element_text(size=20,face='bold',hjust=.5))+
  xlab("Standard Length (mm)")+
  ylab("Count")+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))
length%>%
  filter(age == 1 & year == 2003 & month == 5)%>%
  ggplot(aes(mmSL))+geom_histogram(binwidth=3,colour='black',fill='white')+
  xlim(20,126)+ylim(0,120)+
  ggtitle("May 2003")+
  theme(plot.title = element_text(size=20,face='bold',hjust=.5))+
  xlab("Standard Length (mm)")+
  ylab("Count")+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))

# 2016 cohort
length%>%
  filter(age==0 & year == 2016 & month == 10)%>%
  ggplot(aes(mmSL))+geom_histogram(binwidth = 3, colour='black',fill='white')+
  xlim(30,140)+ylim(0,100)+
  ggtitle("October 2016")+
  theme(plot.title = element_text(size=20,face='bold',hjust = .5))+
  xlab("Standard Length (mm)")+
  ylab("Count")+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))
length%>%
  filter(age==0 & year ==2016 & month == 11)%>%
  ggplot(aes(mmSL))+geom_histogram(binwidth=3,colour='black',fill='white')+
  xlim(30,140)+ylim(0,100)+
  ggtitle("November 2016")+
  theme(plot.title = element_text(size=20,face='bold',hjust=0.5))+
  xlab("Standard Length (mm)")+
  ylab("Count")+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))
length%>%
  filter(age==1 & year == 2017 & month == 5)%>%
  ggplot(aes(mmSL))+geom_histogram(binwidth = 3,colour='black',fill='white')+
  xlim(30,140)+ylim(0,100)+
  ggtitle("May 2017")+
  theme(plot.title = element_text(size=20,face='bold',hjust=.5))+
  xlab("Standard Length (mm)")+
  ylab("Count")+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))

