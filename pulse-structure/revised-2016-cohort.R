# mixture distribution and pulse identification for presentation
# 2016 cohort

# ----- set working directory ----
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/")

#load packages

library(lubridate)
library(mixdist)
library(ggpubr)
library(tidyverse)

#load data
length<-read.csv("./data/data-working/newman-length.csv")%>%mutate(Species=replace(Species,Species=="AC ","AC"))%>%
  filter(Species=="AC")
trips<-read.csv("./data/data-working/newman-trips.csv")

# ---- format dates ----
length$date<-ymd(paste(length$Year,length$Month,length$Day,sep="-"))

# ---- multimodal distribution -----
# Pulse structure of age 0 cod for all trips
# from 1996 to 2017

# ---- age 0 2016 ----
# May
age0.2016<-filter(length, Year==2016 & Age == 0 & Month == 5)

#July
age0.2016<-filter(length,Year==2016 & Age ==0 & Month ==7)

# August trip 15
age0.2016<-filter(length,Year==2016 & Age ==0 & Trip==15)

# check histogram
qplot(mmSL, data=age0.2016, binwidth=5)


# # detremine max and min SL
summarise(age0.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age0.2016,mmSL)

# convert to frequency table
age0group<-mixgroup(group2016, breaks = c(0,seq(40,55,5),58),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store results
head(age0.2016)
trip15.2016<-bind_cols(par)%>%
  mutate(year=2016,month=8,day=16,trip=15)
trip15.2016<-mutate(trip15.2016,dummy_pulse=rev(seq(1:nrow(trip15.2016))))

# add to mixtures
mixtures<-trip15.2016

# August trip 16
age0.2016<-filter(length, Year == 2016 & Age == 0 & Trip==16)

# check histogram
qplot(mmSL, data=age0.2016, binwidth=5)

# # detremine max and min SL
summarise(age0.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age0.2016,mmSL)

# convert to frequency table
age0group<-mixgroup(group2016, breaks = c(0,seq(45,65,5),70),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store fit 1 results
head(age0.2016)
trip16.2016<-bind_cols(par)%>%
  mutate(year=2016,month=8,day=31,trip=16)
trip16.2016<-mutate(trip16.2016,dummy_pulse=rev(seq(1:nrow(trip16.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2016)

# September trip 17
# trip2
age0.2016<-filter(length, Year == 2016 & Age == 0 & Trip==17)

# check histogram
qplot(mmSL, data=age0.2016, binwidth=5)

# # detremine max and min SL
summarise(age0.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age0.2016,mmSL)

# convert to frequency table
age0group<-mixgroup(group2016, breaks = c(0,seq(45,75,5),80),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,63),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=3, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

par<-groupstats(age0group)


# store model results
head(age0.2016)
trip17.2016<-bind_cols(par)%>%
  mutate(year=2016,month=9,day=13, trip=17)
trip17.2016<-mutate(trip17.2016,dummy_pulse=rev(seq(1:nrow(trip17.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2016)

# September trip 18
age0.2016<-filter(length,Year==2016 & Age ==0 & Trip==18)

# check histogram
qplot(mmSL, data=age0.2016, binwidth=5)

# # detremine max and min SL
summarise(age0.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age0.2016,mmSL)

# convert to frequency table
age0group<-mixgroup(group2016, breaks = c(0,seq(50,85,5),92),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(52,70),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2016)
trip18.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=9,day=28, trip=18)
trip18.2016<-mutate(trip18.2016,dummy_pulse=rev(seq(1:nrow(trip18.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2016)

# October trip 19
age0.2016<-filter(length,Year==2016 & Age ==0 & Trip==19)

# check histogram
qplot(mmSL, data=age0.2016, binwidth=5)

# # detremine max and min SL
summarise(age0.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age0.2016,mmSL)

# convert to frequency table
age0group<-mixgroup(group2016, breaks = c(0,seq(45,95,5),99),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(55,80),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1,main="Newman Sound: 14 October 2016")
plot(fit1,root=T)

# fit 1
head(age0.2016)
trip19.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=10,day=13,trip=19)
trip19.2016<-mutate(trip19.2016,dummy_pulse=rev(seq(1:nrow(trip19.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2016)

# October trip 20
age0.2016<-filter(length,Year==2016 & Age ==0 & Trip==20)

# check histogram
qplot(mmSL, data=age0.2016, binwidth=5)

# # detremine max and min SL
summarise(age0.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age0.2016,mmSL)

# convert to frequency table
age0group<-mixgroup(group2016, breaks = c(0,seq(35,105,5),110),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,65,85),c(6),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age0.2016)
trip20.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=10,day=27, trip=20)
trip20.2016<-mutate(trip20.2016,dummy_pulse=rev(seq(1:nrow(trip20.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2016)

# November trip 21
age0.2016<-filter(length,Year==2016 & Age == 0 & Trip==21)

# check histogram
qplot(mmSL, data=age0.2016, binwidth=5)

# # detremine max and min SL
summarise(age0.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age0.2016,mmSL)

# convert to frequency table
age0group<-mixgroup(group2016, breaks = c(0,seq(40,115,5),118),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,60,88),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1,main="Newman Sound: 14 November 2016")
plot(fit1,root=T)

# store model results
head(age0.2016)
trip21.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=11,day=14, trip =21)
trip21.2016<-mutate(trip21.2016,dummy_pulse=rev(seq(1:nrow(trip21.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2016)

# November trip 22 
age0.2016<-filter(length,Year==2016 & Age ==0 & Trip == 22)

# check histogram
qplot(mmSL, data=age0.2016, binwidth=5)

# # detremine max and min SL
summarise(age0.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age0.2016,mmSL)

# convert to frequency table
age0group<-mixgroup(group2016, breaks = c(0,seq(50,125,5),128),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(62,100),c(7),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# add to results
head(age0.2016)
trip22.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=11,day=29, trip=22)
trip22.2016<-mutate(trip22.2016,dummy_pulse=rev(seq(1:nrow(trip22.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip22.2016)



# ---- pulse determination ----
# ---- load functions ----
source("./pulse-structure/pulse_range_fct.R")

# ---- load data -----

age0pulse<-read.csv("./data/output/age-0-mixture-dist.csv")
count<-read.csv("./data/data-working/catch-pulse-newman.csv")
#fullcount<-read.csv("./data/data-working/newman-catch.csv")
catch<-read.csv("./data/data-working/catch_haul.csv")

# ---- Date and data organization ----

#format date
age0pulse$date<-ymd(paste(age0pulse$year,age0pulse$month,age0pulse$day,sep="-"))
#fullcount$date<-ymd(paste(fullcount$year,fullcount$month,fullcount$day,sep="-"))

# create pulse range for length
original<-length%>%
  filter(Age==0)%>%
  group_by(Year,Trip,Pulse,date)%>%
  summarise(mean_size=mean(mmSL),min_size=min(mmSL),max_size=max(mmSL))%>%
  mutate(cohort=Year)%>%
  data.frame()
#create min and max for age 1
# 2 standard deviations
new<-age0pulse%>%
  select(date, cohort, month, age, dummy_pulse,mu,mu.se,sigma,sigma.se)%>%
  filter(mu<300)%>% # get rid of outliers (>300 mm SL)
  rename(mean_size=mu,stdev=sigma)%>%
  mutate(min_size=mean_size-(stdev),max_size=mean_size+(stdev))%>%
  select(-stdev,-sigma.se,-mu.se)%>%
  rename(pulse=dummy_pulse)%>%
  data.frame()

names(original)
names(new)

original$Pulse<-as.factor(original$Pulse)
original$cohort<-as.factor(original$cohort)

new$pulse<-as.factor(new$pulse)
new$cohort<-as.factor(new$cohort)


# ---- Visualize data ----

original%>%
  filter(cohort==2016)%>%
  ggplot(aes(x=date,y=mean_size,shape=factor(Pulse)))+geom_point()+
  geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)

new%>%
  filter(cohort==2016)%>%
  ggplot(aes(x=date,y=mean_size,shape=factor(pulse)))+geom_point(size=3)+
  geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)+
  theme_bw()+
  ylab("Standard Length (mm)")+
  xlab("Date")+
  scale_x_date(date_labels="%d-%b",
               date_breaks="2 weeks")+
  theme(axis.text.x = element_text(angle=40,hjust=1))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  scale_shape_discrete(name="Pulse")+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=12))

# mixture models with raw data
age0<-length%>%
  filter(Age==0 & Species == 'AC' & Year==2016)%>%
  mutate(date2=date+3)%>%
  mutate(cohort=Year)

ggplot()+
  geom_point(data=filter(new,cohort==2016), aes(x=date,y=mean_size,shape=pulse))+
  geom_point(size=2)+
  geom_errorbar(data=filter(new,cohort==2016),aes(x=date,ymin=min_size,ymax=max_size),width=0)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels = "%b")+
  theme(axis.text.x = element_text(angle=40))

ggplot()+
  geom_point(data=filter(new,cohort==2016), aes(x=date,y=mean_size,shape=pulse))+
  geom_point(size=2)+
  geom_errorbar(data=filter(new,cohort==2016),aes(x=date,ymin=min_size,ymax=max_size),width=0)+
  geom_jitter(data=age0, aes(x=date2,y=mmSL),size=1,alpha=0.25,colour='lightskyblue3')+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="2 weeks",
               date_labels = "%d-%b")+
  theme(axis.text.x = element_text(angle=40,hjust=1))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  scale_shape_discrete(name="Pulse")+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=12))

# Assigning pulses from mixture distribution models
# ---- Dummy pulses ----
# Purpose: assign dummy pulses to length data for all age 0 cod


# make dataframe with min and max pulse ranges
# use +/- standard deviation for "min" and "max"
# and incorporate the 'in between' ranges by splitint the 
# difference between min of pulse 1 and max of pulse 2, etc.
pulserange<-pulse_range(age0pulse)%>%
  mutate(cohort=year)
# next step:
# create a data frame that fills in all possible length 
# possibilities for every pulse option

pulse_assign<-data.frame(trip=rep(pulserange$trip,pulserange$max-pulserange$min+1),
                         year=rep(pulserange$year,pulserange$max-pulserange$min+1),
                         cohort=rep(pulserange$cohort,pulserange$max-pulserange$min+1),
                         pulse=rep(pulserange$dummy_pulse,pulserange$max-pulserange$min+1),
                         mmSL=unlist(mapply(seq,pulserange$min,pulserange$max)))
# add additinal info such as date, cohort, pulse so that it is present in dataframe
#View(pulse_assign)
#is.na(pulse_assign)
summary(pulse_assign)
glimpse(pulse_assign)

# add year back to pulse_assign (easier to do this now than to add it to above code)
pulse_assign<-pulse_assign%>%
  mutate(year=cohort)%>%
  data.frame()

# assign pulses to age 0 length data

#View(lengthtrip)
age0length<-length%>%
  filter(Age==0)%>%
  select(-Pulse)%>%
  rename(trip=Trip,year=Year,age=Age)
#View(age0length)

# assign pulses to subsetted age0 length data

age0length_pulse<-left_join(age0length,pulse_assign)%>%
  filter(cohort==2016)
#View(age0length_pulse)

# check assignments
# create summary table of min and max for each assigned pulse by year
table1<-age0length_pulse%>%
  group_by(year,cohort,trip,date,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))
#View(table1)

# ---- Pulse Assignment Final Sorting  -----
# format trip dates
trips16<-trips%>%
  filter(year==2016)%>%
  group_by(trip)%>%
  arrange(month,day)%>%
  filter(row_number()==1)

range2016<-age0length_pulse%>%
  group_by(year,cohort, trip, pulse)%>%
  rename(month=Month,day=Day)%>%
  summarise(mean=mean(mmSL),minSL=min(mmSL),maxSL=max(mmSL))%>%
  ungroup()%>%
  left_join(trips16)%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))


age0length_pulse


age0length_pulse%>%
  mutate(date2=date+3)%>%
  ggplot()+
  geom_jitter(aes(x=date2,y=mmSL,colour=factor(pulse)),alpha=0.25,size=1.5)+
  geom_point(data=range2016,aes(x=date,y=mean,shape=factor(pulse)),size=2)+
  geom_errorbar(data=range2016,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="2 weeks",
               date_labels="%d-%b")+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  scale_shape_discrete(name="Pulse")+
  scale_colour_discrete(name="Pulse")+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=12))

# second assignment (or initial in this case)
final<-age0length_pulse%>%
  mutate(pulse=replace(pulse,year==2016 & trip<18,1))%>%
  mutate(pulse=replace(pulse,year==2016 & trip==17 & mmSL<41,2))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 19 & mmSL<45,3))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 20 & mmSL<40,4))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 21 & mmSL<43,4))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 21 & pulse == 2 & mmSL<54,3))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 22 & pulse ==2 & mmSL<55,3))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 22 & mmSL<50,4))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 22 & is.na(pulse),1))

pulse.range2016<-final%>%
  group_by(age,year,trip,pulse)%>%
  summarise(mean=mean(mmSL),minSL=min(mmSL),maxSL=max(mmSL))%>%
  ungroup()%>%
  left_join(trips16)%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))
  
final%>%
  mutate(date2=date+3)%>%
  ggplot()+
  geom_jitter(aes(x=date2,y=mmSL,colour=as.character(pulse)),alpha=0.25,size=1)+
  geom_point(data=pulse.range2016,aes(x=date,y=mean,shape=factor(pulse)),size=2)+
  geom_errorbar(data=pulse.range2016,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="2 weeks",
               date_labels="%d-%b")+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  scale_shape_discrete(name="Pulse")+
  scale_colour_discrete(name="Pulse")+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=12))
# ---- Age 1 pulse assignments -----

# age 1 mixture distributions
age1mix<-read.csv("./data/output/age-1-mixture-dist.csv")%>%
  filter(cohort==2016)
age1length<-length%>%
  filter(Species=="AC" & Age == 1 & Year ==2017)%>%
  select(-Pulse)%>%
  mutate(cohort=Year-1)%>%
  rename(year=Year,trip=Trip,age=Age)

head(pulse.range2016)
head(age1mix)

mix2<-age1mix%>%
  rename(pulse=dummy_pulse,mean=mu)%>%
  mutate(minSL=mean-sigma,maxSL=mean+sigma)%>%
  select(age,year,trip,pulse,mean,minSL,maxSL,month, day)%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  bind_rows(pulse.range2016)

length2<-bind_rows(final,age1length)

length2%>%
  mutate(date2=date+3)%>%
  ggplot()+
  geom_jitter(aes(x=date2,y=mmSL,colour=as.character(pulse),shape=as.character(age)),alpha=0.25,size=1)+
  geom_point(data=mix2,aes(x=date,y=mean,shape=factor(pulse)),size=2)+
  geom_errorbar(data=mix2,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%d-%b")+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.position = "none")

#age1pulserange<-read.csv("./data/output/pulse_range_age1_final.csv")%>%
#  filter(cohort==2016)
age1length<-read.csv("./data/output/length_pulse_age1.csv")%>%
  filter(age==1 & species=="AC" & year == 2017)%>%
  select(-date)%>%
  mutate(date=ymd(paste(year,month,day,sep = "-")))

trips17<-trips%>%
  filter(year==2017)%>%
  group_by(trip)%>%
  arrange(month,day)%>%
  filter(row_number()==1)

age1pulse<-age1length%>%
  group_by(age,year,trip,pulse)%>%
  summarise(mean=mean(mmSL),minSL=min(mmSL),maxSL=max(mmSL))%>%
  ungroup()%>%
  left_join(trips17)%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))
pulses<-bind_rows(pulse.range2016,age1pulse)

length2016<-final%>%
  rename(month=Month,julian.date=Julian.Date,time=Time,site=Site,species=Species,notes=Notes,day=Day)%>%
  select(-Date)%>%
  bind_rows(age1length)%>%
  mutate(pulse=replace(pulse, age==1 & trip ==10 & year == 2017 & pulse == 2 & mmSL <69,3)) # temporarily add a "potential pulse 3"


length2016%>%
  mutate(date2=date+3)%>%
  ggplot()+
  geom_jitter(aes(x=date2,y=mmSL,colour=as.character(pulse),shape=as.character(age)),alpha=1,size=1)+
  geom_point(data=pulses,aes(x=date,y=mean,shape=factor(pulse)),size=2)+
  geom_errorbar(data=pulses,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%d-%b")+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.position = "none")

# ---- frequency histogram ----

length2016%>%
  filter(trip==19 & age == 0 & species=="AC")%>%
  ggplot()+
  geom_histogram(aes(x=mmSL,fill=factor(pulse)),bins=35)

length2016%>%
filter(age==1 & trip == 10 & species=="AC")%>%
  ggplot()+
  geom_histogram(aes(x=mmSL,fill=factor(pulse)),bins=35)


