#Size Frequency
# Newman Sound
# Age 1 Atlantic cod

#---- Purpose ----
# Determine pulse structure of age 1 cod using mixture models

# ----- set working directory ----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

#load packages
library(tidyverse)
library(lubridate)
library(mixdist)
#library(rlist)

#load data
length<-read.csv("./data/data-working/newman-length.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")

# ---- check data structure ----
str(length)
summary(length)
head(length)
tail(length)
names(length)
dim(length)

str(trips)
summary(trips)
head(trips)
tail(trips)
names(trips)
dim(trips)

# ---- format dates ----
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))

# ---- combine trip dates and length data ----
length<-left_join(length,trips)

# ---- multimodal distribution -----
# Pulse structure of age 1 cod for all trips
# from 1996 to 2017


# ---- age 1 1996 -----

# May
age1.1996<-length%>%
  filter(age==1)%>%
  filter(year==1996)%>%
  filter(month==5)%>%
  data.frame()
# not enough data - 0 observations in May

# July
age1.1996<-filter(length, age==1 & year==1996 & month==7)
# not enough data

# August week 1
age1.1996<-filter(length, age==1 & year==1996 & month==8)

# Not enough data for August

# September
age1.1996<-filter(length,age==1 & year == 1996 & month == 9)
# trip 18 only

# check histogram
qplot(mmSL,data=age1.1996,binwidth=5)+theme_classic()

# min and max sl
summarise(age1.1996,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1996<-select(age1.1996,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(group1996, breaks=c(0,seq(125,185,5),190),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(130,145,158,190),c(3))
plot(age1group,age1param,"gamma")


#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)

# store data
# note very poor fit!
head(age1.1996)
trip18.1996<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1996,month=9,day=26,trip=18,notes="very poor fit")# date = Sept 26
trip18.1996<-mutate(trip18.1996,dummy_pulse=rev(seq(1:nrow(trip18.1996))))

mixtures<-trip18.1996

# October
age1.1996<-filter(length,age==1 & year == 1996 & month == 10)
# not enough data

# November
age1.1996<-filter(length,age==1 & year ==1996 & month ==11)
# not enough data

# ---- age 1 1997 ----

# May
age1.1997<-length%>%
  filter(age==1)%>%
  filter(year==1997)%>%
  filter(month==5)%>%
  data.frame()

# not enough data - 0 observations in May

# July
age1.1997<-length%>%
  filter(age==1 & year==1997 & month==7)%>%
  data.frame()

# not enough data - 0 observations in July

# August
age1.1997<-length%>%
  filter(age==1 & year ==1997 & month ==8)

# no fish in August
# September
age1.1997<-length%>%
  filter(age==1 & year ==1997 & month ==9)
#no fish

# October
age1.1997<-length%>%
  filter(age==1 & year ==1997 & month ==10)
# no fish
# November
age1.1997<-length%>%
  filter(age==1 & year ==1997 & month ==11)
# no fish

# ---- age 1 1998 ----

# May

age1.1998<-length%>%
  filter(age==1 & year==1998 & month==5)

# not enough data - 0 observations in May

# July
age1.1998<-filter(length,age==1 & year == 1998 & month==7)

# not enough data - 9 observations only

# August
age1.1998<-filter(length,age==1 & year == 1998 & month==8)
# not enough for each trip


# ---- age 1 1999 ----

# May
age1.1999<-filter(length,age==1 & year == 1999 & month ==5)

# check histogram
qplot(mmSL,data=age1.1999,binwidth=5)

# detremine max and min SL
summarise(age1.1999,min(mmSL),max(mmSL), n())

# create dataframe with SL only
group1999<-select(age1.1999,mmSL)

# convert to frequency table
age1group<-mixgroup(group1999, breaks = c(0,seq(45,125,5),130),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(55,130),c(3))
plot(age1group,age1param,"gamma")


#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1, root=T)

#Single component
par<-groupstats(age1group)

# store data
head(age1.1999)
trip10.1999<-bind_cols(par)%>%
  mutate(year=1999,month=5,day=25,trip=10)
trip10.1999<-mutate(trip10.1999,dummy_pulse=rev(seq(1:nrow(trip10.1999))))
# add May 1999 to mixtures

mixtures<-bind_rows(mixtures,trip10.1999)

# July

age1.1999<-filter(length,age == 1 & year == 1999 & month == 7)

#check histogram
qplot(mmSL,data=age1.1999,binwidth=5)

# low samaple size

# August
age1.1999<-filter(length, age==1 & year ==1999 & month == 8)

# not enough data

# September
age1.1999<-filter(length, age==1 & year ==1999 & month == 9)
# not enough data for individual trips

# ---- age 1 2000 ----

# May
age1.2000<-filter(length,year == 2000 & age == 1 & month == 5)

# no May fish

# July trip 13
age1.2000<-filter(length,year == 2000 & age == 1 & month == 7 & trip == 13)


qplot(mmSL, data=age1.2000, binwidth=5)

# # detremine max and min SL
summarise(age1.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age1.2000,mmSL)

# convert to frequency table
age1group<-mixgroup(group2000, breaks = c(0,seq(75,140,5),145),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(90,117),c(4))
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=20, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)

# store model fit
head(age1.2000)
trip13.2000<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=7,day=17,trip=13)
trip13.2000<-mutate(trip13.2000,dummy_pulse=rev(seq(1:nrow(trip13.2000))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip13.2000)


# September trip 17
age1.2000<-filter(length,year==2000,age==1 & trip==17)
# Trip 17

qplot(mmSL, data=age1.2000, binwidth=5)

# # detremine max and min SL
summarise(age1.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age1.2000,mmSL)

# convert to frequency table
age1group<-mixgroup(group2000, breaks = c(0,seq(105,145,5),150),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,130,150),c(4))
plot(age1group,age1param,"gamma")
age1param<-mixparam(c(110,130,150),c(5,6,7),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma", mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)

# store results
head(age1.2000)
trip17.2000<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=9,day=12, trip=17) # Sept 12
trip17.2000<-mutate(trip17.2000,dummy_pulse=rev(seq(1:nrow(trip17.2000))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip17.2000)

# September week 2
age1.2000<-filter(length,year==2000,age==1 & trip==18)
# small sample size

qplot(mmSL, data=age1.2000, binwidth=5)

# # detremine max and min SL
summarise(age1.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age1.2000,mmSL)

# convert to frequency table
age1group<-mixgroup(group2000, breaks = c(0,seq(115,180,5),185),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(120,148,168,184),c(3),pi=NULL)
plot(age1group,age1param,"gamma")


# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)

# Fit 1.... (poor fit)
head(age1.2000)
trip18.2000<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=9,day=27,trip=18,notes="poor fit") # Sept 27
trip18.2000<-mutate(trip18.2000,dummy_pulse=rev(seq(1:nrow(trip18.2000))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip18.2000)

# October
age1.2000<-filter(length,year==2000 & age == 1 & trip == 19)
# Trip 19
# not enough

# October Trip 20
age1.2000<-filter(length,year==2000 & age == 1 & trip==20)

qplot(mmSL, data=age1.2000, binwidth=5)

# # detremine max and min SL
summarise(age1.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age1.2000,mmSL)

# convert to frequency table
age1group<-mixgroup(group2000, breaks = c(0,seq(115,190,5),195),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(115,145,160,190),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1,root=T)

# Fit 1....
head(age1.2000)
trip20.2000<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=10,day=24,trip=20,notes="medium fit") # Oct 24
trip20.2000<-mutate(trip20.2000,dummy_pulse=rev(seq(1:nrow(trip20.2000))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip20.2000)

# November trip 21
age1.2000<-filter(length,year==2000 & age ==1 & trip==21)
# Trip 21 only (not enough for 22)

qplot(mmSL, data=age1.2000, binwidth=5)

# # detremine max and min SL
summarise(age1.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age1.2000,mmSL)

# convert to frequency table
age1group<-mixgroup(group2000, breaks = c(0,seq(115,195,5),200),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(120,145,177),c(3),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store results
head(age1.2000)
trip21.2000<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=11,day=13,trip=21) # Nov 13
trip21.2000<-mutate(trip21.2000,dummy_pulse=rev(seq(1:nrow(trip21.2000))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip21.2000)

# ---- age 1 2001 ----

# May
age1.2001<-filter(length,year == 2001 & age == 1 & month == 5)

# no fish in May

# July trip 14
age1.2001<-filter(length,year==2001 & age ==1 & month ==7)

qplot(mmSL, data=age1.2001, binwidth=5)

# # detremine max and min SL
summarise(age1.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age1.2001,mmSL)

# convert to frequency table
age1group<-mixgroup(group2001, breaks = c(0,seq(65,120,5),125),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,110),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age1.2001)
trip14.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=7,day=18, trip=14)
trip14.2001<-mutate(trip14.2001,dummy_pulse=rev(seq(1:nrow(trip14.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip14.2001)

# August 
age1.2001<-filter(length,year==2001 & age ==1 & month ==8)
# not enough data

# September
age1.2001<-filter(length,year==2001 & age ==1 & month ==9)
# not enough data

# October
age1.2001<-filter(length,year==2001 & age ==1 & trip==19)
# Trip 19 right on cusp....

qplot(mmSL, data=age1.2001, binwidth=5)

# # detremine max and min SL
summarise(age1.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age1.2001,mmSL)

# convert to frequency table
age1group<-mixgroup(group2001, breaks = c(0,seq(105,170,5),175),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(100,120,140,175),c(2,3,4,5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)


# fit 1. store data
head(age1.2001)
trip19.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=10,day=2,trip=19)
trip19.2001<-mutate(trip19.2001,dummy_pulse=rev(seq(1:nrow(trip19.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip19.2001)

# October trip 20
age1.2001<-filter(length,year==2001 & age ==1 & trip==20)
# Trip 20

qplot(mmSL, data=age1.2001, binwidth=5)

# # detremine max and min SL
summarise(age1.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age1.2001,mmSL)

# convert to frequency table
age1group<-mixgroup(group2001, breaks = c(0,seq(125,185,5),190),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(120,140,160,190),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1,root=T)


# store results
head(age1.2001)
trip20.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=10,day=17,trip=20,notes="medium fit") # october 16
trip20.2001<-mutate(trip20.2001,dummy_pulse=rev(seq(1:nrow(trip20.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip20.2001)

# November trip 21
age1.2001<-filter(length,year==2001 & age ==1 & trip==21)


qplot(mmSL, data=age1.2001, binwidth=5)

# # detremine max and min SL
summarise(age1.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age1.2001,mmSL)

# convert to frequency table
age1group<-mixgroup(group2001, breaks = c(0,seq(130,200,5),205),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(140,160,180),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age1.2001)
trip21.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=11,day=1,trip=21,notes="medium fit") # november 1
trip21.2001<-mutate(trip21.2001,dummy_pulse=rev(seq(1:nrow(trip21.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip21.2001)

# November # Trip 22

age1.2001<-filter(length,year==2001 & age ==1 & trip==22)


qplot(mmSL, data=age1.2001, binwidth=5)

# # detremine max and min SL
summarise(age1.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age1.2001,mmSL)

# convert to frequency table
age1group<-mixgroup(group2001, breaks = c(0,seq(130,205,5),210),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(125,145,160,180,210),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit1..
# poor model fit
head(age1.2001)
trip22.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=11,day=15,trip=22,notes="very poor fit") # november 15
trip22.2001<-mutate(trip22.2001,dummy_pulse=rev(seq(1:nrow(trip22.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip22.2001)

# ---- age 1 2002 ----

# May
age1.2002<-filter(length,year==2002,age==1,month==5)

qplot(mmSL, data=age1.2002, binwidth=5)

# # detremine max and min SL
summarise(age1.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age1.2002,mmSL)

# convert to frequency table
age1group<-mixgroup(group2002, breaks = c(0,seq(40,105,5),110),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(45,65,90),c(5),pi=NULL)
plot(age1group,age1param)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store fit 1 results
head(age1.2002)
trip10.2002<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2002,month=5,day=23,trip=10)
trip10.2002<-mutate(trip10.2002,dummy_pulse=rev(seq(1:nrow(trip10.2002))))
# add to mixtures
mixtures<-bind_rows(mixtures,trip10.2002)

# June trip 11
age1.2002<-filter(length,year==2002 &age==1 & trip==11)

# check histogram
qplot(mmSL, data=age1.2002, binwidth=5)

# # detremine max and min SL
summarise(age1.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age1.2002,mmSL)

# convert to frequency table
age1group<-mixgroup(group2002, breaks = c(0,seq(45,100,5),106),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(45,65,102),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

#Fit 2. store results 
head(age1.2002)
trip11.2002<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(year=2002,month=6,day=11,trip=11)
trip11.2002<-mutate(trip11.2002,dummy_pulse=rev(seq(1:nrow(trip11.2002))))
# add to mixtures
mixtures<-bind_rows(mixtures,trip11.2002)

# July trip 12
age1.2002<-filter(length,year==2002 &age==1 & trip==12)

# check histogram
qplot(mmSL, data=age1.2002, binwidth=5)

# # detremine max and min SL
summarise(age1.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age1.2002,mmSL)

# convert to frequency table
age1group<-mixgroup(group2002, breaks = c(0,seq(55,130,5),133),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(55,80,100,130),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age1.2002)
trip12.2002<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(year=2002,month=6,day=26,trip=12)
trip12.2002<-mutate(trip12.2002,dummy_pulse=rev(seq(1:nrow(trip12.2002))))
# add to mixtures
mixtures<-bind_rows(mixtures,trip12.2002)

# July trip 13
age1.2002<-filter(length,year==2002 & age==1 & trip==13)

# check histogram
qplot(mmSL, data=age1.2002, binwidth=5)

# # detremine max and min SL
summarise(age1.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age1.2002,mmSL)

# convert to frequency table
age1group<-mixgroup(group2002, breaks = c(0,seq(60,115,5),120),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(60,80,110),c(5),pi=NULL)
plot(age1group,age1param)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age1.2002)
trip13.2002<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2002,month=7,day=10,trip=13,notes="poor fit")
trip13.2002<-mutate(trip13.2002,dummy_pulse=rev(seq(1:nrow(trip13.2002))))

mixtures<-bind_rows(mixtures,trip13.2002)

# July trip 14
age1.2002<-filter(length,year==2002 & age==1 & trip==14)

# check histogram
qplot(mmSL, data=age1.2002, binwidth=5)

# # detremine max and min SL
summarise(age1.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age1.2002,mmSL)

# convert to frequency table
age1group<-mixgroup(group2002, breaks = c(0,seq(70,115,5),119),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,110),c(5),pi=NULL)
plot(age1group,age1param)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store results
head(age1.2002)
trip14.2002<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2002,month=7,day=24,trip=14)
trip14.2002<-mutate(trip14.2002,dummy_pulse=rev(seq(1:nrow(trip14.2002))))
# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2002)

# August
age1.2002<-filter(length,year==2002 & age ==1 & trip==15)
# no fish
age1.2002<-filter(length,year==2002 & age == 1 & trip == 16)
# September
age1.2002<-filter(length,year==2002 & age == 1 & trip==17)
# not enough in sept
age1.2002<-filter(length,year==2002 & age == 1 & trip == 18)
# October
age1.2002<-filter(length,year==2002 & age == 1 & trip==19)
# no fish
# November
age1.2002<-filter(length,year==2002 & age == 1 & trip==20)
# no fish

# ---- age 1 2003 ----

# May
age1.2003<-filter(length,year==2003,age==1,month==5)

# check histogram
qplot(mmSL, data=age1.2003, binwidth=5)

# # detremine max and min SL
summarise(age1.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age1.2003,mmSL)

# convert to frequency table
age1group<-mixgroup(group2003, breaks = c(0,seq(50,125,5),131),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(50,80,115),c(6),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# won't let me drop to 2....
head(age1.2003)
trip10.2003<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=5,day=28,trip=10,notes="medium fit")
trip10.2003<-mutate(trip10.2003,dummy_pulse=rev(seq(1:nrow(trip10.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip10.2003)

# July
age1.2003<-filter(length,year==2003 & age==1 & trip==13)

# check histogram
qplot(mmSL, data=age1.2003, binwidth=5)

# # detremine max and min SL
summarise(age1.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age1.2003,mmSL)

# convert to frequency table
age1group<-mixgroup(group2003, breaks = c(0,seq(75,150,5),155),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(85,105,120),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=30, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


#store results
head(age1.2003)
trip13.2003<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=7,day=14,trip=13)
trip13.2003<-mutate(trip13.2003,dummy_pulse=rev(seq(1:nrow(trip13.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.2003)

# July trip 14
age1.2003<-filter(length,year==2003 & age==1 & trip==14)

# check histogram
qplot(mmSL, data=age1.2003, binwidth=5)

# # detremine max and min SL
summarise(age1.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age1.2003,mmSL)

# convert to frequency table
age1group<-mixgroup(group2003, breaks = c(0,seq(85,140,5),143),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,100,118,133),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param<-mixparam(c(75,100,125),c(3),pi=NULL)
fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps=15,usecondit = FALSE,print.level=0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store fit 2 results
head(age1.2003)
trip14.2003<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2003,month=7,day=31,trip=14)
trip14.2003<-mutate(trip14.2003,dummy_pulse=rev(seq(1:nrow(trip14.2003))))

mixtures<-bind_rows(mixtures,trip14.2003)

#August trip 16
age1.2003<-filter(length,year==2003 & age == 1 & trip==16)

# check histogram
qplot(mmSL, data=age1.2003, binwidth=5)

# # detremine max and min SL
summarise(age1.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age1.2003,mmSL)

# convert to frequency table
age1group<-mixgroup(group2003, breaks = c(0,seq(90,150,5),155),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(98,115,135),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

#Store results
head(age1.2003)
trip16.2003<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=8,day=27,trip=16) # Aug 27
trip16.2003<-mutate(trip16.2003,dummy_pulse=rev(seq(1:nrow(trip16.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2003)

# september trip 17
age1.2003<-filter(length,year==2003 & age ==1 & trip==17)
# week 1 Trip 17

# check histogram
qplot(mmSL, data=age1.2003, binwidth=5)

# # detremine max and min SL
summarise(age1.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age1.2003,mmSL)

# convert to frequency table
age1group<-mixgroup(group2003, breaks = c(0,seq(110,165,5),170),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(105,125,145,160),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


#Fit 1
head(age1.2003)
trip17.2003<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=9,day=9,trip=17,notes="medium fit") # sept 9
trip17.2003<-mutate(trip17.2003,dummy_pulse=rev(seq(1:nrow(trip17.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2003)

# september trip 18
age1.2003<-filter(length,year==2003 & age ==1 & trip==18)
# week 2 Trip 18

# check histogram
qplot(mmSL, data=age1.2003, binwidth=5)

# # detremine max and min SL
summarise(age1.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age1.2003,mmSL)

# convert to frequency table
age1group<-mixgroup(group2003, breaks = c(0,seq(105,170,5),175),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(100,130,170),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

#store results
head(age1.2003)
trip18.2003<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=9,day=25,trip=18) # sept 25
trip18.2003<-mutate(trip18.2003,dummy_pulse=rev(seq(1:nrow(trip18.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2003)

# October trip 19
age1.2003<-filter(length,year==2003 & age ==1 & trip==19)
# week 1 and trip 19

# check histogram
qplot(mmSL, data=age1.2003, binwidth=5)

# # detremine max and min SL
summarise(age1.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age1.2003,mmSL)

# convert to frequency table
age1group<-mixgroup(group2003, breaks = c(0,seq(105,185,5),190),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,130,155,190),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

#Fit 1
head(age1.2003)
trip19.2003<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=10,day=9,trip=19) # oct 9
trip19.2003<-mutate(trip19.2003,dummy_pulse=rev(seq(1:nrow(trip19.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2003)

# October trip 20
age1.2003<-filter(length,year==2003 & age ==1 & trip==20)
# week 2 and trip 20

# check histogram
qplot(mmSL, data=age1.2003, binwidth=5)

# # detremine max and min SL
summarise(age1.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age1.2003,mmSL)

# convert to frequency table
age1group<-mixgroup(group2003, breaks = c(0,seq(105,215,5),220),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,135,160,180,200),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

#Fit 1
head(age1.2003)
trip20.2003<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=10,day=24,trip=20) # oct 24
trip20.2003<-mutate(trip20.2003,dummy_pulse=rev(seq(1:nrow(trip20.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2003)

# November
age1.2003<-filter(length,year==2003 & age ==1 & month ==11)
# not enough fish

# ---- age 1 2004 ----

# May 
age1.2004<-filter(length,year==2004 & age == 1 & month ==5)

# not enough data

# July
age1.2004<-filter(length,year==2004 & age ==1 & month == 7)

# not enough fish

# August 
age1.2004<-filter(length,year==2004 & age ==1 & month ==8)
# not enough data

# September 
age1.2004<-filter(length,year==2004 & age ==1 & month ==9)
# note enough data

# October 
age1.2004<-filter(length,year==2004 & age ==1 & month ==10)
# not enough data

# November
age1.2004<-filter(length,year==2004 & age ==1 & month ==11)
# not enough data

# ---- age 1 2005 ----
# May

age1.2005<-filter(length,year==2005 & age == 1 & month == 5)

# check histogram
qplot(mmSL, data=age1.2005, binwidth=5)

# # detremine max and min SL
summarise(age1.2005,min(mmSL),max(mmSL))

# create dataframe with SL only
group2005<-select(age1.2005,mmSL)

# convert to frequency table
age1group<-mixgroup(group2005, breaks = c(0,seq(40,100,5),103),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(45,60,80,100),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2005)
trip9.2005<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=5,day=17,trip=9)
trip9.2005<-mutate(trip9.2005,dummy_pulse=rev(seq(1:nrow(trip9.2005))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip9.2005)

# July trip 13
age1.2005<-filter(length,year==2005 & age==1 & trip==13)

# check histogram
qplot(mmSL, data=age1.2005, binwidth=5)

# # detremine max and min SL
summarise(age1.2005,min(mmSL),max(mmSL))

# create dataframe with SL only
group2005<-select(age1.2005,mmSL)

# convert to frequency table
age1group<-mixgroup(group2005, breaks = c(0,seq(70,120,5),125),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,100,125),c(2,3,4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# Forcing three modes

# Store results of fit1
head(age1.2005)
trip13.2005<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=7,day=6,trip=13)
trip13.2005<-mutate(trip13.2005,dummy_pulse=rev(seq(1:nrow(trip13.2005))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.2005)

# August
age1.2005<-filter(length,year==2005 & age==1 & month ==8)
# no data

# September # trip 18
age1.2005<-filter(length,year==2005 & age==1 & trip==18)

# check histogram
qplot(mmSL, data=age1.2005, binwidth=5)

# # detremine max and min SL
summarise(age1.2005,min(mmSL),max(mmSL))

# create dataframe with SL only
group2005<-select(age1.2005,mmSL)

# convert to frequency table
age1group<-mixgroup(group2005, breaks = c(0,seq(120,170,5),175),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(135,170),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

#Store results
head(age1.2005)
trip18.2005<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=9,day=19,trip=18)
trip18.2005<-mutate(trip18.2005,dummy_pulse=rev(seq(1:nrow(trip18.2005))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2005)

# October trip 20
age1.2005<-filter(length, year==2005 & age ==1 & trip==20)
# Trip 20

# check histogram
qplot(mmSL, data=age1.2005, binwidth=5)

# # detremine max and min SL
summarise(age1.2005,min(mmSL),max(mmSL))

# create dataframe with SL only
group2005<-select(age1.2005,mmSL)

# convert to frequency table
age1group<-mixgroup(group2005, breaks = c(0,seq(145,200,5),205),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(140,165,190),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1 model results
head(age1.2005)
trip20.2005<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=10,day=18,trip=20)
trip20.2005<-mutate(trip20.2005,dummy_pulse=rev(seq(1:nrow(trip20.2005))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2005)

# November
age1.2005<-filter(length,year==2005 & age ==1 & month ==11)
# not enough data

# ---- age 1 2006 ----

# May
age1.2006<-filter(length,year==2006 & age == 1 & month ==5)

# small data set but going with it.

# check histogram
qplot(mmSL, data=age1.2006, binwidth=5)

# # detremine max and min SL
summarise(age1.2006,min(mmSL),max(mmSL))

# create dataframe with SL only
group2006<-select(age1.2006,mmSL)

# convert to frequency table
age1group<-mixgroup(group2006, breaks = c(0,seq(45,65,5),71),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
par<-groupstats(age1group)

# Fit 1
head(age1.2006)
trip9.2006<-bind_cols(par)%>%
  mutate(year=2006,month=5,day=10, trip=9)
trip9.2006<-mutate(trip9.2006,dummy_pulse=rev(seq(1:nrow(trip9.2006))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip9.2006)

# July
age1.2006<-filter(length,year==2006 & age ==1,month==7)

# not enough data. Move on

# August
age1.2006<-filter(length,year==2006 & age ==1,month==8)
# no data

# September
age1.2006<-filter(length,year==2006 & age ==1,month==9)
#only one fish

# October
age1.2006<-filter(length,year==2006 & age ==1,month==10)
# no data

# November
age1.2006<-filter(length,year==2006 & age ==1,month==11)
# no data

# ---- age 1 2007 ----

# May
age1.2007<-filter(length,year==2007 & age == 1 & month == 5)

# check histogram
qplot(mmSL, data=age1.2007, binwidth=5)

# # detremine max and min SL
summarise(age1.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age1.2007,mmSL)

# convert to frequency table
age1group<-mixgroup(group2007, breaks = c(0,seq(45,85,5),92),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(45,60,80),c(5,5,6),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age1.2007)
trip9.2007<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2007,month=5,day=7,trip=9)
trip9.2007<-mutate(trip9.2007,dummy_pulse=rev(seq(1:nrow(trip9.2007))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip9.2007)

# July trip 12
age1.2007<-filter(length,year==2007 & age == 1 & trip ==12)

# check histogram
qplot(mmSL, data=age1.2007, binwidth=5)

# # detremine max and min SL
summarise(age1.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age1.2007,mmSL)

# convert to frequency table
age1group<-mixgroup(group2007, breaks = c(0,seq(60,85,5),92),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(65,82),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age1.2007)
trip12.2007<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(year=2007,month=7,day=3,trip=12)
trip12.2007<-mutate(trip12.2007,dummy_pulse=rev(seq(1:nrow(trip12.2007))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip12.2007)

# August
age1.2007<-filter(length,year==2007 & age ==1 & month ==8)
# not enough

# September
age1.2007<-filter(length,year==2007 & age ==1 & month ==9)
# only one fish

# October
age1.2007<-filter(length,year==2007 & age ==1 & month ==10)
# nodata

# November
age1.2007<-filter(length,year==2007 & age ==1 & month ==11)
# not enough 

# ---- age 1 2008 ----
# May
age1.2008<-filter(length,year==2008,age==1,month==5)

# check histogram
qplot(mmSL, data=age1.2008, binwidth=5)

# # detremine max and min SL
summarise(age1.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age1.2008,mmSL)

# convert to frequency table
age1group<-mixgroup(group2008, breaks = c(0,seq(45,120,5),124),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(60,90),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1, root=T)


#store model results
head(age1.2008)
trip10.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=5,day=20,trip=10)
trip10.2008<-mutate(trip10.2008,dummy_pulse=rev(seq(1:nrow(trip10.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip10.2008)

# July trip 12
age1.2008<-filter(length,year==2008 & age == 1 & trip==12)

# check histogram
qplot(mmSL, data=age1.2008, binwidth=5)

# # detremine max and min SL
summarise(age1.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age1.2008,mmSL)

# convert to frequency table
age1group<-mixgroup(group2008, breaks = c(0,seq(60,100,5),105),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(70,105),c(5,10),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2008)
trip12.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=7,day=3,trip=12)
trip12.2008<-mutate(trip12.2008,dummy_pulse=rev(seq(1:nrow(trip12.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip12.2008)

#July trip 13
age1.2008<-filter(length,year==2008 & age == 1 & trip==13)
# check histogram
qplot(mmSL, data=age1.2008, binwidth=5)

# # detremine max and min SL
summarise(age1.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age1.2008,mmSL)

# convert to frequency table
age1group<-mixgroup(group2008, breaks = c(0,seq(60,145,5),152),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,100,130),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2008)
trip13.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=7,day=16,trip=13)
trip13.2008<-mutate(trip13.2008,dummy_pulse=rev(seq(1:nrow(trip13.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.2008)

#July trip 14
age1.2008<-filter(length,year==2008 & age == 1 & trip==14)

# check histogram
qplot(mmSL, data=age1.2008, binwidth=5)

# # detremine max and min SL
summarise(age1.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age1.2008,mmSL)

# convert to frequency table
age1group<-mixgroup(group2008, breaks = c(0,seq(75,160,5),165),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,100,120),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2008)
trip14.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=7,day=29,trip=14)
trip14.2008<-mutate(trip14.2008,dummy_pulse=rev(seq(1:nrow(trip14.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2008)

# August trip 16
age1.2008<-filter(length, year==2008 & age ==1 & trip==16)
# Trip 16

# check histogram
qplot(mmSL, data=age1.2008, binwidth=5)

# # detremine max and min SL
summarise(age1.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age1.2008,mmSL)

# convert to frequency table
age1group<-mixgroup(group2008, breaks = c(0,seq(90,150,5),155),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(85,107,120,135,155),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1 results
head(age1.2008)
trip16.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=8,day=28,trip=16)
trip16.2008<-mutate(trip16.2008,dummy_pulse=rev(seq(1:nrow(trip16.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2008)

# September trip 18
age1.2008<-filter(length,year==2008 & age==1 & trip==18)
# Trip 18

# check histogram
qplot(mmSL, data=age1.2008, binwidth=5)

# # detremine max and min SL
summarise(age1.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age1.2008,mmSL)

# convert to frequency table
age1group<-mixgroup(group2008, breaks = c(0,seq(110,180,5),185),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(105,122,145,160,180),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1 results
head(age1.2008)
trip18.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=9,day=29,trip=18)
trip18.2008<-mutate(trip18.2008,dummy_pulse=rev(seq(1:nrow(trip18.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2008)

# October
age1.2008 <- filter(length, year==2008 & age == 1 & trip==20)
# not enough

# November
age1.2008 <-filter(length,year==2008 & age ==1 & month ==11)
# notenough data

# ---- age 1 2009 ----

# May
age1.2009<-filter(length,year==2009 & age == 1& month == 5)

# check histogram
qplot(mmSL, data=age1.2009, binwidth=5)

# # detremine max and min SL
summarise(age1.2009,min(mmSL),max(mmSL))

# create dataframe with SL only
group2009<-select(age1.2009,mmSL)

# convert to frequency table
age1group<-mixgroup(group2009, breaks = c(0,seq(40,80,5),85),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(45,65,85),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2009)
trip9.2009<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2009,month=5,day=12,trip=9)
trip9.2009<-mutate(trip9.2009,dummy_pulse=rev(seq(1:nrow(trip9.2009))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip9.2009)

# July
age1.2009<-filter(length,year==2009 & age == 1 & month == 7)

# not enough data

# August
age1.2009<-filter(length,year==2009 & age == 1 & month == 8)
# not enough data

# September
age1.2009<-filter(length,year==2009 & age == 1 & month == 9)
# not enough data

# October
age1.2009<-filter(length,year==2009 & age == 1 & month == 10)
# not enough data

# November
age1.2009<-filter(length,year==2009 & age == 1 & month == 11)
# no data

# ----- age 1 2010 ----

# May 
age1.2010<-filter(length,year==2010 & age ==1 & month == 5)

# check histogram
qplot(mmSL, data=age1.2010, binwidth=5)

## detremine max and min SL
summarise(age1.2010,min(mmSL),max(mmSL))

# create dataframe with SL only
group2010<-select(age1.2010,mmSL)

# convert to frequency table
age1group<-mixgroup(group2010, breaks = c(0,seq(40,95,5),99),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(55,75,100),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# Store fit 1 results
head(age1.2010)
trip10.2010<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2010,month=5,day=17,trip=10)
trip10.2010<-mutate(trip10.2010,dummy_pulse=rev(seq(1:nrow(trip10.2010))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip10.2010)

# July trip 13
age1.2010<-filter(length,year==2010 & age ==1 & trip==13)

# check histogram
qplot(mmSL, data=age1.2010, binwidth=5)

# # detremine max and min SL
summarise(age1.2010,min(mmSL),max(mmSL))

# create dataframe with SL only
group2010<-select(age1.2010,mmSL)

# convert to frequency table
age1group<-mixgroup(group2010, breaks = c(0,seq(75,105,5),112),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(78,100),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)
# I guess fit 1. poor fit
head(age1.2010)
trip13.2010<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2010,month=7,day=12,trip=13,notes="poor fit")
trip13.2010<-mutate(trip13.2010,dummy_pulse=rev(seq(1:nrow(trip13.2010))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.2010)

# August 
age1.2010<-filter(length,year==2010 & age ==1 & month ==8)
# not enough data

# September
age1.2010<-filter(length,year==2010 & age ==1 & month ==9)
# not enough data

# October
age1.2010<-filter(length,year==2010 & age ==1 & month ==10)
# no data

# November
age1.2010<-filter(length,year==2010 & age ==1 & month ==11)
# no data

# ---- age 1 2011 ----
# May
age1.2011<-filter(length,year== 2011 & age ==1 & month == 5)

# check histogram
qplot(mmSL, data=age1.2011, binwidth=5)

# # detremine max and min SL
summarise(age1.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age1.2011,mmSL)

# convert to frequency table
age1group<-mixgroup(group2011, breaks = c(0,seq(45,110,5),117),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(55,80,100),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(55,100),c(2,3),pi=NULL)

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store fit 2 results
head(age1.2011)
trip10.2011<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2011,month=5,day=18,trip=10)
trip10.2011<-mutate(trip10.2011,dummy_pulse=rev(seq(1:nrow(trip10.2011))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip10.2011)

# July trip 12
age1.2011<-filter(length,year==2011 & age == 1 & trip==12)

# check histogram
qplot(mmSL, data=age1.2011, binwidth=5)

# # detremine max and min SL
summarise(age1.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age1.2011,mmSL)

# convert to frequency table
age1group<-mixgroup(group2011, breaks = c(0,seq(70,95,5),102),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(70,100),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=3, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2011)
trip12.2011<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=7,day=4,trip=12)
trip12.2011<-mutate(trip12.2011,dummy_pulse=rev(seq(1:nrow(trip12.2011))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip12.2011)

# July trip 13
# second trip
age1.2011<-filter(length,year==2011 & age == 1 & trip==13)

# check histogram
qplot(mmSL, data=age1.2011, binwidth=5)

# # detremine max and min SL
summarise(age1.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age1.2011,mmSL)

# convert to frequency table
age1group<-mixgroup(group2011, breaks = c(0,seq(75,145,5),150),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,110,130),c(4,6,6),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2011)
trip13.2011<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=7,day=18,trip=13,notes="medium/poor fit")
trip13.2011<-mutate(trip13.2011,dummy_pulse=rev(seq(1:nrow(trip13.2011))))

# addd to mixtures
mixtures<-bind_rows(mixtures,trip13.2011)

# August trip 15
age1.2011<-filter(length,year==2011 & age ==1 & trip==15)
# trip 15

# check histogram
qplot(mmSL, data=age1.2011, binwidth=5)

# # detremine max and min SL
summarise(age1.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age1.2011,mmSL)

# convert to frequency table
age1group<-mixgroup(group2011, breaks = c(0,seq(90,135,5),140),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(98,115,140),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# go with Fit 1 - 
head(age1.2011)
trip15.2011<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=8,day=16,trip=15)
trip15.2011<-mutate(trip15.2011,dummy_pulse=rev(seq(1:nrow(trip15.2011))))

# addd to mixtures
mixtures<-bind_rows(mixtures,trip15.2011)

# November
age1.2011<-filter(length,year==2011 & age == 1 & month ==11)
# not enough data

# ---- age 1 2012 ----

# May
age1.2012<-filter(length,year==2012 & age == 1 & month == 5)

# check histogram
qplot(mmSL, data=age1.2012, binwidth=5)

# # detremine max and min SL
summarise(age1.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age1.2012,mmSL)

# convert to frequency table
age1group<-mixgroup(group2012, breaks = c(0,seq(50,105,5),111),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(55,80,100),c(6),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2012)
trip10.2012<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2012,month=5,day=22,trip=10)
trip10.2012<-mutate(trip10.2012,dummy_pulse=rev(seq(1:nrow(trip10.2012))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip10.2012)

# July trip 12
age1.2012<-filter(length,year==2012 & age ==1 & trip ==12)

# check histogram
qplot(mmSL, data=age1.2012, binwidth=5)

# # detremine max and min SL
summarise(age1.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age1.2012,mmSL)

# convert to frequency table
age1group<-mixgroup(group2012, breaks = c(0,seq(65,90,5),96),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
par<-groupstats(age1group)

# store fit 2 results
head(age1.2012)
trip12.2012<-bind_cols(par)%>%
  mutate(year=2012,month=7,day=3,trip=12) 
trip12.2012<-mutate(trip12.2012,dummy_pulse=rev(seq(1:nrow(trip12.2012))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip12.2012)

# August
age1.2012<-filter(length,year==2012 & age ==1 & month ==8)
# no fish

# September
age1.2012<-filter(length,year==2012 & age ==1 & month ==9)
# not enough fish

# October trip 20
age1.2012<-length%>%
  filter(year==2012 & age ==1 & trip ==20)%>%
  filter(!is.na(mmSL))
# Trip 20

# check histogram
qplot(mmSL, data=age1.2012, binwidth=5)

# # detremine max and min SL
summarise(age1.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age1.2012,mmSL)

# convert to frequency table
age1group<-mixgroup(group2012, breaks = c(0,seq(130,205,5),210),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(130,150,170,200),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# Fit 1
head(age1.2012)
trip20.2012<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2012,month=10,day=29,trip=20)
trip20.2012<-mutate(trip20.2012,dummy_pulse=rev(seq(1:nrow(trip20.2012))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2012)

# November
age1.2012<-filter(length,year==2012 & age ==1 & month ==11)
# not enough data

# ---- age 1 2013 -----

# May
age1.2013<-filter(length,year==2013 & age == 1 & month ==5)

# check histogram
qplot(mmSL, data=age1.2013, binwidth=5)

# # detremine max and min SL
summarise(age1.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age1.2013,mmSL)

# convert to frequency table
age1group<-mixgroup(group2013, breaks = c(0,seq(40,125,5),129),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(55,80),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2013)
trip9.2013<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=5,day=7,trip=9)
trip9.2013<-mutate(trip9.2013,dummy_pulse=rev(seq(1:nrow(trip9.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip9.2013)

# July
age1.2013<-filter(length,year == 2013 & age == 1 & month ==7)

# no july data.......

# August
age1.2013<-filter(length,year == 2013 & age == 1 & month ==8)
# not enough data

# September trip 17
age1.2013<-filter(length,year == 2013 & age == 1 & trip == 17)
# Trip 17

# check histogram
qplot(mmSL, data=age1.2013, binwidth=5)

# # detremine max and min SL
summarise(age1.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age1.2013,mmSL)

# convert to frequency table
age1group<-mixgroup(group2013, breaks = c(0,seq(105,150,5),155),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,130,150),c(2,3,4),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2013)
trip17.2013<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=9,day=3, trip=17)
trip17.2013<-mutate(trip17.2013,dummy_pulse=rev(seq(1:nrow(trip17.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2013)

# October trip 19
age1.2013<-filter(length, year==2013 & age ==1 & trip==19)
# week 1 Trip 19

# check histogram
qplot(mmSL, data=age1.2013, binwidth=5)

# # detremine max and min SL
summarise(age1.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age1.2013,mmSL)

# convert to frequency table
age1group<-mixgroup(group2013, breaks = c(0,seq(120,200,5),205),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(130,150),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

par<-groupstats(age1group)

# store model results
head(age1.2013)
trip19.2013<-bind_cols(par)%>%
  mutate(year=2013,month=10,day=1,trip=19)
trip19.2013<-mutate(trip19.2013,dummy_pulse=rev(seq(1:nrow(trip19.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2013)

# October trip 20
age1.2013<-filter(length, year==2013 & age ==1 & trip ==20)
# week 2 Trip 20

# check histogram
qplot(mmSL, data=age1.2013, binwidth=5)

# # detremine max and min SL
summarise(age1.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age1.2013,mmSL)

# convert to frequency table
age1group<-mixgroup(group2013, breaks = c(0,seq(110,175,5),180),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,130,155),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age1.2013)
trip20.2013<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=10,day=15,trip=20)
trip20.2013<-mutate(trip20.2013,dummy_pulse=rev(seq(1:nrow(trip20.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2013)

# November trip 21
age1.2013<-filter(length,year==2013 & age ==1 & trip==21)
# week 1 trip 21

# check histogram
qplot(mmSL, data=age1.2013, binwidth=5)

# # detremine max and min SL
summarise(age1.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age1.2013,mmSL)

# convert to frequency table
age1group<-mixgroup(group2013, breaks = c(0,seq(125,200,5),205),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(140,160,180,200),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1
head(age1.2013)
trip21.2013<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=11,day=4,trip=21)
trip21.2013<-mutate(trip21.2013,dummy_pulse=rev(seq(1:nrow(trip21.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2013)

# November trip22
age1.2013<-filter(length,year==2013 & age ==1 & trip == 22)
# week 2 trip 22

# check histogram
qplot(mmSL, data=age1.2013, binwidth=5)

# # detremine max and min SL
summarise(age1.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age1.2013,mmSL)

# convert to frequency table
age1group<-mixgroup(group2013, breaks = c(0,seq(130,170,5),175),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(125,145,170),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# fit 1
head(age1.2013)
trip22.2013<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=11,day=18,trip=22)
trip22.2013<-mutate(trip22.2013,dummy_pulse=rev(seq(1:nrow(trip22.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip22.2013)


# ---- age 1 2014 -----
# May
age1.2014<-filter(length,year==2014 & age ==1 & month ==5)

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(45,130,5),136),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(55,80,110),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store fit 1 results..
head(age1.2014)
trip10.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=5,day=27,trip=10)
trip10.2014<-mutate(trip10.2014,dummy_pulse=rev(seq(1:nrow(trip10.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip10.2014)

# July
age1.2014<-filter(length,year == 2014 & age == 1 & trip==13)

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(65,195,5),198),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,120,170),c(5),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store fit 1 results
head(age1.2014)
trip13.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=7,day=14,trip=13)
trip13.2014<-mutate(trip13.2014,dummy_pulse=rev(seq(1:nrow(trip13.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.2014)

# July second sampling week
age1.2014<-filter(length,year == 2014 & age == 1 & trip==14)

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(75,135,5),141),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(85,120,140),c(2,3,4),pi=NULL)
plot(age1group,age1param,'gamma')

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(75,100,120),c(2,3,4),pi=NULL)
fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# Fit 2 very poor fit
head(age1.2014)
trip14.2014<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2014,month=7,day=29,trip=14,notes='very poor fit')
trip14.2014<-mutate(trip14.2014,dummy_pulse=rev(seq(1:nrow(trip14.2014))))

mixtures<-bind_rows(mixtures,trip14.2014)

# August trip 16
age1.2014<-filter(length,year==2014 & age == 1 & trip==16)

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(90,170,5),178),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(90,120,145,170),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store fit 2 results
head(age1.2014)
trip16.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=8,day=25,trip=16)
trip16.2014<-mutate(trip16.2014,dummy_pulse=rev(seq(1:nrow(trip16.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2014)

# September trip 17
age1.2014<-filter(length,year==2014 & age == 1 & trip==17)

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(90,185,5),190),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(120,162),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store fit 2 results
head(age1.2014)
trip17.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=9,day=8,trip=17)
trip17.2014<-mutate(trip17.2014,dummy_pulse=rev(seq(1:nrow(trip17.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2014)


# September trip 18
age1.2014<-filter(length,year==2014 & age == 1 & trip==18)

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(100,155,5),160),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,130,160),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store fit 2 results
head(age1.2014)
trip18.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=9,day=23,trip=18)
trip18.2014<-mutate(trip18.2014,dummy_pulse=rev(seq(1:nrow(trip18.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2014)

# October trip 19
age1.2014<-filter(length,year==2014 & age == 1 & trip==19)

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(105,185,5),190),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(115,140,165),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store fit 2 results
head(age1.2014)
trip19.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=10,day=7,trip=19)
trip19.2014<-mutate(trip19.2014,dummy_pulse=rev(seq(1:nrow(trip19.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2014)

# October trip 20
age1.2014 <- filter(length,year==2014 & age == 1 & trip == 20)

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(105,185,5),189),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(115,140,165),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store fit 2 results
head(age1.2014)
trip20.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=10,day=21,trip=20)
trip20.2014<-mutate(trip20.2014,dummy_pulse=rev(seq(1:nrow(trip20.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2014)

# November
age1.2014<-filter(length,year==2014 & age ==1 & trip==21)
# week 1 trip 21

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(115,200,5),206),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(130,150,180),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# Fit 1 store results
head(age1.2014)
trip21.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=11,day=4,trip=21)
trip21.2014<-mutate(trip21.2014,dummy_pulse=rev(seq(1:nrow(trip21.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2014)

# November trip 22
age1.2014<-filter(length,year==2014 & age ==1 & trip==22)
# week 2 trip 22

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(125,175,5),180),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(130,150,170),c(2,3,4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

# store model results
head(age1.2014)
trip22.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=11,day=18,trip=22)
trip22.2014<-mutate(trip22.2014,dummy_pulse=rev(seq(1:nrow(trip22.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip22.2014)

# ---- age 1 2015 ----
# May
age1.2015<-filter(length, year == 2015 & age == 1 & month == 5)

# check histogram
qplot(mmSL, data=age1.2015, binwidth=5)

# # detremine max and min SL
summarise(age1.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age1.2015,mmSL)

# convert to frequency table
age1group<-mixgroup(group2015, breaks = c(0,seq(35,120,5),127),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(45,70,95),c(3),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age1.2015)
trip10.2015<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=5,day=19,trip=10)
trip10.2015<-mutate(trip10.2015,dummy_pulse=rev(seq(1:nrow(trip10.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip10.2015)

# July 
age1.2015<-filter(length, year == 2015 & age == 1 & month==7)
# not enough fish

# August
age1.2015<-filter(length,year==2015 & age ==1 & month ==8)
# not enough 

# September trip 17
age1.2015<-filter(length,year==2015 & age ==1 & trip == 17)
# Trip 17

# check histogram
qplot(mmSL, data=age1.2015, binwidth=5)

# # detremine max and min SL
summarise(age1.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age1.2015,mmSL)

# convert to frequency table
age1group<-mixgroup(group2015, breaks = c(0,seq(105,160,5),165),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,125,140,165),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# go with fit 1
head(age1.2015)
trip17.2015<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=9,day=14,trip=17)
trip17.2015<-mutate(trip17.2015,dummy_pulse=rev(seq(1:nrow(trip17.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2015)

# October
age1.2015<-filter(length, year==2015 & age == 1 & month ==10)
# not enough data

# November
age1.2015<-filter(length, year==2015 & age == 1 & month ==11)
# not enough data

# ---- age 1 2016 ----
# May
age1.2016<-filter(length, year==2016 & age == 1 & month == 5)

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(35,110,5),118),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(45,65,105),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store fit 1 results
head(age1.2016)
trip10.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=5,day=16,trip=10)
trip10.2016<-mutate(trip10.2016,dummy_pulse=rev(seq(1:nrow(trip10.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip10.2016)

# July trip 12
age1.2016<-filter(length, year == 2016 & age == 1 & trip==12)


# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(50,195,5),200),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(60,90,110,200),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store fit 1 results
head(age1.2016)
trip12.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=7,day=4,trip=12)
trip12.2016<-mutate(trip12.2016,dummy_pulse=rev(seq(1:nrow(trip12.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip12.2016)

# July trip 13
age1.2016<-filter(length, year == 2016 & age == 1 & trip==13) 
# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(60,145,5),149),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(70,95,120),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2016)
trip13.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=7,day=18, trip=13)
trip13.2016<-mutate(trip13.2016,dummy_pulse=rev(seq(1:nrow(trip13.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.2016)

# August trip 14
age1.2016<-filter(length,year==2016 & age ==1 & trip ==14)
# week 1 trip 14

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(75,115,5),120),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(85,110),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2016)
trip14.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=8,day=2,trip=14)
trip14.2016<-mutate(trip14.2016,dummy_pulse=rev(seq(1:nrow(trip14.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2016)

# August trip 15
age1.2016<-filter(length,year==2016 & age ==1 & trip==15)

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(70,125,5),132),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(85,103),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1
head(age1.2016)
trip15.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=8,day=16,trip=15)
trip15.2016<-mutate(trip15.2016,dummy_pulse=rev(seq(1:nrow(trip15.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.2016)

# August/September Trip 16
age1.2016<-filter(length,year==2016 & trip==16)
# week 3 trip 16

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(45,135,5),139),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(55,100,135),c(6),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2016)
trip16.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=8,day=31, trip=16)
trip16.2016<-mutate(trip16.2016,dummy_pulse=rev(seq(1:nrow(trip16.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2016)

# September trip 17
age1.2016<-filter(length,year==2016 & age == 1 & trip == 17)

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(90,150,5),155),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(100,124,155),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=25, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2016)
trip17.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=9,day=13, trip=17)
trip17.2016<-mutate(trip17.2016,dummy_pulse=rev(seq(1:nrow(trip17.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2016)

# September trip 18 
age1.2016<-filter(length,year==2016 & age ==1& trip==18)

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(100,165,5),170),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,125,145,165),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# fit 1, but poor fit
# add to mixtures df
head(age1.2016)
trip18.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=9,day=28, trip=18,notes="very poor fit")
trip18.2016<-mutate(trip18.2016,dummy_pulse=rev(seq(1:nrow(trip18.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2016)

# october trip 19
age1.2016<-filter(length,year==2016 & age ==1 & trip == 19)

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(105,190,5),196),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,130,150,170),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)
# store results
head(age1.2016)
trip19.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=10,day=13, trip=19)
trip19.2016<-mutate(trip19.2016,dummy_pulse=rev(seq(1:nrow(trip19.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2016)

# october trip 20
age1.2016<-filter(length,year==2016 & age ==1 & trip == 20)

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(110,185,5),190),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(120,150,170),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit1 
head(age1.2016)
trip20.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=10,day=27,trip=20)
trip20.2016<-mutate(trip20.2016,dummy_pulse=rev(seq(1:nrow(trip20.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2016)

# November trip 21
age1.2016<-filter(length,year==2016 & age == 1 & trip == 21)

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(105,190,5),195),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,135,155,190),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2016)
trip21.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=11,day=14, trip=21)
trip21.2016<-mutate(trip21.2016,dummy_pulse=rev(seq(1:nrow(trip21.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2016)

# ----- age 1 2017 ------
# May
age1.2017<-filter(length, year==2017 & age == 1 & month == 5)

# check histogram
qplot(mmSL, data=age1.2017, binwidth=5)

# # detremine max and min SL
summarise(age1.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age1.2017,mmSL)

# convert to frequency table
age1group<-mixgroup(group2017, breaks = c(0,seq(40,130,5),138),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(50,80,125),c(5),pi=NULL)
plot(age1group,age1param,"gamma")
# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2017)
trip10.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=5,day=23,trip=10)
trip10.2017<-mutate(trip10.2017,dummy_pulse=rev(seq(1:nrow(trip10.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip10.2017)

# July trip 13
age1.2017<-filter(length,year==2017 & age ==1 & trip==13)

# check histogram
qplot(mmSL, data=age1.2017, binwidth=5)

# # detremine max and min SL
summarise(age1.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age1.2017,mmSL)

# convert to frequency table
age1group<-mixgroup(group2017, breaks = c(0,seq(55,135,5),140),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(60,85,105,140),c(4),pi=NULL)
plot(age1group,age1param,"gamma")
# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2017)
trip13.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=7,day=10, trip=13)
trip13.2017<-mutate(trip13.2017,dummy_pulse=rev(seq(1:nrow(trip13.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.2017)

# july trip 14
age1.2017<-filter(length,year==2017 & age ==1 & trip==14)

# check histogram
qplot(mmSL, data=age1.2017, binwidth=5)

# # detremine max and min SL
summarise(age1.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age1.2017,mmSL)

# convert to frequency table
age1group<-mixgroup(group2017, breaks = c(0,seq(55,140,5),145),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(65,100,125),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2017)
trip14.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=7,day=24, trip=14)
trip14.2017<-mutate(trip14.2017,dummy_pulse=rev(seq(1:nrow(trip14.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2017)

# August trip 16
age1.2017<-filter(length,year==2017 & age ==1 &trip ==16)

# check histogram
qplot(mmSL, data=age1.2017, binwidth=5)

# # detremine max and min SL
summarise(age1.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age1.2017,mmSL)

# convert to frequency table
age1group<-mixgroup(group2017, breaks = c(0,seq(80,160,5),167),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(85,120,142,163),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2017)
trip16.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=8,day=21, trip=16)
trip16.2017<-mutate(trip16.2017,dummy_pulse=rev(seq(1:nrow(trip16.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2017)

# September trip 17
age1.2017<-filter(length,year==2017 & age ==1& trip ==17)

# check histogram
qplot(mmSL, data=age1.2017, binwidth=5)

# # detremine max and min SL
summarise(age1.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age1.2017,mmSL)

# convert to frequency table
age1group<-mixgroup(group2017, breaks = c(0,seq(82,160,5),162),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(95,112,130,150),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2017)
trip17.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=9,day=5, trip=17,notes="poor model fit")
trip17.2017<-mutate(trip17.2017,dummy_pulse=rev(seq(1:nrow(trip17.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2017)

# September trip 18
age1.2017<-filter(length,year==2017 & age ==1 & trip==18)

# check histogram
qplot(mmSL, data=age1.2017, binwidth=5)

# # detremine max and min SL
summarise(age1.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age1.2017,mmSL)

# convert to frequency table
age1group<-mixgroup(group2017, breaks = c(0,seq(95,175,5),180),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(120,145),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2017)
trip18.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=9,day=18, trip=18)
trip18.2017<-mutate(trip18.2017,dummy_pulse=rev(seq(1:nrow(trip18.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2017)

# October trip 19
age1.2017<-filter(length,year==2017 & age ==1 & trip ==19)

# check histogram
qplot(mmSL, data=age1.2017, binwidth=5)

# # detremine max and min SL
summarise(age1.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age1.2017,mmSL)

# convert to frequency table
age1group<-mixgroup(group2017, breaks = c(0,seq(95,190,5),195),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(115,140,170),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2017)
trip19.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=10,day=2, trip=19)
trip19.2017<-mutate(trip19.2017,dummy_pulse=rev(seq(1:nrow(trip19.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2017)

# October trip 20
age1.2017<-filter(length,year==2017 & age ==1 & trip ==20)

# check histogram
qplot(mmSL, data=age1.2017, binwidth=5)

# # detremine max and min SL
summarise(age1.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age1.2017,mmSL)

# convert to frequency table
age1group<-mixgroup(group2017, breaks = c(0,seq(105,165,5),170),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(118,139,160),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2017)
trip20.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=10,day=17, trip=20)
trip20.2017<-mutate(trip20.2017,dummy_pulse=rev(seq(1:nrow(trip20.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2017)

# October/November
age1.2017<-filter(length,year==2017 & age ==1 & trip ==21)
# not enough fish

# November 
age1.2017<-filter(length,year==2017 & age==1 & month ==11)
# not enough fish

# --- age 1 2018 -----

# May 2018
age1.2018<-filter(length,year==2018 & age ==1 & month == 5)

# check histogram
qplot(mmSL, data=age1.2018, binwidth=5)

# # detremine max and min SL
summarise(age1.2018,min(mmSL),max(mmSL))

# create dataframe with SL only
group2018<-select(age1.2018,mmSL)

# convert to frequency table
age1group<-mixgroup(group2018, breaks = c(0,seq(35,125,5),132),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(45,90,120),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2018)
trip9.2018<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,month=5,day=8, trip=9,notes="poor model fit")
trip9.2018<-mutate(trip9.2018,dummy_pulse=rev(seq(1:nrow(trip9.2018))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip9.2018)

# July 2018 trip 13
age1.2018<-filter(length,year==2018 & age ==1 & trip==13)

# check histogram
qplot(mmSL, data=age1.2018, binwidth=5)

# # detremine max and min SL
summarise(age1.2018,min(mmSL),max(mmSL))

# create dataframe with SL only
group2018<-select(age1.2018,mmSL)

# convert to frequency table
age1group<-mixgroup(group2018, breaks = c(0,seq(60,185,5),192),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(60,95,120,150),c(6),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=60, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2018)
trip13.2018<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,month=7,day=16, trip=13)
trip13.2018<-mutate(trip13.2018,dummy_pulse=rev(seq(1:nrow(trip13.2018))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.2018)

# July 2018 trip 14
age1.2018<-filter(length,year==2018 & age ==1 & trip==14)

# check histogram
qplot(mmSL, data=age1.2018, binwidth=5)

# # detremine max and min SL
summarise(age1.2018,min(mmSL),max(mmSL))

# create dataframe with SL only
group2018<-select(age1.2018,mmSL)

# convert to frequency table
age1group<-mixgroup(group2018, breaks = c(0,seq(70,140,5),145),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,115,140),c(6),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2018)
trip14.2018<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,month=7,day=30, trip=14,notes="medium/poor fit")
trip14.2018<-mutate(trip14.2018,dummy_pulse=rev(seq(1:nrow(trip14.2018))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2018)

# August 2018 trip 15
age1.2018<-filter(length,year==2018 & age ==1 & trip==15)

# check histogram
qplot(mmSL, data=age1.2018, binwidth=5)

# # detremine max and min SL
summarise(age1.2018,min(mmSL),max(mmSL))

# create dataframe with SL only
group2018<-select(age1.2018,mmSL)

# convert to frequency table
age1group<-mixgroup(group2018, breaks = c(0,seq(80,180,5),185),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(90,110,140,182),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2018)
trip15.2018<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,month=8,day=13, trip=15)
trip15.2018<-mutate(trip15.2018,dummy_pulse=rev(seq(1:nrow(trip15.2018))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.2018)

# August trip 16
age1.2018<-filter(length,year==2018 & age ==1 & trip==16)
# not enough fish

# September 2018 trip 17
age1.2018<-filter(length,year==2018 & age ==1 & trip==17)

# check histogram
qplot(mmSL, data=age1.2018, binwidth=5)

# # detremine max and min SL
summarise(age1.2018,min(mmSL),max(mmSL))

# create dataframe with SL only
group2018<-select(age1.2018,mmSL)

# convert to frequency table
age1group<-mixgroup(group2018, breaks = c(0,seq(95,205,5),2011),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(100,130,160,200),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2018)
trip17.2018<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,month=9,day=10, trip=17)
trip17.2018<-mutate(trip17.2018,dummy_pulse=rev(seq(1:nrow(trip17.2018))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2018)

# September 2018 trip 18
age1.2018<-filter(length,year==2018 & age ==1 & trip==18)

# check histogram
qplot(mmSL, data=age1.2018, binwidth=5)

# # detremine max and min SL
summarise(age1.2018,min(mmSL),max(mmSL))

# create dataframe with SL only
group2018<-select(age1.2018,mmSL)

# convert to frequency table
age1group<-mixgroup(group2018, breaks = c(0,seq(105,185,5),189),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(100,115,135,175),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2018)
trip18.2018<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,month=9,day=24, trip=18)
trip18.2018<-mutate(trip18.2018,dummy_pulse=rev(seq(1:nrow(trip18.2018))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2018)

# October 2018 trip 19
age1.2018<-filter(length,year==2018 & age ==1 & trip==19)

# check histogram
qplot(mmSL, data=age1.2018, binwidth=5)

# # detremine max and min SL
summarise(age1.2018,min(mmSL),max(mmSL))

# create dataframe with SL only
group2018<-select(age1.2018,mmSL)

# convert to frequency table
age1group<-mixgroup(group2018, breaks = c(0,seq(110,145,5),152),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(115,145),c(6),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2018)
trip19.2018<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,month=10,day=9, trip=19)
trip19.2018<-mutate(trip19.2018,dummy_pulse=rev(seq(1:nrow(trip19.2018))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2018)

# October 2018 trip 20
age1.2018<-filter(length,year==2018& age ==1 & trip==20)

# check histogram
qplot(mmSL, data=age1.2018, binwidth=5)

# # detremine max and min SL
summarise(age1.2018,min(mmSL),max(mmSL))

# create dataframe with SL only
group2018<-select(age1.2018,mmSL)

# convert to frequency table
age1group<-mixgroup(group2018, breaks = c(0,seq(115,165,5),172),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(110,130,160),c(6),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2018)
trip20.2018<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,month=10,day=22, trip=20)
trip20.2018<-mutate(trip20.2018,dummy_pulse=rev(seq(1:nrow(trip20.2018))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2018)

# November 2018 trip 21
age1.2018<-filter(length,year==2018 & age ==1 & trip==21)
# not enough fish

# November trip 22
age1.2018<-filter(length,year==2018 & age == 1 & trip == 22)
# not enough fish

# December
age1.2018<-filter(length,year==2018 & age == 1 & month == 12)
# not enough fish

View(mixtures)

# add 'cohort', 'age' to mixtures df
# ---- Final mixtures DF -----
mixtures2<-mixtures%>%
  mutate(cohort=year-1,age=1)

write.csv(mixtures2,"./data/output/age-1-mixture-dist.csv",row.names = FALSE)
