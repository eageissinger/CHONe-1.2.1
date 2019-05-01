#Size Frequency
# Newman Sound
# Age 1 Atlantic cod

#---- Purpose ----
# Determine pulse structure of age 1 cod using mixture models

# ----- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

#load packages
library(tidyverse)
library(lubridate)
library(mixdist)

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

# ---- multimodal distribution -----
# Pulse structure of age 0 cod for all trips
# from 1996 to 2017


# ---- age 0 1996 -----

# May
age0.1996<-length%>%
  filter(age==0)%>%
  filter(year==1996)%>%
  filter(month==5)%>%
  data.frame()
# not enough data - 0 observations in May

# July
age0.1996<-filter(length, age==0 & year==1996 & month==7)

# not enough fish

# August
age0.1996<-filter(length, age==0 & year == 1996 & month ==8)
#n not enough fish in august

# September
age0.1996<-filter(length, age==0 & year == 1996 & month ==9)
# not enough

# October
age0.1996<-filter(length, age==0 & year==1996 & month==10 & trip == 19)

# just enough to try for trip 19

# check histogram
qplot(mmSL,data=age0.1996,binwidth=5)+theme_classic()

# min and max sl
summarise(age0.1996,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1996<-select(age0.1996,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1996, breaks=c(0,seq(25,60,5),66),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(40,65),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)
# one mode:
fit2<-groupstats(age0group)

# store data
# note very poor fit!
head(age0.1996)
trip19.1996<-fit2%>%
  mutate(year=1996,month=10,day=7,trip=19)
trip19.1996<-mutate(trip19.1996,dummy_pulse=rev(seq(1:nrow(trip19.1996))))

mixtures<-trip19.1996

# October week 2
age0.1996<-filter(length,age==0 & year == 1996 & month == 10 & trip ==20)

# check histogram
qplot(mmSL,data=age0.1996,binwidth=5)+theme_classic()

# min and max sl
summarise(age0.1996,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1996<-select(age0.1996,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1996, breaks=c(0,seq(20,65,5),69),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(30,50),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
head(age0.1996)
trip20.1996<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1996,month=10,day=21,trip=20)
trip20.1996<-mutate(trip20.1996,dummy_pulse=rev(seq(1:nrow(trip20.1996))))

mixtures<-bind_rows(mixtures,trip20.1996)

# November
age0.1996<-filter(length,age==0 & year ==1996 & month ==11 & trip ==21)

# check histogram
qplot(mmSL,data=age0.1996,binwidth=5)+theme_classic()

# min and max sl
summarise(age0.1996,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1996<-select(age0.1996,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1996, breaks=c(0,seq(30,75,5),79),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(30,40,60,80),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
head(age0.1996)
trip21.1996<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1996,month=11,day=06,trip=21)
trip21.1996<-mutate(trip21.1996,dummy_pulse=rev(seq(1:nrow(trip21.1996))))

mixtures<-bind_rows(mixtures,trip21.1996)

#November trip 22
age0.1996<-filter(length,age==0 & year == 1996 & month == 11 & trip ==22)

# ---- age 0 1997 ----

# May
age0.1997<-length%>%
  filter(age==0)%>%
  filter(year==1997)%>%
  filter(month==5)%>%
  data.frame()

# no fish
# July
age0.1997<-length%>%
  filter(age==0 & year==1997 & month==7)%>%
  data.frame()

# no fish

# August
age0.1997<-length%>%
  filter(age==0 & year ==1997 & month ==8)

# no fish in August

# September
age0.1997<-length%>%
  filter(age==0 & year ==1997 & month ==9)
#no fish

# October
age0.1997<-length%>%
  filter(age==0 & year ==1997 & month ==10)
# no fish
# November
age0.1997<-length%>%
  filter(age==0 & year ==1997 & month ==11)
# no fish

# ---- age 0 1998 ----

# May

age0.1998<-length%>%
  filter(age==0 & year==1998 & month==5)

# no fish
age0.1996<-filter(length,age==0 & year == 1998 & trip ==13)

# July
age0.1998<-filter(length,age==0 & year == 1998 & month==7 & trip ==14)

# check histogram
qplot(mmSL,data=age0.1998,binwidth=5)+theme_classic()


# check histogram again
qplot(mmSL,data=age0.1998,binwidth=5)

# detremine max and min SL
summarise(age0.1998,min(mmSL),max(mmSL), n())

# create dataframe with SL only
group1998<-select(age0.1998,mmSL)

# convert to frequency table
age0group<-mixgroup(group1998, breaks = c(0,seq(35,45,5),46),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

#Single component
par<-groupstats(age0group)

# store data
head(age0.1998)
trip14.1998<-bind_cols(par)%>%
  mutate(year=1998,month=7,day=29,trip=14)
trip14.1998<-mutate(trip14.1998,dummy_pulse=rev(seq(1:nrow(trip14.1998))))
# add May 1999 to mixtures

mixtures<-bind_rows(mixtures,trip14.1998)

# August
age0.1998<-filter(length,age==0 & year == 1998 & month==8 & trip ==15)

# check histogram
qplot(mmSL,data=age0.1998,binwidth=5)

# min and max sl
summarise(age0.1998,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1998<-select(age0.1998,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1998, breaks=c(0,seq(40,50,5),52),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(35,45),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

par<-groupstats(age0group)

# store data
head(age0.1998)
trip15.1998<-bind_cols(par)%>%
  mutate(year=1998,month=8,day=05,trip=15)
trip15.1998<-mutate(trip15.1998,dummy_pulse=rev(seq(1:nrow(trip15.1998))))

mixtures<-bind_rows(mixtures,trip15.1998)

# August week 2
# August
age0.1998<-filter(length,age==0 & year == 1998 & month==8 & trip ==16)

# check histogram
qplot(mmSL,data=age0.1998,binwidth=5)

# min and max sl
summarise(age0.1998,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1998<-select(age0.1998,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1998, breaks=c(0,seq(45,60,5),63),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(40,53,65),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

par<-groupstats(age0group)

# store data
head(age0.1998)
trip16.1998<-bind_cols(par)%>%
  mutate(year=1998,month=8,day=20,trip=16)
trip16.1998<-mutate(trip16.1998,dummy_pulse=rev(seq(1:nrow(trip16.1998))))

mixtures<-bind_rows(mixtures,trip16.1998)

# September
age0.1998<-filter(length,age==0 & year == 1998 & month==9 & trip ==18)

# check histogram
qplot(mmSL,data=age0.1998,binwidth=5)

# min and max sl
summarise(age0.1998,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1998<-select(age0.1998,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1998, breaks=c(0,seq(70,80,5),85),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(65,75,85),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

par<-groupstats(age0group)

# store data
head(age0.1998)
trip18.1998<-bind_cols(par)%>%
  mutate(year=1998,month=9,day=15,trip=18)
trip18.1998<-mutate(trip18.1998,dummy_pulse=rev(seq(1:nrow(trip18.1998))))

mixtures<-bind_rows(mixtures,trip18.1998)

# October
age0.1998<-filter(length,age==0 & year == 1998 & month==10)
# not enough observations

# December
age0.1998<-filter(length,age==0 & year == 1998 & month == 12)
# no fish

# ---- age 0 1999 ----

# May
age0.1999<-filter(length,age==0 & year == 1999 & month ==5)

# July
age0.1999<-filter(length,age==0 & year ==1999 & month == 7)
# not enough fish

# August
age0.1999<-filter(length,age==0 & year == 1999 & month == 8 & trip == 15)

# check histogram
qplot(mmSL,data=age0.1999,binwidth=5)

# detremine max and min SL
summarise(age0.1999,min(mmSL),max(mmSL), n())

# create dataframe with SL only
group1999<-select(age0.1999,mmSL)

# convert to frequency table
age0group<-mixgroup(group1999, breaks = c(0,seq(40,65,5),67),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

#Single component
par<-groupstats(age0group)

# store data
head(age0.1999)
trip15.1999<-bind_cols(par)%>%
  mutate(year=1999,month=8,day=11,trip=15)
trip15.1999<-mutate(trip15.1999,dummy_pulse=rev(seq(1:nrow(trip15.1999))))
# add May 1999 to mixtures

mixtures<-bind_rows(mixtures,trip15.1999)

# August week 2 (trip 16)
age0.1999<-filter(length,age==0 & year == 1999 & month == 8 & trip == 16)

# check histogram
qplot(mmSL,data=age0.1999,binwidth=5)

# detremine max and min SL
summarise(age0.1999,min(mmSL),max(mmSL), n())

# create dataframe with SL only
group1999<-select(age0.1999,mmSL)

# convert to frequency table
age0group<-mixgroup(group1999, breaks = c(0,seq(45,75,5),77),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

#Single component
par<-groupstats(age0group)

# store data
head(age0.1999)
trip16.1999<-bind_cols(par)%>%
  mutate(year=1999,month=8,day=25,trip=16)
trip16.1999<-mutate(trip16.1999,dummy_pulse=rev(seq(1:nrow(trip16.1999))))

mixtures<-bind_rows(mixtures,trip16.1999)

# September trip 17

age0.1999<-filter(length,age == 0 & year == 1999 & month == 9 & trip == 17)

# check histogram
qplot(mmSL,data=age0.1999,binwidth=5)

# min and max sl
summarise(age0.1999,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1999<-select(age0.1999,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1999, breaks=c(0,seq(35,75,5),81),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(30,65),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

par<-groupstats(age0group)

# store data
head(age0.1999)
trip17.1998<-bind_cols(par)%>%
  mutate(year=1999,month=9,day=7,trip=17)
trip17.1998<-mutate(trip17.1998,dummy_pulse=rev(seq(1:nrow(trip17.1998))))

mixtures<-bind_rows(mixtures,trip17.1998)

# September/October trip 18
age0.1999<-filter(length,age == 0 & year == 1999 & trip == 18)

# check histogram
qplot(mmSL,data=age0.1999,binwidth=5)

# min and max sl
summarise(age0.1999,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1999<-select(age0.1999,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1999, breaks=c(0,seq(45,90,5),95),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(55,75,95),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
head(age0.1999)
trip18.1998<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1999,month=9,day=29,trip=18)
trip18.1998<-mutate(trip18.1998,dummy_pulse=rev(seq(1:nrow(trip18.1998))))

mixtures<-bind_rows(mixtures,trip18.1998)

# October
age0.1999<-filter(length,year==1999 & age ==0 & month == 10 & trip == 19)

# check histogram
qplot(mmSL,data=age0.1999,binwidth=5)

# min and max sl
summarise(age0.1999,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1999<-select(age0.1999,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1999, breaks=c(0,seq(45,95,5),101),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(60,80),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
head(age0.1999)
trip19.1999<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1999,month=10,day=12,trip=19)
trip19.1999<-mutate(trip19.1999,dummy_pulse=rev(seq(1:nrow(trip19.1999))))

mixtures<-bind_rows(mixtures,trip19.1999)

# October trip 20
age0.1999<-filter(length,year==1999 & age ==0 & month == 10 & trip == 20)

# check histogram
qplot(mmSL,data=age0.1999,binwidth=5)

# min and max sl
summarise(age0.1999,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1999<-select(age0.1999,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1999, breaks=c(0,seq(40,110,5),112),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(40,65,90),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
head(age0.1999)
trip20.1999<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1999,month=10,day=25,trip=20)
trip20.1999<-mutate(trip20.1999,dummy_pulse=rev(seq(1:nrow(trip20.1999))))

mixtures<-bind_rows(mixtures,trip20.1999)

# November
age0.1999<-filter(length,year==1999 & age ==0 & month == 11 & trip ==21)

# check histogram
qplot(mmSL,data=age0.1999,binwidth=5)

# min and max sl
summarise(age0.1999,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1999<-select(age0.1999,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1999, breaks=c(0,seq(45,95,5),101),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(45,68,95),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
head(age0.1999)
trip21.1999<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1999,month=11,day=7,trip=21)
trip21.1999<-mutate(trip21.1999,dummy_pulse=rev(seq(1:nrow(trip21.1999))))

mixtures<-bind_rows(mixtures,trip21.1999)

# November trip 22
age0.1999<-filter(length,year==1999 & age ==0 & month == 11 & trip ==22)

# check histogram
qplot(mmSL,data=age0.1999,binwidth=5)

# min and max sl
summarise(age0.1999,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1999<-select(age0.1999,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1999, breaks=c(0,seq(45,100,5),106),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(50,68,100),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
head(age0.1999)
trip22.1999<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1999,month=11,day=21,trip=22)
trip22.1999<-mutate(trip22.1999,dummy_pulse=rev(seq(1:nrow(trip22.1999))))

mixtures<-bind_rows(mixtures,trip22.1999)

# December
age0.1999<-filter(length,age==0 & year == 1999 & month ==12)

# ---- age 0 2000 ----

# May
age0.2000<-filter(length,year == 2000 & age == 0 & month == 5)

# no May fish

# July
age0.2000<-filter(length,year == 2000 & age == 0 & month == 7)

# August
age0.2000<-filter(length,year == 2000 & age == 0 & trip ==16)
# trip 16

qplot(mmSL, data=age0.2000, binwidth=5)

# # detremine max and min SL
summarise(age0.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age0.2000,mmSL)

# convert to frequency table
age0group<-mixgroup(group2000, breaks = c(0,seq(45,65,5),68),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# Go with fit 3, not great but better than nothing!
head(age0.2000)
trip16.2000<-bind_cols(par)%>%
  mutate(year=2000,month=8,day=28,trip=16)
trip16.2000<-mutate(trip16.2000,dummy_pulse=rev(seq(1:nrow(trip16.2000))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip16.2000)


# September
age0.2000<-filter(length,year == 2000 & age == 0 & month == 9 & trip == 17)

qplot(mmSL, data=age0.2000, binwidth=5)

# # detremine max and min SL
summarise(age0.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age0.2000,mmSL)

# convert to frequency table
age0group<-mixgroup(group2000, breaks = c(0,seq(40,75,5),79),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(35,50,65),c(4),pi=NULL)
plot(age0group,age0param,"gamma")


# fit mixture
fit1<-mix(age0group,age0param,dist="gamma", mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
#single pulse
par<-groupstats(age0group)


# Store fit 1 data
head(age0.2000)
trip17<-bind_cols(par)%>%
  mutate(year=2000,month=9,day=12,trip=17)
trip17<-mutate(trip17,dummy_pulse=rev(seq(1:nrow(trip17))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip17)

# Setpember trip 18
age0.2000<-filter(length,year==2000,age==0  & trip == 18)

qplot(mmSL, data=age0.2000, binwidth=5)

# # detremine max and min SL
summarise(age0.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age0.2000,mmSL)

# convert to frequency table
age0group<-mixgroup(group2000, breaks = c(0,seq(50,85,5),90),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(55,70),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma", mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)

# store results
head(age0.2000)
trip18.2000<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=9,day=27, trip=18)
trip18.2000<-mutate(trip18.2000,dummy_pulse=rev(seq(1:nrow(trip18.2000))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip18.2000)

# October trip 19
age0.2000<-filter(length,year==2000,age==0 & month ==10 & trip ==19)

qplot(mmSL, data=age0.2000, binwidth=5)

# # detremine max and min SL
summarise(age0.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age0.2000,mmSL)

# convert to frequency table
age0group<-mixgroup(group2000, breaks = c(0,seq(30,90,5),93),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,70),c(3),pi=NULL)
plot(age0group,age0param,"gamma")


# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)

# Fit 1
head(age1.2000)
trip19.2000<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=10,day=10,trip=19)
trip19.2000<-mutate(trip19.2000,dummy_pulse=rev(seq(1:nrow(trip19.2000))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip19.2000)

# October trip 20
age0.2000<-filter(length,year==2000 & age == 0 & month == 10 & trip == 20)

qplot(mmSL, data=age0.2000, binwidth=5)

# # detremine max and min SL
summarise(age0.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age0.2000,mmSL)

# convert to frequency table
age0group<-mixgroup(group2000, breaks = c(0,seq(45,95,5),100),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(55,90),c(3),pi=NULL)
plot(age0group,age0param,"gamma")


# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)

# Fit 1
# store fit 1 results
head(age0.2000)
trip20.2000<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=10,day=24,trip=20) # Oct 10
trip20.2000<-mutate(trip20.2000,dummy_pulse=rev(seq(1:nrow(trip20.2000))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip20.2000)

# November trip 21
age0.2000<-filter(length,year==2000 & age == 0 & month == 11 & trip == 21)

qplot(mmSL, data=age0.2000, binwidth=5)

# # detremine max and min SL
summarise(age0.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age0.2000,mmSL)

# convert to frequency table
age0group<-mixgroup(group2000, breaks = c(0,seq(45,100,5),104),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,70,85),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1,root=T)

# store results
head(age0.2000)
trip21.2000<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=11,day=13,trip=21) # Oct 10
trip21.2000<-mutate(trip21.2000,dummy_pulse=rev(seq(1:nrow(trip21.2000))))
#add to mixtures
mixtures<-bind_rows(mixtures,trip21.2000)

# November trip 22
age0.2000<-filter(length,year==2000 & age ==0 & month == 11 & trip==22)

qplot(mmSL, data=age0.2000, binwidth=5)

# # detremine max and min SL
summarise(age0.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age0.2000,mmSL)

# convert to frequency table
age0group<-mixgroup(group2000, breaks = c(0,seq(35,100,5),104),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,70,90),c(6),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# Store data
head(age0.2000)
trip22.2000<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=11,day=26,trip=22) # Nov 13
trip22.2000<-mutate(trip22.2000,dummy_pulse=rev(seq(1:nrow(trip22.2000))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip22.2000)

# December
age0.2000<-filter(length,year==2000 & age == 0 & month ==12)

# ---- age 0 2001 ----

# May
age0.2001<-filter(length,year == 2001 & age == 0 & month == 5)

# no fish in May

# July
age0.2001<-filter(length,year==2001 & age ==0 & month ==7)

# August trip 16
age0.2001<-filter(length,year==2001 & age ==0 & month == 8 & trip ==16)

qplot(mmSL, data=age0.2001, binwidth=5)

# # detremine max and min SL
summarise(age0.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age0.2001,mmSL)

# convert to frequency table
age0group<-mixgroup(group2001, breaks = c(0,seq(45,65,5),68),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)


#Store results
head(age0.2001)
trip16.2001<-bind_cols(par)%>%
  mutate(year=2001,month=8,day=20, trip=16)
trip16.2001<-mutate(trip16.2001,dummy_pulse=rev(seq(1:nrow(trip16.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip16.2001)

# September trip 17
age0.2001<-filter(length,year==2001 & age ==0 & trip ==17)

qplot(mmSL, data=age0.2001, binwidth=5)

# # detremine max and min SL
summarise(age0.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age0.2001,mmSL)

# convert to frequency table
age0group<-mixgroup(group2001, breaks = c(0,seq(45,70,5),75),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,63),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)


# fit 1. store data
head(age0.2001)
trip17.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=9,day=4,trip=17)
trip17.2001<-mutate(trip17.2001,dummy_pulse=rev(seq(1:nrow(trip17.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip17.2001)

# September trip 18
age0.2001<-filter(length,year==2001 & age ==0 & trip ==18)

qplot(mmSL, data=age0.2001, binwidth=5)

# # detremine max and min SL
summarise(age0.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age0.2001,mmSL)

# convert to frequency table
age0group<-mixgroup(group2001, breaks = c(0,seq(50,75,5),81),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,68),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1. store data
head(age0.2001)
trip18.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=9,day=15,trip=18)
trip18.2001<-mutate(trip18.2001,dummy_pulse=rev(seq(1:nrow(trip18.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip18.2001)


# October # trip 19
age0.2001<-filter(length,year==2001 & age ==0 & trip==19)

qplot(mmSL, data=age0.2001, binwidth=5)

# # detremine max and min SL
summarise(age0.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age0.2001,mmSL)

# convert to frequency table
age0group<-mixgroup(group2001, breaks = c(0,seq(45,85,5),87),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,75),c(7),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1,root=T)


# store results
head(age0.2001)
trip19.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=10,day=2,trip=19)
trip19.2001<-mutate(trip19.2001,dummy_pulse=rev(seq(1:nrow(trip19.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip19.2001)

# October trip 20
age0.2001<-filter(length,year==2001 & age ==0 & trip==20)

qplot(mmSL, data=age0.2001, binwidth=5)

# # detremine max and min SL
summarise(age0.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age0.2001,mmSL)

# convert to frequency table
age0group<-mixgroup(group2001, breaks = c(0,seq(50,105,5),110),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(55,90),c(7),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1,root=T)


# store results
head(age0.2001)
trip20.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=10,day=16,trip=20)
trip20.2001<-mutate(trip20.2001,dummy_pulse=rev(seq(1:nrow(trip20.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip20.2001)

# November
age0.2001<-filter(length,year==2001 & age ==0 & trip == 21)
# Trip 21

qplot(mmSL, data=age0.2001, binwidth=5)

# # detremine max and min SL
summarise(age0.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age0.2001,mmSL)

# convert to frequency table
age0group<-mixgroup(group2001, breaks = c(0,seq(45,105,5),110),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,70,100),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)



# store results
head(age0.2001)
trip21.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=11,day=1,trip=21) 
trip21.2001<-mutate(trip21.2001,dummy_pulse=rev(seq(1:nrow(trip21.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip21.2001)

# November # Trip 22

age0.2001<-filter(length,year==2001 & age ==0 & month ==11 & trip == 22)


qplot(mmSL, data=age0.2001, binwidth=5)

# # detremine max and min SL
summarise(age0.2001,min(mmSL),max(mmSL))

# create dataframe with SL only
group2001<-select(age0.2001,mmSL)

# convert to frequency table
age0group<-mixgroup(group2001, breaks = c(0,seq(50,110,5),112),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,70,105),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# fit1..
# poor model fit
head(age0.2001)
trip22.2001<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=11,day=15,trip=22) # november 15
trip22.2001<-mutate(trip22.2001,dummy_pulse=rev(seq(1:nrow(trip22.2001))))

#add to mixtures
mixtures<-bind_rows(mixtures,trip22.2001)

# December
age0.2001<-filter(length,year==2001 & age == 0 & month ==12)

# ---- age 0 2002 ----

# May
age0.2002<-filter(length,year==2002,age==0,month==5)

# July
age0.2002<-filter(length,year == 2002 & age == 0 & month ==7)
# not enough

# August
age0.2002<-filter(length,year==2002 & age == 0 & month == 8 & trip == 16)

qplot(mmSL, data=age0.2002, binwidth=5)

# # detremine max and min SL
summarise(age0.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age0.2002,mmSL)

# convert to frequency table
age0group<-mixgroup(group2002, breaks = c(0,seq(45,55,5),61),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# store fit 1 results
head(age0.2002)
trip16.2002<-bind_cols(par)%>%
  mutate(year=2002,month=8,day=22,trip=16)
trip16.2002<-mutate(trip16.2002,dummy_pulse=rev(seq(1:nrow(trip16.2002))))
# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2002)

# September
age0.2002<-filter(length,year==2002 &age==0 & month==9 & trip == 17)

# check histogram
qplot(mmSL, data=age0.2002, binwidth=5)

# # detremine max and min SL
summarise(age0.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age0.2002,mmSL)

# convert to frequency table
age0group<-mixgroup(group2002, breaks = c(0,seq(40,70,5),74),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# single pulse
par <- groupstats(age0group)

#store results 
head(age0.2002)

trip17.2002<-bind_cols(par)%>%
  mutate(year=2002,month=9,day=6,trip=17)
trip17.2002<-mutate(trip17.2002,dummy_pulse=rev(seq(1:nrow(trip17.2002))))
# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2002)

# September trip 18
age0.2002<-filter(length,year==2002 & age==0 & month==9 & trip==18)

# check histogram
qplot(mmSL, data=age0.2002, binwidth=5)

# # detremine max and min SL
summarise(age0.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age0.2002,mmSL)

# convert to frequency table
age0group<-mixgroup(group2002, breaks = c(0,seq(45,75,5),77),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# single pulse
par<-groupstats(age0group)

# store results
head(age0.2002)
trip18.2002<-bind_cols(par)%>%
  mutate(year=2002,month=9,day=20,trip=18)
trip18.2002<-mutate(trip18.2002,dummy_pulse=rev(seq(1:nrow(trip18.2002))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2002)

# October
age0.2002<-filter(length,year==2002 & age ==0 & month ==10 & trip ==19)

# check histogram
qplot(mmSL, data=age0.2002, binwidth=5)

# # detremine max and min SL
summarise(age0.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age0.2002,mmSL)

# convert to frequency table
age0group<-mixgroup(group2002, breaks = c(0,seq(45,80,5),82),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# single pulse
par<-groupstats(age0group)

# store results
head(age0.2002)
trip19.2002<-bind_cols(par)%>%
  mutate(year=2002,month=10,day=5,trip=19)
trip19.2002<-mutate(trip19.2002,dummy_pulse=rev(seq(1:nrow(trip19.2002))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2002)

# October trip 20
age0.2002<-filter(length,year==2002 & age ==0 & month ==10 & trip ==20)

# check histogram
qplot(mmSL, data=age0.2002, binwidth=5)

# # detremine max and min SL
summarise(age0.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age0.2002,mmSL)

# convert to frequency table
age0group<-mixgroup(group2002, breaks = c(0,seq(55,85,5),90),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,75),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# still single pulse
par<-groupstats(age0group)

# store results
head(age0.2002)
trip20.2002<-bind_cols(par)%>%
  mutate(year=2002,month=10,day=20,trip=20)
trip20.2002<-mutate(trip20.2002,dummy_pulse=rev(seq(1:nrow(trip20.2002))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2002)


# November
age0.2002<-filter(length,year==2002 & age == 0 & month == 11 & trip ==21)
# check histogram
qplot(mmSL, data=age0.2002, binwidth=5)

# # detremine max and min SL
summarise(age0.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age0.2002,mmSL)

# convert to frequency table
age0group<-mixgroup(group2002, breaks = c(0,seq(45,85,5),90),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,73),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age0.2002)
trip21.2002<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2002,month=11,day=3,trip=21)
trip21.2002<-mutate(trip21.2002,dummy_pulse=rev(seq(1:nrow(trip21.2002))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2002)

# November trip 22
age0.2002<-filter(length,year==2002 & age == 0 & month == 11 & trip ==22)

# check histogram
qplot(mmSL, data=age0.2002, binwidth=5)

# # detremine max and min SL
summarise(age0.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age0.2002,mmSL)

# convert to frequency table
age0group<-mixgroup(group2002, breaks = c(0,seq(40,90,5),95),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,55,75),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age0.2002)
trip22.2002<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2002,month=11,day=20,trip=22)
trip22.2002<-mutate(trip22.2002,dummy_pulse=rev(seq(1:nrow(trip22.2002))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip22.2002)

# December
age0.2002<-filter(length,year==2002 & age == 0 & month == 12)
# no fish

# ---- age 0 2003 ----

# May
age0.2003<-filter(length,year==2003,age==0,month==5)

# July
age0.2003<-filter(length,year==2003 & age == 0 & month == 7)

# August
age0.2003<-filter(length,year==2003 & age ==0 & month == 8 & trip == 16)

# check histogram
qplot(mmSL, data=age0.2003, binwidth=5)

# # detremine max and min SL
summarise(age0.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age0.2003,mmSL)

# convert to frequency table
age0group<-mixgroup(group2003, breaks = c(0,seq(45,65,5),68),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# single pulse
par<-groupstats(age0group)

# store results.
head(age0.2003)
trip16.2003<-bind_cols(par)%>%
  mutate(year=2003,month=8,day=27,trip=16)
trip16.2003<-mutate(trip16.2003,dummy_pulse=rev(seq(1:nrow(trip16.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2003)

# September
age0.2003<-filter(length,year==2003 & age==0 & month==9 & trip==17)

# check histogram
qplot(mmSL, data=age0.2003, binwidth=5)

# # detremine max and min SL
summarise(age0.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age0.2003,mmSL)

# convert to frequency table
age0group<-mixgroup(group2003, breaks = c(0,seq(35,70,5),73),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# single pulse
par<-groupstats(age0group)

#store results
head(age0.2003)
trip17.2003<-bind_cols(par)%>%
  mutate(year=2003,month=9,day=9,trip=17)
trip17.2003<-mutate(trip17.2003,dummy_pulse=rev(seq(1:nrow(trip17.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2003)

# September trip 18
age0.2003<-filter(length,year==2003 & age==0 & month==9 & trip ==18)

# check histogram
qplot(mmSL, data=age0.2003, binwidth=5)

# # detremine max and min SL
summarise(age0.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age0.2003,mmSL)

# convert to frequency table
age0group<-mixgroup(group2003, breaks = c(0,seq(55,70,5),75),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(53,74),c(7),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# sigma can't be estimated
# store results
head(age0.2003)
trip18.2003<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=9,day=25,trip=18)
trip18.2003<-mutate(trip18.2003,dummy_pulse=rev(seq(1:nrow(trip18.2003))))

mixtures<-bind_rows(mixtures,trip18.2003)

# October trip 19
age0.2003<-filter(length,year==2003 & age == 0 & month == 10 & trip ==19)

# check histogram
qplot(mmSL, data=age0.2003, binwidth=5)

# # detremine max and min SL
summarise(age0.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age0.2003,mmSL)

# convert to frequency table
age0group<-mixgroup(group2003, breaks = c(0,seq(45,85,5),89),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,60,82),c(4),pi=NULL)
plot(age0group,age0param)

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


#Store results
head(age0.2003)
trip19.2003<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=10,day=9,trip=19) 
trip19.2003<-mutate(trip19.2003,dummy_pulse=rev(seq(1:nrow(trip19.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2003)

# October trip 20
age0.2003<-filter(length,year==2003 & age ==0 & month ==10 & trip ==20)

# check histogram
qplot(mmSL, data=age0.2003, binwidth=5)

# # detremine max and min SL
summarise(age0.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age0.2003,mmSL)

# convert to frequency table
age0group<-mixgroup(group2003, breaks = c(0,seq(40,95,5),97),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,70,90),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


#Fit 1
head(age0.2003)
trip20.2003<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=10,day=24,trip=20) # sept 9
trip20.2003<-mutate(trip20.2003,dummy_pulse=rev(seq(1:nrow(trip20.2003))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2003)

# November
age0.2003<-filter(length,year==2003 & age ==0 & month ==11 & trip == 22)
# not enough november fish

# ---- age 0 2004 ----

# May 
age0.2004<-filter(length,year==2004 & age == 0 & month ==5)

# no fish

# July
age0.2004<-filter(length,year==2004 & age ==0 & month == 7)

# not enough fish

# August trip 16
age0.2004<-filter(length,year==2004 & age == 0 & trip ==16)

# check histogram
qplot(mmSL, data=age0.2004, binwidth=5)

# # detremine max and min SL
summarise(age0.2004,min(mmSL),max(mmSL))

# create dataframe with SL only
group2004<-select(age0.2004,mmSL)

# convert to frequency table
age0group<-mixgroup(group2004, breaks = c(0,seq(50,70,5),76),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# Store results
head(age0.2004)
trip16.2004<-bind_cols(par)%>%
  mutate(year=2004,month=8,day=30,trip=16) # halfway between trip 13 and 14
trip16.2004<-mutate(trip16.2004,dummy_pulse=rev(seq(1:nrow(trip16.2004))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2004)

# September trip 17
age0.2004<-filter(length,year==2004 & age ==0 & month ==9 & trip ==17)

# check histogram
qplot(mmSL, data=age0.2004, binwidth=5)

# # detremine max and min SL
summarise(age0.2004,min(mmSL),max(mmSL))

# create dataframe with SL only
group2004<-select(age0.2004,mmSL)

# convert to frequency table
age0group<-mixgroup(group2004, breaks = c(0,seq(50,80,5),84),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,75),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age0.2004)
trip17.2004<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2004,month=9,day=13,trip=17)
trip17.2004<-mutate(trip17.2004,dummy_pulse=rev(seq(1:nrow(trip17.2004))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2004)

# October trip 19
age0.2004<-filter(length,year==2004 & age ==0 & trip ==19)

# check histogram
qplot(mmSL, data=age0.2004, binwidth=5)

# # detremine max and min SL
summarise(age0.2004,min(mmSL),max(mmSL))

# create dataframe with SL only
group2004<-select(age0.2004,mmSL)

# convert to frequency table
age0group<-mixgroup(group2004, breaks = c(0,seq(45,110,5),115),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,65,115),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age0.2004)
trip19.2004<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2004,month=10,day=13,trip=19)
trip19.2004<-mutate(trip19.2004,dummy_pulse=rev(seq(1:nrow(trip19.2004))))

mixtures<-bind_rows(mixtures,trip19.2004)

# October trip 20
age0.2004<-filter(length,year==2004 & age ==0 & month ==10 & trip ==20)
# check histogram
qplot(mmSL, data=age0.2004, binwidth=5)

# # detremine max and min SL
summarise(age0.2004,min(mmSL),max(mmSL))

# create dataframe with SL only
group2004<-select(age0.2004,mmSL)

# convert to frequency table
age0group<-mixgroup(group2004, breaks = c(0,seq(45,115,5),121),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,120),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)
# store model results
head(age0.2004)
trip20.2004<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2004,month=10,day=25,trip=20)
trip20.2004<-mutate(trip20.2004,dummy_pulse=rev(seq(1:nrow(trip20.2004))))

mixtures<-bind_rows(mixtures,trip20.2004)

# November trip 21
age0.2004<-filter(length,year==2004 & age ==0 & month ==11)
# not enough data

# ---- age 0 2005 ----
# May

age0.2005<-filter(length,year==2005 & age == 0 & month == 5)

#July
age0.2005<-filter(length,year==2005 & age == 0 & month == 7)

# August trip 15
age0.2005<-filter(length,year==2005 & age==0 & month ==8 & trip ==15)

# check histogram
qplot(mmSL, data=age0.2005, binwidth=5)

# # detremine max and min SL
summarise(age0.2005,min(mmSL),max(mmSL))

# create dataframe with SL only
group2005<-select(age0.2005,mmSL)

# convert to frequency table
age0group<-mixgroup(group2005, breaks = c(0,seq(45,55,5),62),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# store model results
head(age0.2005)
trip15.2005<-bind_cols(par)%>%
  mutate(year=2005,month=8,day=8,trip=15)
trip15.2005<-mutate(trip15.2005,dummy_pulse=rev(seq(1:nrow(trip15.2005))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.2005)

# August trip 16
age0.2005<-filter(length,year==2005 & age==0 & month ==8 & trip==16)
# use first week only

# check histogram
qplot(mmSL, data=age0.2005, binwidth=5)

# # detremine max and min SL
summarise(age0.2005,min(mmSL),max(mmSL))

# create dataframe with SL only
group2005<-select(age0.2005,mmSL)

# convert to frequency table
age0group<-mixgroup(group2005, breaks = c(0,seq(45,70,5),74),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# Store results 
head(age0.2005)
trip16.2005<-bind_cols(par)%>%
  mutate(year=2005,month=8,day=22,trip=16)
trip16.2005<-mutate(trip16.2005,dummy_pulse=rev(seq(1:nrow(trip16.2005))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2005)

# September trip 17
age0.2005<-filter(length,year==2005 & age==0 & month ==9 & trip == 17)
# check histogram
qplot(mmSL, data=age0.2005, binwidth=5)

# # detremine max and min SL
summarise(age0.2005,min(mmSL),max(mmSL))

# create dataframe with SL only
group2005<-select(age0.2005,mmSL)

# convert to frequency table
age0group<-mixgroup(group2005, breaks = c(0,seq(50,85,5),90),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,75),c(5),pi=NULL)
plot(age0group,age0param,"gamma")
# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# Store results 
head(age0.2005)
trip17.2005<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=9,day=6,trip=17)
trip17.2005<-mutate(trip17.2005,dummy_pulse=rev(seq(1:nrow(trip17.2005))))

mixtures<-bind_rows(mixtures,trip17.2005)

# September trip 18
age0.2005<-filter(length,year==2005 & age==0 & month ==9 & trip ==18)

# check histogram
qplot(mmSL, data=age0.2005, binwidth=5)

# # detremine max and min SL
summarise(age0.2005,min(mmSL),max(mmSL))

# create dataframe with SL only
group2005<-select(age0.2005,mmSL)

# convert to frequency table
age0group<-mixgroup(group2005, breaks = c(0,seq(50,105,5),110),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,95),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

#Store results
head(age0.2005)
trip18.2005<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=9,day=19,trip=18)
trip18.2005<-mutate(trip18.2005,dummy_pulse=rev(seq(1:nrow(trip18.2005))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2005)

# October trip 19
age0.2005<-filter(length, year==2005 & age ==0 & trip ==19)

# check histogram
qplot(mmSL, data=age0.2005, binwidth=5)

# # detremine max and min SL
summarise(age0.2005,min(mmSL),max(mmSL))

# create dataframe with SL only
group2005<-select(age0.2005,mmSL)

# convert to frequency table
age0group<-mixgroup(group2005, breaks = c(0,seq(45,105,5),109),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(58,100),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1 model results
head(age0.2005)
trip19.2005<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=10,day=4,trip=19)
trip19.2005<-mutate(trip19.2005,dummy_pulse=rev(seq(1:nrow(trip19.2005))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2005)

# October trip 20
age0.2005<-filter(length,year==2005 & age ==0 & month ==10 & trip ==20)
# not enough data

# November
age0.2005<-filter(length,year==2005 & age == 0 & month == 11)
# not enough fish

# ---- age 0 2006 ----

# May
age0.2006<-filter(length,year==2006 & age == 0 & month ==5)

# no fish
# July trip14
age0.2006<-filter(length,year==2006 & age ==0 & month ==7)

# check histogram
qplot(mmSL, data=age0.2006, binwidth=5)

# # detremine max and min SL
summarise(age0.2006,min(mmSL),max(mmSL))

# create dataframe with SL only
group2006<-select(age0.2006,mmSL)

# convert to frequency table
age0group<-mixgroup(group2006, breaks = c(0,seq(45,50,5),55),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# Fit 1
head(age0.2006)
trip14.2006<-bind_cols(par)%>%
  mutate(year=2006,month=7,day=26, trip=14)
trip14.2006<-mutate(trip14.2006,dummy_pulse=rev(seq(1:nrow(trip14.2006))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2006)

# August
age0.2006<-filter(length,year==2006 & age ==0,month==8 & trip==15)
# check histogram
qplot(mmSL, data=age0.2006, binwidth=5)

# # detremine max and min SL
summarise(age0.2006,min(mmSL),max(mmSL))

# create dataframe with SL only
group2006<-select(age0.2006,mmSL)

# convert to frequency table
age0group<-mixgroup(group2006, breaks = c(0,seq(45,65,5),72),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# Fit 1
head(age0.2006)
trip15.2006<-bind_cols(par)%>%
  mutate(year=2006,month=8,day=8, trip=15)
trip15.2006<-mutate(trip15.2006,dummy_pulse=rev(seq(1:nrow(trip15.2006))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.2006)

# August trip 16
age0.2006<-filter(length,year==2006 & age ==0,month==8 & trip ==16)

# check histogram
qplot(mmSL, data=age0.2006, binwidth=5)

# # detremine max and min SL
summarise(age0.2006,min(mmSL),max(mmSL))

# create dataframe with SL only
group2006<-select(age0.2006,mmSL)

# convert to frequency table
age0group<-mixgroup(group2006, breaks = c(0,seq(40,80,5),83),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,75),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# Fit 1
head(age0.2006)
trip16.2006<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2006,month=8,day=22, trip=16)
trip16.2006<-mutate(trip16.2006,dummy_pulse=rev(seq(1:nrow(trip16.2006))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2006)

# September trip 17
age0.2006<-filter(length,year==2006 & age ==0,month==9)
# not enough fish

# October
age0.2006<-filter(length,year==2006 & age ==0,month==10)
# not enough fish

# November
age0.2006<-filter(length,year==2006 & age ==0,month==11 & trip ==22)
# not enough fish

# ---- age 0 2007 ----

# May
age0.2007<-filter(length,year==2007 & age == 0 & month == 5)
#July
age0.2007<-filter(length,year==2007 & age == 0 & month == 7)

#August trip 15
age0.2007<-filter(length,year==2007 & age == 0 & trip==15)

# check histogram
qplot(mmSL, data=age0.2007, binwidth=5)

# # detremine max and min SL
summarise(age0.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age0.2007,mmSL)

# convert to frequency table
age0group<-mixgroup(group2007, breaks = c(0,seq(35,50,5),58),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# store restuls
head(age0.2007)
trip15.2007<-bind_cols(par)%>%
  mutate(year=2007,month=8,day=13,trip=15)
trip15.2007<-mutate(trip15.2007,dummy_pulse=rev(seq(1:nrow(trip15.2007))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.2007)

# August trip 16
age0.2007<-filter(length,year==2007 & age == 0 & month == 8 & trip ==16)

# check histogram
qplot(mmSL, data=age0.2007, binwidth=5)

# # detremine max and min SL
summarise(age0.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age0.2007,mmSL)

# convert to frequency table
age0group<-mixgroup(group2007, breaks = c(0,seq(40,65,5),72),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# store results
head(age0.2007)
trip16.2007<-bind_cols(par)%>%
  mutate(year=2007,month=8,day=27,trip = 16)
trip16.2007<-mutate(trip16.2007,dummy_pulse=rev(seq(1:nrow(trip16.2007))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2007)

# September # trip 17
age0.2007<-filter(length,year==2007 & age ==0 & month ==9 & trip == 17)

# check histogram
qplot(mmSL, data=age0.2007, binwidth=5)

# # detremine max and min SL
summarise(age0.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age0.2007,mmSL)

# convert to frequency table
age0group<-mixgroup(group2007, breaks = c(0,seq(35,75,5),82),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,65),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1, root=T)

# store results
head(age0.2007)
trip17.2007<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2007,month=9,day=11,trip = 17)
trip17.2007<-mutate(trip17.2007,dummy_pulse=rev(seq(1:nrow(trip17.2007))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2007)

# September # trip 18
age0.2007<-filter(length,year==2007 & age ==0 & month ==9 & trip == 18)

# check histogram
qplot(mmSL, data=age0.2007, binwidth=5)

# # detremine max and min SL
summarise(age0.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age0.2007,mmSL)

# convert to frequency table
age0group<-mixgroup(group2007, breaks = c(0,seq(35,75,5),82),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(48,68),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1, root=T)

# store results
head(age0.2007)
trip18.2007<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2007,month=9,day=24,trip = 18)
trip18.2007<-mutate(trip18.2007,dummy_pulse=rev(seq(1:nrow(trip18.2007))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2007)

# October trip 19
age0.2007<-filter(length,year==2007 & age ==0 & month ==10 & trip ==19)

# check histogram
qplot(mmSL, data=age0.2007, binwidth=5)

# # detremine max and min SL
summarise(age0.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age0.2007,mmSL)

# convert to frequency table
age0group<-mixgroup(group2007, breaks = c(0,seq(30,105,5),111),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(30,55,81),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1, root=T)

# store results
head(age0.2007)
trip19.2007<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2007,month=10,day=9,trip = 19)
trip19.2007<-mutate(trip19.2007,dummy_pulse=rev(seq(1:nrow(trip19.2007))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2007)

# October trip 20
age0.2007<-filter(length,year==2007 & age ==0 & trip==20)
# check histogram
qplot(mmSL, data=age0.2007, binwidth=5)

# # detremine max and min SL
summarise(age0.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age0.2007,mmSL)

# convert to frequency table
age0group<-mixgroup(group2007, breaks = c(0,seq(40,95,5),100),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,65,95),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1, root=T)

# store results
head(age0.2007)
trip20.2007<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2007,month=10,day=22,trip = 20)
trip20.2007<-mutate(trip20.2007,dummy_pulse=rev(seq(1:nrow(trip20.2007))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2007)

# November trip 21
age0.2007<-filter(length,year==2007 & age ==0 & trip==21)

# check histogram
qplot(mmSL, data=age0.2007, binwidth=5)

# # detremine max and min SL
summarise(age0.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age0.2007,mmSL)

# convert to frequency table
age0group<-mixgroup(group2007, breaks = c(0,seq(35,80,5),84),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,70),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1, root=T)

# store results
head(age0.2007)
trip21.2007<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2007,month=11,day=7,trip = 21)
trip21.2007<-mutate(trip21.2007,dummy_pulse=rev(seq(1:nrow(trip21.2007))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2007)

# November trip 22
age0.2007<-filter(length,year==2007 & age ==0 & trip==22)

# check histogram
qplot(mmSL, data=age0.2007, binwidth=5)

# # detremine max and min SL
summarise(age0.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age0.2007,mmSL)

# convert to frequency table
age0group<-mixgroup(group2007, breaks = c(0,seq(40,85,5),90),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,75),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1, root=T)

# store results
head(age0.2007)
trip22.2007<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2007,month=11,day=22,trip = 22)
trip22.2007<-mutate(trip22.2007,dummy_pulse=rev(seq(1:nrow(trip22.2007))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip22.2007)

# ---- age 0 2008 ----
# May
age0.2008<-filter(length,year==2008,age==0,month==5)
#July
age0.2008<-filter(length,year==2008 & age ==0 & trip==13)

# check histogram
qplot(mmSL, data=age0.2008, binwidth=5)

# # detremine max and min SL
summarise(age0.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age0.2008,mmSL)

# convert to frequency table
age0group<-mixgroup(group2008, breaks = c(0,seq(30,45,5),50),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

#store model results
head(age0.2008)
trip13.2008<-bind_cols(par)%>%
  mutate(year=2008,month=7,day=16,trip=13)
trip13.2008<-mutate(trip13.2008,dummy_pulse=rev(seq(1:nrow(trip13.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.2008)

#July trip 14
age0.2008<-filter(length,year==2008 & age ==0 & trip==14)

# check histogram
qplot(mmSL, data=age0.2008, binwidth=5)

# # detremine max and min SL
summarise(age0.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age0.2008,mmSL)

# convert to frequency table
age0group<-mixgroup(group2008, breaks = c(0,seq(40,50,5),55),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

#store model results
head(age0.2008)
trip14.2008<-bind_cols(par)%>%
  mutate(year=2008,month=7,day=29,trip=14)
trip14.2008<-mutate(trip14.2008,dummy_pulse=rev(seq(1:nrow(trip14.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2008)

#August trip 15
age0.2008<-filter(length,year==2008 & age ==0 & trip ==15)

# check histogram
qplot(mmSL, data=age0.2008, binwidth=5)

# # detremine max and min SL
summarise(age0.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age0.2008,mmSL)

# convert to frequency table
age0group<-mixgroup(group2008, breaks = c(0,seq(45,65,5),72),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)


#store model results
head(age0.2008)
trip15.2008<-bind_cols(par)%>%
  mutate(year=2008,month=8,day=14,trip=15)
trip15.2008<-mutate(trip15.2008,dummy_pulse=rev(seq(1:nrow(trip15.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.2008)

# August trip 16
age0.2008<-filter(length,year==2008 & age == 0 & trip==16)

# check histogram
qplot(mmSL, data=age0.2008, binwidth=5)

# # detremine max and min SL
summarise(age0.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age0.2008,mmSL)

# convert to frequency table
age0group<-mixgroup(group2008, breaks = c(0,seq(40,75,5),79),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,70),c(6),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2008)
trip16.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=8,day=28,trip=16)
trip16.2008<-mutate(trip16.2008,dummy_pulse=rev(seq(1:nrow(trip16.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2008)

# September trip 17
age0.2008<-filter(length,year==2008 & age == 0 & month == 9 & trip == 17)

# check histogram
qplot(mmSL, data=age0.2008, binwidth=5)

# # detremine max and min SL
summarise(age0.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age0.2008,mmSL)

# convert to frequency table
age0group<-mixgroup(group2008, breaks = c(0,seq(40,85,5),87),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,62,80),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2008)
trip17.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=9,day=15,trip=17)
trip17.2008<-mutate(trip17.2008,dummy_pulse=rev(seq(1:nrow(trip17.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2008)

# September trip 18
age0.2008<-filter(length, year==2008 & age ==0 & trip ==18)

# check histogram
qplot(mmSL, data=age0.2008, binwidth=5)

# # detremine max and min SL
summarise(age0.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age0.2008,mmSL)

# convert to frequency table
age0group<-mixgroup(group2008, breaks = c(0,seq(45,90,5),96),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,55,85),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1 results
head(age0.2008)
trip18.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=9,day=29,trip=18)
trip18.2008<-mutate(trip18.2008,dummy_pulse=rev(seq(1:nrow(trip18.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2008)

# October trip 19
age0.2008<-filter(length,year==2008 & age==0 & trip==19)

# check histogram
qplot(mmSL, data=age0.2008, binwidth=5)

# # detremine max and min SL
summarise(age0.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age0.2008,mmSL)

# convert to frequency table
age0group<-mixgroup(group2008, breaks = c(0,seq(40,90,5),96),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(55,95),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1 results
head(age0.2008)
trip19.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=10,day=14,trip=19)
trip19.2008<-mutate(trip19.2008,dummy_pulse=rev(seq(1:nrow(trip19.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2008)

# October trip 20
age0.2008 <- filter(length, year==2008 & age == 0 &trip ==20)

qplot(mmSL, data=age0.2008, binwidth=5)

# # detremine max and min SL
summarise(age0.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age0.2008,mmSL)

# convert to frequency table
age0group<-mixgroup(group2008, breaks = c(0,seq(30,110,5),115),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,70,90),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1 results
head(age0.2008)
trip20.2008<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=10,day=27,trip=20)
trip20.2008<-mutate(trip20.2008,dummy_pulse=rev(seq(1:nrow(trip20.2008))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2008)
# November trip 21
age0.2008 <-filter(length,year==2008 & age ==0 & trip==21)

# notenough data

# ---- age 0 2009 ----

# May
age0.2009<-filter(length,year==2009 & age == 0& month == 5)
# July trip 14
age0.2009<-filter(length,year==2009 & age == 0 & month ==7)

# check histogram
qplot(mmSL, data=age0.2009, binwidth=5)

# # detremine max and min SL
summarise(age0.2009,min(mmSL),max(mmSL))

# create dataframe with SL only
group2009<-select(age0.2009,mmSL)

# convert to frequency table
age0group<-mixgroup(group2009, breaks = c(0,seq(35,40,5),46),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# store model results
head(age0.2009)
trip14.2009<-bind_cols(par)%>%
  mutate(year=2009,month=7,day=21,trip=14)
trip14.2009<-mutate(trip14.2009,dummy_pulse=rev(seq(1:nrow(trip14.2009))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2009)

# August trip 15
age0.2009<-filter(length,year==2009 & age == 0 & trip ==15)

# check histogram
qplot(mmSL, data=age0.2009, binwidth=5)

# # detremine max and min SL
summarise(age0.2009,min(mmSL),max(mmSL))

# create dataframe with SL only
group2009<-select(age0.2009,mmSL)

# convert to frequency table
age0group<-mixgroup(group2009, breaks = c(0,seq(40,50,5),57),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# store model results
head(age0.2009)
trip15.2009<-bind_cols(par)%>%
  mutate(year=2009,month=8,day=14,trip=15)
trip15.2009<-mutate(trip15.2009,dummy_pulse=rev(seq(1:nrow(trip15.2009))))

mixtures<-bind_rows(mixtures,trip15.2009)

# August trip 16
age0.2009<-filter(length,year==2009 & age == 0 & trip ==16)

# check histogram
qplot(mmSL, data=age0.2009, binwidth=5)

# # detremine max and min SL
summarise(age0.2009,min(mmSL),max(mmSL))

# create dataframe with SL only
group2009<-select(age0.2009,mmSL)

# convert to frequency table
age0group<-mixgroup(group2009, breaks = c(0,seq(40,50,5),57),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# store model results
head(age0.2009)
trip16.2009<-bind_cols(par)%>%
  mutate(year=2009,month=8,day=19,trip=16)
trip16.2009<-mutate(trip16.2009,dummy_pulse=rev(seq(1:nrow(trip16.2009))))

mixtures<-bind_rows(mixtures,trip16.2009)

# September trip 17
age0.2009<-filter(length,year==2009 & age == 0 & trip ==17)

# check histogram
qplot(mmSL, data=age0.2009, binwidth=5)

# # detremine max and min SL
summarise(age0.2009,min(mmSL),max(mmSL))

# create dataframe with SL only
group2009<-select(age0.2009,mmSL)

# convert to frequency table
age0group<-mixgroup(group2009, breaks = c(0,seq(45,70,5),75),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)


# set parameters
age0param<-mixparam(c(42,60),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2009)
trip17.2009<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2009,month=9,day=3,trip=17)
trip17.2009<-mutate(trip17.2009,dummy_pulse=rev(seq(1:nrow(trip17.2009))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2009)

# September Trip 18
age0.2009<-filter(length,year==2009 & age == 0 & trip ==18)

# check histogram
qplot(mmSL, data=age0.2009, binwidth=5)

# # detremine max and min SL
summarise(age0.2009,min(mmSL),max(mmSL))

# create dataframe with SL only
group2009<-select(age0.2009,mmSL)

# convert to frequency table
age0group<-mixgroup(group2009, breaks = c(0,seq(50,75,5),82),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)


# set parameters
age0param<-mixparam(c(40,70),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

par<-groupstats(age0group)

# store model results
head(age0.2009)
trip18.2009<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2009,month=9,day=15,trip=18)
trip18.2009<-mutate(trip18.2009,dummy_pulse=rev(seq(1:nrow(trip18.2009))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2009)

# October trip 19
age0.2009<-filter(length,year==2009 & age == 0 & trip ==19)

# check histogram
qplot(mmSL, data=age0.2009, binwidth=5)

# # detremine max and min SL
summarise(age0.2009,min(mmSL),max(mmSL))

# create dataframe with SL only
group2009<-select(age0.2009,mmSL)

# convert to frequency table
age0group<-mixgroup(group2009, breaks = c(0,seq(35,95,5),102),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)


# set parameters
age0param<-mixparam(c(30,48,80),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=20, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2009)
trip19.2009<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2009,month=10,day=5,trip=19)
trip19.2009<-mutate(trip19.2009,dummy_pulse=rev(seq(1:nrow(trip19.2009))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2009)

# October trip 20
age0.2009<-filter(length,year==2009 & age == 0 & trip==20)
# not enough fish

# November
age0.2009<-filter(length,year==2009 & age == 0 & trip ==22)

# check histogram
qplot(mmSL, data=age0.2009, binwidth=5)

# # detremine max and min SL
summarise(age0.2009,min(mmSL),max(mmSL))

# create dataframe with SL only
group2009<-select(age0.2009,mmSL)

# convert to frequency table
age0group<-mixgroup(group2009, breaks = c(0,seq(35,50,5),58),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)


# set parameters
age0param<-mixparam(c(30,45),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=20, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

par<-groupstats(age0group)

# store model results
head(age0.2009)
trip22.2009<-bind_cols(par)%>%
  mutate(year=2009,month=11,day=16,trip=22)
trip22.2009<-mutate(trip22.2009,dummy_pulse=rev(seq(1:nrow(trip22.2009))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip22.2009)

# ----- age 0 2010 ----

# May 
age0.2010<-filter(length,year==2010 & age ==0 & month == 5)
# July
age0.2010<-filter(length,year==2010 & age == 0 & month==7)
#August
age0.2010<-filter(length,year==2010 & age == 0 & month == 8)
# not enough fish

# September trip 18
age0.2010<-filter(length,year==2010 & age == 0 & month ==9 & trip ==18)

# check histogram
qplot(mmSL, data=age0.2010, binwidth=5)

## detremine max and min SL
summarise(age0.2010,min(mmSL),max(mmSL))

# create dataframe with SL only
group2010<-select(age0.2010,mmSL)

# convert to frequency table
age0group<-mixgroup(group2010, breaks = c(0,seq(35,70,5),78),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,75),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=25, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# Store fit 1 results
head(age0.2010)
trip18.2010<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2010,month=9,day=20,trip=18)
trip18.2010<-mutate(trip18.2010,dummy_pulse=rev(seq(1:nrow(trip18.2010))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2010)

# October trip 20
age0.2010<-filter(length,year==2010 & age ==0 & trip==20)

# check histogram
qplot(mmSL, data=age0.2010, binwidth=5)

# # detremine max and min SL
summarise(age0.2010,min(mmSL),max(mmSL))

# create dataframe with SL only
group2010<-select(age0.2010,mmSL)

# convert to frequency table
age0group<-mixgroup(group2010, breaks = c(0,seq(35,70,5),73),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(30,45,67),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)
# I guess fit 1..... poor fit
head(age0.2010)
trip20.2010<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2010,month=10,day=19,trip=20)
trip20.2010<-mutate(trip20.2010,dummy_pulse=rev(seq(1:nrow(trip20.2010))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2010)

# November trip 21 
age0.2010<-filter(length,year==2010 & age ==0 & trip==21)
# check histogram
qplot(mmSL, data=age0.2010, binwidth=5)

# # detremine max and min SL
summarise(age0.2010,min(mmSL),max(mmSL))

# create dataframe with SL only
group2010<-select(age0.2010,mmSL)

# convert to frequency table
age0group<-mixgroup(group2010, breaks = c(0,seq(40,65,5),70),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,67),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

head(age0.2010)
trip21.2010<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2010,month=11,day=2,trip=21)
trip21.2010<-mutate(trip21.2010,dummy_pulse=rev(seq(1:nrow(trip21.2010))))

mixtures<-bind_rows(mixtures,trip21.2010)


# November trip 22
age0.2010<-filter(length,year==2010 & age ==0 & trip==22)

# ---- age 0 2011 ----
# May
age0.2011<-filter(length,year== 2011 & age ==0 & month == 5)

# July
age0.2011<-filter(length,year==2011 & age == 0 & month == 7)
# one fish onliy

# August trip 15
age0.2011<-filter(length,year==2011 & age == 0 & trip==15)


# check histogram
qplot(mmSL, data=age0.2011, binwidth=5)

# # detremine max and min SL
summarise(age0.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age0.2011,mmSL)

# convert to frequency table
age0group<-mixgroup(group2011, breaks = c(0,seq(40,50,5),57),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

#parameters
par<-groupstats(age0group)

# store fit 2 results
head(age0.2011)
trip15.2011<-bind_cols(par)%>%
  mutate(year=2011,month=8,day=15,trip=15)
trip15.2011<-mutate(trip15.2011,dummy_pulse=rev(seq(1:nrow(trip15.2011))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.2011)

# August trip 16
age0.2011<-filter(length,year==2011 & age == 0 & trip==16)

# check histogram
qplot(mmSL, data=age0.2011, binwidth=5)

# # detremine max and min SL
summarise(age0.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age0.2011,mmSL)

# convert to frequency table
age0group<-mixgroup(group2011, breaks = c(0,seq(45,60,5),65),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

#parameters
par<-groupstats(age0group)

# store fit 2 results
head(age0.2011)
trip16.2011<-bind_cols(par)%>%
  mutate(year=2011,month=8,day=29,trip=16)
trip16.2011<-mutate(trip16.2011,dummy_pulse=rev(seq(1:nrow(trip16.2011))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2011)

# September trip 17
age0.2011<-filter(length,year==2011 & age == 0 & trip==17)

# check histogram
qplot(mmSL, data=age0.2011, binwidth=5)

# # detremine max and min SL
summarise(age0.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age0.2011,mmSL)

# convert to frequency table
age0group<-mixgroup(group2011, breaks = c(0,seq(30,65,5),68),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(30,55),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2011)
trip17.2011<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=9,day=13,trip=17)
trip17.2011<-mutate(trip17.2011,dummy_pulse=rev(seq(1:nrow(trip17.2011))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2011)

# September trip 18
# second trip
age0.2011<-filter(length,year==2011 & age == 0 & trip==18)

# check histogram
qplot(mmSL, data=age0.2011, binwidth=5)

# # detremine max and min SL
summarise(age0.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age0.2011,mmSL)

# convert to frequency table
age0group<-mixgroup(group2011, breaks = c(0,seq(35,80,5),86),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,70),c(5),pi=NULL)
plot(age0group,age0param,"gamma")


# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2011)
trip18.2011<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=9,day=27,trip=18)
trip18.2011<-mutate(trip18.2011,dummy_pulse=rev(seq(1:nrow(trip18.2011))))

# addd to mixtures
mixtures<-bind_rows(mixtures,trip18.2011)

# October trip 19 
age0.2011<-filter(length,year==2011 & age ==0 & trip ==19)

# check histogram
qplot(mmSL, data=age0.2011, binwidth=5)

# # detremine max and min SL
summarise(age0.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age0.2011,mmSL)

# convert to frequency table
age0group<-mixgroup(group2011, breaks = c(0,seq(30,90,5),95),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,75),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# go with Fit 1 - 
head(age0.2011)
trip19.2011<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=10,day=12,trip=19)
trip19.2011<-mutate(trip19.2011,dummy_pulse=rev(seq(1:nrow(trip19.2011))))

# addd to mixtures
mixtures<-bind_rows(mixtures,trip19.2011)

# October trip 20
age0.2011<-filter(length,year==2011 & age == 0 & trip==20)

# check histogram
qplot(mmSL, data=age0.2011, binwidth=5)

# # detremine max and min SL
summarise(age0.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age0.2011,mmSL)

# convert to frequency table
age0group<-mixgroup(group2011, breaks = c(0,seq(30,90,5),94),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(46,95),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=3, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# go with Fit 1 - 
head(age0.2011)
trip20.2011<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=10,day=25,trip=20)
trip20.2011<-mutate(trip20.2011,dummy_pulse=rev(seq(1:nrow(trip20.2011))))

# addd to mixtures
mixtures<-bind_rows(mixtures,trip20.2011)

#November trip 21
age0.2011<-filter(length,year==2011 & age ==0 & trip==21)

# check histogram
qplot(mmSL, data=age0.2011, binwidth=5)

# # detremine max and min SL
summarise(age0.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age0.2011,mmSL)

# convert to frequency table
age0group<-mixgroup(group2011, breaks = c(0,seq(30,70,5),76),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(46,65),c(5),pi=NULL)
plot(age0group,age0param,"gamma")
# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=3, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

par<-groupstats(age0group)

# store results
head(age0.2011)
trip21.2011<-bind_cols(par)%>%
  mutate(year=2011,month=11,day=7,trip=21)
trip21.2011<-mutate(trip21.2011,dummy_pulse=rev(seq(1:nrow(trip21.2011))))

# addd to mixtures
mixtures<-bind_rows(mixtures,trip21.2011)

#November trip 22
age0.2011<-filter(length,year==2011 & age ==0 & trip==22)

# check histogram
qplot(mmSL, data=age0.2011, binwidth=5)

# # detremine max and min SL
summarise(age0.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age0.2011,mmSL)

# convert to frequency table
age0group<-mixgroup(group2011, breaks = c(0,seq(40,70,5),75),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)


# set parameters
age0param<-mixparam(c(46,70),c(5),pi=NULL)
plot(age0group,age0param,"gamma")
# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=3, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)
par<-groupstats(age0group)

# go with Fit 1 - 
head(age0.2011)
trip22.2011<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=11,day=22,trip=22)
trip22.2011<-mutate(trip22.2011,dummy_pulse=rev(seq(1:nrow(trip22.2011))))

# addd to mixtures
mixtures<-bind_rows(mixtures,trip22.2011)
# ---- age 0 2012 ----

# May
age0.2012<-filter(length,year==2012 & age == 0 & month == 5)
# no fish yet

# July
age0.2012 <- filter(length, year == 2012 & age == 0 & trip==13)

# check histogram
qplot(mmSL, data=age0.2012, binwidth=5)

# # detremine max and min SL
summarise(age0.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age0.2012,mmSL)

# convert to frequency table
age0group<-mixgroup(group2012, breaks = c(0,seq(30,35,5),39),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store model results
head(age0.2012)
trip13.2012<-bind_cols(par)%>%
  mutate(year=2012,month=7,day=17,trip=13)
trip13.2012<-mutate(trip13.2012,dummy_pulse=rev(seq(1:nrow(trip13.2012))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.2012)

# July trip 14
age0.2012<-filter(length,year==2012 & age ==0 & trip==14)

# check histogram
qplot(mmSL, data=age0.2012, binwidth=5)

# # detremine max and min SL
summarise(age0.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age0.2012,mmSL)

# convert to frequency table
age0group<-mixgroup(group2012, breaks = c(0,seq(40,55,5),60),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store fit 2 results
head(age0.2012)
trip14.2012<-bind_cols(par)%>%
  mutate(year=2012,month=7,day=31,trip=14)
trip14.2012<-mutate(trip14.2012,dummy_pulse=rev(seq(1:nrow(trip14.2012))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2012)

# August trip 15
age0.2012<-filter(length,year==2012 & age ==0 & trip==15)

# check histogram
qplot(mmSL, data=age0.2012, binwidth=5)

# # detremine max and min SL
summarise(age0.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age0.2012,mmSL)

# convert to frequency table
age0group<-mixgroup(group2012, breaks = c(0,seq(40,65,5),70),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store results
head(age0.2012)
trip15.2012<-bind_cols(par)%>%
  mutate(year=2012,month=8,day=14,trip=15)
trip15.2012<-mutate(trip15.2012,dummy_pulse=rev(seq(1:nrow(trip15.2012))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.2012)

# August trip 16
age0.2012<-filter(length,year==2012 & age ==0 & trip==16)

# check histogram
qplot(mmSL, data=age0.2012, binwidth=5)

# # detremine max and min SL
summarise(age0.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age0.2012,mmSL)

# convert to frequency table
age0group<-mixgroup(group2012, breaks = c(0,seq(35,75,5),79),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# Fit 1
head(age0.2012)
trip16.2011<-bind_cols(par)%>%
  mutate(year=2012,month=8,day=29,trip=16)
trip16.2011<-mutate(trip16.2011,dummy_pulse=rev(seq(1:nrow(trip16.2011))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2011)

# September trip 17
age0.2012<-filter(length,year==2012 & age ==0 & trip==17)
# check histogram
qplot(mmSL, data=age0.2012, binwidth=5)

# # detremine max and min SL
summarise(age0.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age0.2012,mmSL)

# convert to frequency table
age0group<-mixgroup(group2012, breaks = c(0,seq(40,80,5),85),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# Fit 1
head(age0.2012)
trip17.2011<-bind_cols(par)%>%
  mutate(year=2012,month=9,day=13,trip=17)
trip17.2011<-mutate(trip17.2011,dummy_pulse=rev(seq(1:nrow(trip17.2011))))
# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2011)

# September trip 18
age0.2012<-filter(length,year==2012 & age ==0 & trip==18)
# check histogram
qplot(mmSL, data=age0.2012, binwidth=5)

# # detremine max and min SL
summarise(age0.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age0.2012,mmSL)

# convert to frequency table
age0group<-mixgroup(group2012, breaks = c(0,seq(45,90,5),95),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,65),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2012)
trip18.2012<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2012,month=9,day=26,trip=18)
trip18.2012<-mutate(trip18.2012,dummy_pulse=rev(seq(1:nrow(trip18.2012))))

mixtures<-bind_rows(mixtures,trip18.2012)

# October trip 19
age0.2012<-filter(length,year==2012 & age ==0 & trip==19)
# check histogram
qplot(mmSL, data=age0.2012, binwidth=5)

# # detremine max and min SL
summarise(age0.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age0.2012,mmSL)

# convert to frequency table
age0group<-mixgroup(group2012, breaks = c(0,seq(40,90,5),94),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,75),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2012)
trip19.2012<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2012,month=10,day=15,trip=19)
trip19.2012<-mutate(trip19.2012,dummy_pulse=rev(seq(1:nrow(trip19.2012))))

mixtures<-bind_rows(mixtures,trip19.2012)

# October trip 20
age0.2012<-filter(length,year==2012 & age ==0 & trip==20)
# check histogram
qplot(mmSL, data=age0.2012, binwidth=5)

# # detremine max and min SL
summarise(age0.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age0.2012,mmSL)

# convert to frequency table
age0group<-mixgroup(group2012, breaks = c(0,seq(30,105,5),110),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(30,60,90),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2012)
trip20.2012<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2012,month=10,day=29,trip=20)
trip20.2012<-mutate(trip20.2012,dummy_pulse=rev(seq(1:nrow(trip20.2012))))

mixtures<-bind_rows(mixtures,trip20.2012)

# November trip 21
age0.2012<-filter(length,year==2012 & age ==0 & trip==21)
# check histogram
qplot(mmSL, data=age0.2012, binwidth=5)

# # detremine max and min SL
summarise(age0.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age0.2012,mmSL)

# convert to frequency table
age0group<-mixgroup(group2012, breaks = c(0,seq(40,105,5),112),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,60,100),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2012)
trip21.2012<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2012,month=11,day=13,trip=21)
trip21.2012<-mutate(trip21.2012,dummy_pulse=rev(seq(1:nrow(trip21.2012))))

mixtures<-bind_rows(mixtures,trip21.2012)

# November trip 22
age0.2012<-filter(length,year==2012 & age ==0 & trip==22)
# check histogram
qplot(mmSL, data=age0.2012, binwidth=5)

# # detremine max and min SL
summarise(age0.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age0.2012,mmSL)

# convert to frequency table
age0group<-mixgroup(group2012, breaks = c(0,seq(50,115,5),118),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,70,105),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2012)
trip22.2012<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2012,month=11,day=26,trip=22)
trip22.2012<-mutate(trip22.2012,dummy_pulse=rev(seq(1:nrow(trip22.2012))))

mixtures<-bind_rows(mixtures,trip22.2012)

# ---- age 0 2013 -----

# May
age0.2013<-filter(length,year==2013 & age == 0 & month ==5)

# July trip 14
age0.2013<-filter(length,year==2013 & age == 0 & trip==14)

# check histogram
qplot(mmSL, data=age0.2013, binwidth=5)

# # detremine max and min SL
summarise(age0.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age0.2013,mmSL)

# convert to frequency table
age0group<-mixgroup(group2013, breaks = c(0,seq(35,45,5),50),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store model results
head(age0.2013)
trip14.2013<-bind_cols(par)%>%
  mutate(year=2013,month=7,day=22,trip=14)
trip14.2013<-mutate(trip14.2013,dummy_pulse=rev(seq(1:nrow(trip14.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2013)

# August trip 15
age0.2013<-filter(length,year == 2013 & age == 0 & trip==15)

# check histogram
qplot(mmSL, data=age0.2013, binwidth=5)

# # detremine max and min SL
summarise(age0.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age0.2013,mmSL)

# convert to frequency table
age0group<-mixgroup(group2013, breaks = c(0,seq(40,60,5),67),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store model results
head(age0.2013)
trip15.2013<-bind_cols(par)%>%
  mutate(year=2013,month=8,day=6,trip=15)
trip15.2013<-mutate(trip15.2013,dummy_pulse=rev(seq(1:nrow(trip15.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.2013)

# August trip 16
age0.2013<-filter(length,year == 2013 & age == 0 & trip==16)

# check histogram
qplot(mmSL, data=age0.2013, binwidth=5)

# # detremine max and min SL
summarise(age0.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age0.2013,mmSL)

# convert to frequency table
age0group<-mixgroup(group2013, breaks = c(0,seq(40,65,5),71),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store model results
head(age0.2013)
trip16.2013<-bind_cols(par)%>%
  mutate(year=2013,month=8,day=19, trip=16)
trip16.2013<-mutate(trip16.2013,dummy_pulse=rev(seq(1:nrow(trip16.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2013)

# September trip 17
age0.2013<-filter(length, year==2013 & age ==0 & trip==17)

# check histogram
qplot(mmSL, data=age0.2013, binwidth=5)

# # detremine max and min SL
summarise(age0.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age0.2013,mmSL)

# convert to frequency table
age0group<-mixgroup(group2013, breaks = c(0,seq(45,75,5),79),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,60),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=20, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2013)
trip17.2013<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=9,day=3, trip=17)
trip17.2013<-mutate(trip17.2013,dummy_pulse=rev(seq(1:nrow(trip17.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2013)

# September trip 18
age0.2013<-filter(length,year==2013 & age == 0 & trip ==18 & mmSL<100) # take out age 1's

# check histogram
qplot(mmSL, data=age0.2013, binwidth=5)

# # detremine max and min SL
summarise(age0.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age0.2013,mmSL)

# convert to frequency table
age0group<-mixgroup(group2013, breaks = c(0,seq(35,80,5),86),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

par<-groupstats(age0group)

# store model results
head(age0.2013)
trip18.2013<-bind_cols(par)%>%
  mutate(year=2013,month=9,day=16,trip=18)
trip18.2013<-mutate(trip18.2013,dummy_pulse=rev(seq(1:nrow(trip18.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2013)

# October trip 19
age0.2013<-filter(length, year==2013 & age ==0 & trip ==19 & mmSL<120) # take out age 1's

# check histogram
qplot(mmSL, data=age0.2013, binwidth=5)

# # detremine max and min SL
summarise(age0.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age0.2013,mmSL)

# convert to frequency table
age0group<-mixgroup(group2013, breaks = c(0,seq(40,95,5),101),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store results
head(age0.2013)
trip19.2013<-bind_cols(par)%>%
  mutate(year=2013,month=10,day=1,trip=19)
trip19.2013<-mutate(trip19.2013,dummy_pulse=rev(seq(1:nrow(trip19.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2013)

# October trip 20
age0.2013<-filter(length,year==2013 & age ==0 & trip == 20)

# check histogram
qplot(mmSL, data=age0.2013, binwidth=5)

# # detremine max and min SL
summarise(age0.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age0.2013,mmSL)

# convert to frequency table
age0group<-mixgroup(group2013, breaks = c(0,seq(35,105,5),109),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(35,55,80),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1
head(age0.2013)
trip20.2013<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=10,day=15,trip=20)
trip20.2013<-mutate(trip20.2013,dummy_pulse=rev(seq(1:nrow(trip20.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2013)

# November trip 21
age0.2013<-filter(length,year==2013 & age ==0 & trip == 21 & mmSL < 130)

# check histogram
qplot(mmSL, data=age0.2013, binwidth=5)

# # detremine max and min SL
summarise(age0.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age0.2013,mmSL)

# convert to frequency table
age0group<-mixgroup(group2013, breaks = c(0,seq(45,120,5),125),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,70,100),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# fit 1
head(age0.2013)
trip21.2013<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=11,day=4,trip=21)
trip21.2013<-mutate(trip21.2013,dummy_pulse=rev(seq(1:nrow(trip21.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2013)

# November trip22
age0.2013<-filter(length,year==2013& age == 0 & trip ==22)

# check histogram
qplot(mmSL, data=age0.2013, binwidth=5)

# # detremine max and min SL
summarise(age0.2013,min(mmSL),max(mmSL))

# create dataframe with SL only
group2013<-select(age0.2013,mmSL)

# convert to frequency table
age0group<-mixgroup(group2013, breaks = c(0,seq(50,115,5),120),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,70,100,120),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=3, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1
head(age0.2013)
trip22.2013<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=11,day=18,trip=22)
trip22.2013<-mutate(trip22.2013,dummy_pulse=rev(seq(1:nrow(trip22.2013))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip22.2013)

# ---- age 0 2014 -----
# May
age0.2014<-filter(length,year==2014 & age ==0 & month ==5)
#July
age0.2014<-filter(length,year==2014 & age == 0 & month ==7)
#August
age0.2014 <- filter(length,year==2014 & age == 0 & month==8)
# not enough

# September trip 17
age0.2014<-filter(length,year==2014 & age ==0 & trip==17)
# check histogram
qplot(mmSL, data=age0.2014, binwidth=5)

# # detremine max and min SL
summarise(age0.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age0.2014,mmSL)

# convert to frequency table
age0group<-mixgroup(group2014, breaks = c(0,seq(40,70,5),76),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,65),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store fit 1 results..
head(age0.2014)
trip17.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=9,day=8,trip=17)
trip17.2014<-mutate(trip17.2014,dummy_pulse=rev(seq(1:nrow(trip17.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2014)

# September trip 18
age0.2014<-filter(length,year == 2014 & age == 0 & trip==18)

# check histogram
qplot(mmSL, data=age0.2014, binwidth=5)

# # detremine max and min SL
summarise(age0.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age0.2014,mmSL)

# convert to frequency table
age0group<-mixgroup(group2014, breaks = c(0,seq(40,80,5),86),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(55,75),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

par<-groupstats(age0group)


# store fit 1 results
head(age0.2014)
trip18.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=9,day=23,trip=18)
trip18.2014<-mutate(trip18.2014,dummy_pulse=rev(seq(1:nrow(trip18.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2014)

# October trip 19
age0.2014<-filter(length,year == 2014 & age == 0 & trip==19)

# check histogram
qplot(mmSL, data=age0.2014, binwidth=5)

# # detremine max and min SL
summarise(age0.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age0.2014,mmSL)

# convert to frequency table
age0group<-mixgroup(group2014, breaks = c(0,seq(40,90,5),95),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(35,60,80),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age0.2014)
trip19.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=10,day=7,trip=19)
trip19.2014<-mutate(trip19.2014,dummy_pulse=rev(seq(1:nrow(trip19.2014))))

mixtures<-bind_rows(mixtures,trip19.2014)

# October trip 20
age0.2014<-filter(length,year==2014 & age == 0 & trip ==20)

# check histogram
qplot(mmSL, data=age0.2014, binwidth=5)

# # detremine max and min SL
summarise(age0.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age0.2014,mmSL)

# convert to frequency table
age0group<-mixgroup(group2014, breaks = c(0,seq(45,90,5),97),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,65,85),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age0.2014)
trip20.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=10,day=21,trip=20)
trip20.2014<-mutate(trip20.2014,dummy_pulse=rev(seq(1:nrow(trip20.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2014)

# November trip 21
age0.2014<-filter(length,year==2014 & age ==0 & trip==21)

# check histogram
qplot(mmSL, data=age0.2014, binwidth=5)

# # detremine max and min SL
summarise(age0.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age0.2014,mmSL)

# convert to frequency table
age0group<-mixgroup(group2014, breaks = c(0,seq(50,95,5),99),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(53,70),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# Fit 1 store results
head(age0.2014)
trip21.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=11,day=4,trip=21)
trip21.2014<-mutate(trip21.2014,dummy_pulse=rev(seq(1:nrow(trip21.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2014)

# November trip 22
age0.2014<-filter(length,year==2014 & age ==0 & month==11 & trip==22)

# check histogram
qplot(mmSL, data=age0.2014, binwidth=5)

# # detremine max and min SL
summarise(age0.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age0.2014,mmSL)

# convert to frequency table
age0group<-mixgroup(group2014, breaks = c(0,seq(50,100,5),106),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,75,98),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

# store model results
head(age0.2014)
trip22.2014<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=11,day=18,trip=22)
trip22.2014<-mutate(trip22.2014,dummy_pulse=rev(seq(1:nrow(trip22.2014))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip22.2014)

# ---- age 0 2015 ----
# May
age0.2015<-filter(length, year == 2015 & age == 0 & month == 5)
# July trip 14
age0.2015<-filter(length,year==2015 & age == 0 & month ==7)

# check histogram
qplot(mmSL, data=age0.2015, binwidth=5)

# # detremine max and min SL
summarise(age0.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age0.2015,mmSL)

# convert to frequency table
age0group<-mixgroup(group2015, breaks = c(0,seq(40,45,5),53),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)
# store results
head(age0.2015)
trip14.2015<-bind_cols(par)%>%
  mutate(year=2015,month=7,day=28,trip=14)
trip14.2015<-mutate(trip14.2015,dummy_pulse=rev(seq(1:nrow(trip14.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2015)

# August 
age0.2015<-filter(length, year == 2015 & age == 0 & trip==15)

# check histogram
qplot(mmSL, data=age0.2015, binwidth=5)

# # detremine max and min SL
summarise(age0.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age0.2015,mmSL)

# convert to frequency table
age0group<-mixgroup(group2015, breaks = c(0,seq(40,55,5),60),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store results
head(age0.2015)
trip15.2015<-bind_cols(par)%>%
  mutate(year=2015,month=8,day=12,trip=15) # halfway between trip 13 and 14
trip15.2015<-mutate(trip15.2015,dummy_pulse=rev(seq(1:nrow(trip15.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.2015)

# August trip 16
age0.2015<-filter(length,year==2015 & age ==0 & trip==16)

# check histogram
qplot(mmSL, data=age0.2015, binwidth=5)

# # detremine max and min SL
summarise(age0.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age0.2015,mmSL)

# convert to frequency table
age0group<-mixgroup(group2015, breaks = c(0,seq(50,60,5),65),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store results
head(age0.2015)
trip16.2015<-bind_cols(par)%>%
  mutate(year=2015,month=8,day=25,trip=16) # halfway between trip 13 and 14
trip16.2015<-mutate(trip16.2015,dummy_pulse=rev(seq(1:nrow(trip16.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2015)

# September trip 17
age0.2015<-filter(length,year==2015 & age ==0 & trip==17)

# check histogram
qplot(mmSL, data=age0.2015, binwidth=5)

# # detremine max and min SL
summarise(age0.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age0.2015,mmSL)

# convert to frequency table
age0group<-mixgroup(group2015, breaks = c(0,seq(40,75,5),81),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,65,80),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# go with fit 1
head(age0.2015)
trip17.2015<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=9,day=14,trip=17)
trip17.2015<-mutate(trip17.2015,dummy_pulse=rev(seq(1:nrow(trip17.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2015)

# September trip 18
age0.2015<-filter(length, year==2015 & age == 0 & trip==18)

# check histogram
qplot(mmSL, data=age0.2015, binwidth=5)

# # detremine max and min SL
summarise(age0.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age0.2015,mmSL)

# convert to frequency table
age0group<-mixgroup(group2015, breaks = c(0,seq(40,85,5),91),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,55,70,90),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=1, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# go with fit 1
head(age0.2015)
trip18.2015<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=9,day=28,trip=18)
trip18.2015<-mutate(trip18.2015,dummy_pulse=rev(seq(1:nrow(trip18.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2015)

# October trip 19
age0.2015<-filter(length, year==2015 & age == 0 & trip==19)

# check histogram
qplot(mmSL, data=age0.2015, binwidth=5)

# # detremine max and min SL
summarise(age0.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age0.2015,mmSL)

# convert to frequency table
age0group<-mixgroup(group2015, breaks = c(0,seq(40,95,5),102),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(50,75,100),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# go with fit 1
head(age0.2015)
trip19.2015<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=10,day=12,trip=19)
trip19.2015<-mutate(trip19.2015,dummy_pulse=rev(seq(1:nrow(trip19.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2015)

# October trip 20
age0.2015<-filter(length, year==2015 & age == 0 & trip==20 & mmSL<100)

# check histogram
qplot(mmSL, data=age0.2015, binwidth=5)

# # detremine max and min SL
summarise(age0.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age0.2015,mmSL)

# convert to frequency table
age0group<-mixgroup(group2015, breaks = c(0,seq(35,95,5),98),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,58,85),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# go with fit 1
head(age0.2015)
trip20.2015<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=10,day=26,trip=20)
trip20.2015<-mutate(trip20.2015,dummy_pulse=rev(seq(1:nrow(trip20.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2015)

# November Trip 21
age0.2015<-filter(length, year==2015 & age == 0 & trip==21 & mmSL < 110) # get rid of age 1's

# check histogram
qplot(mmSL, data=age0.2015, binwidth=5)

# # detremine max and min SL
summarise(age0.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age0.2015,mmSL)

# convert to frequency table
age0group<-mixgroup(group2015, breaks = c(0,seq(40,95,5),100),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,62),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# go with fit 1
head(age0.2015)
trip21.2015<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=11,day=9,trip=21)
trip21.2015<-mutate(trip21.2015,dummy_pulse=rev(seq(1:nrow(trip21.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2015)

# November Trip 22
age0.2015<-filter(length, year==2015 & age == 0 & trip==22)

# check histogram
qplot(mmSL, data=age0.2015, binwidth=5)

# # detremine max and min SL
summarise(age0.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age0.2015,mmSL)

# convert to frequency table
age0group<-mixgroup(group2015, breaks = c(0,seq(40,110,5),114),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,62,100),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# go with fit 1
head(age0.2015)
trip22.2015<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=11,day=23,trip=22)
trip22.2015<-mutate(trip22.2015,dummy_pulse=rev(seq(1:nrow(trip22.2015))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip22.2015)

# ---- age 0 2016 ----
# May
age0.2016<-filter(length, year==2016 & age == 0 & month == 5)

#July
age0.2016<-filter(length,year==2016 & age ==0 & month ==7)

# August trip 15
age0.2016<-filter(length,year==2016 & age ==0 & trip==15)

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
mixtures<-bind_rows(mixtures,trip15.2016)

# August trip 16
age0.2016<-filter(length, year == 2016 & age == 0 & trip==16)

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
age0.2016<-filter(length, year == 2016 & age == 0 & trip==17)

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
age0.2016<-filter(length,year==2016 & age ==0 & trip==18)

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
age0.2016<-filter(length,year==2016 & age ==0 & trip==19)

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
plot(fit1)
plot(fit1,root=T)

# fit 1
head(age0.2016)
trip19.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=10,day=13,trip=19)
trip19.2016<-mutate(trip19.2016,dummy_pulse=rev(seq(1:nrow(trip19.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2016)

# October trip 20
age0.2016<-filter(length,year==2016 & age ==0 & trip==20)

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
age0.2016<-filter(length,year==2016 & age == 0 & trip==21)

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
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2016)
trip21.2016<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=11,day=14, trip =21)
trip21.2016<-mutate(trip21.2016,dummy_pulse=rev(seq(1:nrow(trip21.2016))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2016)

# November trip 22 
age0.2016<-filter(length,year==2016 & age ==0 & trip == 22)

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

# ----- age 0 2017 ------
# May
age0.2017<-filter(length, year==2017 & age == 0 & month == 5)
#July
age0.2017<-filter(length, year==2017 & age == 0 & trip==14)
# check histogram
qplot(mmSL, data=age0.2017, binwidth=5)

# # detremine max and min SL
summarise(age0.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age0.2017,mmSL)

# convert to frequency table
age0group<-mixgroup(group2017, breaks = c(0,seq(30,35,5),39),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store model results
head(age0.2017)
trip14.2017<-bind_cols(par)%>%
  mutate(year=2017,month=7,day=24,trip=14)
trip14.2017<-mutate(trip14.2017,dummy_pulse=rev(seq(1:nrow(trip14.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.2017)

# August trip 15
age0.2017<-filter(length,year==2017 & age ==0 & trip == 15)

# check histogram
qplot(mmSL, data=age0.2017, binwidth=5)

# # detremine max and min SL
summarise(age0.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age0.2017,mmSL)

# convert to frequency table
age0group<-mixgroup(group2017, breaks = c(0,seq(35,55,5),59),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store model results
head(age0.2017)
trip.15.2017<-bind_cols(par)%>%
  mutate(year=2017,month=8,day=9, trip=15)
trip.15.2017<-mutate(trip.15.2017,dummy_pulse=rev(seq(1:nrow(trip.15.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip.15.2017)

# August trip 16
age0.2017<-filter(length,year==2017 & age ==0 & trip==16)

# check histogram
qplot(mmSL, data=age0.2017, binwidth=5)

# # detremine max and min SL
summarise(age0.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age0.2017,mmSL)

# convert to frequency table
age0group<-mixgroup(group2017, breaks = c(0,seq(45,60,5),68),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store model results
head(age0.2017)
trip16.2017<-bind_cols(par)%>%
  mutate(year=2017,month=8,day=21, trip=16)
trip16.2017<-mutate(trip16.2017,dummy_pulse=rev(seq(1:nrow(trip16.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip16.2017)

# Setpember trip 17
age0.2017<-filter(length,year==2017 & age ==0 & trip==17)

# check histogram
qplot(mmSL, data=age0.2017, binwidth=5)

# # detremine max and min SL
summarise(age0.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age0.2017,mmSL)

# convert to frequency table
age0group<-mixgroup(group2017, breaks = c(0,seq(50,70,5),78),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)

# store model results
head(age0.2017)
trip17.2017<-bind_cols(par)%>%
  mutate(year=2017,month=9,day=5,trip=17)
trip17.2017<-mutate(trip17.2017,dummy_pulse=rev(seq(1:nrow(trip17.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip17.2017)

# September trip 18
age0.2017<-filter(length,year==2017 & age ==0 & month == 9 & trip ==18)

# check histogram
qplot(mmSL, data=age0.2017, binwidth=5)

# # detremine max and min SL
summarise(age0.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age0.2017,mmSL)

# convert to frequency table
age0group<-mixgroup(group2017, breaks = c(0,seq(60,85,5),90),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
par<-groupstats(age0group)
# store model results
head(age0.2017)
trip18.2017<-bind_cols(par)%>%
  mutate(year=2017,month=9,day=18, trip=18)
trip18.2017<-mutate(trip18.2017,dummy_pulse=rev(seq(1:nrow(trip18.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip18.2017)

# October trip 19
age0.2017<-filter(length,year==2017 & age ==0 & trip==19)

# check histogram
qplot(mmSL, data=age0.2017, binwidth=5)

# # detremine max and min SL
summarise(age0.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age0.2017,mmSL)

# convert to frequency table
age0group<-mixgroup(group2017, breaks = c(0,seq(55,80,5),88),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(55,73),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age0.2017)
trip19.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=10,day=2, trip=19)
trip19.2017<-mutate(trip19.2017,dummy_pulse=rev(seq(1:nrow(trip19.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.2017)

# October trip 20
age0.2017<-filter(length,year==2017 & age ==0 & trip ==20)

# check histogram
qplot(mmSL, data=age0.2017, binwidth=5)

# # detremine max and min SL
summarise(age0.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age0.2017,mmSL)

# convert to frequency table
age0group<-mixgroup(group2017, breaks = c(0,seq(50,90,5),95),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,70,95),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results ***Absolutely terrible fit
head(age0.2017)
trip20.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=10,day=17, trip=20)
trip20.2017<-mutate(trip20.2017,dummy_pulse=rev(seq(1:nrow(trip20.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.2017)

# November trip 21
age0.2017<-filter(length,year==2017 & age ==0 & trip ==21)

# check histogram
qplot(mmSL, data=age0.2017, binwidth=5)

# # detremine max and min SL
summarise(age0.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age0.2017,mmSL)

# convert to frequency table
age0group<-mixgroup(group2017, breaks = c(0,seq(45,95,5),102),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(40,55,75,95),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2017)
trip21.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=10,day=31, trip=21)
trip21.2017<-mutate(trip21.2017,dummy_pulse=rev(seq(1:nrow(trip21.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip21.2017)

# November trip 22
age0.2017<-filter(length,year==2017 & age ==0 & trip ==22)

# check histogram
qplot(mmSL, data=age0.2017, binwidth=5)

# # detremine max and min SL
summarise(age0.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age0.2017,mmSL)

# convert to frequency table
age0group<-mixgroup(group2017, breaks = c(0,seq(50,95,5),101),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age0group)

# set parameters
age0param<-mixparam(c(45,60,80,95),c(4),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age0.2017)
trip22.2017<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=11,day=15, trip=22)
trip22.2017<-mutate(trip22.2017,dummy_pulse=rev(seq(1:nrow(trip22.2017))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip22.2017)

# ---- age 0 2018----
yr2018<-length%>%
  filter(year==2018)
# May
# --- select data 
july2018<-filter(yr2018, age ==0 & month == 7)
#not enough july fish

# August trip 15
august2018<-filter(yr2018, age == 0 & trip ==15)

# --- check histogram 
qplot(mmSL, data = august2018, binwidth = 5)

# --- create dataframe with SL only 
SL <- select(august2018, mmSL)

# ---- determine min and max 
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table 
group.2018<-mixgroup(SL, breaks = c(0,seq(35,55,5),57),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table 
plot(group.2018)

# parameters
trip15<-groupstats(group.2018)
head(august2018)

trip15<-trip15%>%
  mutate(trip=15,year=2018,month=8,day=13)
trip15<-mutate(trip15,dummy_pulse=rev(seq(1:nrow(trip15))))


# August trip 16
august2018<-filter(yr2018, age == 0 & trip ==16)

# --- check histogram 
qplot(mmSL, data = august2018, binwidth = 5)

# --- create dataframe with SL only 
SL <- select(august2018, mmSL)

# ---- determine min and max 
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table 
group.2018<-mixgroup(SL, breaks = c(0,seq(35,60,5),65),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table 
plot(group.2018)

# parameters
trip16<-groupstats(group.2018)
head(august2018)

trip16<-trip16%>%
  mutate(trip=16,year=2018,month=8,day=27)
trip16<-mutate(trip16,dummy_pulse=rev(seq(1:nrow(trip16))))

# September trip 17
sept2018<-filter(yr2018, age == 0 & trip ==17)

# --- check histogram 
qplot(mmSL, data = sept2018, binwidth = 5)

# --- create dataframe with SL only 
SL <- select(sept2018, mmSL)

# ---- determine min and max 
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table 
group.2018<-mixgroup(SL, breaks = c(0,seq(45,70,5),76),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table 
plot(group.2018)

# parameters
trip17<-groupstats(group.2018)
head(sept2018)

trip17<-trip17%>%
  mutate(trip=17,year=2018,month=9,day=10)
trip17<-mutate(trip17,dummy_pulse=rev(seq(1:nrow(trip17))))
# September trip 18
sept2018<-filter(yr2018, age == 0 & trip ==18)

# --- check histogram 
qplot(mmSL, data = sept2018, binwidth = 5)

# --- create dataframe with SL only
SL <- select(sept2018, mmSL)

# ---- determine min and max 
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table 
group.2018<-mixgroup(SL, breaks = c(0,seq(55,75,5),81),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table 
plot(group.2018)

# parameters
trip18<-groupstats(group.2018)
head(sept2018)

trip18<-trip18%>%
  mutate(trip=18,year=2018,month=9,day=24)
trip18<-mutate(trip18,dummy_pulse=rev(seq(1:nrow(trip18))))
# October trip 19
oct2018<-filter(yr2018, age == 0 & trip ==19)

# --- check histogram 
qplot(mmSL, data = oct2018, binwidth = 5)

# --- create dataframe with SL only 
SL <- select(oct2018, mmSL)

# ---- determine min and max 
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table 
group.2018<-mixgroup(SL, breaks = c(0,seq(45,90,5),93),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table 
plot(group.2018)

#parameters
trip19<-groupstats(group.2018)
head(oct2018)
trip19<-trip19%>%
  mutate(trip=19,year=2018,month=10,day=9)
trip19<-mutate(trip19,dummy_pulse=rev(seq(1:nrow(trip19))))
# October trip 20
oct2018<-filter(yr2018, age == 0 & trip ==20)

# --- check histogram
qplot(mmSL, data = oct2018, binwidth = 5)

# --- create dataframe with SL only
SL <- select(oct2018, mmSL)

# ---- determine min and max
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table
group.2018<-mixgroup(SL, breaks = c(0,seq(40,95,5),101),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table
plot(group.2018)


# ---- set initial parameters
par<-mixparam(c(58,80),c(5),pi=NULL)
plot(group.2018,par,"gamma")

# fit mixture
fit1<-mix(group.2018,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

head(oct2018)
trip20<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(trip=20,year=2018,month=10,day=22)
trip20<-mutate(trip20,dummy_pulse=rev(seq(1:nrow(trip20))))

# November trip 21
nov2018<-filter(yr2018, age == 0 & trip ==21)

# --- check histogram
qplot(mmSL, data = nov2018, binwidth = 5)

# --- create dataframe with SL only
SL <- select(nov2018, mmSL)

# ---- determine min and max
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table
group.2018<-mixgroup(SL, breaks = c(0,seq(45,100,5),106),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table
plot(group.2018)


# ---- set initial parameters
par<-mixparam(c(40,63,85),c(5),pi=NULL)
plot(group.2018,par,"gamma")

# fit mixture
fit1<-mix(group.2018,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

head(nov2018)
trip21<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(trip=21,year=2018,month=11,day=5)
trip21<-mutate(trip21,dummy_pulse=rev(seq(1:nrow(trip21))))

# November trip 22
nov2018<-filter(yr2018, age == 0 & trip ==22)

# --- check histogram
qplot(mmSL, data = nov2018, binwidth = 5)

# --- create dataframe with SL only
SL <- select(nov2018, mmSL)

# ---- determine min and max
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table
group.2018<-mixgroup(SL, breaks = c(0,seq(45,100,5),106),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table
plot(group.2018)


# ---- set initial parameters
par<-mixparam(c(50,65,85),c(5),pi=NULL)
plot(group.2018,par,"gamma")

# fit mixture
fit1<-mix(group.2018,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

head(nov2018)
trip22<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(trip=22,year=2018,month=11,day=19)
trip22<-mutate(trip22,dummy_pulse=rev(seq(1:nrow(trip22))))

# December trip 23
dec2018<-filter(yr2018, age == 0 & trip ==23)

# --- check histogram
qplot(mmSL, data = dec2018, binwidth = 5)

# --- create dataframe with SL only
SL <- select(dec2018, mmSL)

# ---- determine min and max
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table
group.2018<-mixgroup(SL, breaks = c(0,seq(50,105,5),112),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table
plot(group.2018)


# ---- set initial parameters
par<-mixparam(c(50,70,90),c(5),pi=NULL)
plot(group.2018,par,"gamma")

# fit mixture
fit1<-mix(group.2018,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

head(dec2018)
trip23<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(trip=23,year=2018,month=12,day=4)
trip23<-mutate(trip23,dummy_pulse=rev(seq(1:nrow(trip23))))


# store results in a dataframe
mixtures<-bind_rows(mixtures,trip15,trip16,trip17,trip18,trip19,trip20,trip21,trip22,trip23)%>%
  mutate(cohort=year)



View(mixtures)

# add 'cohort', 'age' to mixtures df
# ---- Final mixtures DF -----
mixtures2<-mixtures%>%
  mutate(cohort=year,age=0)

write.csv(mixtures2,"./data/data-working/age-0-mixture-dist.csv",row.names = FALSE)
