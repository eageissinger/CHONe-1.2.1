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

# ---- combine trip dates and length data ----
length<-left_join(length,trips)

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

# combination of two trips
# cusp of what works, try it out...

# check histogram
qplot(mmSL,data=age0.1996,binwidth=5)+theme_classic()

# min and max sl
summarise(age0.1996,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1996<-select(age0.1996,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1996, breaks=c(0,seq(30,55,5),60),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(38,60),c(5),pi=NULL)
plot(age0group,age0param,"gamma")


#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10,usecondit = FALSE,print.level = 0)
fitted.mix(fit1)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)

groupstats(age0group) # if there is only one group....
# store results
head(age0.1996)
trip17.18.1996<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1996,month=9,day=10,trip=17,trip2=18)
trip17.18.1996<-mutate(trip17.18.1996,dummy_pulse=rev(seq(1:nrow(trip17.18.1996))))

mixtures<-trip17.18.1996

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

mixtures<-bind_rows(mixtures,trip19.1996)

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

# November
age0.1998<-filter(length,age==0 & year == 1998 & month == 11)

#combine trips
# check histogram
qplot(mmSL,data=age0.1998,binwidth=5)

# min and max sl
summarise(age0.1998,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1998<-select(age0.1998,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(group1998, breaks=c(0,seq(40,80,5),85),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(40,55,80),c(3),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
head(age0.1998)
trip22.23.1998<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1998,month=11,day=15,trip=22,trip2=23)
trip22.23.1998<-mutate(trip22.23.1998,dummy_pulse=rev(seq(1:nrow(trip22.23.1998))))

mixtures<-bind_rows(mixtures,trip22.23.1998)

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

# can't determine pulse structure

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
trip14.2006<-bind_cols(fit1$parameters,fit1$se)%>%
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
trip15.2006<-bind_cols(fit1$parameters,fit1$se)%>%
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
age1group<-mixgroup(group2011, breaks = c(0,seq(45,115,5),120),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(60,80,100),c(2,3,4),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
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
may11<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2011,month=5,day=18,dist="gamma")
may11<-mutate(may11,dummy_pulse=rev(seq(1:nrow(may11))))

# add to mixtures
mixtures<-bind_rows(mixtures,may11)

# July
# first trip
age1.2011<-filter(length,year==2011 & age == 1 & month == 7)
age1.2011<-filter(length,year==2011 & age == 1 & month == 7 & day < 7)

# check histogram
qplot(mmSL, data=age1.2011, binwidth=5)

# # detremine max and min SL
summarise(age1.2011,min(mmSL),max(mmSL))

# create dataframe with SL only
group2011<-select(age1.2011,mmSL)

# convert to frequency table
age1group<-mixgroup(group2011, breaks = c(0,seq(65,100,5),102),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(70,100),c(2,3),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=3, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2011)
july11w1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=7,day=4,dist="gamma")
july11w1<-mutate(july11w1,dummy_pulse=rev(seq(1:nrow(july11w1))))

# add to mixtures
mixtures<-bind_rows(mixtures,july11w1)

# July
# second trip
age1.2011<-filter(length,year==2011 & age == 1 & month == 7 & day >6)
# use second week of july data

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
age1param<-mixparam(c(80,110,130),c(2,3,4),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2011)
july11w2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=7,day=18,dist="norm")
july11w2<-mutate(july11w2,dummy_pulse=rev(seq(1:nrow(july11w2))))

# addd to mixtures
mixtures<-bind_rows(mixtures,july11w2)

# August 
age1.2011<-filter(length,year==2011 & age ==1 & month ==8 & Trip ==15)
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
age1param<-mixparam(c(100,115,140),c(2,3,4),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# go with Fit 1 - 
head(age1.2011)
aug11<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2011,month=8,day=16,dist="norm")
aug11<-mutate(aug11,dummy_pulse=rev(seq(1:nrow(aug11))))

# addd to mixtures
mixtures<-bind_rows(mixtures,aug11)

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
age1param<-mixparam(c(55,80),c(3),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2012)
may12<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2012,month=5,day=22,dist="gamma")
may12<-mutate(may12,dummy_pulse=rev(seq(1:nrow(may12))))

# add to mixtures
mixtures<-bind_rows(mixtures,may12)

# July
age1.2012<-filter(length,year==2012 & age ==1 & month == 7)

# small sample size but let's try anyway

# check histogram
qplot(mmSL, data=age1.2012, binwidth=5)

# # detremine max and min SL
summarise(age1.2012,min(mmSL),max(mmSL))

# create dataframe with SL only
group2012<-select(age1.2012,mmSL)

# convert to frequency table
age1group<-mixgroup(group2012, breaks = c(0,seq(65,120,5),125),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(65,80,95,110,125),c(2,3,4,5,6),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(80,100),c(2,3),pi=NULL)

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps= 15, usecondit = FALSE, print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)


# store fit 2 results
head(age1.2012)
july12<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2012,month=7,day=10,dist="gamma") # halfway between trip 12 and 13
july12<-mutate(july12,dummy_pulse=rev(seq(1:nrow(july12))))

# add to mixtures
mixtures<-bind_rows(mixtures,july12)

# August
age1.2012<-filter(length,year==2012 & age ==1 & month ==8)
# no fish

# September
age1.2012<-filter(length,year==2012 & age ==1 & month ==9)
# not enough fish

# October
age1.2012<-length%>%
  filter(year==2012 & age ==1 & month ==10 & Trip ==20)%>%
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
age1param<-mixparam(c(130,150,165,180,200),c(2,3,4,5,6),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# Fit 1
head(age1.2012)
oct12<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2012,month=10,day=29,dist="norm")
oct12<-mutate(oct12,dummy_pulse=rev(seq(1:nrow(oct12))))

# add to mixtures
mixtures<-bind_rows(mixtures,oct12)

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
age1param<-mixparam(c(55,80,100),c(2,3,4),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2013)
may13<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=5,day=7,dist="gamma")
may13<-mutate(may13,dummy_pulse=rev(seq(1:nrow(may13))))

# add to mixtures
mixtures<-bind_rows(mixtures,may13)

# July
age1.2013<-filter(length,year == 2013 & age == 1 & month ==7)

# no july data.......

# August
age1.2013<-filter(length,year == 2013 & age == 1 & month ==8)
# not enough data

# September
age1.2013<-filter(length,year == 2013 & age == 1 & month ==9 & Trip == 17)
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

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2013)
sept13<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=9,day=3, dist="lnorm")
sept13<-mutate(sept13,dummy_pulse=rev(seq(1:nrow(sept13))))

# add to mixtures
mixtures<-bind_rows(mixtures,sept13)

# October
age1.2013<-filter(length, year==2013 & age ==1 & month ==10 & Trip ==19)
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
age1param<-mixparam(c(130,150,190),c(2,3,4),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2013)
oct13wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=10,day=1,dist="gamma")
oct13wk1<-mutate(oct13wk1,dummy_pulse=rev(seq(1:nrow(oct13wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,oct13wk1)

# October week 2
age1.2013<-filter(length, year==2013 & age ==1 & month ==10 & Trip ==20)
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
age1param<-mixparam(c(120,130,145,160),c(2,3,4,5),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age1.2013)
oct13wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=10,day=15,dist="gamma")
oct13wk2<-mutate(oct13wk2,dummy_pulse=rev(seq(1:nrow(oct13wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,oct13wk2)

# November
age1.2013<-filter(length,year==2013 & age ==1 & month ==11 & Trip == 21)
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
age1param<-mixparam(c(140,160,180,200),c(2,3,4,5),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1
head(age1.2013)
nov13wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=11,day=4,dist="norm")
nov13wk1<-mutate(nov13wk1,dummy_pulse=rev(seq(1:nrow(nov13wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,nov13wk1)

# November week 2
age1.2013<-filter(length,year==2013 & age ==1 & month ==11 & Trip == 22)
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
age1param<-mixparam(c(135,150,160,170),c(2,3,4,5),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# fit 1
head(age1.2013)
nov13wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2013,month=11,day=18,dist="gamma")
nov13wk2<-mutate(nov13wk2,dummy_pulse=rev(seq(1:nrow(nov13wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,nov13wk2)


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
age1param<-mixparam(c(55,80,100,115),c(2,3,4,5),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(45,75,95,110),c(2,3,4,5),pi=NULL)

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# try with fewer pulses.....
age1param3<-mixparam(c(50,90,120),c(2,3,4),pi=NULL)

fit3<-mix(age1group,age1param3,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE, print.level = 0)
summary(fit3) 
plot(fit3)
plot(fit3,root=T)



# store fit 1 results..
head(age1.2014)
may14<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=5,day=27,dist="gamma")
may14<-mutate(may14,dummy_pulse=rev(seq(1:nrow(may14))))

# add to mixtures
mixtures<-bind_rows(mixtures,may14)

# July
age1.2014<-filter(length,year == 2014 & age == 1 & month == 7 & mmSL<=150)
age1.2014<-filter(length,year == 2014 & age == 1 & month == 7 & day <20 & mmSL<=150)
# went with first sampling week

# check histogram
qplot(mmSL, data=age1.2014, binwidth=5)

# # detremine max and min SL
summarise(age1.2014,min(mmSL),max(mmSL))

# create dataframe with SL only
group2014<-select(age1.2014,mmSL)

# convert to frequency table
age1group<-mixgroup(group2014, breaks = c(0,seq(65,140,5),145),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,110,130),c(2,3,4),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(80,115,135),c(2,3,4),pi=NULL)
fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)


# store fit 1 results
head(age1.2014)
july14w1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=7,day=14,dist="gamma")
july14w1<-mutate(july14w1,dummy_pulse=rev(seq(1:nrow(july14w1))))

# add to mixtures
mixtures<-bind_rows(mixtures,july14w1)

# July second sampling week
age1.2014<-filter(length,year == 2014 & age == 1 & month == 7 & mmSL<=150)
age1.2014<-filter(length,year == 2014 & age == 1 & month == 7 & day >19 & mmSL<=150)

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
july14w2<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2014,month=7,day=29,dist="gamma")
july14w2<-mutate(july14w2,dummy_pulse=rev(seq(1:nrow(july14w2))))

mixtures<-bind_rows(mixtures,july14w2)

# August
age1.2014<-filter(length,year==2014 & age == 1 & month ==8 & Trip ==16)
# Trip 16

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
age1param<-mixparam(c(100,113,125,145,170),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(100,120,145,175),c(5),pi=NULL)
fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=3,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)



# store fit 2 results
head(age1.2014)
aug14<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2014,month=8,day=25,dist="gamma")
aug14<-mutate(aug14,dummy_pulse=rev(seq(1:nrow(aug14))))

# add to mixtures
mixtures<-bind_rows(mixtures,aug14)

# November
age1.2014<-filter(length,year==2014 & age ==1 & month==11 & Trip==21)
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
age1param<-mixparam(c(130,150,200),c(2,3,4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=3, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# Fit 1 store results
head(age1.2014)
nov14wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=11,day=4,dist="gamma")
nov14wk1<-mutate(nov14wk1,dummy_pulse=rev(seq(1:nrow(nov14wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,nov14wk1)

# November week 2
age1.2014<-filter(length,year==2014 & age ==1 & month==11 & Trip==22)
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
nov14wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2014,month=11,day=18,dist="lnorm")
nov14wk2<-mutate(nov14wk2,dummy_pulse=rev(seq(1:nrow(nov14wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,nov14wk2)

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
may15<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=5,day=19,dist="lnorm")
may15<-mutate(may15,dummy_pulse=rev(seq(1:nrow(may15))))

# add to mixtures
mixtures<-bind_rows(mixtures,may15)

# July 
age1.2015<-filter(length, year == 2015 & age == 1 & month == 7)
# use entire month 
# check histogram
qplot(mmSL, data=age1.2015, binwidth=5)

# # detremine max and min SL
summarise(age1.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age1.2015,mmSL)

# convert to frequency table
age1group<-mixgroup(group2015, breaks = c(0,seq(70,125,5),131),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(78,98,115),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
head(age1.2015)
july15<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=7,day=21,dist="gamma") # halfway between trip 13 and 14
july15<-mutate(july15,dummy_pulse=rev(seq(1:nrow(july15))))

# add to mixtures
mixtures<-bind_rows(mixtures,july15)

# August
age1.2015<-filter(length,year==2015 & age ==1 & month ==8)
# not enough 

# September 
age1.2015<-filter(length,year==2015 & age ==1 & month ==9 & Trip==17)
# Trip 17

# check histogram
qplot(mmSL, data=age1.2015, binwidth=5)

# # detremine max and min SL
summarise(age1.2015,min(mmSL),max(mmSL))

# create dataframe with SL only
group2015<-select(age1.2015,mmSL)

# convert to frequency table
age1group<-mixgroup(group2015, breaks = c(0,seq(100,160,5),165),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(105,125,140,165),c(2,3,4,5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# go with fit 1
head(age1.2015)
sept15<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2015,month=9,day=14,dist="gamma")
sept15<-mutate(sept15,dummy_pulse=rev(seq(1:nrow(sept15))))

# add to mixtures
mixtures<-bind_rows(mixtures,sept15)

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

age1param2<-mixparam(c(40,70),c(2,3),pi=NULL) # won't take 2.....

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)


# store fit 1 results
head(age1.2016)
may16<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=5,day=16,dist="lnorm")
may16<-mutate(may16,dummy_pulse=rev(seq(1:nrow(may16))))

# add to mixtures
mixtures<-bind_rows(mixtures,may16)

# July
# trip 1
age1.2016<-filter(length, year == 2016 & age == 1 & month == 7 & mmSL <= 150)
age1.2016<-filter(length, year == 2016 & age == 1 & month == 7 & day < 15 & mmSL <= 150) # go with first week
# take out 200 mm outlier

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(50,110,5),115),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(60,90,110),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(60,90),c(2,3),pi=NULL)

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store fit 1 results
head(age1.2016)
july16w1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=7,day=4,dist="gamma")
july16w1<-mutate(july16w1,dummy_pulse=rev(seq(1:nrow(july16w1))))

# add to mixtures
mixtures<-bind_rows(mixtures,july16w1)

# July
# trip2
age1.2016<-filter(length, year == 2016 & age == 1 & month == 7 & mmSL <= 150)
age1.2016<-filter(length, year == 2016 & age == 1 & month == 7 & day >10 & mmSL <= 150) # go with first week
# take out 200 mm outlier

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
july16w2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=7,day=18, dist="gamma")
july16w2<-mutate(july16w2,dummy_pulse=rev(seq(1:nrow(july16w2))))

# add to mixtures
mixtures<-bind_rows(mixtures,july16w2)

# August
age1.2016<-filter(length,year==2016 & age ==1 & month ==8 & Trip==14)
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
aug16wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=8,day=2, dist="gamma")
aug16wk1<-mutate(aug16wk1,dummy_pulse=rev(seq(1:nrow(aug16wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,aug16wk1)

# August week 2
age1.2016<-filter(length,year==2016 & age ==1 & month ==8 & Trip==15)
# week 2 trip 15

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(70,130,5),132),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(85,105,135),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1
head(age1.2016)
aug16wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=8,day=16,dist="gamma")
aug16wk2<-mutate(aug16wk2,dummy_pulse=rev(seq(1:nrow(aug16wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,aug16wk2)

# August/September week 3
age1.2016<-filter(length,year==2016 & age ==1& Trip==16)
# week 3 trip 16

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(85,135,5),149),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(100,135),c(6),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2016)
aug16wk3<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=8,day=31, dist="gamma")
aug16wk3<-mutate(aug16wk3,dummy_pulse=rev(seq(1:nrow(aug16wk3))))

# add to mixtures
mixtures<-bind_rows(mixtures,aug16wk3)

# September
age1.2016<-filter(length,year==2016 & age == 1 & month == 9 & Trip == 17)
# week 1 trip 17

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
age1param<-mixparam(c(92,105,124,155),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2016)
sept16wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=9,day=13, dist="lnorm")
sept16wk1<-mutate(sept16wk1,dummy_pulse=rev(seq(1:nrow(sept16wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,sept16wk1)

# September 
age1.2016<-filter(length,year==2016 & age ==1& month==9 & Trip==18)
# week 2 trip 18

# check histogram
qplot(mmSL, data=age1.2016, binwidth=5)

# # detremine max and min SL
summarise(age1.2016,min(mmSL),max(mmSL))

# create dataframe with SL only
group2016<-select(age1.2016,mmSL)

# convert to frequency table
age1group<-mixgroup(group2016, breaks = c(0,seq(95,165,5),170),xname=NULL, k = NULL, usecondit = FALSE)

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
sept16wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=9,day=28, dist="gamma")
sept16wk2<-mutate(sept16wk2,dummy_pulse=rev(seq(1:nrow(sept16wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,sept16wk2)

# october
age1.2016<-filter(length,year==2016 & age ==1 & month ==10 & Trip == 19)
# week 1 trip 19

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
age1param<-mixparam(c(110,125,150,165,180),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(110,125,150,170),c(5),pi=NULL)
fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit = FALSE, print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)


# store fit 2 results
head(age1.2016)
oct16wk1<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2016,month=10,day=13, dist="gamma")
oct16wk1<-mutate(oct16wk1,dummy_pulse=rev(seq(1:nrow(oct16wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,oct16wk1)

# october week 2
age1.2016<-filter(length,year==2016 & age ==1 & month ==10 & Trip == 20)
# week 2 trip 20

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
age1param<-mixparam(c(111,125,150,170),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)



# fit 1
head(age1.2016)
oct16wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=10,day=27, dist="gamma")
oct16wk2<-mutate(oct16wk2,dummy_pulse=rev(seq(1:nrow(oct16wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,oct16wk2)

# November
age1.2016<-filter(length,year==2016 & age == 1 & month ==11 & Trip == 21)
# week 1 trip 21

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
nov16<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2016,month=11,day=14, dist="gamma")
nov16<-mutate(nov16,dummy_pulse=rev(seq(1:nrow(nov16))))

# add to mixtures
mixtures<-bind_rows(mixtures,nov16)

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
age1group<-mixgroup(group2017, breaks = c(0,seq(40,135,5),138),xname=NULL, k = NULL, usecondit = FALSE)

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
may17<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=5,day=23,dist="gamma")
may17<-mutate(may17,dummy_pulse=rev(seq(1:nrow(may17))))

# add to mixtures
mixtures<-bind_rows(mixtures,may17)

# July
age1.2017<-filter(length,year==2017 & age ==1 & month == 7 & day <24)

#week 1 July 10

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
july17wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=7,day=10, dist="gamma")
july17wk1<-mutate(july17wk1,dummy_pulse=rev(seq(1:nrow(july17wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,july17wk1)

# july week 2
age1.2017<-filter(length,year==2017 & age ==1 & month == 7 & day >20)

#week 2: July 24

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
july17wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=7,day=24, dist="gamma")
july17wk2<-mutate(july17wk2,dummy_pulse=rev(seq(1:nrow(july17wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,july17wk2)

# August
age1.2017<-filter(length,year==2017 & age ==1 & month == 8&trip ==16)
# week 2

# check histogram
qplot(mmSL, data=age1.2017, binwidth=5)

# # detremine max and min SL
summarise(age1.2017,min(mmSL),max(mmSL))

# create dataframe with SL only
group2017<-select(age1.2017,mmSL)

# convert to frequency table
age1group<-mixgroup(group2017, breaks = c(0,seq(80,165,5),167),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(85,120,142,170),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2017)
aug17<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=8,day=21, dist="gamma")
aug17<-mutate(aug17,dummy_pulse=rev(seq(1:nrow(aug17))))

# add to mixtures
mixtures<-bind_rows(mixtures,aug17)

# September #week1
age1.2017<-filter(length,year==2017 & age ==1 & month == 9 & trip ==17)
# week 1

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
age1param<-mixparam(c(95,110,130,150),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2017)
setp17wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=9,day=5, dist="gamma")
setp17wk1<-mutate(setp17wk1,dummy_pulse=rev(seq(1:nrow(setp17wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,setp17wk1)

# September #week2
age1.2017<-filter(length,year==2017 & age ==1 & month == 9 & trip==18)
# week 1

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
age1param<-mixparam(c(120,140),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store model results
head(age1.2017)
setp17wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=9,day=18, dist="gamma")
setp17wk2<-mutate(setp17wk2,dummy_pulse=rev(seq(1:nrow(setp17wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,setp17wk2)

# October week 1
age1.2017<-filter(length,year==2017 & age ==1 & month == 10 & trip ==19)
# week 1

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
age1param<-mixparam(c(110,130,145,170),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2017)
oct17wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=10,day=2, dist="gamma")
oct17wk1<-mutate(oct17wk1,dummy_pulse=rev(seq(1:nrow(oct17wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,oct17wk1)

# October week 2
age1.2017<-filter(length,year==2017 & age ==1 & month == 10 & trip ==20)
# week 1

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
oct17wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2017,month=10,day=17, dist="gamma")
oct17wk2<-mutate(oct17wk2,dummy_pulse=rev(seq(1:nrow(oct17wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,oct17wk2)

# October/November
age1.2017<-filter(length,year==2017 & age ==1 & trip ==21)
# not enough fish

# November 
age1.2017<-filter(length,year==2017 & age==1 & month ==11)
# not enough fish


View(mixtures)

# ---- add notes -----
mixtures$notes<-NA
mixtures2<-mixtures%>%
  mutate(notes=replace(notes,year==1996 & month==7, "combined trips"))%>%
  mutate(notes=replace(notes,year==1996 & month == 9, "poor model fit"))%>%
  mutate(notes=replace(notes,year==1999 & month ==5,"excluded fish >150"))%>%
  mutate(notes=replace(notes,year==2000 & month ==8, "n=18"))%>%
  mutate(notes=replace(notes,year==2000 & month == 9 & day ==27, "poor model fit"))%>%
  mutate(notes=replace(notes,year==2001 & day ==15, "poor model fit"))%>%
  mutate(notes=replace(notes,year==2004 & month == 7, "combined trips"))%>%
  mutate(notes=replace(notes,year==2010 & month == 7, "combined trips"))%>%
  mutate(notes=replace(notes, year== 2012 & month == 7, "combined trips"))%>%
  mutate(notes=replace(notes,year==2014 & month==7,"poor model fit"))%>%
  mutate(notes=replace(notes, year== 2015 & month == 7, "combined trips"))%>%
  mutate(notes=replace(notes, year==2016 & month == 7, "excluded fish >150"))%>%
  mutate(notes=replace(notes,year==2016 & month == 8 & day ==16, "poor fit towards larger end"))%>%
  mutate(notes=replace(notes,year==2016 & day ==28,"poor model fit"))%>%
  mutate(notes=replace(notes,year==2017 & day==2,"poor model fit"))

# add 'cohort', 'age' to mixtures df
# ---- Final mixtures DF -----
mixtures3<-mixtures2%>%
  mutate(cohort=year-1,age=1)

write.csv(mixtures3,"./data/data-working/age-1-mixture-dist.csv",row.names = FALSE)
