#Size Frequency
# Newman Sound
# Age 1 Atlantic cod

#---- Purpose ----
# Determine pulse structure of age 1 cod using mixture models

# ----- set working directory ----
setwd("C:/Users/eageissinger/Documents/Emilie-Lab-comp/")

#load packages
library(tidyverse)
library(lubridate)
library(mixdist)
library(rlist)

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

# July data is combination of entire month
# not enough data to split by trip

# check histogram
qplot(mmSL,data=age1.1996,binwidth=5)+theme_classic()

# min and max sl
summarise(age1.1996,min(mmSL),max(mmSL),n())

#create dataframe with only mmSL
group1996<-select(age1.1996,mmSL)

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(group1996, breaks=c(0,seq(90,130,5),135),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(95,108,125),c(5),pi=c(0.1,0.60,0.25))
plot(age1group,age1param,"gamma")
age1param<-mixparam(c(93,108,123),c(3),pi=c(0.11,0.47,0.17))
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10,usecondit = FALSE,print.level = 0)
fitted.mix(fit1)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)

# store results
head(age1.1996)
july96<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1996,month=7,day=24,dist="gamma")# date = July 24 (mid between Trip 13 and 14)
july96<-mutate(july96,dummy_pulse=rev(seq(1:nrow(july96))))

mixtures<-july96

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
age1param<-mixparam(c(130,145,158,190),c(3),c(0.25,0.25,0.25,0.25))
plot(age1group,age1param,"gamma")
age1param<-mixparam(c(125,135,150,160,190),c(2),pi=NULL)

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)

# store data
# note very poor fit!
head(age1.1996)
sept96<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=1996,month=9,day=26,dist="gamma")# date = Sept 26
sept96<-mutate(sept96,dummy_pulse=rev(seq(1:nrow(sept96))))

mixtures<-bind_rows(mixtures,sept96)

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
qplot(mmSL,data=age1.1999,binwidth=5)+theme_classic()

# set cutoff to 150 mm for all fish
# take out of dataset
age1.1999<-filter(length,age==1 & year == 1999 & month ==5 & mmSL<=150)

# check histogram again
qplot(mmSL,data=age1.1999,binwidth=5)

# detremine max and min SL
summarise(age1.1999,min(mmSL),max(mmSL), n())

# create dataframe with SL only
group1999<-select(age1.1999,mmSL)

# convert to frequency table
age1group<-mixgroup(group1999, breaks = c(0,seq(40,125,5),130),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

#Single component
par<-groupstats(age1group)

# store data
head(age1.1999)
may99<-bind_cols(par)%>%
  mutate(year=1999,month=5,day=25,dist="gamma")
may99<-mutate(may99,dummy_pulse=rev(seq(1:nrow(may99))))
# add May 1999 to mixtures

mixtures<-bind_rows(mixtures,may99)

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

# July
age1.2000<-filter(length,year == 2000 & age == 1 & month == 7 & day <21)

# first week of July 

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
age1param<-mixparam(c(90,117,142),c(4),c(.5,.25,.25))
plot(age1group,age1param,"gamma")
age1param<-mixparam(c(90,117,142),c(5,7,10),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)

# Go with fit 3, not great but better than nothing!
head(age1.2000)
july00<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=7,day=17,dist="gamma")
july00<-mutate(july00,dummy_pulse=rev(seq(1:nrow(july00))))

#add to mixtures
mixtures<-bind_rows(mixtures,july00)

# August
age1.2000<-filter(length,year == 2000 & age == 1 & month == 8 & Trip==16)
# Try with 18....

qplot(mmSL, data=age1.2000, binwidth=5)

# # detremine max and min SL
summarise(age1.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age1.2000,mmSL)

# convert to frequency table
age1group<-mixgroup(group2000, breaks = c(0,seq(90,140,5),145),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(103,135),c(4),pi=NULL)
plot(age1group,age1param,"gamma")


# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

#2nd attempt
age1param<-mixparam(c(92,102,112,135),c(2,3,4,5),pi=NULL)
plot(age1group,age1param,"gamma")


fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps=15,usecondit = FALSE,print.level=0)
summary(fit2)
plot(fit2)


# Store fit 1 data
head(age1.2000)
aug00<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=8,day=28,dist="gamma") # August 28
aug00<-mutate(aug00,dummy_pulse=rev(seq(1:nrow(aug00))))

#add to mixtures
mixtures<-bind_rows(mixtures,aug00)

# September
age1.2000<-filter(length,year==2000,age==1 & month ==9 & Trip ==17)
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
age1param<-mixparam(c(110,130,150),c(4),pi=c(0.45,0.45,0.1))
plot(age1group,age1param,"gamma")
age1param<-mixparam(c(110,130,150),c(5,6,7),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma", mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)



# store results
head(age1.2000)
sept00wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=9,day=12, dist="gamma") # Sept 12
sept00wk1<-mutate(sept00wk1,dummy_pulse=rev(seq(1:nrow(sept00wk1))))

#add to mixtures
mixtures<-bind_rows(mixtures,sept00wk1)

# September week 2
age1.2000<-filter(length,year==2000,age==1 & month ==9 & Trip ==18)
# Trip 18

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
sept00wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=9,day=27,dist="gamma") # Sept 27
sept00wk2<-mutate(sept00wk2,dummy_pulse=rev(seq(1:nrow(sept00wk2))))

#add to mixtures
mixtures<-bind_rows(mixtures,sept00wk2)

# October
age1.2000<-filter(length,year==2000 & age == 1 & month == 10 & Trip == 19)
# Trip 19

qplot(mmSL, data=age1.2000, binwidth=5)

# # detremine max and min SL
summarise(age1.2000,min(mmSL),max(mmSL))

# create dataframe with SL only
group2000<-select(age1.2000,mmSL)

# convert to frequency table
age1group<-mixgroup(group2000, breaks = c(0,seq(115,160,5),165),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(118,130,145,160),c(3),pi=NULL)
plot(age1group,age1param,"gamma")


# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)

plot(fit1, root=T)
anova(fit1)
coef.mix(fit1)

age1param<-mixparam(c(118,145),c(3),pi=NULL)
fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit2)
plot(fit2,root=T)

# Fit 1
# store fit 1 results
head(age1.2000)
oct00wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=10,day=10,dist="gamma") # Oct 10
oct00wk1<-mutate(oct00wk1,dummy_pulse=rev(seq(1:nrow(oct00wk1))))

#add to mixtures
mixtures<-bind_rows(mixtures,oct00wk1)

# October Trip 20
age1.2000<-filter(length,year==2000 & age == 1 & month == 10 & Trip == 20)

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
oct00wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2000,month=10,day=24,dist="gamma") # Oct 24
oct00wk2<-mutate(oct00wk2,dummy_pulse=rev(seq(1:nrow(oct00wk2))))

#add to mixtures
mixtures<-bind_rows(mixtures,oct00wk2)

# November
age1.2000<-filter(length,year==2000 & age ==1 & month == 11 & Trip ==21)
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
age1param<-mixparam(c(121,145,168,183),c(3),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param<-mixparam(c(115,140,170),c(7,8,9),pi=NULL)
plot(age1group,age1param,"gamma")

fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps=15,usecondit = FALSE,print.level=0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# Fit 2. Store data
head(age1.2000)
nov00<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2000,month=11,day=13,dist="gamma") # Nov 13
nov00<-mutate(nov00,dummy_pulse=rev(seq(1:nrow(nov00))))

#add to mixtures
mixtures<-bind_rows(mixtures,nov00)

# ---- age 1 2001 ----

# May
age1.2001<-filter(length,year == 2001 & age == 1 & month == 5)

# no fish in May

# July
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
fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)


# fit 2. Store results
head(age1.2001)
july01<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2001,month=7,day=18, dist="gamma")
july01<-mutate(july01,dummy_pulse=rev(seq(1:nrow(july01))))

#add to mixtures
mixtures<-bind_rows(mixtures,july01)

# August 
age1.2001<-filter(length,year==2001 & age ==1 & month ==8)
# not enough data

# September
age1.2001<-filter(length,year==2001 & age ==1 & month ==9 & Trip ==18)
# not enough data

# October
age1.2001<-filter(length,year==2001 & age ==1 & month ==10& Trip ==19)
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
age1param<-mixparam(c(110,125,140,175),c(2,3,4,5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)


# fit 1. store data
head(age1.2001)
oct01wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=10,day=2,dist="gamma")
oct01wk1<-mutate(oct01wk1,dummy_pulse=rev(seq(1:nrow(oct01wk1))))

#add to mixtures
mixtures<-bind_rows(mixtures,oct01wk1)

# October
age1.2001<-filter(length,year==2001 & age ==1 & month ==10& Trip ==20)
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
oct01wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=10,day=16,dist="gamma") # october 16
oct01wk2<-mutate(oct01wk2,dummy_pulse=rev(seq(1:nrow(oct01wk2))))

#add to mixtures
mixtures<-bind_rows(mixtures,oct01wk2)

# November

age1.2001<-filter(length,year==2001 & age ==1 & month ==11 & Trip == 21)
# Trip 21

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
age1param<-mixparam(c(140,160,180),c(3),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)



# store results
head(age1.2001)
nov01wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=11,day=1,dist="gamma") # november 1
nov01wk1<-mutate(nov01wk1,dummy_pulse=rev(seq(1:nrow(nov01wk1))))

#add to mixtures
mixtures<-bind_rows(mixtures,nov01wk1)

# November # Trip 22

age1.2001<-filter(length,year==2001 & age ==1 & month ==11 & Trip == 22)


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
age1param<-mixparam(c(145,160,180,210),c(4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


age1param<-mixparam(c(145,155,180),c(4),pi=NULL)
fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps=5,usecondit = FALSE,print.level=0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)


# fit1..
# poor model fit
head(age1.2001)
nov01wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2001,month=11,day=15,dist="gamma") # november 15
nov01wk2<-mutate(nov01wk2,dummy_pulse=rev(seq(1:nrow(nov01wk2))))

#add to mixtures
mixtures<-bind_rows(mixtures,nov01wk2)

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
age1param<-mixparam(c(45,65,90),c(2,3,4),pi=NULL)
plot(age1group,age1param)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store fit 1 results
head(age1.2002)
may02<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2002,month=5,day=23,dist="gamma")
may02<-mutate(may02,dummy_pulse=rev(seq(1:nrow(may02))))
# add to mixtures
mixtures<-bind_rows(mixtures,may02)

# July
age1.2002<-filter(length,year==2002 &age==1 & month==7)
age1.2002<-filter(length,year==2002 & age==1 & month==7 & day<20)

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
age1param<-mixparam(c(60,77,110),c(3),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(70,100),c(2,3),pi=NULL)

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps=15,usecondit = FALSE,print.level=0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)


#Fit 2. store results 
head(age1.2002)
july02w1<-bind_cols(fit2$parameters, fit2$se)%>%
  mutate(year=2002,month=7,day=10,dist="gamma")
july02w1<-mutate(july02w1,dummy_pulse=rev(seq(1:nrow(july02w1))))
# add to mixtures
mixtures<-bind_rows(mixtures,july02w1)

# July trip 2
age1.2002<-filter(length,year==2002 & age==1 & month==7 & day>20)

# check histogram
qplot(mmSL, data=age1.2002, binwidth=5)

# # detremine max and min SL
summarise(age1.2002,min(mmSL),max(mmSL))

# create dataframe with SL only
group2002<-select(age1.2002,mmSL)

# convert to frequency table
age1group<-mixgroup(group2002, breaks = c(0,seq(70,115,5),120),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,100),c(3),pi=NULL)
plot(age1group,age1param)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


# store results
head(age1.2002)
july02w2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2002,month=7,day=24,dist="gamma")
july02w2<-mutate(july02w2,dummy_pulse=rev(seq(1:nrow(july02w2))))

# add to mixtures
mixtures<-bind_rows(mixtures,july02w2)

# August
age1.2002<-filter(length,year==2002 & age ==1 & month ==8 & Trip==16)
# no fish

# September
age1.2002<-filter(length,year==2002 & age == 1 & month == 9)
# not enough in sept

# October
age1.2002<-filter(length,year==2002 & age == 1 & month == 10)
# no fish

# November
age1.2002<-filter(length,year==2002 & age == 1 & month == 11)
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
age1param<-mixparam(c(50,80,115),c(2,3,4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(60,80,100),c(2,3,4),pi=NULL)

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps=15,usecondit = FALSE,print.level=0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)


# Fit 1 is the best I can do, won't let me drop to 2....
head(age1.2003)
may03<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=5,day=28,dist="gamma")
may03<-mutate(may03,dummy_pulse=rev(seq(1:nrow(may03))))

# add to mixtures
mixtures<-bind_rows(mixtures,may03)

# July
age1.2003<-filter(length,year==2003 & age==1 & month==7)
age1.2003<-filter(length,year==2003 & age==1 & month==7 & day<20 & mmSL <=150)

# check histogram
qplot(mmSL, data=age1.2003, binwidth=5)

# # detremine max and min SL
summarise(age1.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age1.2003,mmSL)

# convert to frequency table
age1group<-mixgroup(group2003, breaks = c(0,seq(70,135,5),140),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(75,95,110,130),c(2,3,4,5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)



#store results
head(age1.2003)
july03w1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=7,day=14,dist="gamma")
july03w1<-mutate(july03w1,dummy_pulse=rev(seq(1:nrow(july03w1))))

# add to mixtures
mixtures<-bind_rows(mixtures,july03w1)

# July week 2
age1.2003<-filter(length,year==2003 & age==1 & month==7 & day>20 & mmSL <=150)

# check histogram
qplot(mmSL, data=age1.2003, binwidth=5)

# # detremine max and min SL
summarise(age1.2003,min(mmSL),max(mmSL))

# create dataframe with SL only
group2003<-select(age1.2003,mmSL)

# convert to frequency table
age1group<-mixgroup(group2003, breaks = c(0,seq(80,140,5),143),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(75,100,118,133),c(3),pi=NULL)
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
july03w2<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2003,month=7,day=31,dist="gamma")
july03w2<-mutate(july03w2,dummy_pulse=rev(seq(1:nrow(july03w2))))

mixtures<-bind_rows(mixtures,july03w2)

#August week 2
age1.2003<-filter(length,year==2003 & age == 1 & month == 8 & Trip == 16)

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
age1param<-mixparam(c(100,110,135,140),c(2,3,4,5),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param<-mixparam(c(95,110,140),c(3),pi=NULL)
fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

#Store fit 2 results
head(age1.2003)
aug03w2<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2003,month=8,day=27,dist="gamma") # Aug 27
aug03w2<-mutate(aug03w2,dummy_pulse=rev(seq(1:nrow(aug03w2))))

# add to mixtures
mixtures<-bind_rows(mixtures,aug03w2)

# september
age1.2003<-filter(length,year==2003 & age ==1 & month ==9 & Trip ==17)
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
age1param<-mixparam(c(105,120,135,155),c(3),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


#Fit 1
head(age1.2003)
sept03wk1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=9,day=9,dist="gamma") # sept 9
sept03wk1<-mutate(sept03wk1,dummy_pulse=rev(seq(1:nrow(sept03wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,sept03wk1)

# september week 2
age1.2003<-filter(length,year==2003 & age ==1 & month ==9 & Trip ==18)
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
age1param<-mixparam(c(115,125,140,170),c(4),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

#store results
head(age1.2003)
sept03wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=9,day=25,dist="gamma") # sept 25
sept03wk2<-mutate(sept03wk2,dummy_pulse=rev(seq(1:nrow(sept03wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,sept03wk2)

# October
age1.2003<-filter(length,year==2003 & age ==1 & month == 10 & Trip==19)
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
age1param<-mixparam(c(110,130,150,170,190),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param.2<-mixparam(c(110,125,150,170),c(2,3,4,5),pi=NULL)

fit2<-mix(age1group,age1param.2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 4, usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

#Fit 2
head(age1.2003)
oct03wk1<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2003,month=10,day=9,dist="gamma") # oct 9
oct03wk1<-mutate(oct03wk1,dummy_pulse=rev(seq(1:nrow(oct03wk1))))

# add to mixtures
mixtures<-bind_rows(mixtures,oct03wk1)

# October week 2
age1.2003<-filter(length,year==2003 & age ==1 & month == 10 & Trip==20)
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
age1param<-mixparam(c(110,130,155,175,200),c(2,3,4,5,6),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param<-mixparam(c(110,135,165,200),c(4,5,6,7),pi=NULL)
fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

#Fit 1
head(age1.2003)
oct03wk2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2003,month=10,day=24,dist="gamma") # oct 24
oct03wk2<-mutate(oct03wk2,dummy_pulse=rev(seq(1:nrow(oct03wk2))))

# add to mixtures
mixtures<-bind_rows(mixtures,oct03wk2)

# November
age1.2003<-filter(length,year==2003 & age ==1 & month ==11)
# not enough fish

# ---- age 1 2004 ----

# May 
age1.2004<-filter(length,year==2004 & age == 1 & month ==5)

# not enough data

# July
age1.2004<-filter(length,year==2004 & age ==1 & month == 7)

# use entire month

# check histogram
qplot(mmSL, data=age1.2004, binwidth=5)

# # detremine max and min SL
summarise(age1.2004,min(mmSL),max(mmSL))

# create dataframe with SL only
group2004<-select(age1.2004,mmSL)

# convert to frequency table
age1group<-mixgroup(group2004, breaks = c(0,seq(80,125,5),130),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,92,108,130),c(2,3,4,5),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(80,92,105,127),c(2,3,4,5),pi=NULL)

# Store fit 1 results
head(age1.2004)
july04<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2004,month=7,day=24,dist="gamma") # halfway between trip 13 and 14
july04<-mutate(july04,dummy_pulse=rev(seq(1:nrow(july04))))

# add to mixtures
mixtures<-bind_rows(mixtures,july04)

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
age1group<-mixgroup(group2005, breaks = c(0,seq(40,100,5),105),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(45,60,80),c(2,3,4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2005)
may05<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=5,day=17,dist="gamma")
may05<-mutate(may05,dummy_pulse=rev(seq(1:nrow(may05))))

# add to mixtures
mixtures<-bind_rows(mixtures,may05)

# July
age1.2005<-filter(length,year==2005 & age==1 & month ==7 & day <10)
# use first week only

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
july05<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=7,day=6,dist="gamma")
july05<-mutate(july05,dummy_pulse=rev(seq(1:nrow(july05))))

# add to mixtures
mixtures<-bind_rows(mixtures,july05)

# August
age1.2005<-filter(length,year==2005 & age==1 & month ==8)
# no data

# September
age1.2005<-filter(length,year==2005 & age==1 & month ==9 & Trip ==18)
# Trip 18

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
age1param<-mixparam(c(135,170),c(3),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

#Store results
head(age1.2005)
sept05<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=9,day=19,dist="gamma")
sept05<-mutate(sept05,dummy_pulse=rev(seq(1:nrow(sept05))))

# add to mixtures
mixtures<-bind_rows(mixtures,sept05)

# October
age1.2005<-filter(length, year==2005 & age ==1 & month ==10 & Trip ==20)
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
age1param<-mixparam(c(140,165,190),c(2,3,4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param<-mixparam(c(140,165,190,200),c(4),pi=NULL)

# fit mixture
fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

fit3<-mix(age1group,age1param,dist="lnorm",mixconstr(consigma = "CCV"),
          emsteps = 5, usecondit = FALSE,print.level = 0)
summary(fit3)
plot(fit3)

# fit 1 model results
head(age1.2005)
oct05<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2005,month=10,day=18,dist="norm")
oct05<-mutate(oct05,dummy_pulse=rev(seq(1:nrow(oct05))))

# add to mixtures
mixtures<-bind_rows(mixtures,oct05)

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
age1param<-mixparam(c(45,57,70),c(2,3,4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param<-mixparam(c(40,45,57,70),c(3),pi=NULL)
# fit mixture
fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=20, usecondit=FALSE, print.level=0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)


# Fit 1
head(age1.2006)
may06<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2006,month=5,day=10, dist="gamma")
may06<-mutate(may06,dummy_pulse=rev(seq(1:nrow(may06))))

# add to mixtures
mixtures<-bind_rows(mixtures,may06)

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
age1group<-mixgroup(group2007, breaks = c(0,seq(45,90,5),95),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(45,60,80),c(2,3,4),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param<-mixparam(c(40,58,80),c(3),pi=NULL)
# fit mixture
fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

age1param2<-mixparam(c(45,60,80,100),c(2,3,4,5),pi=NULL)

# fit mixture
fit3<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit3)
plot(fit3)
plot(fit3,root=T)

# Fit 3 is best
head(age1.2007)
may07<-bind_cols(fit3$parameters,fit3$se)%>%
  mutate(year=2007,month=5,day=7,dist="gamma")
may07<-mutate(may07,dummy_pulse=rev(seq(1:nrow(may07))))

# add to mixtures
mixtures<-bind_rows(mixtures,may07)

# July
age1.2007<-filter(length,year==2007 & age == 1 & month == 7)
age1.2007<-filter(length,year==2007 & age == 1 & month == 7 & day <18)

# check histogram
qplot(mmSL, data=age1.2007, binwidth=5)

# # detremine max and min SL
summarise(age1.2007,min(mmSL),max(mmSL))

# create dataframe with SL only
group2007<-select(age1.2007,mmSL)

# convert to frequency table
age1group<-mixgroup(group2007, breaks = c(0,seq(60,90,5),95),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(70,80,100),c(2,3,4),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param<-mixparam(c(70,85),c(3),pi=NULL)
fit2<-mix(age1group,age1param,dist="gamma", mixconstr(consigma="CCV"),
          emsteps = 15, usecondit = FALSE, print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store fit 2 results
head(age1.2007)
july07<-bind_cols(fit2$parameters, fit2$se)%>%
  mutate(year=2007,month=7,day=3,dist="gamma")
july07<-mutate(july07,dummy_pulse=rev(seq(1:nrow(july07))))

# add to mixtures
mixtures<-bind_rows(mixtures,july07)

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
age1group<-mixgroup(group2008, breaks = c(0,seq(45,120,5),125),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(60,90,120),c(2,3,4),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1, root=T)


#store model results
head(age1.2008)
may08<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=5,day=20,dist="gamma")
may08<-mutate(may08,dummy_pulse=rev(seq(1:nrow(may08))))

# add to mixtures
mixtures<-bind_rows(mixtures,may08)

# July 
# week 1
age1.2008<-filter(length,year==2008 & age == 1 & month == 7 & mmSL<=150)
age1.2008<-filter(length,year==2008 & age == 1 & month == 7 & mmSL<=150 & day < 5)
# check histogram
qplot(mmSL, data=age1.2008, binwidth=5)

# # detremine max and min SL
summarise(age1.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age1.2008,mmSL)

# convert to frequency table
age1group<-mixgroup(group2008, breaks = c(0,seq(55,100,5),105),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(70,110),c(2,3),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2008)
july08w1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=7,day=3,dist="norm")
july08w1<-mutate(july08w1,dummy_pulse=rev(seq(1:nrow(july08w1))))

# add to mixtures
mixtures<-bind_rows(mixtures,july08w1)

#July week 2
age1.2008<-filter(length,year==2008 & age == 1 & month == 7 & mmSL<=150)
age1.2008<-filter(length,year==2008 & age == 1 & month == 7 & mmSL<=150 & day > 10 & day < 20)
# check histogram
qplot(mmSL, data=age1.2008, binwidth=5)

# # detremine max and min SL
summarise(age1.2008,min(mmSL),max(mmSL))

# create dataframe with SL only
group2008<-select(age1.2008,mmSL)

# convert to frequency table
age1group<-mixgroup(group2008, breaks = c(0,seq(60,140,5),144),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(80,100,130),c(2,3,4),pi=NULL)

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store model results
head(age1.2008)
july08w2<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=7,day=16,dist="gamma")
july08w2<-mutate(july08w2,dummy_pulse=rev(seq(1:nrow(july08w2))))

# add to mixtures
mixtures<-bind_rows(mixtures,july08w2)

# August
age1.2008<-filter(length, year==2008 & age ==1 & month ==8 & Trip ==16)
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
age1param<-mixparam(c(90,110,120,135,155),c(2,3,4,5,6),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# fit 1 results
head(age1.2008)
aug08<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=8,day=28,dist="gamma")
aug08<-mutate(aug08,dummy_pulse=rev(seq(1:nrow(aug08))))

# add to mixtures
mixtures<-bind_rows(mixtures,aug08)

# September
age1.2008<-filter(length,year==2008 & age==1 & month == 9 & Trip ==18)
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
sept08<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2008,month=9,day=29,dist="gamma")
sept08<-mutate(sept08,dummy_pulse=rev(seq(1:nrow(sept08))))

# add to mixtures
mixtures<-bind_rows(mixtures,sept08)

# October
age1.2008 <- filter(length, year==2008 & age == 1 & month ==10 & Trip ==20)
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
may09<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2009,month=5,day=12,dist="gamma")
may09<-mutate(may09,dummy_pulse=rev(seq(1:nrow(may09))))

# add to mixtures
mixtures<-bind_rows(mixtures,may09)

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

age1param<-mixparam(c(55,75),c(4),pi=NULL)
fit2<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)


# Store fit 1 results
head(age1.2010)
may10<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2010,month=5,day=17,dist="norm")
may10<-mutate(may10,dummy_pulse=rev(seq(1:nrow(may10))))

# add to mixtures
mixtures<-bind_rows(mixtures,may10)

# July week 1
age1.2010<-filter(length,year==2010 & age ==1 & month == 7 & day < 25)

# use entire month

# check histogram
qplot(mmSL, data=age1.2010, binwidth=5)

# # detremine max and min SL
summarise(age1.2010,min(mmSL),max(mmSL))

# create dataframe with SL only
group2010<-select(age1.2010,mmSL)

# convert to frequency table
age1group<-mixgroup(group2010, breaks = c(0,seq(75,110,5),115),xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency data
plot(age1group)

# set parameters
age1param<-mixparam(c(78,100),c(2,3),pi=NULL)
plot(age1group,age1param,"gamma")

# fit mixture
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=5, usecondit=FALSE, print.level=0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)
# I guess fit 1..... They all stink, I think there was high mortality between May and July.... Just a thought
head(age1.2010)
july10<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2010,month=7,day=12,dist="gamma")
july10<-mutate(july10,dummy_pulse=rev(seq(1:nrow(july10))))

# add to mixtures
mixtures<-bind_rows(mixtures,july10)

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
