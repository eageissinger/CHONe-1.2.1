## Pulse structure for 2019 AC Newman Sound

# ----- set working directory ----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

# load source functions
source("./pulse-structure/pulse_range_fct.R")

#load packages
library(tidyverse)
library(lubridate)
library(mixdist)

#load data
length<-read.csv("./data/TNNP/TNNP19_length.csv")
length2018<-read.csv("./data/TNNP Revised Data/TNNP/output/length/revised_TNNP2018a.csv")

# ---- check data structure ----
str(length)
summary(length)
head(length)
tail(length)
names(length)
dim(length)

# ---- format dates ----
length%>%
  mutate(Year=as.integer(str_sub(Date,start=1,end = 4)),
         Month=as.integer(str_sub(Date,start=5,end=6)),
         Day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(Date=ymd(paste(Year,Month,Day,sep="-")))->length2

length2018%>%
  mutate(Month=as.integer(str_sub(Date,start=5,end=6)),
         Day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(Date=ymd(paste(Year,Month,Day,sep="-")))->length2018

# Select cod 
cod<-length2%>%
  filter(Species=="AC" | Species=="AC ")

cod18<-length2018%>%
  filter(Species=="AC" | Species =="AC ")

# check cod data
summary(cod)
# filter out outler (for now)
cod<-cod%>%
  filter(mmSL>10)

# ---- multimodal distribution -----
# ---- Trip 9 ----

# Age-0
cod%>%
  filter(Age==0)%>%
  filter(Trip==9)
# Age-1
T9.1<-cod%>%
  filter(Age==1)%>%
  filter(Trip==9)

# check histogram
qplot(mmSL,data=T9.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T9group<-select(T9.1,mmSL)

# min and max sl
summarise(T9.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T9group, breaks=c(0,seq(40,115,5),119),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(50,80),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(50,80,100),c(5),pi=NULL)
fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store data
# go with fit2
trip9.age1<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2019,trip=9,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit2$parameters))))

mixtures<-trip9.age1

# --- Trip 12 ----
# Age 0 
cod%>%
  filter(Age==0 & Trip == 12)

# Age 1
cod%>%
  filter(Age == 1 & Trip == 12)->T12.1

# check histogram
qplot(mmSL,data=T12.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T12group<-select(T12.1,mmSL)

# min and max sl
summarise(T12.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T12group, breaks=c(0,seq(55,120,5),125),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(70,95,115),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip12.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2019,trip=12,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip12.age1)

# ---- Trip 13 ----
# Age 0
cod%>%
  filter(Age ==0 & Trip == 13)

# Age 1
cod%>%
  filter(Age==1 & Trip==13)->T13.1


# check histogram
qplot(mmSL,data=T13.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T13group<-select(T13.1,mmSL)

# min and max sl
summarise(T13.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T13group, breaks=c(0,seq(65,140,5),142),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(75,100,125),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip13.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2019,trip=13,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip13.age1)

# ---- Trip 14 -----
# age 0
cod%>%
  filter(Age==0 & Trip == 14)->T14.0

# check histogram
qplot(mmSL,data=T14.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T14group<-select(T14.0,mmSL)

# min and max sl
summarise(T14.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T14group, breaks=c(0,seq(35,45,5),50),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

#single pulse
fit1<-groupstats(age0group)

# store data
trip14.age0<-bind_cols(fit1)%>%
  mutate(year=2019,trip=14,age=0)%>%
  mutate(dummy_pulse=1)

mixtures<-bind_rows(mixtures,trip14.age0)

# Age 1
cod%>%
  filter(Age == 1 & Trip == 14)->T14.1

# check histogram
qplot(mmSL,data=T14.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T14group<-select(T14.1,mmSL)

# min and max sl
summarise(T14.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T14group, breaks=c(0,seq(65,140,5),144),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(80,100,135),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip14.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2019,trip=14,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip14.age1)

# ---- Trip 15 ----
# Age 0
cod%>%
  filter(Age==0 & Trip == 15)->T15.0

# check histogram
qplot(mmSL,data=T15.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T15group<-select(T15.0,mmSL)

# min and max sl
summarise(T15.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T15group, breaks=c(0,seq(35,55,5),60),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

#single pulse
fit1<-groupstats(age0group)

# store data
trip15.age0<-bind_cols(fit1)%>%
  mutate(year=2019,trip=15,age=0)%>%
  mutate(dummy_pulse=1)

mixtures<-bind_rows(mixtures,trip15.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 15) ->T15.1

# check histogram
qplot(mmSL,data=T15.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T15group<-select(T15.1,mmSL)

# min and max sl
summarise(T15.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T15group, breaks=c(0,seq(85,155,5),158),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(90,110,135),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(90,110,130,145),c(2,3,4,5),pi=NULL)
plot(age1group,age1param2,"gamma")

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store data
# fit2
trip15.age1<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2019,trip=15,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit2$parameters))))

mixtures<-bind_rows(mixtures,trip15.age1)

# ---- Trip 16 ----
# Age 0
cod%>%
  filter(Age==0 & Trip==16)->T16.0

# check histogram
qplot(mmSL,data=T16.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T16group<-select(T16.0,mmSL)

# min and max sl
summarise(T16.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T16group, breaks=c(0,seq(40,80,5),84),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# pulse 2 emerging?
# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(35,50),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)
# single pulse
fit2<-groupstats(age0group)

# store data
trip16.age0<-bind_cols(fit2)%>%
  mutate(year=2019,trip=16,age=0)%>%
  mutate(dummy_pulse=1)

mixtures<-bind_rows(mixtures,trip16.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 16)->T16.1

# check histogram
qplot(mmSL,data=T16.1,binwidth=5)+theme_classic()

# not enough data

# ---- Trip 17 -----
# Age 0
cod%>%
  filter(Age==0 & Trip == 17)->T17.0

# check histogram
qplot(mmSL,data=T17.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T17group<-select(T17.0,mmSL)

# min and max sl
summarise(T17.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T17group, breaks=c(0,seq(45,75,5),78),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# pulse 2 emerging?
# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(35,55),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)
# single pulse
fit2<-groupstats(age0group)

# store data
trip17.age0<-bind_cols(fit2)%>%
  mutate(year=2019,trip=17,age=0)%>%
  mutate(dummy_pulse=1)

mixtures<-bind_rows(mixtures,trip17.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 17)->T17.1

# check histogram
qplot(mmSL,data=T17.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T17group<-select(T17.1,mmSL)

# min and max sl
summarise(T17.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T17group, breaks=c(0,seq(100,145,5),150),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(105,120,145),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
# fit 1
trip17.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2019,trip=17,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip17.age1)

# ---- Trip 18 ----
# Age 0
cod%>%
  filter(Age==0 &Trip==18)->T18.0

# check histogram
qplot(mmSL,data=T18.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T18group<-select(T18.0,mmSL)

# min and max sl
summarise(T18.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T18group, breaks=c(0,seq(50,110,5),112),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(60,90,105),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age0param2<-mixparam(c(60,105),c(5),pi=NULL)

fit2<-mix(age0group,age0param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 10,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store data
# fit 2
trip18.age0<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2019,trip=18,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit2$parameters))))

mixtures<-bind_rows(mixtures,trip18.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 18)%>%
  filter(mmSL<200)->T18.1

# check histogram
qplot(mmSL,data=T18.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T18group<-select(T18.1,mmSL)

# min and max sl
summarise(T18.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T18group, breaks=c(0,seq(110,190,5),194),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(105,125,145,160,200),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)


age1param2<-mixparam(c(125,160,195),c(5),pi=NULL)

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15,usecondit = FALSE,print.level = 0)

summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store data
# fit2
trip18.age1<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2019,trip=18,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit2$parameters))))

mixtures<-bind_rows(mixtures,trip18.age1)

# ---- Trip 19 ----
# Age 0
cod%>%
  filter(Age==0 & Trip == 19)->T19.0

# check histogram
qplot(mmSL,data=T19.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T19group<-select(T19.0,mmSL)

# min and max sl
summarise(T19.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T19group, breaks=c(0,seq(35,95,5),98),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# pulse 2 emerging?
# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(50,75),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age0param2<-mixparam(c(35,55,75),c(5),pi=NULL)

fit2<-mix(age0group,age0param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store data
# fit 1
trip19.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2019,trip=19,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip19.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 19)->T19.1

# check histogram
qplot(mmSL,data=T19.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T19group<-select(T19.1,mmSL)

# min and max sl
summarise(T19.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T19group, breaks=c(0,seq(110,170,5),172),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(110,125,140,155,170),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)
# store results
# fit 1
trip19.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2019,trip=19,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip19.age1)

# ---- Trip 20 ----
# Age 0
cod%>%
  filter(Age==0 & Trip == 20)%>%
  filter(mmSL<120)->T20.0

# check histogram
qplot(mmSL,data=T20.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T20group<-select(T20.0,mmSL)

# min and max sl
summarise(T20.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T20group, breaks=c(0,seq(40,95,5),98),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(45,60,80),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age0param2<-mixparam(c(45,75),c(5),pi=NULL)
plot(age0group,age0param2,"gamma")

fit2<-mix(age0group,age0param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store data
# fit 1
trip20.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2019,trip=20,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip20.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 20)%>%
  filter(mmSL<200)->T20.1

# check histogram
qplot(mmSL,data=T20.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T20group<-select(T20.1,mmSL)

# min and max sl
summarise(T20.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T20group, breaks=c(0,seq(105,180,5),185),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(110,135,160,180),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

#attempt 2
age1param2<-mixparam(c(135,160),c(5),pi=NULL)

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)


# store results
# fit 2
trip20.age1<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2019,trip=20,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit2$parameters))))%>%
  filter(mu>53) # first mixture is not realistic, take out
mixtures<-bind_rows(mixtures,trip20.age1)



# ---- Trip 21 ----
# Age 0

cod%>%
  filter(Age==0 & Trip == 21)->T21.0

# check histogram
qplot(mmSL,data=T21.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T21group<-select(T21.0,mmSL)

# min and max sl
summarise(T21.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T21group, breaks=c(0,seq(25,110,5),116),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(45,70,90),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age0param2<-mixparam(c(45,80),c(5),pi=NULL)
plot(age0group,age0param2,"gamma")

fit2<-mix(age0group,age0param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# attempt 3
age0param3<-mixparam(c(20,45,80),c(5),pi=NULL)
plot(age0group,age0param3,"gamma")

fit3<-mix(age0group,age0param3,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15,usecondit = FALSE,print.level = 0)

summary(fit3)
plot(fit3)

# store data
# fit 2
trip21.age0<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2019,trip=21,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit2$parameters))))

mixtures<-bind_rows(mixtures,trip21.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 21)

# not enough age 1 

# ---- Trip 22 -----
#Age 0
cod%>%
  filter(Age==0 & Trip == 22)->T22.0

# check histogram
qplot(mmSL,data=T22.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T22group<-select(T22.0,mmSL)

# min and max sl
summarise(T22.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T22group, breaks=c(0,seq(40,105,5),111),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(50,80),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
# fit 1
trip22.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2019,trip=22,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip22.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 22)

# not enough Age 1's

# ----- Trip 23 -----
# Age 0
cod%>%
  filter(Age == 0 & Trip == 23)->T23.0

# check histogram
qplot(mmSL,data=T23.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T23group<-select(T23.0,mmSL)

# min and max sl
summarise(T23.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T23group, breaks=c(0,seq(40,100,5),104),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(55,85),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
# fit 1
trip23.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2019,trip=23,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip23.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 23)
# not enough fish

View(mixtures)

# add 'cohort', 'age' to mixtures df
# ---- Final mixtures DF -----
write.csv(mixtures,"./data/output/AC-mixture-dist-2019.csv",row.names = FALSE)


# ---- Part 2: Estimating pulses ----
cod.pulse<-mixtures%>%
  rename(mean=mu,pulse = dummy_pulse)%>%
  mutate(min=mean-sigma,max=mean+sigma)%>%
  select(-sigma,-sigma.se,-mu.se,-pi,-pi.se)

# Add dates to trips
tripdates<-cod%>%
  select(Trip,Year,Month,Day)%>%
  rename(trip=Trip,year=Year,month=Month,day=Day)%>%
  distinct()%>%
  group_by(trip,year,month)%>%
  summarise(day=min(day))
cod.pulse<-left_join(cod.pulse,tripdates)%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  filter(mean<200)

# visualize pulses
cod%>%
  filter(Age<2)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(Age)),alpha=0.25,size=1)+
  geom_point(data=cod.pulse,aes(x=date,y=mean,shape=factor(pulse),fill=factor(age)),size=2)+
  geom_errorbar(data=cod.pulse,aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2019 Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))+
  scale_color_manual(values = c('grey40','blue'))

cod<-cod%>%
  mutate(cohort=2019)%>%
  mutate(cohort=replace(cohort,Age==1,2018))%>%
  mutate(cohort=replace(cohort,Age==2,2017))
cod%>%
  filter(cohort==2019)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL),alpha=0.25,size=1,colour='blue')+
  geom_point(data=filter(cod.pulse,age==0),aes(x=date,y=mean,shape=factor(pulse),fill=factor(age)),size=2)+
  geom_errorbar(data=filter(cod.pulse,age==0),aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2019 Cohort Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

# Prepare 2018 cohort data
cod18<-cod18%>%
  mutate(cohort=2018)%>%
  mutate(cohort=replace(cohort,Age==1,2017))%>%
  mutate(cohort=replace(cohort,Age==2,2016))

cohort.2018<-bind_rows(cod,cod18)%>%
  filter(cohort==2018)%>%
  mutate(Date2=Date+5)

tripdates18<-cod18%>%
  select(Trip,Year,Month,Day)%>%
  distinct()%>%
  group_by(Trip,Year,Month)%>%
  summarise(Day=min(Day))

cod.pulse2018<-cod18%>%
  group_by(Year,Trip,Age,Pulse)%>%
  summarise(mean=mean(mmSL),min=min(mmSL),max=max(mmSL))%>%
  left_join(tripdates18)%>%
  rename(year=Year,trip=Trip,age=Age,pulse=Pulse,month=Month,day=Day)%>%
  filter(age==0)%>%
  bind_rows(filter(cod.pulse,age==1))%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))

#2018 Cohort plot (2019 age 1s)
ggplot()+
  geom_jitter(data=cohort.2018,aes(x=Date2,y=mmSL),alpha=0.25,size=1,colour='blue')+
  geom_point(data=cod.pulse2018,aes(x=date,y=mean,shape=factor(pulse),fill=factor(age)),size=2)+
  geom_errorbar(data=cod.pulse2018,aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2018 Cohort Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

#pdf("allplots.pdf",onefile = TRUE)
#fig1
#fig2
#fig3
#dev.off()

# ---- Part 3: Pulse Assignments ----
pulse.range<-pulse_range(mixtures)

# use min and max for each pulse to then create a dataframe with all length possibilities per pulse
pulse.assign<-data.frame(age=rep(pulse.range$age,pulse.range$max-pulse.range$min+1),
                         trip=rep(pulse.range$trip,pulse.range$max-pulse.range$min+1),
                         pulse=rep(pulse.range$dummy_pulse,pulse.range$max-pulse.range$min+1),
                         mmSL=unlist(mapply(seq,pulse.range$min,pulse.range$max)))
# add additinal info such as date, cohort, pulse so that it is present in dataframe
#check the new dataframe
View(pulse.assign)
summary(pulse.assign)
glimpse(pulse.assign)

# assign pulses to length data
length.pulse<-cod%>%
  select(-Pulse)%>%
  filter(Age<2)%>%
  rename(age=Age,trip=Trip,year=Year)%>%
  left_join(pulse.assign)

# ---- Part 4: Verify pulse assignment ----
# replot pulses

revised<-length.pulse%>%
  group_by(year,trip,age,pulse)%>%
  summarise(mean=mean(mmSL),min=min(mmSL),max=max(mmSL))%>%
  left_join(tripdates)%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))

# visualize pulses
ggplot(revised,aes(x=date,y=mean,shape=factor(pulse),colour=age))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2015 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

pulse.range2<-pulse_range(mixtures)

# Assignment Round 1:
mydata<-pulse.range2%>%
  rename(pulse=dummy_pulse)%>%
  mutate(pulse=replace(pulse,age==1 & trip <= 14 & pulse ==3,5))%>%
  mutate(pulse=replace(pulse,age==1 & trip ==15 & pulse == 4,5))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,age==1 & trip <=14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip==17 & pulse ==2,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 19 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 20 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,age==1 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,age==0 & trip== 18 & pulse==1,NA))%>%
  mutate(pulse=replace(pulse,age==0 & trip>=21 & pulse==2,3))


# deal with the outliers
# start by combining pulse range to length data
# use min and max for each pulse to then create a dataframe with all length possibilities per pulse
pulse.assign<-data.frame(age=rep(pulse.range$age,pulse.range$max-pulse.range$min+1),
                         trip=rep(pulse.range$trip,pulse.range$max-pulse.range$min+1),
                         pulse=rep(pulse.range$dummy_pulse,pulse.range$max-pulse.range$min+1),
                         mmSL=unlist(mapply(seq,pulse.range$min,pulse.range$max)))


pulse_assign2<-data.frame(age=rep(mydata$age,mydata$max-mydata$min+1),
                          trip=rep(mydata$trip,mydata$max-mydata$min+1),
                          pulse=rep(mydata$pulse,mydata$max-mydata$min+1),
                          mmSL=unlist(mapply(seq,mydata$min,mydata$max)))
cod2<-cod%>%
  select(-Pulse)%>%
  rename(trip=Trip,age=Age,year=Year)%>%
  left_join(pulse_assign2)

final<-cod2%>%
  mutate(pulse=replace(pulse,age==1 & trip<=18 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,age==1 & mmSL>225,NA))%>%
  mutate(age=replace(age,age==1 & mmSL>225,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 12 & mmSL<80,5))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 12 & pulse == 2 & mmSL<110,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip ==13 & pulse == 2 & mmSL<118,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 15 & pulse ==2 & mmSL<120,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 16 & is.na(pulse) & mmSL>130,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 16 & is.na(pulse) & mmSL<110,5))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 16 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & pulse==3 & mmSL<113,5))%>%
  mutate(age=replace(age,age==0 & trip == 18 & is.na(pulse) & mmSL>83,1))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 18 & mmSL<110,5))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 20 & mmSL<125,5))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 20 & mmSL>155,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 20 & mmSL>200,1))%>%
  mutate(pulse=replace(pulse,age==1 & trip >20 & mmSL<127,5))%>%
  mutate(pulse=replace(pulse,age==1 & trip >20 & mmSL>200,1))%>%
  mutate(pulse=replace(pulse,age==1 & trip ==21 & mmSL>150,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 22 & mmSL>175,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip >=21 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 21 & mmSL>200,1))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 19 & mmSL<40,3))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 20 & pulse==2 & mmSL>66,1))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 21 & pulse==1 & mmSL<74,2))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 21 & mmSL<31,4))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 22 & pulse==1 & mmSL<76,2))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 22 & mmSL<40,4))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 23 & pulse == 1 & mmSL<79,2))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 23 & mmSL<45,4))%>%
  mutate(pulse=replace(pulse,age ==0 &trip==20 & mmSL>100,NA))%>%
  mutate(pulse=replace(pulse,age ==0 &trip==21 & mmSL>110,NA))%>%
  mutate(age=replace(age, age ==0 & is.na(pulse)& mmSL>83,1))%>%
  mutate(pulse=replace(pulse,age==0 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,age==1 & is.na(pulse),5))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 18 & pulse == 2,1))
  
pulse.range2019<-final%>%
  group_by(age,year,trip,pulse)%>%
  summarise(mean=mean(mmSL),minSL=min(mmSL),maxSL=max(mmSL))%>%
  filter(age!=2)%>%
  ungroup()%>%
  left_join(tripdates)%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))
write.csv(pulse.range2019,"data/output/pulse_range2019.csv",row.names = FALSE)  

final%>%
  filter(age<2)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(age)),alpha=0.25,size=1)+
  geom_point(data=pulse.range2019,aes(x=date,y=mean,shape=factor(pulse),fill=as.character(age)),size=2)+
  geom_errorbar(data=pulse.range2019,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2019 Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))+
  scale_color_manual(values = c('red','blue'))

final%>%
  filter(age==0)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(pulse)),alpha=0.25,size=1)+
  geom_point(data=filter(pulse.range2019,age==0),aes(x=date,y=mean,shape=factor(pulse),fill=as.character(age)),size=2)+
  geom_errorbar(data=filter(pulse.range2019,age==0),aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2019 Newman Sound Atlantic cod: Age 0")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

final%>%
  filter(age==1)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(pulse)),alpha=0.25,size=1)+
  geom_point(data=filter(pulse.range2019,age==1),aes(x=date,y=mean,shape=factor(pulse),fill=as.character(age)),size=2)+
  geom_errorbar(data=filter(pulse.range2019,age==1),aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2019 Newman Sound Atlantic cod: Age 1")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

#pdf("final2019.pdf",onefile = TRUE)
#fig4
#fig5
#fig6
#dev.off()

# ---- Part 5: Final assignment ----
summary(final)
head(length)
summary(pulse.range2019)

final2<-final%>%
  rename(Age2=age)

# use min and max for each pulse to then create a dataframe with all length possibilities per pulse
pulse.assign<-data.frame(trip=rep(pulse.range2019$trip,pulse.range2019$maxSL-pulse.range2019$minSL+1),
                         age=rep(pulse.range2019$age,pulse.range2019$maxSL-pulse.range2019$minSL+1),
                         year=rep(pulse.range2019$year,pulse.range2019$maxSL-pulse.range2019$minSL+1),
                         pulse=rep(pulse.range2019$pulse,pulse.range2019$maxSL-pulse.range2019$minSL+1),
                         mmSL=unlist(mapply(seq,pulse.range2019$minSL,pulse.range2019$maxSL)))
pulsefinal<-pulse.assign%>%
  mutate(Species="AC")%>%
  rename(Trip=trip,Age=age,Year=year,Pulse=pulse)
  
length3<-length%>%
  mutate(Year=as.integer(str_sub(Date,start=1,end=4)))%>%
  select(-Pulse)

final2<-left_join(length3,pulsefinal)


final2%>%
  filter(Species=="AC" & is.na(Pulse))

final3<-final2%>%
  mutate(Age=replace(Age, Species == "AC" & is.na(Pulse) & Age == 1 & mmSL>23,2))%>%
  mutate(Age=replace(Age, Species == "AC" &is.na(Pulse) & Age == 0 & Trip == 16,1))%>%
  mutate(Age=replace(Age, Species == "AC" &is.na(Pulse) & Age == 0 & Trip == 18,1))%>%
  mutate(Age=replace(Age, Species == "AC" &is.na(Pulse) & Age == 0 & Trip >=20,1))%>%
  select(-Pulse)%>%
  left_join(pulsefinal)

final3%>%filter(Species=="AC" & is.na(Pulse))

summary(final3)
dim(final3)
dim(length)

write.csv(final3, "./data/TNNP/TNNP19_length_revised_2020-03-19.csv",row.names = FALSE)

final3%>%
  filter(Age==0)%>%
  filter(Species=="AC")%>%
  ggplot(aes(x=Julian.Date,y=mmSL,colour=as.character(Pulse)))+geom_point()
