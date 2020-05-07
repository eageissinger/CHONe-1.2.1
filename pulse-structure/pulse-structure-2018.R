## Pulse structure for 2018 AC Newman Sound

# ----- set working directory ----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

# load source functions
source("./pulse-structure/pulse_range_fct.R")

#load packages
library(tidyverse)
library(lubridate)
library(mixdist)

#load data
length<-read.csv("./data/TNNP Revised Data/TNNP/length/TNNP2018a.csv")
length2017<-read.csv("./data/TNNP Revised Data/TNNP/output/length/revised_TNNP2017a.csv")

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

length2017%>%
  mutate(Month=as.integer(str_sub(Date,start=5,end=6)),
         Day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(Date=ymd(paste(Year,Month,Day,sep="-")))->length2017

# Select cod 
cod<-length2%>%
  filter(Species=="AC" | Species == "AC ")

cod17<-length2017%>%
  filter(Species=="AC" | Species == "AC ")

# check cod data
summary(cod)

# ---- multimodal distribution -----

# ----- Trip 9 -----
T9.1<-cod%>%
  filter(Age==1 & Trip == 9)

# check histogram
qplot(mmSL, data=T9.1, binwidth=5)

# create dataframe with SL only
T9group<-select(T9.1,mmSL)

# # detremine max and min SL
summarise(T9.1,min(mmSL),max(mmSL))

# convert to frequency table
age1group<-mixgroup(T9group, breaks = c(0,seq(35,125,5),132),xname=NULL, k = NULL, usecondit = FALSE)

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
trip9.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,trip=9,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

# add to mixtures
mixtures<-trip9.age1

# ------ Trip 13 -----
T13.1<-cod%>%
  filter(Age==1)%>%
  filter(Trip==13)

# check histogram
qplot(mmSL, data=T13.1, binwidth=5)

# create dataframe with SL only
T13group<-select(T13.1,mmSL)

# # detremine max and min SL
summarise(T13.1,min(mmSL),max(mmSL))

# convert to frequency table
age1group<-mixgroup(T13group, breaks = c(0,seq(60,185,5),192),xname=NULL, k = NULL, usecondit = FALSE)

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
trip13.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,trip=13,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip13.age1)

# ---- Trip 14 -----
# Age 1
T14.1<-cod%>%
  filter(Age==1 & Trip == 14)

# check histogram
qplot(mmSL, data=T14.1, binwidth=5)

# create dataframe with SL only
T14group<-select(T14.1,mmSL)

# # detremine max and min SL
summarise(T14.1,min(mmSL),max(mmSL))

# convert to frequency table
age1group<-mixgroup(T14group, breaks = c(0,seq(70,140,5),145),xname=NULL, k = NULL, usecondit = FALSE)

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
trip14.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,trip=14,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip14.age1)

# ----- Trip 15 ------

# Age 0
T15.0<-cod%>%
  filter(Age==0 & Trip == 15)

# check histogram
qplot(mmSL, data = T15.0, binwidth = 5)

# create dataframe with SL only 
T15group <- select(T15.0, mmSL)

# determine min and max
summarise(T15.0, min(mmSL), max(mmSL))

# convert to frequency table 
age0group<-mixgroup(T15group, breaks = c(0,seq(35,55,5),57),
                     xname=NULL, k = NULL, usecondit = FALSE)

# plot frequency table 
plot(age0group)

# parameters
par<-groupstats(age0group)

# store results
trip15.age0<-par%>%
  mutate(year=2018,trip=15,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(par))))

mixtures<-bind_rows(mixtures,trip15.age0)

# Age 1
T15.1<-cod%>%
  filter(Age==1 & Trip ==15)

# check histogram
qplot(mmSL, data=T15.1, binwidth=5)

# create dataframe with SL only
T15group<-select(T15.1,mmSL)

# # detremine max and min SL
summarise(T15.1,min(mmSL),max(mmSL))

# convert to frequency table
age1group<-mixgroup(T15group, breaks = c(0,seq(80,180,5),185),xname=NULL, k = NULL, usecondit = FALSE)

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
trip15.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,trip=15,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip15.age1)

# ----- Trip 16 -----

# Age 0 
T16.0<-cod%>%
  filter(Age==0 & Trip == 16)

# check histogram 
qplot(mmSL, data = T16.0, binwidth = 5)

# create dataframe with SL only
T16group <- select(T16.0, mmSL)

# determine min and max
summarise(T16.0, min(mmSL), max(mmSL))

# convert to frequency table
age0group<-mixgroup(T16group, breaks = c(0,seq(45,60,5),65),
                     xname=NULL, k = NULL, usecondit = FALSE)
# plot frequency table
plot(age0group)

# parameters
par<-groupstats(age0group)

trip16.age0<-par%>%
  mutate(year=2018,trip=16,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(par))))

mixtures<-bind_rows(mixtures,trip16.age0)

# Age 1
 T16.1<-cod%>%
   filter(Age==1 & Trip == 16)
# not enough fish
 
 # ---- Trip 17 -----
 
 # Age 0
 T17.0<-cod%>%
   filter(Age==0 & Trip ==17)
 
 # check histogram
 qplot(mmSL, data = T17.0, binwidth = 5)
 
 # create dataframe with SL only
 T17group <- select(T17.0, mmSL)
 
 # determine min and max
 summarise(T17.0, min(mmSL), max(mmSL))
 
# convert to frequency table
 age0group<-mixgroup(T17group, breaks = c(0,seq(45,70,5),76),
                      xname=NULL, k = NULL, usecondit = FALSE)
 # --- plot frequency table ----
 plot(age0group)
 
 # parameters
 par<-groupstats(age0group)
 
 trip17.age0<-par%>%
   mutate(year=2018,trip=17,age=0)%>%
   mutate(dummy_pulse=rev(seq(1:nrow(par))))

 mixtures<-bind_rows(mixtures,trip17.age0)  

 # Age 1
 T17.1<-cod%>%
   filter(Age==1 & Trip == 17)

 # check histogram
 qplot(mmSL, data=T17.1, binwidth=5)
 
 # create dataframe with SL only
 T17group<-select(T17.1,mmSL)
 
 # # detremine max and min SL
 summarise(T17.1,min(mmSL),max(mmSL))

 
 # convert to frequency table
 age1group<-mixgroup(T17group, breaks = c(0,seq(95,205,5),2011),xname=NULL, k = NULL, usecondit = FALSE)
 
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
 trip17.age1<-bind_cols(fit1$parameters,fit1$se)%>%
   mutate(year=2018,trip=17,age=1)%>%
   mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))
 
 # add to mixtures
 mixtures<-bind_rows(mixtures,trip17.age1)

 # ---- Trip 18 -----
 
 # Age 0
 T18.0<-cod%>%
   filter(Age==0 & Trip ==18)
 
 # --- check histogram ----
 qplot(mmSL, data = T18.0, binwidth = 5)
 
 # --- create dataframe with SL only ----
 T18group <- select(T18.0, mmSL)
 
 # ---- determine min and max ----
 summarise(T18.0, min(mmSL), max(mmSL))
 
 # ---- convert to frequency table ----
 age0group<-mixgroup(T18group, breaks = c(0,seq(55,75,5),81),
                      xname=NULL, k = NULL, usecondit = FALSE)
 # --- plot frequency table ----
 plot(age0group)
 
 # parameters
 par<-groupstats(age0group)

 trip18.age0<-par%>%
   mutate(year=2018,trip=18,age=0)%>%
   mutate(dummy_pulse=rev(seq(1:nrow(par))))

 mixtures<-bind_rows(mixtures,trip18.age0) 

 # Age 1
 T18.1<-cod%>%
   filter(Age==1 & Trip ==18)

 # check histogram
 qplot(mmSL, data=T18.1, binwidth=5)
 
 # create dataframe with SL only
 T18group<-select(T18.1,mmSL)
 
 # # detremine max and min SL
 summarise(T18.1,min(mmSL),max(mmSL))
 
 # convert to frequency table
 age1group<-mixgroup(T18group, breaks = c(0,seq(105,185,5),189),xname=NULL, k = NULL, usecondit = FALSE)
 
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
 trip18.age1<-bind_cols(fit1$parameters,fit1$se)%>%
   mutate(year=2018,trip=18, age=1)%>%
   mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters)))) 

 mixtures<-bind_rows(mixtures,trip18.age1) 

 # ---- Trip 19 -----
 # Age 0
 T19.0<-cod%>%
   filter(Age==0 & Trip == 19)

 # check histogram
 qplot(mmSL, data = T19.0, binwidth = 5)
 
 # create dataframe with SL only
 T19group <- select(T19.0, mmSL)
 
 #determine min and max
 summarise(T19.0, min(mmSL), max(mmSL))
 
 # convert to frequency table 
 age0group<-mixgroup(T19group, breaks = c(0,seq(45,90,5),93),
                      xname=NULL, k = NULL, usecondit = FALSE)
 # plot frequency table 
 plot(age0group)
 
 #parameters
 par<-groupstats(age0group)
 trip19.age0<-par%>%
   mutate(year=2018,trip=19,age=0)%>%
   mutate(dummy_pulse=rev(seq(1:nrow(par)))) 
mixtures<-bind_rows(mixtures,trip19.age0)

# Age 1
T19.1<-cod%>%
  filter(Age==1 & Trip == 19)

# check histogram
qplot(mmSL, data=T19.1, binwidth=5)

# create dataframe with SL only
T19group<-select(T19.1,mmSL)

# # detremine max and min SL
summarise(T19.1,min(mmSL),max(mmSL))

# convert to frequency table
age1group<-mixgroup(T19group, breaks = c(0,seq(110,145,5),152),xname=NULL, k = NULL, usecondit = FALSE)

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
trip19.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018, trip=19,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip19.age1)

# ---- Trip 20 -----
 #Age 0
T20.0<-cod%>%
  filter(Age==0 & Trip == 20)

# check histogram 
qplot(mmSL, data = T20.0, binwidth = 5)

# create dataframe with SL only
T20group <- select(T20.0, mmSL)

# determine min and max 
summarise(T20.0, min(mmSL), max(mmSL))

# convert to frequency table
age0group<-mixgroup(T20group, breaks = c(0,seq(40,95,5),101),
                     xname=NULL, k = NULL, usecondit = FALSE)
# plot frequency table
plot(age0group)


# set initial parameters
par<-mixparam(c(58,80),c(5),pi=NULL)
plot(age0group,par,"gamma")

# fit mixture
fit1<-mix(age0group,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

#store results
trip20.age0<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(year=2018,trip=20,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip20.age0)

# Age 1
T20.1<-cod%>%
  filter(Age==1 & Trip ==20)

# check histogram
qplot(mmSL, data=T20.1, binwidth=5)

# create dataframe with SL only
T20group<-select(T20.1,mmSL)

# # detremine max and min SL
summarise(T20.1,min(mmSL),max(mmSL))

# convert to frequency table
age1group<-mixgroup(T20group, breaks = c(0,seq(115,165,5),172),xname=NULL, k = NULL, usecondit = FALSE)

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
trip20.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2018,trip=20,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

# add to mixtures
mixtures<-bind_rows(mixtures,trip20.age1)

# ----- Trip 21 -----

#Age0
T21.0<-cod%>%
  filter(Age==0 & Trip == 21)

# check histogram
qplot(mmSL, data = T21.0, binwidth = 5)

# create dataframe with SL only
T21group <- select(T21.0, mmSL)

# ---- determine min and max ----
summarise(T21.0, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
age0group<-mixgroup(T21group, breaks = c(0,seq(45,100,5),106),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(age0group)


# ---- set initial parameters ----
par<-mixparam(c(40,63,85),c(5),pi=NULL)
plot(age0group,par,"gamma")

# fit mixture
fit1<-mix(age0group,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

# strore results
trip21.age0<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(year=2018,trip=21,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip21.age0)

# Age 1
T21.1<-cod%>%
  filter(Age==1 & Trip == 21)
# not enough fish

# ----- Trip 22 ------

#Age 0
T22.0<-cod%>%
  filter(Age==0& Trip == 22)

# check histogram
qplot(mmSL, data = T22.0, binwidth = 5)

# create dataframe with SL only
T22group <- select(T22.0, mmSL)

# determine min and max
summarise(T22.0, min(mmSL), max(mmSL))

# convert to frequency table 
age0group<-mixgroup(T22group, breaks = c(0,seq(45,100,5),106),
                     xname=NULL, k = NULL, usecondit = FALSE)
# plot frequency table
plot(age0group)


# set initial parameters
par<-mixparam(c(50,65,85),c(5),pi=NULL)
plot(age0group,par,"gamma")

# fit mixture
fit1<-mix(age0group,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
trip22.age0<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(year=2018,trip=22,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip22.age0)

# Age 1
T22.1<-cod%>%
  filter(Age==1 & Trip == 22)
# not enough fish

# ---- Trip 23 ------
T23.0<-cod%>%
  filter(Age==0 & Trip == 23)

# check histogram 
qplot(mmSL, data = T23.0, binwidth = 5)

# create dataframe with SL only 
T23group <- select(T23.0, mmSL)

# determine min and max 
summarise(T23.0, min(mmSL), max(mmSL))

# convert to frequency table 
age0group<-mixgroup(T23group, breaks = c(0,seq(50,105,5),112),
                     xname=NULL, k = NULL, usecondit = FALSE)
# plot frequency table 
plot(age0group)


# set initial parameters
par<-mixparam(c(50,70,90),c(5),pi=NULL)
plot(age0group,par,"gamma")

# fit mixture
fit1<-mix(age0group,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store results
trip23.age0<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(year=2018,trip=23,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip23.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip ==23)
# not enough fish

# ---- Final mixtures DF -----
write.csv(mixutres, "./data/output/AC-mixture-dist-2018.csv",row.names = FALSE)

# ----- Part 2: Estimating Pulses ------
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
  mutate(date=ymd(paste(year,month,day,sep="-")))

# visualize pulses
cod%>%
  filter(Age<2)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(Age)),alpha=0.25,size=1)+
  geom_point(data=cod.pulse,aes(x=date,y=mean,shape=factor(pulse),fill=factor(age)),size=2)+
  geom_errorbar(data=cod.pulse,aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2018 Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))+
  scale_color_manual(values = c('grey40','blue'))

##### Double Check all of this #####
cod<-cod%>%
  mutate(cohort=2018)%>%
  mutate(cohort=replace(cohort,Age==1,2017))%>%
  mutate(cohort=replace(cohort,Age==2,2016))
cod%>%
  filter(cohort==2018)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL),alpha=0.25,size=1,colour='blue')+
  geom_point(data=filter(cod.pulse,age==0),aes(x=date,y=mean,shape=factor(pulse),fill=factor(age)),size=2)+
  geom_errorbar(data=filter(cod.pulse,age==0),aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2018 Cohort Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

# Prepare 2017 cohort data
cod17<-cod17%>%
  mutate(cohort=2017)%>%
  mutate(cohort=replace(cohort,Age==1,2016))%>%
  mutate(cohort=replace(cohort,Age==2,2015))

cohort.2017<-bind_rows(cod,cod17)%>%
  filter(cohort==2017)%>%
  mutate(Date2=Date+5)

tripdates17<-cod17%>%
  select(Trip,Year,Month,Day)%>%
  distinct()%>%
  group_by(Trip,Year,Month)%>%
  summarise(Day=min(Day))

cod.pulse2017<-cod17%>%
  group_by(Year,Trip,Age,Pulse)%>%
  summarise(mean=mean(mmSL),min=min(mmSL),max=max(mmSL))%>%
  left_join(tripdates17)%>%
  rename(year=Year,trip=Trip,age=Age,pulse=Pulse,month=Month,day=Day)%>%
  filter(age==0)%>%
  bind_rows(filter(cod.pulse,age==1))%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))

#2017 Cohort plot (2018 age 1s)
ggplot()+
  geom_jitter(data=cohort.2017,aes(x=Date2,y=mmSL),alpha=0.25,size=1,colour='blue')+
  geom_point(data=cod.pulse2017,aes(x=date,y=mean,shape=factor(pulse),fill=factor(age)),size=2)+
  geom_errorbar(data=cod.pulse2017,aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2017 Cohort Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

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
  ggtitle("2018")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

pulse.range2<-pulse_range(mixtures)
# Assignment Round 1:
mydata<-pulse.range2%>%
  rename(pulse=dummy_pulse)%>%
  mutate(pulse=replace(pulse,age==1 & trip == 9 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 13 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 13 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 14 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 15 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 15 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & pulse == 1,NA))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 18 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 19 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 20 & pulse == 3,4))

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

cod2%>%
  filter(age!=2)%>%
  ggplot(aes(x=Date,y=mmSL,group=age,colour=factor(pulse)))+
  geom_jitter(size=.25)+
  theme_bw()+
  ylim(c(25,222))+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))


final<-cod2%>%
  mutate(pulse=replace(pulse,age==1 & trip == 9 & pulse==1 & mmSL<118,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 9 & pulse ==4 & mmSL>60,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 9 & pulse ==2 & mmSL<92,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 13 & pulse==1 & mmSL<140,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 13 & pulse == 2 & mmSL<120,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 14 & pulse == 4 & mmSL>87,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 14 & pulse == 2 & mmSL<125,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 15 & pulse == 1 & mmSL<175,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 15 & pulse == 2 & mmSL<126,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 16,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 16 & mmSL<100,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 16 & mmSL>150,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & pulse ==4 & mmSL>108,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & pulse == 2 & mmSL<149,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & pulse == 1 & mmSL<170,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 18 & mmSL<115,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 18 & pulse==2 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 18 & pulse == 1 & mmSL<175,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 19 & mmSL<125,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 19 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 20 & pulse ==1,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 20 & mmSL<127,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 20 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 20 & mmSL>160,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 21 & mmSL<130,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 21 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 21 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 21 & pulse == 1 & mmSL<190,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 22,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 23,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & is.na(pulse),1))

pulse.range2018<-final%>%
  group_by(age,year,trip,pulse)%>%
  summarise(mean=mean(mmSL),minSL=min(mmSL),maxSL=max(mmSL))%>%
  filter(age!=2)%>%
  ungroup()%>%
  left_join(tripdates)%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))
#write.csv(pulse.range2019,"pulse_range2018.csv",row.names = FALSE)  

final%>%
  filter(age<2)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(age)),alpha=0.25,size=1)+
  geom_point(data=pulse.range2018,aes(x=date,y=mean,shape=factor(pulse),fill=as.character(age)),size=2)+
  geom_errorbar(data=pulse.range2018,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2018 Newman Sound Atlantic cod")+
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
  geom_point(data=filter(pulse.range2018,age==0),aes(x=date,y=mean,shape=factor(pulse),fill=as.character(age)),size=2)+
  geom_errorbar(data=filter(pulse.range2018,age==0),aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2018 Newman Sound Atlantic cod: Age 0")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

final%>%
  filter(age==1)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(pulse)),alpha=0.25,size=1)+
  geom_point(data=filter(pulse.range2018,age==1),aes(x=date,y=mean,shape=factor(pulse),fill=as.character(age)),size=2)+
  geom_errorbar(data=filter(pulse.range2018,age==1),aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2018 Newman Sound Atlantic cod: Age 1")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))
