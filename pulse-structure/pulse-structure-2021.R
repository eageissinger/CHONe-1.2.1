## Pulse structure for 2021 AC Newman Sound

# set working directory if you are not working in a project folder
#setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/pulse-structure/")

# load source functions
source("./pulse-structure/pulse_range_fct.R") # custom function to assign pulse ranges based on mixture distribution output

#load packages
library(tidyverse) # has functions for plotting, data summaries, and data manipulation
library(lubridate) # format dates
library(mixdist) # mixture distribution package

#load data
length<-read.csv("./data/TNNP2021/TNNP_2021_Revised_data.csv")%>%
  dplyr::rename(X=X.)%>%
  mutate(Month=as.integer(str_sub(Date,start=5,end=6)),
         Day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(Date=ymd(paste(Year,Month,Day,sep="-")))
length20<-read.csv("./data/TNNP/length/revised_TNNP2020a.csv")%>%
  mutate(Month=as.integer(str_sub(Date,start=5,end=6)),
         Day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(Date=ymd(paste(Year,Month,Day,sep="-")))


# ---- check data structure ----
str(length)
summary(length)
head(length)
tail(length)
names(length)
dim(length)


#check species names/code for AC
length%>%distinct(Species)
# Select cod 
cod<-length%>%
  filter(Species=="AC" | Species==" AC" | Species == "AC ")

cod20<-length20%>%
  filter(Species=="AC")

# check cod data
summary(cod)


# ---- Part 1: multimodal distribution -----
# first trip:
cod%>%distinct(Trip)
# ---- Trip 9 ----

# Age-1
T9.1<-cod%>%
  filter(Age==1)%>%
  filter(Trip==9) # input trip number here

# check histogram
qplot(mmSL,data=T9.1,binwidth=5)

#create dataframe with only mmSL
T9group<-select(T9.1,mmSL)

# min and max sl
summarise(T9.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T9group, breaks=c(0,seq(16,105,5),110), # input minimum first, then the second number is one bin below the max, followed by the bin, last is the max
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mode and sd, pi = constant
age1param<-mixparam(c(20,50,80,100),c(5),pi=NULL) # the estimate should be a little off
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip9.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2021,trip=9,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-trip9.age1

# --- Trip 13 ----
# Age-0
T13.0<-cod%>%
  filter(Age==0)%>%
  filter(Trip==13)
T13.0
# Likley an age 1

# Age-1
T13.1 <- cod%>%
  filter(Age ==1 )%>%
  filter(Trip ==13)

# check histogram
qplot(mmSL,data=T13.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T13group<-select(T13.1,mmSL)

# min and max sl
summarise(T13.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T13group, breaks=c(0,seq(75,140,5),144),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(80,100,120,145),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1) # poor fit
plot(fit1)
plot(fit1,root=T)

# cant assign, not enough data, poor fit

# ---- Trip 14 ----
T14.0<-cod%>%
  filter(Age==0 & Trip == 14)

# check histogram
qplot(mmSL,data=T14.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T14group<-select(T14.0,mmSL)

# min and max sl
summarise(T14.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T14group, breaks=c(0,seq(25,40,5),45),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

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

#single pulse present
fit2<-groupstats(age0group) # single mode, use groupstats
fit2

# store data, fit2
trip14.age0<-fit2%>%
  mutate(year=2021,trip=14,age=0)%>%
  mutate(dummy_pulse=1)

mixtures<-bind_rows(mixtures,trip14.age0)

# Age 1
cod%>%
  filter(Age == 1 & Trip == 14)->T14.1

# check histogram
qplot(mmSL,data=T14.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T15group<-select(T14.1,mmSL)

# min and max sl
summarise(T14.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T15group, breaks=c(0,seq(85,155,5),159),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(90,130,160),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# not enough cod


# --- Trip 15 ----
# Age 0 
T15.0<-cod%>%
  filter(Age==0 & Trip == 15)

# check histogram
qplot(mmSL,data=T15.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T15group<-select(T15.0,mmSL)

# min and max sl
summarise(T15.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T15group, breaks=c(0,seq(20,50,5),52),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

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

#single pulse present
fit2<-groupstats(age0group)
fit2

# store data, fit2
trip15.age0<-fit2%>%
  mutate(year=2021,trip=15,age=0)%>%
  mutate(dummy_pulse=1)

mixtures<-bind_rows(mixtures,trip15.age0)

# Age 1
cod%>%
  filter(Age == 1 & Trip == 15)->T15.1

# not enough cod


# ---- Trip 16 ----
# Age 0
cod%>%
  filter(Age ==0 & Trip == 16)->T16.0

# check histogram
qplot(mmSL,data=T16.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T16group<-select(T16.0,mmSL)

# min and max sl
summarise(T16.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T16group, breaks=c(0,seq(35,60,5),65),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

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

# single ppulse
fit2<-groupstats(age0group)
fit2

# store data, fit2
trip16.age0<-fit2%>%
  mutate(year=2021,trip=16,age=0)%>%
  mutate(dummy_pulse=1)

mixtures<-bind_rows(mixtures,trip16.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip==16)

#not enough fish


# ---- Trip 17 -----
# age 0
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
age0group<-mixgroup(T17group, breaks=c(0,seq(50,80,5),84),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(55,70,95),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data, fit2
trip17.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2021,trip=17,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip17.age0)


# Age 1
cod%>%
  filter(Age == 1 & Trip == 17)->T17.1

# check histogram
qplot(mmSL,data=T17.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T17group<-select(T17.1,mmSL)

# min and max sl
summarise(T17.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T17group, breaks=c(0,seq(105,190,5),191),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(120,150,180),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=20,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip17.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2021,trip=17,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip17.age1)

# ---- Trip 18 ----
# Age 0
cod%>%
  filter(Age==0 & Trip == 18)->T18.0

# check histogram
qplot(mmSL,data=T18.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T18group<-select(T18.0,mmSL)

# min and max sl
summarise(T18.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T18group, breaks=c(0,seq(34,105,5),106),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(40,70,100),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# weak model
# store data
trip18.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2021,trip=18,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip18.age0)


# Age 1
cod%>%
  filter(Age==1 & Trip == 18)->T18.1

# not enough cod


# ---- Trip 19 ----
# Age 0
cod%>%
  filter(Age==0 & Trip==19)->T19.0

# check histogram
qplot(mmSL,data=T19.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T19group<-select(T19.0,mmSL)

# min and max sl
summarise(T19.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T19group, breaks=c(0,seq(25,105,5),110),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(50,90),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip19.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2021,trip=19,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip19.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 19)

# not enough data

# ---- Trip 20 -----
# Age 0
cod%>%
  filter(Age==0 & Trip == 20)->T20.0

# check histogram
qplot(mmSL,data=T20.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T20group<-select(T20.0,mmSL)

# min and max sl
summarise(T20.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T20group, breaks=c(0,seq(35,115,5),116),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
age0param<-mixparam(c(50,80,110),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# can't fit this data/trip


# Age 1
cod%>%
  filter(Age==1 & Trip == 20)->T20.1

# check histogram
qplot(mmSL,data=T20.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T20group<-select(T20.1,mmSL)

# min and max sl
summarise(T20.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T20group, breaks=c(0,seq(113,215,5),220),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(120,140,170,200),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip20.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2021,trip=20,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip20.age1)

# ---- Trip 21 ----
# Age 0
cod%>%
  filter(Age==0 &Trip==21)->T21.0

# check histogram
qplot(mmSL,data=T21.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T21group<-select(T21.0,mmSL)

# min and max sl
summarise(T21.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T21group, breaks=c(0,seq(32,120,5),125),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(45,70,100),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip21.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2021,trip=21,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip21.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 21)->T21.1

# not enough data


# ---- Trip 22 ----
# Age 0
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
age0group<-mixgroup(T22group, breaks=c(0,seq(36,120,5),123),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)


# set parameters using mixparam
age0param<-mixparam(c(60,90),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# weak fit

# store data
# fit 1
trip22.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2021,trip=22,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip22.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 22)

# not enough fish

# ---- Trip 23 ----
# Age 0
cod%>%
  filter(Age==0 & Trip == 23)->T23.0

# check histogram
qplot(mmSL,data=T23.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T23group<-select(T23.0,mmSL)

# min and max sl
summarise(T23.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T23group, breaks=c(0,seq(43,120,5),125),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(70,100),c(5),pi=NULL)
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
  mutate(year=2021,trip=23,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip23.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 23)

# not enough fish


View(mixtures)

# add 'cohort', 'age' to mixtures df
# ---- Final mixtures DF -----
write.csv(mixtures,"./data/TNNP2021/AC-mixture-dist-2021.csv",row.names = FALSE)


# ---- Part 2: Estimating pulses ----
cod.pulse<-mixtures%>%
  rename(mean=mu,pulse = dummy_pulse, Trip=trip,Age=age)%>%
  mutate(min=mean-sigma,max=mean+sigma)%>%
  select(-sigma,-sigma.se,-mu.se,-pi,-pi.se)

# Add dates to trips
tripdates<-cod%>%
  select(Trip,Year,Month,Day)%>%
  #rename(trip=Trip,year=Year,month=Month,day=Day)%>%
  distinct()%>%
  group_by(Trip,Year,Month)%>%
  summarise(Day=min(Day))
cod.pulse<-left_join(cod.pulse,tripdates)%>%
  mutate(date=ymd(paste(Year,Month,Day,sep="-")))

# visualize pulses
fig1<-cod%>%
  filter(Age<2)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(Age)),alpha=0.25,size=1)+
  geom_point(data=cod.pulse,aes(x=date,y=mean,shape=factor(pulse),fill=factor(Age)),size=2)+
  geom_errorbar(data=cod.pulse,aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2021 Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))+
  scale_color_manual(values = c('grey40','blue'))
fig1

# add cohort
cod<-cod%>%
  mutate(cohort=2021)%>%
  mutate(cohort=replace(cohort,Age==1,2020))%>%
  mutate(cohort=replace(cohort,Age==2,2019))

fig2<-cod%>%
  filter(cohort==2021)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL),alpha=0.25,size=1,colour='blue')+
  geom_point(data=filter(cod.pulse,Age==0),aes(x=date,y=mean,shape=factor(pulse),fill=factor(Age)),size=2)+
  geom_errorbar(data=filter(cod.pulse,Age==0),aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2021 Cohort Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))
fig2

# Prepare 2019 cohort data
cod20<-cod20%>%
  mutate(cohort=2020)%>%
  mutate(cohort=replace(cohort,Age==1,2019))%>%
  mutate(cohort=replace(cohort,Age==2,2018))

cohort.2020<-bind_rows(cod,cod20)%>%
  filter(cohort==2020)%>%
  mutate(Date2=Date+5)

tripdates20<-cod20%>%
  select(Trip,Year,Month,Day)%>%
  distinct()%>%
  group_by(Trip,Year,Month)%>%
  summarise(Day=min(Day))

cod.pulse2020<-cod20%>%
  group_by(Year,Trip,Age,Pulse)%>%
  rename(pulse=Pulse)%>%
  summarise(mean=mean(mmSL),min=min(mmSL),max=max(mmSL))%>%
  left_join(tripdates20)%>%
  filter(Age==0)%>%
  bind_rows(filter(cod.pulse,Age==1))%>%
  mutate(date=ymd(paste(Year,Month,Day,sep="-")))

#2020 Cohort plot (2021 age 1s)
fig3<-ggplot()+
  geom_jitter(data=cohort.2020,aes(x=Date2,y=mmSL),alpha=0.25,size=1,colour='blue')+
  geom_point(data=cod.pulse2020,aes(x=date,y=mean,shape=factor(pulse),fill=factor(Age)),size=2)+
  geom_errorbar(data=cod.pulse2020,aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2020 Cohort Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))
fig3

pdf("../data/TNNP2021/2021-pulse-plots.pdf",onefile = TRUE)
fig1
fig2
fig3
dev.off()

# ---- Part 3: Pulse Assignments ----
pulse.range<-pulse_range(mixtures)

# use min and max for each pulse to then create a dataframe with all length possibilities per pulse
pulse.assign<-data.frame(Age=rep(pulse.range$age,pulse.range$max-pulse.range$min+1),
                         Trip=rep(pulse.range$trip,pulse.range$max-pulse.range$min+1),
                         Pulse=rep(pulse.range$dummy_pulse,pulse.range$max-pulse.range$min+1),
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
  #rename(age=Age,trip=Trip,year=Year)%>%
  left_join(pulse.assign)

# ---- Part 4: Verify pulse assignment ----
# replot pulses

# calculate mean, min, and max of current pulse assignments
revised<-length.pulse%>%
  group_by(Year,Trip,Age,Pulse)%>%
  summarise(mean=mean(mmSL),min=min(mmSL),max=max(mmSL))%>%
  left_join(tripdates)%>%
  mutate(date=ymd(paste(Year,Month,Day,sep="-")))

# visualize pulses
ggplot(revised,aes(x=date,y=mean,shape=factor(Pulse),colour=factor(Age)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2021 AC")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))


# use pulse.range to reassign pulses
# Assignment Round 1:
data<-pulse.range%>%
  rename(pulse=dummy_pulse)%>%
  mutate(pulse=replace(pulse,age==0 & trip ==17,1))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 18 & pulse ==1,NA))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 18 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 18 & pulse ==3,2))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 21 & pulse == 3,2))


# deal with the outliers
# start by combining pulse range to length data
# use min and max for each pulse to then create a dataframe with all length possibilities per pulse

pulse_assign2<-data.frame(Age=rep(data$age,data$max-data$min+1),
                          Trip=rep(data$trip,data$max-data$min+1),
                          Pulse=rep(data$pulse,data$max-data$min+1),
                          mmSL=unlist(mapply(seq,data$min,data$max)))
cod2<-cod%>%
  select(-Pulse)%>%
  #rename(trip=Trip,age=Age,year=Year)%>%
  left_join(pulse_assign2)

# final assignments
final<-cod2%>%
  # age boundary
  mutate(Age=replace(Age,Trip==13 & Age==0,1))%>%
  mutate(Age=replace(Age,Trip==19 & mmSL<115,0))%>%
  mutate(Age=replace(Age,Trip ==20 & mmSL<126,0))%>%
  mutate(Age=replace(Age,Trip==21 & mmSL<135,0))%>%
  mutate(Age=replace(Age,Trip==22 & mmSL<150,0))%>%
  mutate(Age=replace(Age,Trip==23 & mmSL<150,0))%>%
  # age 0
  mutate(Pulse=replace(Pulse,Age==0 & Trip == 15,1))%>%
  mutate(Pulse=replace(Pulse,Age==0 & Trip == 17 & mmSL<55,2))%>%
  mutate(Pulse=replace(Pulse,Age == 0 & Trip == 18 & mmSL<62,2))%>%
  mutate(Pulse=replace(Pulse,Age == 0 & Trip == 19 & mmSL< 68,2))%>%
  mutate(Pulse=replace(Pulse, Age==0 & Trip == 19 & mmSL<30,3))%>%
  mutate(Pulse=replace(Pulse, Age== 0 & Trip == 19 & mmSL>100,1))%>%
  mutate(Pulse=replace(Pulse,Age==0 & Trip==20 & mmSL<73,2))%>%
  mutate(Pulse=replace(Pulse,Age==0 & Trip == 20 & mmSL>=73,1))%>%
  mutate(Pulse=replace(Pulse,Age==0 & Trip == 21 & mmSL<78,2))%>%
  mutate(Pulse=replace(Pulse,Age==0 & Trip == 21 & mmSL>100,1))%>%
  mutate(Pulse=replace(Pulse, Age == 0 & Trip == 21 & mmSL< 37,3))%>%
  mutate(Pulse=replace(Pulse, Age == 0 & Trip == 22 & mmSL<87,2))%>%
  mutate(Pulse=replace(Pulse,Age==0 & Trip == 22 & mmSL>100,1))%>%
  mutate(Pulse=replace(Pulse, Age== 0 & Trip == 22 & mmSL<42,3))%>%
  mutate(Pulse=replace(Pulse, Age == 0 & Trip == 23 & mmSL<88,2))%>%
  mutate(Pulse=replace(Pulse,Age==0 & Trip == 23 & mmSL>100,1))%>%
  mutate(Pulse=replace(Pulse,Age == 0 & Trip == 23 & mmSL<47,3))%>%
  # Age 1 
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 9 & Pulse == 1 & mmSL<100,2))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 9 & Pulse ==4,3))%>%
  mutate(Pulse=replace(Pulse, Age==1 & is.na(Pulse),2))%>%
  mutate(Pulse=replace(Pulse,Age == 1 & Trip == 13 & mmSL<110,3))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 13 & mmSL>140,1))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 14 & mmSL<125,3))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 14 & mmSL>150,1))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 15 & mmSL<125,3))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 15 & mmSL>200,1))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 16,3))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 17 & mmSL<150,3))%>%
  mutate(Pulse=replace(Pulse, Age == 1 & Trip == 17 & mmSL>=150,2))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 17 & mmSL>180,1))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 18 & mmSL<150,3))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 18 & mmSL>185,1))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 19 & mmSL<150,3))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 20 & mmSL <170,3))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 20 & Pulse==1 & mmSL<198,2))%>%
  mutate(Pulse=replace(Pulse, Age==1 & Trip==21 & mmSL<175,3))%>%
  mutate(Pulse=replace(Pulse, Age==1 & Trip == 21 & mmSL>200,1))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 22,3))%>%
  mutate(Pulse=replace(Pulse,Age==1 & Trip == 23 & mmSL<175,3))%>%
  #AC age 2
  mutate(Pulse=replace(Pulse,Trip==14 & Age == 2,1))%>%
  mutate(Age=replace(Age,Trip==14 & Age ==2 & Pulse == 1,1))%>%
  mutate(Pulse=replace(Pulse,Trip==18 & Age ==2 & mmSL<250,1))%>%
  mutate(Age=replace(Age,Trip==18 & Age ==2 & Pulse ==1,1))%>%
  mutate(Species="AC") # correct AC typos throughout
  
# create mean, min, and max of pulse assignments
pulse.range2021<-final%>%
  group_by(Age,Year,Trip,Pulse)%>%
  summarise(mean=mean(mmSL),minSL=min(mmSL),maxSL=max(mmSL))%>%
  filter(Age!=2)%>%
  ungroup()%>%
  left_join(tripdates)%>%
  mutate(date=ymd(paste(Year,Month,Day,sep="-")))%>%
  # add_row(age=1,year=2020,trip=18.5,pulse=1,mean=197,minSL=197,maxSL=197,month=9,day=30)%>% # adjust to include large age-1 fish
  unite(Date,Year,Month,Day,sep="")%>%
  mutate(Year=as.integer(str_sub(Date,start=1,end=4)))%>%
  rename(meanSL=mean)%>%
  select(Date,Year, Age,Trip,Pulse,meanSL,minSL,maxSL,date)

write.csv(pulse.range2021,"./data/TNNP2021/pulse_range2021.csv",row.names = FALSE)  

final%>%
  filter(Age<2)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(Age)),alpha=0.25,size=1)+
  geom_point(data=pulse.range2021,aes(x=date,y=meanSL,shape=factor(Pulse)),size=2)+
  geom_errorbar(data=pulse.range2021,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2021 Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))+
  scale_color_manual(values = c('red','blue')) ->fig4

final%>%
  filter(Age==0)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(Pulse)),alpha=0.25,size=1)+
  geom_point(data=filter(pulse.range2021,Age==0),aes(x=date,y=meanSL,shape=factor(Pulse),fill=as.character(Age)),size=2)+
  geom_errorbar(data=filter(pulse.range2021,Age==0),aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2021 Newman Sound Atlantic cod: Age 0")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40)) -> fig5

final%>%
  filter(Age==1)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(Pulse)),alpha=.5,size=1)+
  geom_point(data=filter(pulse.range2021,Age==1),aes(x=date,y=meanSL,shape=factor(Pulse),fill=as.character(Age)),size=2)+
  geom_errorbar(data=filter(pulse.range2021,Age==1),aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2021 Newman Sound Atlantic cod: Age 1")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40)) ->fig7
final%>%
  filter(Age==1)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(Pulse)),alpha=0.5,size=1)+
  geom_point(data=filter(pulse.range2021,Age==1),aes(x=date,y=meanSL,shape=factor(Pulse),fill=as.character(Age)),size=2)+
  geom_errorbar(data=filter(pulse.range2021,Age==1),aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  geom_jitter(data=filter(cod20,Age==0),aes(x=Date+3,y=mmSL,colour=as.character(Pulse)),alpha=.5,size=1)+
  theme_bw()+
  ggtitle("2021 Newman Sound Atlantic cod: Age 1")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40)) ->fig6

pdf("./data/TNNP2021/Pulses-2021.pdf",onefile = TRUE)
fig4
fig5
fig6
fig7
dev.off()

# ---- Part 5: Final assignment ----

summary(final)
head(length)
summary(pulse.range2021)

# add AC data back to full dataset
length2<-length%>%
  filter(Species!="AC" & Species != " AC" & Species != "AC ")%>% # remove AC from list
  bind_rows(final)%>% # add corrected AC to dataset
  arrange(X)%>% # arrange in order of X variable (original order)
  select(-cohort)%>% # remove cohort column
  mutate(Date=str_remove_all(Date,"-"))%>% # format date back to original version
  mutate(Month=replace(Month,Month==5,"MAY"),
         Month=replace(Month,Month==7,"JUL"),
         Month=replace(Month,Month==8,"AUG"),
         Month=replace(Month,Month==9,"SEPT"),
         Month=replace(Month,Month==10,"OCT"),
         Month=replace(Month,Month==11,"NOV"),
         Month=replace(Month,Month==12,"DEC")) # reformat months
  
summary(length2)
dim(length2)
dim(length)


#check
length2%>%
  filter(Age==0)%>%
  filter(Species=="AC")%>%
  ggplot(aes(x=Julian.Date,y=mmSL,colour=as.character(Pulse)))+geom_point()

length2%>%
  filter(Age==1)%>%
  filter(Species=="AC")%>%
  ggplot(aes(x=Julian.Date,y=mmSL,colour=as.character(Pulse)))+geom_point()
length2%>%
  filter(Species=="AC")%>%
  ggplot(aes(x=Julian.Date,y=mmSL,colour=as.character(Pulse),shape=as.factor(Age)))+geom_point()

# save dataframe
write.csv(length2, "./data/TNNP2021/TNNP21_length_with_pulses_2022-02-09.csv",row.names = FALSE, na = "")
