## Pulse structure for 2020 AC Newman Sound

# ----- set working directory ----
setwd("C:/Users/geissingere/Documents/pulse-structure/")

# load source functions
source("pulse_range_fct.R")

#load packages
library(tidyverse)
library(lubridate)
library(mixdist)

#load data
length<-read.csv("./data/TNNP2020_length.csv")
length2019<-read.csv("./data/TNNP2019/TNNP19_length_revised_2021-04-15.csv")

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

length2019%>%
  mutate(Month=as.integer(str_sub(Date,start=5,end=6)),
         Day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(Date=ymd(paste(Year,Month,Day,sep="-")))->length19

# Select cod 
cod<-length2%>%
  filter(Species=="AC" | Species=="Ac")

cod19<-length19%>%
  filter(Species=="AC")

# check cod data
summary(cod)


# ---- multimodal distribution -----
# ---- Trip 14 ----

# Age-0
T14.0<-cod%>%
  filter(Age==0)%>%
  filter(Trip==14)

# check histogram
qplot(mmSL,data=T14.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T14group<-select(T14.0,mmSL)

# min and max sl
summarise(T14.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T14group, breaks=c(0,seq(10,40,5),42),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(10,35),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip14.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2020,trip=14,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-trip14.age0

# Age-1
T14.1<-cod%>%
  filter(Age==1)%>%
  filter(Trip==14)

# check histogram
qplot(mmSL,data=T14.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T14group<-select(T14.1,mmSL)

# min and max sl
summarise(T14.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T14group, breaks=c(0,seq(75,130,5),136),
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
  mutate(year=2020,trip=14,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip14.age1)

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
age0group<-mixgroup(T15group, breaks=c(0,seq(32,50,5),55),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(25,45),c(5),pi=NULL)
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
  mutate(year=2020,trip=15,age=0)%>%
  mutate(dummy_pulse=1)

mixtures<-bind_rows(mixtures,trip15.age0)

# Age 1
cod%>%
  filter(Age == 1 & Trip == 15)->T15.1

# check histogram
qplot(mmSL,data=T15.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T15group<-select(T15.1,mmSL)

# min and max sl
summarise(T15.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T15group, breaks=c(0,seq(65,105,5),112),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(60,90,110),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param2<-mixparam(c(60,90),c(5),pi=NULL)
plot(age1group,age1param2,"gamma")

#Mixture model
fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# store data
trip15.age1<-bind_cols(fit2$parameters,fit2$se)%>%
  mutate(year=2020,trip=15,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit2$parameters))))

mixtures<-bind_rows(mixtures,trip15.age1)

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
age0group<-mixgroup(T16group, breaks=c(0,seq(42,65,5),71),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(50,70),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data, fit2
trip16.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2020,trip=16,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

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
age0group<-mixgroup(T17group, breaks=c(0,seq(35,80,5),84),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(30,55),c(5),pi=NULL)
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
trip17.age0<-fit2%>%
  mutate(year=2020,trip=17,age=0)%>%
  mutate(dummy_pulse=1)

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
age1group<-mixgroup(T17group, breaks=c(0,seq(98,125,5),130),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(95,120),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip17.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2020,trip=17,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip17.age1)

# ---- Trip 18 ----
# Age 0
cod%>%
  filter(Age==0 & Trip == 18)
# not enough fish

# Age 1
cod%>%
  filter(Age==1 & Trip == 18)->T18.1

# check histogram
qplot(mmSL,data=T18.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T18group<-select(T18.1,mmSL)

# min and max sl
summarise(T18.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T18group, breaks=c(0,seq(110,130,5),135),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(105,120,140),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

age1param2<-mixparam(c(105,120),c(5),pi=NULL)
plot(age1group,age1param2,"gamma")

fit2<-mix(age1group,age1param2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15,usecondit = FALSE,print.level = 0)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

# not enough data

# ---- Trip 18.5 -----
# Age 0
cod%>%
  filter(Age==0 & Trip == 18.5)->T185.0

# check histogram
qplot(mmSL,data=T185.0,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T185group<-select(T185.0,mmSL)

# min and max sl
summarise(T185.0,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age0group<-mixgroup(T185group, breaks=c(0,seq(25,95,5),100),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(20,50,100),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip185.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2020,trip=18.5,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip185.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 18.5)->T185.1

# check histogram
qplot(mmSL,data=T185.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T185group<-select(T185.1,mmSL)

# min and max sl
summarise(T185.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T185group, breaks=c(0,seq(98,140,5),144),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(105,130),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip185.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2020,trip=18.5,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip185.age1)



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
age0group<-mixgroup(T19group, breaks=c(0,seq(32,100,5),105),
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
  mutate(year=2020,trip=19,age=0)%>%
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
age0group<-mixgroup(T20group, breaks=c(0,seq(37,120,5),124),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
age0param<-mixparam(c(55,100),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip20.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2020,trip=20,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip20.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 20)

# not enough fish

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
age0group<-mixgroup(T21group, breaks=c(0,seq(51,95,5),102),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(45,70,95),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

#Mixture model
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip21.age0<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2020,trip=21,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip21.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 21)->T21.1

# check histogram
qplot(mmSL,data=T21.1,binwidth=5)+theme_classic()

#create dataframe with only mmSL
T21group<-select(T21.1,mmSL)

# min and max sl
summarise(T21.1,min(mmSL),max(mmSL),n())

# Convert data to frequency table
# use mixgroup, providing size range and breaks
age1group<-mixgroup(T21group, breaks=c(0,seq(120,175,5),182),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age1group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age1param<-mixparam(c(130,180),c(5),pi=NULL)
plot(age1group,age1param,"gamma")

#Mixture model
fit1<-mix(age1group,age1param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=10,usecondit = FALSE,print.level = 0)
summary(fit1)
plot(fit1)
plot(fit1,root=T)

# store data
trip21.age1<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(year=2020,trip=21,age=1)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip21.age1)

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
age0group<-mixgroup(T22group, breaks=c(0,seq(40,115,5),119),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)


# set parameters using mixparam
age0param<-mixparam(c(45,70,95),c(5),pi=NULL)
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
  mutate(year=2020,trip=22,age=0)%>%
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
age0group<-mixgroup(T23group, breaks=c(0,seq(39,80,5),86),
                    xname=NULL,k=NULL,usecondit=FALSE)

# check new histogram
plot(age0group)

# set parameters using mixparam
# estimate mean and sd, pi = constant
age0param<-mixparam(c(45,70),c(5),pi=NULL)
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
  mutate(year=2020,trip=23,age=0)%>%
  mutate(dummy_pulse=rev(seq(1:nrow(fit1$parameters))))

mixtures<-bind_rows(mixtures,trip23.age0)

# Age 1
cod%>%
  filter(Age==1 & Trip == 23)

# not enough fish


View(mixtures)

# add 'cohort', 'age' to mixtures df
# ---- Final mixtures DF -----
write.csv(mixtures,"./output/AC-mixture-dist-2020.csv",row.names = FALSE)


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
  mutate(date=ymd(paste(year,month,day,sep="-")))

# visualize pulses
fig1<-cod%>%
  filter(Age<2)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(Age)),alpha=0.25,size=1)+
  geom_point(data=cod.pulse,aes(x=date,y=mean,shape=factor(pulse),fill=factor(age)),size=2)+
  geom_errorbar(data=cod.pulse,aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2020 Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))+
  scale_color_manual(values = c('grey40','blue'))
fig1

cod<-cod%>%
  mutate(cohort=2020)%>%
  mutate(cohort=replace(cohort,Age==1,2019))%>%
  mutate(cohort=replace(cohort,Age==2,2018))

fig2<-cod%>%
  filter(cohort==2020)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL),alpha=0.25,size=1,colour='blue')+
  geom_point(data=filter(cod.pulse,age==0),aes(x=date,y=mean,shape=factor(pulse),fill=factor(age)),size=2)+
  geom_errorbar(data=filter(cod.pulse,age==0),aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2020 Cohort Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))
fig2

# Prepare 2019 cohort data
cod19<-cod19%>%
  mutate(cohort=2019)%>%
  mutate(cohort=replace(cohort,Age==1,2018))%>%
  mutate(cohort=replace(cohort,Age==2,2017))

cohort.2019<-bind_rows(cod,cod19)%>%
  filter(cohort==2019)%>%
  mutate(Date2=Date+5)

tripdates19<-cod19%>%
  select(Trip,Year,Month,Day)%>%
  distinct()%>%
  group_by(Trip,Year,Month)%>%
  summarise(Day=min(Day))

cod.pulse2019<-cod19%>%
  group_by(Year,Trip,Age,Pulse)%>%
  summarise(mean=mean(mmSL),min=min(mmSL),max=max(mmSL))%>%
  left_join(tripdates19)%>%
  rename(year=Year,trip=Trip,age=Age,pulse=Pulse,month=Month,day=Day)%>%
  filter(age==0)%>%
  bind_rows(filter(cod.pulse,age==1))%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))

#2019 Cohort plot (2020 age 1s)
fig3<-ggplot()+
  geom_jitter(data=cohort.2019,aes(x=Date2,y=mmSL),alpha=0.25,size=1,colour='blue')+
  geom_point(data=cod.pulse2019,aes(x=date,y=mean,shape=factor(pulse),fill=factor(age)),size=2)+
  geom_errorbar(data=cod.pulse2019,aes(x=date,ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2019 Cohort Newman Sound Atlantic cod")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))
fig3

pdf("./output/allplots.pdf",onefile = TRUE)
fig1
fig2
fig3
dev.off()

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
ggplot(revised,aes(x=date,y=mean,shape=factor(pulse),colour=factor(age)))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2020 AC")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

pulse.range2<-pulse_range(mixtures)

# Assignment Round 1:
mydata<-pulse.range2%>%
  rename(pulse=dummy_pulse)%>%
  mutate(pulse=replace(pulse,age==0 & trip <=18,1))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 23 & pulse ==2,3))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 23 & pulse == 1,2))


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
  mutate(pulse=replace(pulse,age==0 & trip ==14 & mmSL<20,NA))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 18,1))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 18.5 & mmSL>58,1))%>%
  mutate(pulse=replace(pulse,age==0 & trip ==18.5 & pulse==3,2))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 19 & pulse==3,2))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 20 & pulse == 1 & mmSL<83,2))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 21 & mmSL<55,3))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 21 & mmSL>90,1))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 22 & pulse==3 & mmSL>57,2))%>%
  mutate(pulse=replace(pulse,age==0 & trip == 22 & pulse ==1 & mmSL<89,2))%>%
  mutate(pulse=replace(pulse,age==0 & trip==23 & mmSL<59,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 15 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip==15 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 15 & pulse == 2 & mmSL<100,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 16,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & mmSL>110,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 17 & mmSL<=110,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 18 & mmSL<115,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 18 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,age==1 & trip ==18.5 & pulse==2,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 18.5 & pulse==1,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 18.5 & pulse == 2 & mmSL<120,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 19 & mmSL>130,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 19 & mmSL<120 & mmSL>80,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip ==19 & mmSL>120 & mmSL < 130,3))%>%
  mutate(pulse=replace(pulse,age==1 & trip ==20 & mmSL<150,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 20 & mmSL>150,2))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 21 & mmSL<140,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip ==21 & mmSL>180,1))%>%
  mutate(pulse=replace(pulse,age==1 & trip == 22,4))%>%
  mutate(pulse=replace(pulse,age==1 & trip ==23,2))%>%
  mutate(age=replace(age,pulse==4 & age==1,0))%>%
  mutate(pulse=replace(pulse,age==0 & pulse==4,1))
  


pulse.range2020<-final%>%
  group_by(age,year,trip,pulse)%>%
  summarise(mean=mean(mmSL),minSL=min(mmSL),maxSL=max(mmSL))%>%
  filter(age!=2)%>%
  ungroup()%>%
  left_join(tripdates)%>%
  add_row(age=1,year=2020,trip=18.5,pulse=1,mean=197,minSL=197,maxSL=197,month=9,day=30)%>% # adjust to include large age-1 fish
  mutate(Year=year)%>%
  unite(Date,year,month,day,sep="")%>%
  rename(Age=age,Trip=trip, Pulse=pulse,meanSL=mean)%>%
  select(Date,Year, Age,Trip,Pulse,meanSL,minSL,maxSL)

write.csv(pulse.range2020,"./output/pulse_range2020.csv",row.names = FALSE)  

final%>%
  filter(age<2)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(age)),alpha=0.25,size=1)+
  geom_point(data=pulse.range2020,aes(x=date,y=mean,shape=factor(pulse),fill=as.character(age)),size=2)+
  geom_errorbar(data=pulse.range2020,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2020 Newman Sound Atlantic cod")+
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
  geom_point(data=filter(pulse.range2020,age==0),aes(x=date,y=mean,shape=factor(pulse),fill=as.character(age)),size=2)+
  geom_errorbar(data=filter(pulse.range2020,age==0),aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2020 Newman Sound Atlantic cod: Age 0")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

fig6<-final%>%
  filter(age==1)%>%
  mutate(Date2=Date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL,colour=as.character(pulse)),alpha=0.25,size=1)+
  geom_point(data=filter(pulse.range2020,age==1),aes(x=date,y=mean,shape=factor(pulse),fill=as.character(age)),size=2)+
  geom_errorbar(data=filter(pulse.range2020,age==1),aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  theme_bw()+
  ggtitle("2020 Newman Sound Atlantic cod: Age 1")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

pdf("./output/final2020.pdf",onefile = TRUE)
fig4
fig5
fig6
dev.off()

# ---- Part 5: Final assignment ----
summary(final)
head(length)
summary(pulse.range2020)

#final2<-final%>%
#  rename(Age2=age)

# use min and max for each pulse to then create a dataframe with all length possibilities per pulse
pulse.assign<-data.frame(trip=rep(pulse.range2020$Trip,pulse.range2020$maxSL-pulse.range2020$minSL+1),
                         age=rep(pulse.range2020$Age,pulse.range2020$maxSL-pulse.range2020$minSL+1),
                         year=rep(pulse.range2020$Year,pulse.range2020$maxSL-pulse.range2020$minSL+1),
                         pulse=rep(pulse.range2020$Pulse,pulse.range2020$maxSL-pulse.range2020$minSL+1),
                         mmSL=unlist(mapply(seq,pulse.range2020$minSL,pulse.range2020$maxSL)))
pulsefinal<-pulse.assign%>%
  mutate(Species="AC")%>%
  rename(Trip=trip,Age=age,Year=year,Pulse=pulse)

length3<-length%>%
  mutate(Year=as.integer(str_sub(Date,start=1,end=4)))%>%
  select(-Pulse)

final2<-left_join(length3,pulsefinal)


final2%>%
  filter(Species=="AC" | Species== "Ac" & is.na(Pulse))

final3<-final2%>%
  mutate(Age=replace(Age, Species == "AC" & is.na(Pulse) & Age == 1 & Trip==15,0))%>%
  mutate(Age=replace(Age, Species == "AC" & is.na(Pulse) & Age == 1 & Trip==18.5,0))%>%
  mutate(Age=replace(Age, Species == "AC" & is.na(Pulse) & Age == 1 & Trip==19,0))%>%
  mutate(Age=replace(Age, Species == "AC" & is.na(Pulse) & Age == 1 & Trip==20,0))%>%
  mutate(Age=replace(Age, Species == "AC" & is.na(Pulse) & Age == 1 & Trip==21,0))%>%
  mutate(Age=replace(Age, Species == "AC" & is.na(Pulse) & Age == 1 & Trip==22,0))%>%
  mutate(Age=replace(Age, Species=="AC" & is.na(Pulse) & Age == 2 & Trip ==18.5 & mmSL==197,1))%>%
  mutate(Species=replace(Species, Species== "Ac", "AC"))%>%
  select(-Pulse)%>%
  left_join(pulsefinal)%>%
  mutate(Notes=as.character(Notes))%>%
  mutate(Notes=replace(Notes, Notes=="Might be 1+", "Might be 1+; evaluated 2021-03-02 as age-0"))%>%
  rename("Julian Date" = "Julian.Date")%>%
  select(Year, Month, Day, Date, 'Julian Date', Trip, Time, Site, Species, mmSL, Age, Pulse, Weighting, Notes)

final3%>%filter(Species=="AC" | Species == "Ac")%>%
  filter(is.na(Pulse))

summary(final3)
dim(final3)
dim(length)

write.csv(final3, "./output/TNNP20_length_with_pulses_2021-03-03.csv",row.names = FALSE, na = "")

final3%>%
  filter(Age==0)%>%
  filter(Species=="AC")%>%
  ggplot(aes(x=Julian.Date,y=mmSL,colour=as.character(Pulse)))+geom_point()

final3%>%
  filter(Age==1)%>%
  filter(Species=="AC")%>%
  ggplot(aes(x=Julian.Date,y=mmSL,colour=as.character(Pulse)))+geom_point()
