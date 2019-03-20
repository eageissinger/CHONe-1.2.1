# --- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

# --- load packages ----
library(tidyverse)
library(mixdist)
library(lubridate)

# load source functions
source("./code/pulse_range_fct.R")
# --- load data ----
length<-read.csv("./data/data-working/newman-length.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")
catch<-read.csv("./data/data-working/newman-catch.csv")
haul<-read.csv("./data/data-working/catch_haul.csv")

# --- format dates ----
head(length)

# remove current trips column
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
head(catch)
catch<-left_join(catch,trips)
catch$date<-ymd(paste(catch$year,catch$month,catch$day,sep="-"))

# ---- Part 1: Mixture Distributions age 0 ----
yr2018<-length%>%
  filter(year==2018)
# May
# --- select data ----
july2018<-filter(yr2018, age ==0 & month == 7)
#not enough july fish

# August trip 15
august2018<-filter(yr2018, age == 0 & trip ==15)

# --- check histogram ----
qplot(mmSL, data = august2018, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(august2018, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group.2018<-mixgroup(SL, breaks = c(0,seq(35,55,5),57),
                        xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group.2018)

# parameters
trip15<-groupstats(group.2018)
head(august2018)

trip15<-trip15%>%
  mutate(trip=15,year=2018,month=8,day=13)
trip15<-mutate(trip15,dummy_pulse=rev(seq(1:nrow(trip15))))


# August trip 16
august2018<-filter(yr2018, age == 0 & trip ==16)

# --- check histogram ----
qplot(mmSL, data = august2018, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(august2018, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group.2018<-mixgroup(SL, breaks = c(0,seq(35,60,5),65),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group.2018)

# parameters
trip16<-groupstats(group.2018)
head(august2018)

trip16<-trip16%>%
  mutate(trip=16,year=2018,month=8,day=27)
trip16<-mutate(trip16,dummy_pulse=rev(seq(1:nrow(trip16))))

# September trip 17
sept2018<-filter(yr2018, age == 0 & trip ==17)

# --- check histogram ----
qplot(mmSL, data = sept2018, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(sept2018, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group.2018<-mixgroup(SL, breaks = c(0,seq(45,70,5),76),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group.2018)

# parameters
trip17<-groupstats(group.2018)
head(sept2018)

trip17<-trip17%>%
  mutate(trip=17,year=2018,month=9,day=10)
trip17<-mutate(trip17,dummy_pulse=rev(seq(1:nrow(trip17))))
# September trip 18
sept2018<-filter(yr2018, age == 0 & trip ==18)

# --- check histogram ----
qplot(mmSL, data = sept2018, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(sept2018, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group.2018<-mixgroup(SL, breaks = c(0,seq(55,75,5),81),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group.2018)

# parameters
trip18<-groupstats(group.2018)
head(sept2018)

trip18<-trip18%>%
  mutate(trip=18,year=2018,month=9,day=24)
trip18<-mutate(trip18,dummy_pulse=rev(seq(1:nrow(trip18))))
# October trip 19
oct2018<-filter(yr2018, age == 0 & trip ==19)

# --- check histogram ----
qplot(mmSL, data = oct2018, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(oct2018, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group.2018<-mixgroup(SL, breaks = c(0,seq(45,90,5),93),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group.2018)

#parameters
trip19<-groupstats(group.2018)
head(oct2018)
trip19<-trip19%>%
  mutate(trip=19,year=2018,month=10,day=9)
trip19<-mutate(trip19,dummy_pulse=rev(seq(1:nrow(trip19))))
# October trip 20
oct2018<-filter(yr2018, age == 0 & trip ==20)

# --- check histogram ----
qplot(mmSL, data = oct2018, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(oct2018, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group.2018<-mixgroup(SL, breaks = c(0,seq(40,95,5),101),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group.2018)


# ---- set initial parameters ----
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

# --- check histogram ----
qplot(mmSL, data = nov2018, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(nov2018, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group.2018<-mixgroup(SL, breaks = c(0,seq(45,100,5),106),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group.2018)


# ---- set initial parameters ----
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

# --- check histogram ----
qplot(mmSL, data = nov2018, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(nov2018, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group.2018<-mixgroup(SL, breaks = c(0,seq(45,100,5),106),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group.2018)


# ---- set initial parameters ----
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

# --- check histogram ----
qplot(mmSL, data = dec2018, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(dec2018, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group.2018<-mixgroup(SL, breaks = c(0,seq(50,105,5),112),
                     xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group.2018)


# ---- set initial parameters ----
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
mixtures<-bind_rows(trip15,trip16,trip17,trip18,trip19,trip20,trip21,trip22,trip23)%>%
  mutate(cohort=year)

# ---- Part 2: Estimating pulses ----
age0<-mixtures%>%
  rename(mean=mu,pulse = dummy_pulse)%>%
  mutate(min=mean-sigma,max=mean+sigma,age=0)%>%
  select(-sigma,-sigma.se,-mu.se,-pi,-pi.se)


# visualize pulses
age0$date<-ymd(paste(age0$year,age0$month,age0$day,sep="-"))

ggplot(age0,aes(x=date,y=mean,shape=factor(pulse)))+geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2018 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

# ---- Part 3: Pulse Assignments ----
# make dataframe with min and max pulse ranges
# use +/- standard deviation for "min" and "max"
pulse.range<-pulse_range(mixtures)

# use min and max for each pulse to then create a dataframe with all length possibilities per pulse
pulse.assign<-data.frame(trip=rep(pulse.range$trip,pulse.range$max-pulse.range$min+1),
                         cohort=rep(pulse.range$cohort,pulse.range$max-pulse.range$min+1),
                         pulse=rep(pulse.range$dummy_pulse,pulse.range$max-pulse.range$min+1),
                         mmSL=unlist(mapply(seq,pulse.range$min,pulse.range$max)))
# add additinal info such as date, cohort, pulse so that it is present in dataframe
#check the new dataframe
View(pulse.assign)
summary(pulse.assign)
glimpse(pulse.assign)

# assign pulses to age 1 length data
# select age 1 cod
age0length<-length%>%filter(age==0 & year ==2018)%>%select(-pulse)

# assign pulses to subsetted age1 length data
age0.length.pulse<-left_join(age0length,pulse.assign)
View(age0.length.pulse)

# ---- Part 4: Verify pulse assignment ----
# replot pulses and calculate pulse abundances
#  combine age 0 and  with revised age 1 data frame (age1.length.pulse)
# structure new age1 data to match age 0 format
age1.revised<-age1.length.pulse%>%
  group_by(year,month,day,trip,age,pulse)%>%
  summarise(mean=mean(mmSL),min=min(mmSL),max=max(mmSL))
pulse.growth2<- bind_rows(age0,age1.revised)

# visualize pulses
pulse.growth2$date<-ymd(paste(pulse.growth2$year,pulse.growth2$month,pulse.growth2$day,sep="-"))

ggplot(pulse.growth2,aes(x=date,y=mean,shape=factor(pulse)))+geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2015 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

# Review total abundances
View(count0)
View(count1)

# Abundance by pulse
# age 0
# tables
View(age0pulse)
age1pulse<-filter(table1,year==2008 & age ==1)
View(age1pulse)

# ---- Part 5: final pulse assignment ----
# age1.length.pulse
age1final<-age1.length.pulse %>%
  mutate(pulse=replace(pulse,trip==12 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse, trip == 12 & pulse == 1,2))

# combine updated age 1 length data with length data
age0length <- length %>%
  filter(age!=1)
age1length<-length%>%
  filter(age==1 & year <2008)
test<-length%>%
  filter(age==1 & year>2008)
age12008<-length%>%
  filter(age==1 & year ==2008)
dim(test)
dim(age0length)
dim(age1length)
dim(age12008)
dim(length) # not sure why the above filters do not work
all.length %>% filter(age == 1 & year == 2008)
all.length$pulse<-as.character(all.length$pulse)
age1final$pulse<-as.character(age1final$pulse)
final<-bind_rows(all.length,age1final)
glimpse(final)

# export full dataset
write.csv(final,"Newman-Sound-length.csv",row.names = FALSE)