# --- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-master/")

# --- load packages ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(mixdist)
library(lubridate)

# --- load data ----
length<-read.csv("./data/data-working/newman-length.csv")
trips<-read.csv("./data/data-working/trip-dates-newman.csv")
catch<-read.csv("./data/data-working/newman-catch.csv")
haul<-read.csv("./data/data-working/catch_haul.csv")

# --- format dates ----
head(length)
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
# remove current trips column
length<-select(length,-trip)

head(trips)
trips<-rename(trips,trip=Trip)

trips$year<-as.numeric(str_sub(trips$Date,start=1,end=4))
trips$month<-as.numeric(str_sub(trips$Date,start=5,end=6))
trips$day<-as.numeric(str_sub(trips$Date,start=7,8))
trips<-select(trips,-Date)
trips$date<-ymd(paste(trips$year,trips$month,trips$day,sep="-"))
head(trips)

head(catch)
catch<-left_join(catch,trips)
catch$date<-ymd(paste(catch$year,catch$month,catch$day,sep="-"))

# --- combine length and trip data ----
length<-left_join(length,trips)

# ---- Part 1: Mixture Distributions ----
# May
# --- select data ----
may2008<-filter(length, age ==1 & year == 2008 & month == 5)

# --- check histogram ----
qplot(mmSL, data = may2008, binwidth = 5)

# --- create dataframe with SL only ----
maySL <- select(may2008, mmSL)

# ---- determine min and max ----
summarise(maySL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group.may2008<-mixgroup(maySL, breaks = c(0,seq(45,120,5),125),
                        xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group.may2008)

# ---- set initial parameters ----
may.par<-mixparam(c(60,100),c(5),pi=NULL)
plot(group.may2008,may.par,"gamma")
may.par<-mixparam(c(60,90,120),c(4,5,6),pi=NULL)
plot(group.may2008,may.par,"gamma")

# fit mixture
fit1<-mix(group.may2008,may.par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

head(may2008)
trip10<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(trip=10,year=2008,month=5,day=20)
trip10<-mutate(trip10,dummy_pulse=rev(seq(1:nrow(trip10))))

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
trip12<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(trip=12,year=2008,month=7,day=3)
trip12<-mutate(trip12,dummy_pulse=rev(seq(1:nrow(trip12))))

#July week 2
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
trip13<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(trip=13,year=2008,month=7,day=16)
trip13<-mutate(trip13,dummy_pulse=rev(seq(1:nrow(trip13))))

# August
age1.2008<-filter(length, year==2008 & age ==1 & month ==8 & trip ==16)
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
trip16<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(trip=16,year=2008,month=8,day=28)
trip16<-mutate(trip16,dummy_pulse=rev(seq(1:nrow(trip16))))


# September
age1.2008<-filter(length,year==2008 & age==1 & month == 9 & trip ==18)
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
trip18<-bind_cols(fit1$parameters,fit1$se)%>%
  mutate(trip=18, year=2008,month=9,day=29)
trip18<-mutate(trip18,dummy_pulse=rev(seq(1:nrow(trip18))))
# store results in a dataframe
mixtures<-bind_rows(trip10,trip12,trip13,trip16,trip18)%>%
  mutate(cohort=year-1)

# ---- Part 2: Estimating pulses ----
age0<-length%>%
  filter(year == 2007 & age==0)%>%
  group_by(year,month,day,trip,age,pulse)%>%
  summarise(mean=mean(mmSL),min=min(mmSL),max=max(mmSL))%>%
  mutate(cohort=year)
glimpse(age0)
age0$pulse<-as.integer(as.character(age0$pulse))


age1<-mixtures%>%
  rename(mean=mu,pulse = dummy_pulse)%>%
  mutate(min=mean-sigma,max=mean+sigma,age=1)%>%
  select(-sigma,-sigma.se,-mu.se,-pi,-pi.se)

#  combine age 0 and age 1 data frames
pulse.growth<- bind_rows(age0,age1)
unique(pulse.growth$pulse)

# visualize pulses
pulse.growth$date<-ymd(paste(pulse.growth$year,pulse.growth$month,pulse.growth$day,sep="-"))

ggplot(pulse.growth,aes(x=date,y=mean,shape=factor(pulse)))+geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0)+
  theme_bw()+
  ggtitle("2015 Cohort")+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))

# calculate total and pulse abundances
# age 0 and age 1 Totals
count0<-catch%>%
  filter(age==0 & year == 2007)%>%
  group_by(year,month,trip,age)%>%
  summarise(count=sum(count))%>%
  ungroup()%>%
  group_by(year,month,age)%>%
  summarise(count=mean(count))
count1<-catch%>%
  filter(age==1 & year ==2008)%>%
  group_by(year,age,month)%>%
  summarise(count=sum(count))%>%
  ungroup()%>%
  group_by(year,month,age)%>%
  summarise(count=mean(count)) 

# age 0 by pulse
# tables
catch2<-haul%>%
  select(year,month,trip,age,total_catch,total_measured,extrap_1,extrap_2,
         extrap_3,extrap_4,extrap_5,extrap_6,extrap_unknown,total)%>%
  gather(key="extrap",value = "catch_haul",c(7:13),na.rm = FALSE)%>%
  mutate(total_measured=replace(total_measured,total_catch==0,0))%>%
  mutate(catch_haul=replace(catch_haul,total_catch==0,0))%>%
  mutate(pulse=str_sub(extrap,start=8,end = 8))%>%
  mutate(pulse=replace(pulse,pulse=="u",NA))

table1<-catch2%>%
  group_by(year,month,trip,age,pulse,total_catch,total_measured)%>%
  summarise(catch_per_haul=mean(catch_haul))

age0pulse<-filter(table1,year==2007 & age ==0)


# ---- Part 3: Pulse Assignments ----
# make dataframe with min and max pulse ranges
# use +/- standard deviation for "min" and "max"
pulse.range<-mixtures%>%
  group_by(year,month,day,trip,dummy_pulse,year,cohort)%>%
  summarise(min=(mu-sigma),max=mu+sigma,sd=sigma)%>%
  mutate(min_round=floor(min),max_round=ceiling(max))%>%# add rounded min and max to create an integer column. Min is rounded down, max is rounded up
  ungroup()%>%
  group_by(year,month,day)%>%
  mutate(diff=(lag(min_round)-max_round)/2)%>% # split difference between the pulse groups
  mutate(plusmax=floor(diff))%>% # round to prevent overlap
  mutate(minusmin=floor(lead(diff)))%>% # round to prevent overlap
  replace_na(list(plusmax=0,minusmin=0))%>% # change NAs to 0
  mutate(min_final=min_round-minusmin,max_final=max_round+plusmax)%>% # add/subtract appropriate terms
  mutate(lagmin=lag(min_final))%>% # create a lag column of min values to compare to max
  mutate(max1=replace(max_final,lagmin==max_final,1))%>% # any point where lagmin and lagmax match, put in a 1
  mutate(max1=replace(max1,max1==max_final,0))%>% # 0 for all others
  mutate(max_final2=max_final-max1)%>% # subtract 1 from max final (gets rid of any remaining overlap)
  select(-diff,-plusmax,-minusmin,-max_final,-lagmin,-max1)%>% #get rid of useless columns
  mutate(adjustmax=ceiling(2*sd))%>% # adjust maximum value to be 2 SDs for pulse 1
  mutate(adjustmax=replace(adjustmax,dummy_pulse!=1,0))%>% # applies the above function to only pulse 1
  mutate(adjustmin=ceiling(2*sd))%>% # adjust maximum value to be 2 SDs for last pulse
  mutate(adjustmin=replace(adjustmin,dummy_pulse!=max(dummy_pulse),0))%>% # applies above function to last pulse only
  mutate(max_final3=adjustmax+max_final2,
         min_final3=min_final-adjustmin)%>% # create final max and min columns
  select(year,month,day,trip,dummy_pulse,year,cohort,min_final3,max_final3)%>% # select final columns
  rename(min=min_final3,max=max_final3) # rename to be min and max
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
age1length<-length%>%filter(age==1 & year ==2008)%>%select(-pulse)

# assign pulses to subsetted age1 length data
age1.length.pulse<-left_join(age1length,pulse.assign)
View(age1.length.pulse)

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
all.length <- length %>%
  filter(age != 1 & year != 2008)
age12008<-length%>%
  filter(age==1 & year ==2008)
dim(all.length)
dim(age12008)
dim(length) # not sure why the above filters do not work
all.length %>% filter(age == 1 & year == 2008)
all.length$pulse<-as.character(all.length$pulse)
age1final$pulse<-as.character(age1final$pulse)
final<-bind_rows(all.length,age1final)
glimpse(final)

# export full dataset
write.csv(final,"Newman-Sound-length.csv",row.names = FALSE)