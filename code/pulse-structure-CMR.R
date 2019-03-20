# --- set working directory ----
setwd("C:/Users/eageissinger/Documents/Emilie-Lab-comp/")

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
yr2016<-length%>%
  filter(year==2016)
# trip 19
# --- select data ----
oct2016<-filter(yr2016, age == 0 & trip==19)

# --- check histogram ----
qplot(mmSL, data = oct2016, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(oct2016, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group2016<-mixgroup(SL, breaks = c(0,seq(45,95,5),99),
                        xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group2016)

# ---- set initial parameters ----
par<-mixparam(c(55,80),c(5),pi=NULL)
plot(group2016,par,"gamma")

# fit mixture
fit1<-mix(group2016,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

head(oct2016)
trip19<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(trip=19,year=2016,month=10,day=13)
trip19<-mutate(trip19,dummy_pulse=rev(seq(1:nrow(trip19))))

# October trip 20
oct2016<-filter(yr2016, age == 0 & trip==20)

# --- check histogram ----
qplot(mmSL, data = oct2016, binwidth = 5)

# --- create dataframe with SL only ----
SL <- select(oct2016, mmSL)

# ---- determine min and max ----
summarise(SL, min(mmSL), max(mmSL))

# ---- convert to frequency table ----
group2016<-mixgroup(SL, breaks = c(0,seq(35,105,5),110),
                    xname=NULL, k = NULL, usecondit = FALSE)
# --- plot frequency table ----
plot(group2016)

# ---- set initial parameters ----
par<-mixparam(c(45,65,80),c(5),pi=NULL)
plot(group2016,par,"gamma")

# fit mixture
fit1<-mix(group2016,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

head(oct2016)
trip20<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(trip=20,year=2016,month=10,day=27)
trip20<-mutate(trip20,dummy_pulse=rev(seq(1:nrow(trip20))))

# store results in a dataframe
mixtures<-bind_rows(trip19,trip20)%>%
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

write.csv(pulse.assign,"./data/data-working/CMR-0pulses.csv",row.names=FALSE)

