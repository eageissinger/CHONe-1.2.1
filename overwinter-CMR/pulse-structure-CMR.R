## Mixture distributions for the non-trip fish

# --- set working directory ----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/overwinter-CMR/")

# --- load packages ----
library(tidyverse)
library(mixdist)
library(lubridate)

# load source functions
source("../pulse-structure/pulse_range_fct.R")
# --- load data ----
fallcatch<-read.csv("../data/data-working/CMR-field-adj.csv")
tripdates<-read.csv("../data/data-working/newman-trips.csv")
catch<-read.csv("../data/data-working/newman-catch.csv")
haul<-read.csv("../data/data-working/catch_haul.csv")

# --- format dates ----
head(fallcatch)
head(tripdates)

# remove current trips column
fallcatch$date<-ymd(paste(fallcatch$year,fallcatch$month,fallcatch$day,sep="-"))

#head(catch)
#catch<-left_join(catch,trips)
#catch$date<-ymd(paste(catch$year,catch$month,catch$day,sep="-"))

# ---- Part 1: Mixture Distributions age 0 ----
# ----- pulse assign ----
data<-fallcatch%>%
  filter(site=="NB")%>%
  mutate(id=as.numeric(str_sub(animal_id,start=4)))%>%
  filter(id<2)
data<-left_join(data,tripdates)%>%
  filter(is.na(trip))%>%
  filter(status==1 & id>=1 | id<1)%>%
  filter(!is.na(sl))

# fill in missing pulses
ggplot(data,aes(x=date,y=sl))+geom_jitter()

qplot(sl,data=data,binwidth=5)
SL<-select(data,sl)
summarise(SL,min(sl),max(sl))
group.oct<-mixgroup(SL,breaks= c(0,seq(50,90,5),93),
                    xname=NULL,k=NULL,usecondit=FALSE)
plot(group.oct)

# ---- set initial parameters ----
par<-mixparam(c(60,80),c(5),pi=NULL)
plot(group.oct,par,"gamma")

# fit mixture
fit1<-mix(group.oct,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

par2<-mixparam(c(52,62,80),c(5),pi=NULL)
plot(group.oct,par2,"gamma")

fit2<-mix(group.oct,par2,dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)
summary(fit2)
plot(fit2)
plot(fit2,root=T)

head(oct)
notrip<-bind_cols(fit2$parameters, fit2$se)%>%
  mutate(trip=1,year=2016,month=10,day=19,cohort=2016)
notrip<-mutate(notrip,dummy_pulse=rev(seq(1:nrow(notrip))))

pulse.range<-pulse_range(notrip)


# use min and max for each pulse to then create a dataframe with all length possibilities per pulse

pulse.assign<-data.frame(trip=rep(pulse.range$trip,pulse.range$max-pulse.range$min+1),
                         cohort=rep(pulse.range$cohort,pulse.range$max-pulse.range$min+1),
                         pulse=rep(pulse.range$dummy_pulse,pulse.range$max-pulse.range$min+1),
                         sl=unlist(mapply(seq,pulse.range$min,pulse.range$max)))

pulse.assign<-pulse.assign%>%
  mutate(year=cohort)%>%
  select(-cohort,-trip,-year)

write.csv(pulse.assign,"../data/data-working/CMR-0pulses.csv",row.names=FALSE)

