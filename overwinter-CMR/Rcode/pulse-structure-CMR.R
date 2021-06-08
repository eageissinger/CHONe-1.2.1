## Mixture distributions for the non-trip fish

# --- set working directory ----
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/overwinter-CMR/")

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
length<-read.csv("../data/output/length_pulse_0_final.csv")%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))
length1<-read.csv("../data/output/length_pulse_age1.csv")%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))

# --- format dates ----
head(fallcatch)
head(tripdates)
head(length)

# remove current trips column
fallcatch$date<-ymd(paste(fallcatch$year,fallcatch$month,fallcatch$day,sep="-"))

#head(catch)
#catch<-left_join(catch,trips)
#catch$date<-ymd(paste(catch$year,catch$month,catch$day,sep="-"))


# Pulse structure 
head(fallcatch)
head(catch)
head(length)

pulses<-length%>%
  filter(year==2016)%>%
  filter(month==10)%>%
  group_by(trip,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))

# add pulses for known trips
pulse_assign<-data.frame(trip=rep(pulses$trip,pulses$max-pulses$min+1),
                         pulse=rep(pulses$pulse,pulses$max-pulses$min+1),
                         mmSL=unlist(mapply(seq,pulses$min,pulses$max)))

cmr.fish<-fallcatch%>%
  filter(site=="NB")%>%
  mutate(id=as.numeric(str_sub(animal_id,start=4)))%>%
  filter(id<2 & status==1 | id<1)%>%
  select(-mark,-status)%>%
  distinct()%>%
  left_join(tripdates)%>%
  rename(mmSL=sl)%>%
  left_join(pulse_assign)%>%
  filter(!is.na(mmSL))%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))

ggplot(cmr.fish)+
  geom_jitter(aes(x=date,y=mmSL,colour=factor(pulse)))

length%>%
  filter(year==2016)%>%
  filter(month>8)%>%
  ggplot()+
  geom_jitter(aes(x=date,y=mmSL,colour=factor(pulse)))+
  geom_jitter(data=cmr.fish,aes(x=date,y=mmSL,colour=factor(pulse)))

# assign pulse structure 
cmr.fish2<-cmr.fish%>%
  mutate(pulse=replace(pulse,trip==20 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,is.na(trip) & mmSL<68,2))%>%
  mutate(pulse=replace(pulse,is.na(trip) & mmSL>=68,1))

ggplot(cmr.fish2)+
  geom_jitter(aes(x=date,y=mmSL,colour=factor(pulse)))

length%>%
  filter(year==2016)%>%
  filter(month>8)%>%
  ggplot()+
  geom_jitter(aes(x=date,y=mmSL,colour=factor(pulse)))+
  geom_jitter(data=cmr.fish2,aes(x=date,y=mmSL,colour=factor(pulse)))




head(cmr.2)
head(cmr.fish2)
cmr.fish2%>%
  filter(date=='2016-10-14')%>%
  group_by(pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL),sd=sd(mmSL))
cmr.fish2%>%
  filter(date=='2016-10-19')%>%
  group_by(pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL),sd=sd(mmSL))

# spring catch summary
# 
pulses1<-length1%>%
  filter(year==2017)%>%
  filter(month==5)%>%
  group_by(pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))

# add pulses for known trips
pulse_assign1<-data.frame(pulse=rep(pulses1$pulse,pulses1$max-pulses1$min+1),
                         mmSL=unlist(mapply(seq,pulses1$min,pulses1$max)))
spring<-fallcatch%>%
  mutate(id=as.numeric(str_sub(animal_id,start=4)))%>%
  filter(id>2 & status==1)%>%
  filter(age==1)%>%
  rename(mmSL=sl)%>%
  left_join(pulse_assign1)%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  mutate(pulse=replace(pulse,mmSL>140,1))%>%
  mutate(pulse=replace(pulse,mmSL==62,2))%>%
  mutate(pulse=replace(pulse,mmSL<40,4))

ggplot(spring)+
  geom_jitter(aes(x=date,y=mmSL,colour=factor(pulse)))
  
spring%>%
  filter(site=="NB")%>%
  group_by(pulse)%>%
  summarise(n(),min(mmSL),max(mmSL),mean(mmSL),sd(mmSL))
spring%>%
  filter(site=="MI")%>%
  group_by(pulse)%>%
  summarise(n(),min(mmSL),max(mmSL),mean(mmSL),sd(mmSL))
spring%>%
  filter(site=="CC")%>%
  group_by(pulse)%>%
  summarise(n(),min(mmSL),max(mmSL),mean(mmSL),sd(mmSL))

pulses
pulses1<-spring%>%
  group_by(pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))%>%
  mutate(age=1, trip = 10, year= 2017)%>%
  ungroup()
  
pulses0<-cmr.fish2%>%
  group_by(date,trip,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))%>%
  mutate(age=0,year=2016)%>%
  ungroup()%>%
  select(-date)

pulses0
pulses1

pulse.list<-bind_rows(pulses0,pulses1)

write.csv(pulse.list,"../data/data-working/CMR-pulses.csv",row.names=FALSE)
