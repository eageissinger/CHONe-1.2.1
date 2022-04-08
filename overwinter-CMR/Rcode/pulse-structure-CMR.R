## Pulse structure for field fish
# Purpose: assign pulses to fish captured and marked in the fall
# 

# --- set working directory ----
# setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/overwinter-CMR/")

# --- load packages ----
library(tidyverse)
library(mixdist)
library(lubridate)

# load source function
source("./pulse-structure/pulse_range_fct.R") # describe basic function processes here*****

# --- load data ----
fallcatch<-read.csv("./data/data-working/CMR/CMR-field-adj.csv") # fish captured and marked in October 2016
tripdates<-read.csv("./data/data-working/newman-trips.csv") # trip numbers and associated dates
length0<-read.csv("./data/data-working/newman-length.csv")%>% # length data and assigned pulses
  filter(Species=="AC" & Age == 0)%>% # select age-0 Atlantic cod
  mutate(Date=ymd(paste(Year,Month,Day,sep='-'))) # format date
length1<-read.csv("./data/data-working/newman-length.csv")%>% # length data and assigned pulses
  filter(Species=="AC" & Age == 1 )%>% # select age-1 Atlantic cod
  mutate(Date=ymd(paste(Year,Month,Day,sep="-"))) # format date

# --- check data ----
head(fallcatch)
head(tripdates)
head(length0)

# format dates
fallcatch$date<-ymd(paste(fallcatch$year,fallcatch$month,fallcatch$day,sep="-"))


# ---- Pulse structure -----
head(fallcatch)
head(length0)

# min and max mmSL for each pulse for age-0 cod in October 2016 trips
pulses<-length0%>%
  filter(Year==2016)%>%
  filter(Month==10)%>%
  group_by(Trip,Pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))

# create list of all possible lengths for each age-0 pulse in October 2016 trips
pulse_assign<-data.frame(trip=rep(pulses$Trip,pulses$max-pulses$min+1),
                         pulse=rep(pulses$Pulse,pulses$max-pulses$min+1),
                         mmSL=unlist(mapply(seq,pulses$min,pulses$max)))

cmr.fish<-fallcatch%>%
  filter(site=="NB")%>% # select marking site
  mutate(id=as.numeric(str_sub(animal_id,start=4)))%>% # create id column
  filter(id<2 & status==1 | id<1)%>% # select fish marked in October
  select(-mark,-status)%>%
  distinct()%>% # remove potential duplicates
  left_join(tripdates)%>% # add trip numbers
  rename(mmSL=sl)%>% #format data to match joining dataframe
  left_join(pulse_assign)%>% # add pulse assignments
  filter(!is.na(mmSL))%>% # remove empty rows
  mutate(date=ymd(paste(year,month,day,sep="-"))) # format date

# plot current pulse assignments
ggplot(cmr.fish)+
  geom_jitter(aes(x=date,y=mmSL,colour=factor(pulse)))

#plot full Fall data with CMR fish
length0%>%
  filter(Year==2016)%>%
  filter(Month>8)%>%
  ggplot()+
  geom_jitter(aes(x=Date,y=mmSL,colour=factor(Pulse)))+
  geom_jitter(data=cmr.fish,aes(x=date,y=mmSL,colour=factor(pulse)))

# assign pulse structure to remaining fish
cmr.fish2<-cmr.fish%>%
  mutate(pulse=replace(pulse,trip==20 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,is.na(trip) & mmSL<68,2))%>%
  mutate(pulse=replace(pulse,is.na(trip) & mmSL>=68,1))

# check assignments
ggplot(cmr.fish2)+
  geom_jitter(aes(x=date,y=mmSL,colour=factor(pulse)))

#check assignments with full fall data and CMR fish
length0%>%
  filter(Year==2016)%>%
  filter(Month>8)%>%
  ggplot()+
  geom_jitter(aes(x=Date,y=mmSL,colour=factor(Pulse)))+
  geom_jitter(data=cmr.fish2,aes(x=date,y=mmSL,colour=factor(pulse)))

# check result output/summaries
head(cmr.fish)
head(cmr.fish2)
cmr.fish2%>%
  filter(date=='2016-10-14')%>%
  group_by(pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL),sd=sd(mmSL))
cmr.fish2%>%
  filter(date=='2016-10-19')%>%
  group_by(pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL),sd=sd(mmSL))

# spring (age 1)
# min and max mmSL for each pulse for age-1 cod in May 2017 trips
pulses1<-length1%>%
  filter(Year==2017)%>%
  filter(Month==5)%>%
  group_by(Pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))

# create list of all possible lengths for each age-1 pulse in May 2017 trips
pulse_assign1<-data.frame(pulse=rep(pulses1$Pulse,pulses1$max-pulses1$min+1),
                         mmSL=unlist(mapply(seq,pulses1$min,pulses1$max)))


spring<-fallcatch%>%
  mutate(id=as.numeric(str_sub(animal_id,start=4)))%>% # create ID column
  filter(id>2 & status==1)%>% # select fish that were captured in May
  filter(age==1)%>% # age 1 only
  rename(mmSL=sl)%>% # rename to match formatting
  left_join(pulse_assign1)%>% # add pulse assignments
  mutate(date=ymd(paste(year,month,day,sep="-")))%>% # format date
  mutate(pulse=replace(pulse,mmSL>140,1))%>% # assign outliers
  mutate(pulse=replace(pulse,mmSL==62,2))%>% # assign outliers
  mutate(pulse=replace(pulse,mmSL<40,4)) # assign outliers

# check assignments
ggplot(spring)+
  geom_jitter(aes(x=date,y=mmSL,colour=factor(pulse)))

# data summary
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

# create list of pulse ranges for age 0 and age 1 cod
pulses
pulses1<-spring%>%
  group_by(pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))%>%
  mutate(age=1, trip = 10, year= 2017)%>%
  add_row(pulse=3,min=62,max=68,age=1,trip=10,year=2017)%>% # add "potential third pulse"
  mutate(min=replace(min,pulse==2,69))%>% # update pulse 2 based on pulse 3 inclusion
  ungroup()
  
pulses0<-cmr.fish2%>%
  group_by(date,trip,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))%>%
  mutate(age=0,year=2016)%>%
  ungroup()%>%
  select(-date)

pulses0
pulses1

# combine into single dataframe
pulse.list<-bind_rows(pulses0,pulses1)

# save file
write.csv(pulse.list,"./data/data-working/CMR/CMR-pulses.csv",row.names=FALSE)
