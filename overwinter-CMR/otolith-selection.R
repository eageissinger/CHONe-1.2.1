# Determine sample size for otolith analysis
# ---- set working directory ----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

# ---- load data ----
data<-read.csv("./data/data-working/CMR-field-May-captures.csv")
pulse1<-read.csv("./data/data-working/pulse_range_age1_final2019-12-6.csv")

# --- load pacakges ----
library(tidyverse)
library(lubridate)

# --- load source functions ----


# --- check data ----
str(data)
summary(data)

str(pulse1)
summary(pulse1)

# ---- format data ----
pulses<-pulse1%>%
  filter(!is.na(min))%>%
  mutate(age=1)%>%
  filter(year==2017 & trip ==10)
  
pulse.assign<-data.frame(trip=rep(pulses$trip,pulses$max-pulses$min+1),
                         year=rep(pulses$year,pulses$max-pulses$min+1),
                         pulse=rep(pulses$pulse,pulses$max-pulses$min+1),
                         age=rep(pulses$age,pulses$max-pulses$min+1),
                         sl=unlist(mapply(seq,pulses$min,pulses$max)))

data$date<-ymd(paste(data$year,data$month,data$day,sep="-"))

data%>%
  filter(month==5 & year == 2017)%>%
  filter(age!=2)->data2

cmr<-left_join(data2,pulse.assign) # bind cmr data and pulses data

# ---- explore numbers ----
# need to know pulse representation from each site
cmr%>%
  group_by(site, pulse)%>%
  summarise(n())

markedfish<-data%>%
  filter(month!=5 & mark == 1 & age ==1)%>%
  left_join(pulse.assign,by=c("sl","age"))%>%
  mutate(fish_num=as.integer(str_sub(animal_id,start=6)))%>%
  distinct(animal_id,fish_num)
markedfish%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl))
cmr%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl))

# pulse 2 fish IDs
cmr%>%
  filter(sl>70 & sl < 97)%>%
  mutate(fish_num=as.integer(str_sub(animal_id,start=6)))%>%
  distinct(animal_id,fish_num)
