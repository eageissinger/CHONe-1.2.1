### Mark-recapture ###

# ---- set working directory ----
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/overwinter-CMR/")

# ---- load required packages ----

library(lubridate)
library(mixdist)
#library(marked)
library(tidyverse)
library(RMark)


# ---- load data ----
data<-read.csv("../data/data-working/CMR/mark-history-october.csv")
#subsample<-read.csv("./data/data-working/subsample_wk1-2-field.csv")
CMRpulse<-read.csv("../data/data-working/CMR/CMR-pulses.csv")
trips<-read.csv("../data/data-working/newman-trips.csv")
# ---- check data ----
names(data)
str(data)
summary(data)
head(data)

#names(subsample)
#str(subsample)
#summary(subsample)
#head(subsample)

str(trips)
# format date
data$date<-ymd(paste(data$year,data$month,data$day,sep="-"))
#subsample$date<-ymd(paste(subsample$Year,subsample$Month,subsample$Day,sep="-"))

trips$date<-ymd(paste(trips$year,trips$month,trips$day,sep="-"))

data$site<-str_sub(data$animal_id,start = 1,end = 2)
data$id<-as.numeric(str_sub(data$animal_id,start=4))
october<-left_join(data,trips)




# ----- pulse assign ----
# Pulse assignments
# Pulse assignments
glimpse(october)
summary(october)
str(october)

pulse.assign<-data.frame(trip=rep(CMRpulse$trip,CMRpulse$max-CMRpulse$min+1),
                         year=rep(CMRpulse$year,CMRpulse$max-CMRpulse$min+1),
                         pulse=rep(CMRpulse$pulse,CMRpulse$max-CMRpulse$min+1),
                         age=rep(CMRpulse$age,CMRpulse$max-CMRpulse$min+1),
                         mmSL=unlist(mapply(seq,CMRpulse$min,CMRpulse$max)))

# assign pulse to fall fish
fall.pulses<-october%>%
  mutate(id=as.numeric(str_sub(animal_id,start=4)))%>%
  left_join(trips)%>%
  rename(mmSL=sl)%>%
  distinct()%>%
  filter(age==0)%>%
  left_join(pulse.assign)

fall.pulses%>%
  ggplot(aes(x=date,y=mmSL,colour=factor(pulse)))+geom_point()
#ggplot(pulse.size,aes(x=date,y=sl,colour=factor(pulse)))+geom_point()


# --- format data ----
mrkdata<-fall.pulses%>%
  rename(sl=mmSL)%>%
  select(date,year,month,animal_id,sl,mark,pulse)%>%
  data.frame()

# Determine time-steps
collection.dates<-distinct(mrkdata,date)
collection.dates$julian<-yday(collection.dates$date)
293-288
302-293
# 5; 9
# ---- format for RMark ----

mrkdata1<-mrkdata%>%
  select(-year,-month)

# melt then cast, equivalent to gather then spread

mrkdata2<-spread(mrkdata1,date,mark,fill=0)

mrkdata3<-unite(mrkdata2,ch,c("2016-10-14","2016-10-19","2016-10-28"),
                sep="",remove=TRUE)
head(mrkdata3)
unique(mrkdata3$ch)
str(mrkdata3)
duplicated(mrkdata3$animal_id)
cod<-mrkdata3%>%
  filter(ch!="000")

head(cod)

# determine the proportion of fish caught (100 and 010) for each pulse for entire sound and apply the ratio to the unmeasured fish
# week 1
cod%>%filter(ch=="100")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(N=n())%>%
  mutate(ratio=N/52)

W1<-cod%>%filter(ch=="100")%>%
  filter(is.na(sl))%>% #88 unassigned fish
  select(-pulse)%>% # remove current pulse column
  add_column(pulse=c(rep(1, each=54),rep(2,each=58))) # assign pulses to unmeasured fish based on ratio

# summary of week 1
W1%>%
  group_by(pulse)%>%
  summarise(n())
cod%>%filter(ch=="100")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl),mean(sl), sd(sl))

116*0.0192 # pulse 1
116*0.0577 # pulse 2

2+7

#week 2
cod%>%filter(ch=="010")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(N=n())%>%
  mutate(ratio=N/29)

W2<-cod%>%filter(ch=="010")%>%
  filter(is.na(sl))%>% #49 unassigned fish
  select(-pulse)%>% # remove current pulse column
  add_column(pulse=c(rep(1, each=4),rep(2,each=9))) # assign pulses to unmeasured fish based on ratio
#summary of week 2
W2%>%
  group_by(pulse)%>%
  summarise(n())
cod%>%filter(ch=="010")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl),mean(sl), sd(sl))
49*0.483 # pulse 1
49*0.517 # pulse 2
24+25

non.SL<-bind_rows(W1,W2)

#add full pulse structure to dataset
cod<-cod%>%
  filter(!is.na(pulse))%>%
  bind_rows(non.SL)
# ---- CJS Run ----
# Newbridge only
# time and dependent - no length yet

cod$sl<-as.numeric(cod$sl)
nb.pulse<-cod%>%
  select(ch,pulse)%>%
  filter(pulse!=3)%>%
  data.frame()

pulse.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb.pulse,time.intervals = c(5,9),
                             groups="pulse")
  nb.ddl<-make.design.data(nb.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  Phi.time<-list(formula=~time)
  Phi.pulse<-list(formula=~pulse)
  Phi.timepluspulse<-list(formula=~time+pulse)
  
  # define models for p
  p.dot<-list(formula=~1)
  #p.time<-list(formula=~time)
  #p.pulse<-list(formula=~pulse)
  #p.timepluspulse<-list(formula=~time+pulse)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=nb.processed,ddl=nb.ddl))
}
pulse.results<-pulse.model()

pulse.results
pulse.results[[4]]
