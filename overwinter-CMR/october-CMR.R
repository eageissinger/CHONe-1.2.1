### Mark-recapture ###

# ---- set working directory ----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

# ---- load required packages ----

library(lubridate)
library(mixdist)
#library(marked)
library(tidyverse)
library(RMark)

source("./misc/pulse_range_fct.R")

# ---- load data ----
data<-read.csv("./data/data-working/CMR-field-OCTOBER.csv")
fallcatch<-read.csv("./data/data-working/CMR-field-adj.csv")
#subsample<-read.csv("./data/data-working/subsample_wk1-2-field.csv")
pulse0<-read.csv("./data/data-working/CMR-0pulses.csv")
#pulse1<-read.csv("./data/data-working/pulse_range_age1_final.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")

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
fallcatch$date<-ymd(paste(fallcatch$year,fallcatch$month,fallcatch$day,sep="-"))
fallcatch<-fallcatch%>%filter(day==19 & status==1)

data$site<-str_sub(data$animal_id,start = 1,end = 2)
data$id<-as.numeric(str_sub(data$animal_id,start=4))
data.recap<-left_join(data,trips)%>%
  filter(status==1)

data.release<-left_join(data,trips)%>%
  filter(id<1)%>%
  filter(!is.na(sl))


# Pulse assignments

# set up age 1 pulses
#glimpse(pulse1)
#summary(pulse1)
#pulse1<-pulse1%>%
#  mutate(pulse=replace(pulse,pulse==3,2),
#         pulse=replace(pulse,pulse==5,3))%>%
#  filter(!is.na(min))
#pulse.assign1<-data.frame(trip=rep(pulse1$trip,pulse1$max-pulse1$min+1),
#                          year=rep(pulse1$year,pulse1$max-pulse1$min+1),
#                          pulse=rep(pulse1$pulse,pulse1$max-pulse1$min+1),
#                          mmSL=unlist(mapply(seq,pulse1$min,pulse1$max)))

#glimpse(pulse.assign1)
#pulse.assign1<-pulse.assign1%>%
#  mutate(age=1)
glimpse(pulse0)
pulses<-pulse0%>%
  mutate(year=cohort, age=0)%>%
  select(-cohort)%>%
  rename(sl=mmSL)%>%
  filter(year==2016)

#pulses<-bind_rows(pulse0,pulse.assign1)%>%
#  rename(sl=mmSL)%>%
#  filter(year>=2016)

# ----- pulse assign ----
october<-bind_rows(data.recap,data.release)
# assign fall fish

fall.pulses<-left_join(october,pulses,by=c('trip','sl'))

#may.pulses<-pulses%>%
#  filter(year==2017 & age==1 & trip ==10)%>%
#  select(-trip,-year)
#spring.fish<-data%>%
#  filter(id>1)

#spring.pulses<-left_join(spring.fish,may.pulses)

#pulse.size<-bind_rows(fall.pulses,spring.pulses)

# fill in missing pulses

fall.pulses%>%
  ggplot(aes(x=date,y=sl,colour=factor(pulse)))+geom_point()
#ggplot(pulse.size,aes(x=date,y=sl,colour=factor(pulse)))+geom_point()


oct.notrip<-fall.pulses%>%filter(day == 19)%>%
  filter(!is.na(sl))
oct<-bind_rows(oct.notrip,fallcatch)%>%# include captured fish
  select(-pulse,-status)

qplot(sl,data=oct,binwidth=5)
SL<-select(oct,sl)
summarise(SL,min(sl),max(sl))
group.oct<-mixgroup(SL,breaks= c(0,seq(50,95,5),97),
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

oct.notrip<-oct.notrip%>%
  select(-pulse)

pulses.notrip<-left_join(oct.notrip,pulse.assign)

# combine
# pulse.size and pulses.notrip
# take out october no trip from fall.pulses, then re-add pulses.notrip

final1<-fall.pulses[1:136,]

final<-bind_rows(final1,pulses.notrip)


final%>%
  ggplot(aes(y=sl,x=date,colour=factor(pulse)))+
  geom_jitter()

final<-final%>%
  select(animal_id,pulse)

oct.cod<-left_join(data,final)
dim(data)
dim(oct.cod)

# --- format data ----
mrkdata<-oct.cod%>%
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
  select(-year,-month,-day)

# melt then cast, equivalent to gather then spread

mrkdata2<-spread(mrkdata1,date,mark,fill=0)

mrkdata3<-unite(mrkdata2,ch,c("2016-10-14","2016-10-19","2016-10-28"),
                sep="",remove=TRUE)
View(mrkdata3)
unique(mrkdata3$ch)
str(mrkdata3)
duplicated(mrkdata3$animal_id)
cod<-mrkdata3

View(cod)

# ---- CJS Run ----
# Newbridge only
# time and dependent - no length yet

cod$sl<-as.numeric(cod$sl)
nb.all<-cod%>%
  select(ch)%>%
  filter(ch!="000")%>%
  data.frame()

nb.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb.all,time.intervals = c(5,9))
  nb.ddl<-make.design.data(nb.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  Phi.time<-list(formula=~time)
  # define models for p
  p.dot<-list(formula=~1)
  p.time<-list(formula=~time)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=nb.processed,ddl=nb.ddl))
  
}
nb.results<-nb.model()
nb.results
summary(nb.results[[2]])

nb.pulse<-cod%>%
  select(ch,pulse)%>%
  filter(ch!="000")%>%
  data.frame()
unique(nb.pulse$pulse)
nb.pulse$pulse<-as.factor(nb.pulse$pulse)
str(nb.pulse)

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
  p.time<-list(formula=~time)
  p.pulse<-list(formula=~pulse)
  p.timepluspulse<-list(formula=~time+pulse)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=nb.processed,ddl=nb.ddl))
}
pulse.results<-pulse.model()

pulse.results
