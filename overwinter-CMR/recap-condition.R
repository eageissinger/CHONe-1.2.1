# ---- CMR Condition analysis -----

setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/overwinter-CMR/")

# ---- packages-----
library(tidyverse)
library(car)

library(lubridate)
library(mixdist)
#library(marked)
library(tidyverse)


# data
data<-read.csv("../data/data-working/CMR-condition.csv")
pulse1<-read.csv("../data/data-working/pulse_range_age1_final2019-12-6.csv")
tripdates<-read.csv("../data/data-working/newman-trips.csv")
recap<-read.csv("../data/data-working/CMR-wide.csv")

# check data
str(data)
names(data)
summary(data)

# format fish id to match animal_id
data<-data%>%
  mutate(animal_id=str_c(site,2,fish,sep="."))%>%
  mutate(animal_id=str_replace(animal_id,'.2.','_2.'))

#---- Add pulse ----
head(pulse1)


pulses<-left_join(pulse1,tripdates)%>%
  filter(year==2017 & month == 5)

pulse.assign<-data.frame(trip=rep(pulses$trip,pulses$max-pulses$min+1),
                         year=rep(pulses$year,pulses$max-pulses$min+1),
                         pulse=rep(pulses$pulse,pulses$max-pulses$min+1),
                         age=rep(pulses$age,pulses$max-pulses$min+1),
                         mmSL=unlist(mapply(seq,pulses$min,pulses$max)))

data<-left_join(data,pulse.assign)

# mark presence/absence
# ---- Condition Factor ----
data%>%
  mutate(K=(body_weight_g/(mmSL*.1)^3)*1000)%>%
  mutate(HSI=((liver_weight_mg*.001)/body_weight_g)*1000)->data

ggplot(data)+geom_jitter(aes(x=site,y=K,colour=factor(pulse)))
ggplot(data)+geom_jitter(aes(x=site,y=HSI,colour=factor(pulse)))

# models

mK<-glm(K~site, family = Gamma(link="log"),data=data)
plot(mK)
Anova(mK,type="III")

mHSI<-glm(HSI~site, family=Gamma(link="log"),data=data)
plot(mHSI)
Anova(mHSI,type="III")

pulse.K<-glm(K~site*factor(pulse),family=Gamma(link="log"),data=data)
plot(pulse.K)
hist(resid(pulse.K))
Anova(pulse.K,type="III")

pulse.HSI<-glm(HSI~site*factor(pulse),family=Gamma(link="log"),data=data)
plot(pulse.HSI)
hist(resid(pulse.HSI))
Anova(pulse.HSI,type="III")

## NEED TO INCORPORATE RECAPTURE VS NO RECAP
head(recap)
recap%>%
  filter(X2017.05.24==1)%>%
  select(animal_id,sl,pulse,site,recap)->recapture

data2<-left_join(data,recapture)%>%
  filter(!is.na(recap))%>%
  mutate(pulse=replace(pulse,is.na(pulse) & sl<=62,4))%>%
  mutate(pulse=replace(pulse,is.na(pulse) & sl>135,1))
str(data2)


# full models
mK.full<-glm(K~site+factor(pulse)+recap,data=data2,
             family=Gamma(link="log"))
plot(mK.full)
hist(resid(mK.full))
Anova(mK.full)

mHSI.full<-glm(HSI~site+factor(pulse)+recap,data=data2,
               family=Gamma(link="log"))
plot(mHSI.full)
hist(resid(mHSI.full))
Anova(mHSI.full)

ggplot(data2,aes(x=factor(pulse),y=HSI))+geom_boxplot()

ggplot(data2,aes(x=factor(pulse),y=HSI))+geom_boxplot()+
  geom_jitter(aes(x=factor(pulse),y=HSI,colour=site))

# determine where the 62 mm fish fit
ggplot(data2,aes(x=site,y=sl,colour=factor(pulse)))+geom_point()
