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

# check data
str(data)
names(data)
summary(data)

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