# ---- CMR Condition analysis -----

setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/overwinter-CMR/")

# ---- packages-----
library(tidyverse)
library(car)

library(lubridate)
library(mixdist)
#library(marked)
library(tidyverse)
library(RMark)

source("./misc/pulse_range_fct.R")
# data
data<-read.csv("../data/data-working/CMR-condition.csv")
pulse1<-read.csv("./data/data-working/pulse_range_age1_final.csv")

# check data
str(data)
names(data)
summary(data)

# ---- Condition Factor ----
data%>%
  mutate(K=(body_weight_g/(mmSL*.1)^3)*1000)%>%
  mutate(HSI=((liver_weight_mg*.001)/body_weight_g)*1000)->data

ggplot(data)+geom_jitter(aes(x=site,y=K))
ggplot(data)+geom_jitter(aes(x=site,y=HSI))

mK<-glm(K~site, family = Gamma(link="log"),data=data)
plot(mK)
Anova(mK,type="III")

mHSI<-glm(HSI~site, family=Gamma(link="log"),data=data)
plot(mHSI)
Anova(mHSI,type="III")

# Add pulse
