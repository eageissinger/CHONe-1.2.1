# ---- set working directory -----
setwd("C:/Users/USER/Documents/Research/CHONe-1.2.1/")

# load data ----
tank_survival<-read.csv("./data/data-working/tank-survival-exp.csv",header=TRUE)
temp<-read.csv("./data/data-working/temperature-exp.csv")

# ---- load packages ----
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(ggfortify)
library(ranger)
library(strucchange)
# data manimpulation
glimpse(tank_survival)
tank_survival<- tank_survival%>%
  rename(year=ï..year)
glimpse(temp)
temp<-temp%>%
  rename(year=ï..year)
tank_survival<-left_join(tank_survival,temp)
df1<-tank_survival%>% 
  uncount(number) %>%
  mutate(status = 0)

# create dataframe with the dead fish
df2<-tank_survival%>%
  mutate(total=9)%>%
  mutate(dead=total-number)%>%
  uncount(dead)%>%
  mutate(status = 1)

df3<-bind_rows(df1,df2)

# check if it worked
df3%>%filter(tank==13,month == 3 & day ==5)

# Format date
df3$date<-ymd(paste(df3$year,df3$month,df3$day,sep="-"))
df3$time<-yday(df3$date)
mydata<-df3%>%mutate(time=replace(time,time==366,0))
levels(mydata$ration)
levels(mydata$size)

mydata$ration<-relevel(mydata$ration,"2.0%","1.0%","0.5%","0.0%")
# build standard suvival object
so<-with(mydata,Surv(time,status))
head(so,80)

# General

s_fit<-survfit(Surv(time,status)~1,data=mydata)
summary(s_fit)
summary(s_fit,times=c(1,20,40,60,80,100,120))
autoplot(s_fit)

s_trt<-survfit(Surv(time,status)~ration,data=mydata)
summary(s_trt,times=c(1,20,40,60,80,100,120))
autoplot(s_trt)

s_size<-survfit(Surv(time,status)~size,data=mydata)
summary(s_size,times=c(1,20,40,60,80,100,120))
autoplot(s_size)


# Large cod
large<-mydata%>%filter(size=="large")
large_trt<-survfit(Surv(time,status)~ration,data=large)
summary(large_trt,times=c(1,20,40,60,80,100,120))
autoplot(s_trt)
summary(large_trt)

# Small cod
small<-mydata%>%filter(size=="small")
small_trt<-survfit(Surv(time,status)~ration,data=small)
summary(small_trt,times=c(1,20,40,60,80,100,120))
autoplot(s_trt)


# Cox Proportional Hazards Model
cox<-coxph(Surv(time,status)~ration+size,data=mydata)
summary(cox)
cox_fit<-survfit(cox)
autoplot(cox_fit)

# Testing proportional Hazards Assumption
test.cox<-cox.zph(cox)
ggcoxzph(test.cox)

# nonproportional hazards

# move on to Aalen's additive regression model


# Aalen's additive regression model
aafit<-aareg(Surv(time,status)~ration+size,data=mydata)
aafit
summary(aafit)
autoplot(aafit)

?aareg

plot(aafit[2],ylim=c(-10,10))



# Structureal change
largebreak<-ts(large,start=1,end=114,frequency=1,
            deltat=1)

large2<-window(largebreak,start=1,end=114)
coint.res<-residuals(lm(status~ration,data=large2))
coint.res<-stats::lag(ts(coint.res,start=1,frequency = 1),k=-1)
large2<-cbind(large2,diff(large2),coint.res)
large2<-window(large2,start=20,end=112)
large2
ecm.model<-diff.consumed~coint.res+diff.temp

large3<-window(large2,start=20,end=50)
me.efp<-efp(ecm.model,type="ME",data=large3,h=0.5)
me.mefp<-mefp(me.efp,alpha = 0.05)
large3<-window(large2,start=20,end=70)
me.mefp<-monitor(me.mefp)
plot(me.mefp)
me.mefp
me.plot2.0<-plot(me.mefp)
me.mefp2<-me.mefp
