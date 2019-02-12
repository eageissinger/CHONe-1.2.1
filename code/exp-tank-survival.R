# ---- set working directory -----
setwd("C:/Users/USER/Documents/Research/CHONe-1.2.1/")

# load data ----
tank_survival<-read.csv("./data/data-working/tank-survival-exp.csv",header=TRUE)

# ---- load packages ----
library(tidyverse)
library(lubridate)
library(survival)

# data manimpulation
glimpse(tank_survival)
tank_survival<- tank_survival%>%
  rename(year=ï..year)


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

# build standard suvival object
so<-with(df3,Surv(time,size,ration,status))
head(so,80)
