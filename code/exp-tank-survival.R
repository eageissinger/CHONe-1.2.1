# ---- set working directory -----
setwd("C:/Users/USER/Documents/Research/CHONe-1.2.1/")

# load data ----
tank_survival<-read.csv("./data/data-working/tank-survival-exp.csv",header=TRUE)

# ---- load packages ----
library(tidyverse)

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

