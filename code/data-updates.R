# ----- Data Updates ------
# update/add new data to length and count files

# ----- set working directory -----
setwd("C:/Users/USER/Documents/Research/CHONe-1.2.1/")

# ----- load data ------
ac0length<-read.csv("./data/data-working/newman-AC0-length.csv")
ac0count<-read.csv("./data/data-working/newman-AC0-catch.csv")
ac1length<-read.csv("./data/data-working/newman-AC1-length.csv")
ac1count<-read.csv("./data/data-working/newman-AC1-catch.csv")
trip.dates<-read.csv("./data/data-working/trip-dates-newman.csv")
# ---- load packages ----
library(tidyverse)
library(lubridate)

# ---- check data -----
summary(ac0length)
summary(ac0count)
summary(ac1length)
summary(ac1count)
summary(trip.dates)

glimpse(ac0length)
glimpse(ac0count)
glimpse(ac1length)
glimpse(ac1count)
glimpse(trip.dates)

# ---- combine years ----
# age 0 and age 1 length
ac1length$Pulse<-as.character(ac1length$Pulse)
ac0length$Pulse<-as.character(ac0length$Pulse)
length<-bind_rows(ac0length,ac1length)
# age 0 and age 1 count
count<-bind_rows(ac0count,ac1count)

# ---- fix date on new data -----
# length
length1<-length%>%
  mutate(month=as.numeric(str_sub(Date,start=5,end=6)),
         day=as.numeric(str_sub(Date,start=7,end=8)))%>%
  rename(year=Year,julian.date=Julian.Date,site=Site,species=Species,
         age=Age,trip=Trip,pulse=Pulse,notes=Notes)%>%
  select(-Time,-Date)
# catch
count1<-count%>%
  mutate(month=as.numeric(str_sub(Date,start=5,end=6)),
         day=as.numeric(str_sub(Date,start=7,end=8)))%>%
  rename(year=Year,julian.date=Julian.Date,site=Site,species=Species,age=Age,
         count=Count,notes=Notes,trip=Trip)%>%
  select(-Date)
#trips
trip1<-trip.dates%>%
  mutate(year=as.numeric(str_sub(Date,start=1,end=4)),
         month=as.numeric(str_sub(Date,start=5,end=6)),
         day=as.numeric(str_sub(Date,start=7,end=8)))%>%
  rename(trip=Trip)%>%
  select(-Date)

View(length1)
View(count1)
View(trip1)

write.csv(length1,"./data/data-working/newman-length.csv",row.names=FALSE)
write.csv(count1,"./data/data-working/newman-catch.csv",row.names=FALSE)
write.csv(trip1,"./data/data-working/newman-trips.csv",row.names=FALSE)
