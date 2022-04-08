setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/condition-field/")

library(tidyverse)
library(lubridate)

# ---- temperature ----
temp1<-read.csv("../data/data-working/daily-temp-corrected-newman.csv")%>%
  select(-date)

summary(temp1)
tail(temp1)
# temp17<-read.csv("../data/data-working/temperature-2017.csv")
# 
# head(temp1)
# head(temp17)
# 
# #format 2017 data
# temp17.2<-temp17%>%
#   rename(date=ï..Date,daily_temp_C=MeanTemp..C.)%>%
#   mutate(year=as.integer(str_sub(date,start=1,end = 4)),
#          month=as.integer(str_sub(date,start=5,end=6)),
#          day=as.integer(str_sub(date,start=7,end=8)))%>%
#   select(year,month,day,daily_temp_C)
# temp1.2<-temp1%>%
#   select(-date)
# 
# temp.full<-bind_rows(temp1.2,temp17.2)
# summary(temp.full)
# glimpse(temp.full)

# read in files from 2017 folder.
files <- list.files(path="../data/data-original/temperature/2017", pattern = ".csv", full.names=TRUE)
data.list <- lapply(files, read.csv, row.names=NULL, header=TRUE)
files
head(data.list[[1]])
head(data.list[[2]])
head(data.list[[3]])
head(data.list[[4]])

temp17<-bind_rows(data.list)%>% #bind dataframes together
  mutate(year=as.integer(str_sub(Date,start=1,end=4)),
         month=as.integer(str_sub(Date,start=5,end=6)),
         day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  group_by(date,year,month,day)%>%
  summarise(daily_temp_C=mean(Temp..C.))%>%
  ungroup()%>%
  select(-date)
glimpse(temp17)

# write out 2017 and send the two files (new and original) to Bob for comparison
#write.csv(temp17,"../data/output/temperature2017-working.csv",row.names = FALSE)

# read in files from 2018 folder.
files <- list.files(path="../data/data-original/temperature/2018", pattern = ".csv", full.names=TRUE)
data.list <- lapply(files, read.csv, row.names=NULL, header=TRUE,skip=7)
head(data.list[[1]])
head(data.list[[2]])
head(data.list[[3]])

temp18<-bind_rows(data.list)%>%
  rename(date=Date.yyyy.mm.dd.)%>%
  group_by(date)%>%
  summarise(daily_temp_C=mean(Temperature...C.))%>%
  mutate(year=as.integer(str_sub(date,start=1,end=4)),
         month=as.integer(str_sub(date,start=6,end=7)),
         day=as.integer(str_sub(date,start=9,10)))%>%
  ungroup()%>%
  select(-date)
glimpse(temp18)

# read in files from 2019 folder
files <- list.files(path="../data/data-original/temperature/2019", pattern = ".csv", full.names=TRUE)
data.list <- lapply(files, read.csv, row.names=NULL, header=TRUE,skip=7)
head(data.list[[1]])
head(data.list[[2]])
head(data.list[[3]])

temp19<-bind_rows(data.list)%>%
  rename(date=Date.yyyy.mm.dd.)%>%
  group_by(date)%>%
  summarise(daily_temp_C=mean(Temperature...C.))%>%
  mutate(year=as.integer(str_sub(date,start=1,end=4)),
         month=as.integer(str_sub(date,start=6,end=7)),
         day=as.integer(str_sub(date,start=9,10)))%>%
  ungroup()%>%
  select(-date)
glimpse(temp19)

# combine all files
head(temp1)
head(temp17)
head(temp18)
head(temp19)


temp.full<-bind_rows(temp1,temp17,temp18,temp19)%>%
  filter(!is.na(year))
summary(temp.full)

write.csv(temp.full,"../data/data-working/newman-temp-to-2019.csv",row.names = FALSE)
