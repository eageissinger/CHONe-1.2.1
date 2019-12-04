# ---- 2017 abundance check -----
## Check abundances of pulse 1, 2, and 3 in May and July of 2017 to determine relationship between survival
## estimates and actual data

# ---- packages ----
library(data.table)
library(tidyverse)
library(lubridate)
# ---- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

# ---- load data ----
#field fish
file_names<-list.files(path="C:/Users/geissingere/Documents/CHONe-1.2.1-office/data/TNNP Revised Data/TNNP/output/length/",
                       full.names=FALSE,include.dirs = FALSE)
head(file_names)
setwd("./data/TNNP Revised Data/TNNP/output/length/")
length<-lapply(file_names,read.csv)
coddata<-rbindlist(length,use.names=TRUE)%>%
  filter(Species=="AC")
alldata<-rbindlist(length,use.names = TRUE)%>%
  filter(Species=="AC")%>%
  filter(Year==2016 | Year == 2017)%>%
  mutate(Month = as.integer(str_sub(Date,start=5,end=6)),
         Day = as.integer(str_sub(Date,start=7,end=8)))
alldata$Date<-ymd(paste(alldata$Year,alldata$Month,alldata$Day,sep="-"))
str(alldata)

alldata%>%
  filter(Year==2017 & Age == 1 & Month == 5)%>%
  distinct(Pulse)

  group_by(Pulse)%>%
  summarise(n())

alldata%>%
  filter(Year==2017 & Age == 1 & Month == 7)%>%
  distinct(Pulse)

alldata%>%
  filter(Year==2016 & Age ==0 & Month ==10 & Site =="NB")%>%
  distinct(Pulse)
alldata%>%
  filter(Year==2016 & Age ==0 & Month == 11 & Site == "NB")%>%
  distinct(Pulse)
alldata%>%
  filter(Year==2016 & Age ==0 & Month ==10)%>%
  distinct(Pulse)
alldata%>%
  filter(Year==2016 & Age ==0 & Month ==10)%>%
  group_by(Pulse)%>%
  summarise(n())
alldata%>%
  filter(Year==2016 & Age ==0 & Month ==10 & Site == "NB")%>%
  group_by(Pulse)%>%
  summarise(n())
alldata%>%
  filter(Year==2016 & Age ==0 & Month ==11 & Site == "NB")%>%
  group_by(Pulse)%>%
  summarise(n())
