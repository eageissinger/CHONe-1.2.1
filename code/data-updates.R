# ----- Data Updates ------
# add new data to length and count files

# ----- set working directory -----
setwd("C:/Users/Emilie/Dropbox/Thesis/Research/CHONe-1.2.1/")

# ----- load data ------
lengthold<-read.csv("./data/data-working/newman-length-to2017.csv")
lengthnew<-read.csv("./data/data-original/newman-length-july-2017.csv")
catchold<-read.csv("./data/data-working/newman-catch-full.csv")
catchnew<-read.csv("./data/data-original/newman-catch-2017.csv")

# ---- load packages ----
library(dplyr)
library(lubridate)
library(stringr)


# ---- check data -----
summary(lengthold)
str(lengthold)
dim(lengthold)
names(lengthold)

summary(lengthnew)
str(lengthnew)
dim(lengthnew)
names(lengthnew)

summary(catchold)
str(catchold)
dim(catchold)
names(catchold)

summary(catchnew)
str(catchnew)
dim(catchnew)
names(catchnew)

# ---- fix date on new data -----
# length
length1<-lengthnew%>%
  mutate(month=as.numeric(str_sub(Date,start=5,end=6)),
         day=as.numeric(str_sub(Date,start=7,end=8)))%>%
  rename(year=Year,julian.date=Julian.Date,site=Site,species=Species,
         age=Age,trip=Trip)%>%
  select(-Time,-Date)
# catch
catch1<-catchnew%>%
  mutate(month=as.numeric(str_sub(Date,start=5,end=6)),
         day=as.numeric(str_sub(Date,start=7,end=8)))%>%
  rename(year=Year,julian.date=Julian.Date,site=Site,species=Species,age=Age,
         count=Count,notes=Notes)%>%
  select(-Date)

lengthall<-bind_rows(lengthold,length1)           
catchall<-bind_rows(catchold,catch1)
summary(lengthall)
summary(catchall)

write.csv(lengthall,"./data/data-working/newman-length.csv",row.names=FALSE)
write.csv(catchall,"./data/data-working/newman-catch.csv",row.names=FALSE)
