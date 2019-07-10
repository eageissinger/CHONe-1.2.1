# ----- Data Updates ------
# update/add new data to length and count files

# ----- set working directory -----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

# ----- set up data for condition analysis ----

revised_files<-list.files(path="C:/Users/geissingere/Documents/CHONe-1.2.1-office/data/TNNP Revised Data/TNNP/output/length/",
                          full.names=FALSE, include.dirs = FALSE)
head(revised_files)
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/data/TNNP Revised Data/TNNP/output/length/")
dataset1<- lapply(revised_files, read.csv)

df1<-bind_rows(dataset1)

lab_revised<-list.files(path="C:/Users/geissingere/Documents/CHONe-1.2.1-office/data/TNNP Revised Data/TNNP/output/lab-measured/",
                        full.names = FALSE,include.dirs = FALSE)
head(lab_revised)
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/data/TNNP Revised Data/TNNP/output/lab-measured/")
dataset2<-lapply(lab_revised,read.csv)

df2<-bind_rows(dataset2)


head(df1)
head(df2)

fulldata<-bind_rows(df1,df2)



# length
length<-fulldata%>%
  mutate(month=as.numeric(str_sub(Date,start=5,end=6)))%>%
  rename(year=Year,julian.date=Julian.Date,trip=Trip,time=Time,site=Site,species=Species,
         age=Age,weighting=Weighting,notes=Notes,pulse=Pulse,day=Day)%>%
  select(-Date,-Month)
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")
write.csv(length,"./data/data-working/newman-length-all.csv",row.names=FALSE)

length.all<-read.csv("./data/data-working/newman-length-all.csv")
length.old<-read.csv("./data/data-working/newman-length.csv")

length.all.check<-length.all%>%
  filter(species=="AC" | species=="Ga")%>%
  filter(!is.na(year))
length.old.check<-length.old%>%
  filter(!is.na(year))

length.old != length.all.check

duplicated(length.old)
# ----- load data ------
ac0length<-read.csv("./data/data-working/newman-AC0-length.csv")
ac0length2018<-read.csv("./data/data-working/newman-AC0-length-2018.csv")
ac0count<-read.csv("./data/data-working/newman-AC0-catch.csv")
ac0count2018<-read.csv("./data/data-working/newman-AC0-catch-2018.csv")
ac1length<-read.csv("./data/data-working/newman-AC1-length.csv")
ac1length2018<-read.csv("./data/data-working/newman-AC1-length-2018.csv")
ac1count<-read.csv("./data/data-working/newman-AC1-catch.csv")
ac1count2018<-read.csv("./data/data-working/newman-AC1-catch-2018.csv")
trip.dates<-read.csv("./data/data-working/trip-dates-newman.csv")
# ---- load packages ----
library(tidyverse)
library(lubridate)

# ---- check data -----
summary(ac0length)
summary(ac0count)
summary(ac0length2018)
summary(ac0count2018)
summary(ac1length)
summary(ac1count)
summary(ac1length2018)
summary(ac1count2018)
summary(trip.dates)

glimpse(ac0length)
glimpse(ac0count)
glimpse(ac0length2018)
glimpse(ac0count2018)
glimpse(ac1length)
glimpse(ac1count)
glimpse(ac1length2018)
glimpse(ac1count2018)
glimpse(trip.dates)

# ---- combine years ----
# age 0 and age 1 length
ac1length$Pulse<-as.character(ac1length$Pulse)
ac0length$Pulse<-as.character(ac0length$Pulse)
ac1length2018$Pulse<-as.character(ac1length2018$Pulse)
ac0length2018$Pulse<-as.character(ac0length2018$Pulse)
length<-bind_rows(ac0length,ac1length,ac0length2018,ac1length2018)
# age 0 and age 1 count
count<-bind_rows(ac0count,ac1count,ac0count2018,ac1count2018)

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
