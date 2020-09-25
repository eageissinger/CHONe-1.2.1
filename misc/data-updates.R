# ----- Data Updates ------
# update/add new data to length and count files

# ---- packages -----
library(tidyverse)
library(lubridate)

# ----- set working directory -----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

# ----- load data ------
# ----- Length data -----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/temp/")
file_list<-list.files(path="C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/temp")
length<-lapply(file_list,read.csv)

length.all<-bind_rows(length)

setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/lab-measured/")

file_list2<-list.files(path="C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/lab-measured/")
lab<-lapply(file_list2,read.csv)

lab.all<-bind_rows(lab)


length.full<-bind_rows(lab.all,length.all)

# --- check data ----
head(length.full)
glimpse(length.full)


# ---- fix date on new data -----
# length
length1<-length.full%>%
  mutate(month=as.numeric(str_sub(Date,start=5,end=6)),
         day=as.numeric(str_sub(Date,start=7,end=8)))%>%
  rename(year=Year,julian.date=Julian.Date,site=Site,species=Species,
         age=Age,trip=Trip,pulse=Pulse,notes=Notes)%>%
  select(-Time,-Date,-Weighting,-Wt...g.,-Month,-Day)%>%
  select(year,month,day,julian.date,trip,site,species,age,mmSL,pulse,notes)

setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")
write.csv(length1,"./data/data-working/newman-length-updated.csv",row.names=FALSE)
#write.csv(catch1,"./data/data-working/newman-count-updated.csv",row.names = FALSE)


# ---- temperature ----
temp1<-read.csv("./data/data-working/daily-temp-corrected-newman.csv")
temp17<-read.csv("./data/data-working/temperature-2017.csv")

head(temp1)
head(temp17)

#format 2017 data
temp17.2<-temp17%>%
  rename(date=ï..Date,daily_temp_C=MeanTemp..C.)%>%
  mutate(year=as.integer(str_sub(date,start=1,end = 4)),
         month=as.integer(str_sub(date,start=5,end=6)),
         day=as.integer(str_sub(date,start=7,end=8)))%>%
  select(year,month,day,daily_temp_C)
temp1.2<-temp1%>%
  select(-date)

temp.full<-left_join(temp1.2,temp17.2)
summary(temp.full)
glimpse(temp.full)

write.csv(temp.full,"./data/data-working/newman-temp-to-2017.csv",row.names = FALSE)
