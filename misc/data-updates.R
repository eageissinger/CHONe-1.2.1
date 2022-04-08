# ----- Data Updates ------
# update/add new data to length and count files

# ---- packages -----
library(tidyverse)
library(lubridate)

# ----- set working directory -----
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/")

# ----- load data ------
# ----- Length data -----
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP/length/")
file_list<-list.files(path="C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP/length")
length<-lapply(file_list,read.csv)

length.all<-bind_rows(length)

setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/lab-measured/")

file_list2<-list.files(path="C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/lab-measured/")
lab<-lapply(file_list2,read.csv)

lab.all<-bind_rows(lab)


length.full<-bind_rows(lab.all,length.all)

# --- check data ----
head(length.full)
glimpse(length.full)


# ---- fix date on new data -----
# length
length1<-length.full%>%
  mutate(Month=as.numeric(str_sub(Date,start=5,end=6)))%>%
  select(Year, Month, Day, Date, Julian.Date, Trip, Time, Site, Species, Age, mmSL, Pulse, Weighting, Notes)

write.csv(length1,"C:/Users/emili/Documents/Research/CHONe-1.2.1/data/data-working/newman-length.csv",row.names=FALSE,na="")
#write.csv(catch1,"./data/data-working/newman-count-updated.csv",row.names = FALSE)

# ---- catch data -----
files<-list.files(path="C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP/catch-original/",
                  full.names = FALSE, include.dirs = FALSE)
head(files)
file_names<-list.files(path="C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP/catch-original/")
head(file_names)

for(i in 1:length(file_names)){
  setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP/catch-original/")
  catch<-read.csv(files[i])
  catch1<-catch%>%
    mutate(Species=replace(Species,Species=="AC ","AC"))%>%
    mutate(Species=replace(Species,Species==" AC","AC"))%>%
    filter(!is.na(Year))%>%
    mutate(Month=as.integer(str_sub(Date,start=5,end=6)))%>%
    select(Year, Month, Day, Date, Julian.Date,Trip, Time, Site, Species,Age, Count,Notes)%>%
    mutate(Time=as.integer(Time))%>%
    mutate(Year=as.integer(Year))%>%
    mutate(Count=as.integer(Count))
  
  setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP/catch/") #writenewfilestohere
  write.csv(x=catch1,row.names=FALSE, file = paste("revised",file_names[i],sep = "_"),na="")
} 

# combine into single file
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP/catch/")
file_list3<-list.files(path="C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP/catch/")
catch<-lapply(file_list3,read.csv)
catch.all<-bind_rows(catch)%>%
  filter(!is.na(Year))

write.csv(x=catch.all,"C:/Users/emili/Documents/Research/CHONe-1.2.1/data/data-working/newman-catch.csv",row.names = FALSE,na="")


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

temp.full<-bind_rows(temp1.2,temp17.2)
summary(temp.full)
glimpse(temp.full)

write.csv(temp.full,"./data/data-working/newman-temp-to-2017.csv",row.names = FALSE)
