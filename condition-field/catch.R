## Format Catch data to filter out good and bad hauls

# set working directory
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1")

# load packages
library(tidyverse)

# load data
notes<-read.csv("./data/output/catch-notes-final.csv") # load haul notes based on the "questionable hauls"



# load full catch data
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

head(catch.all)

# add column

catch.new<-catch.all%>%
  left_join(select(notes,-Notes))
head(catch.new)
test1<-catch.new%>%
  filter(!is.na(Include.Exclude))
test2<-catch.new%>%
  filter(Notes!="")

catch.final<-catch.new%>%
  mutate(Include.Exclude=replace(Include.Exclude,is.na(Include.Exclude),1))
#remove non-sampling sites



#save catch data
write.csv(catch.final,"C:/Users/emili/Documents/Research/CHONe-1.2.1/data/data-working/newman-catch.csv",row.names=FALSE,na="")
