## Format Catch data to filter out good and bad hauls

# set working directory
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1")

# load packages
library(tidyverse)

# load data
notes<-read.csv("./data/output/catch-notes-final.csv") # load haul notes based on the "questionable hauls"

# format notes
notes2<-notes%>%
  mutate(Month=replace(Month,Month==5,"MAY"))%>%
  mutate(Month=replace(Month,Month==6,"JUN"))%>%
  mutate(Month=replace(Month,Month==7,"JUL"))%>%
  mutate(Month=replace(Month,Month==8,"AUG"))%>%
  mutate(Month=replace(Month,Month==9,"SEP"))%>%
  mutate(Month=replace(Month,Month==10,"OCT"))%>%
  mutate(Month=replace(Month,Month==11,"NOV"))%>%
  mutate(Month=replace(Month,Month==12,"DEC"))


# test round
data1<-read.csv("./data/TNNP Revised Data/TNNP/catch/TNNP2020a.csv")

data2<-data1%>%
  left_join(select(notes2,-Notes))%>%
  mutate(Include.Exclude=replace(Include.Exclude,is.na(Include.Exclude),1))

# load full catch data (work with the revised age catch data)
files<-list.files(path="C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/catch",
                  full.names = FALSE, include.dirs = FALSE)
head(files)
file_names<-list.files(path="C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/catch")
head(file_names)




for(i in 1:length(file_names)){
  setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/catch/")
  catch<-read.csv(files[i])
  catch1<-catch%>%
    mutate(Species=replace(Species,Species=="AC ","AC"))%>%
    mutate(Species=replace(Species,Species==" AC","AC"))%>%
    mutate(Month=replace(Month,Month==5,"MAY"))%>%
    mutate(Month=replace(Month,Month==6,"JUN"))%>%
    mutate(Month=replace(Month,Month==7,"JUL"))%>%
    mutate(Month=replace(Month,Month==8,"AUG"))%>%
    mutate(Month=replace(Month,Month==9,"SEP"))%>%
    mutate(Month=replace(Month,Month==10,"OCT"))%>%
    mutate(Month=replace(Month,Month==11,"NOV"))%>%
    mutate(Month=replace(Month,Month==12,"DEC"))%>%
    left_join(select(notes2,-Notes))%>%
    mutate(Include.Exclude=replace(Include.Exclude,is.na(Include.Exclude),1))
  
  setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/catch-final") #writenewfilestohere
  write.csv(x=catch1,row.names=FALSE, file = paste("revised",file_names[i],sep="_"),na="")
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
