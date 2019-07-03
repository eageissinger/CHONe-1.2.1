setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")


library(tidyverse)
library(data.table)

file_names<-list.files(path="C:/Users/geissingere/Documents/CHONe-1.2.1-office/data/TNNP Revised Data/TNNP/length/",
                       full.names=FALSE, include.dirs = FALSE)
head(file_names)

age0<-read.csv("./data/data-working/pulse_range_age0_final.csv")
age1<-read.csv("./data/data-working/pulse_range_age1_final.csv")
head(age0)
head(age1)

mydata0<-age0
pulse_assign0<-data.frame(trip=rep(mydata0$trip,mydata0$max-mydata0$min+1),
                          year=rep(mydata0$year,mydata0$max-mydata0$min+1),
                          pulse=rep(mydata0$pulse,mydata0$max-mydata0$min+1),
                          age=rep(mydata0$age,mydata0$max-mydata0$min+1),
                          mmSL=unlist(mapply(seq,mydata0$min,mydata0$max)))

mydata1<-age1%>%
  filter(!is.na(min))
pulse_assign1<-data.frame(trip=rep(mydata1$trip,mydata1$max-mydata1$min+1),
                          year=rep(mydata1$year,mydata1$max-mydata1$min+1),
                          pulse=rep(mydata1$pulse,mydata1$max-mydata1$min+1),
                          age=rep(mydata1$age,mydata1$max-mydata1$min+1),
                          mmSL=unlist(mapply(seq,mydata1$min,mydata1$max)))


pulses<-bind_rows(pulse_assign0,pulse_assign1)%>%
  mutate(Species='AC')%>%
  rename(Year=year,Trip=trip,Age=age,Pulse=pulse)
head(pulses)

revised<-function(file_names,pulses) {
  

for(i in 1:length(file_names)){
  
  setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/data/TNNP Revised Data/TNNP/length/") #pullfilesfromhere
  
  length<-read.csv(files[i])
  length$Year<-as.integer(as.character(length$Year))
  length$Time<-as.integer(as.character(length$Time))
  
  mydata<-length%>%
    select(-Pulse)
  final<-left_join(mydata,pulses)%>%
    select(-contains("X"))
  
  setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/data/TNNP Revised Data/TNNP/output/") #writenewfilestohere
  
  write.csv(x = final, row.names=FALSE, file = paste("revised",file_names[i],sep = "_"))
} }
revised(file_names,pulses)

# ----- set up data for condition analysis ----

revised_files<-list.files(path="C:/Users/geissingere/Documents/CHONe-1.2.1-office/data/TNNP Revised Data/TNNP/output/",
                          full.names=FALSE, include.dirs = FALSE)
head(revised_files)



dataset<- lapply(revised_files, read.csv)

df<-bind_rows(dataset)

head(df)

# length
length<-df%>%
  mutate(month=as.numeric(str_sub(Date,start=5,end=6)))%>%
  rename(year=Year,julian.date=Julian.Date,trip=Trip,time=Time,site=Site,species=Species,
         age=Age,weighting=Weighting,notes=Notes,pulse=Pulse,day=Day)%>%
  select(-Date,-Month)
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")
write.csv(length,"./data/data-working/newman-length-all.csv",row.names=FALSE)
