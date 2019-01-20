# Pulse Assignment Final Sorting 
# Purpose: Finalize pulse assignments

# ---- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-master/")

# ---- load data ----
age1pulse<-read.csv("./data/data-working/age1_dummypulse.csv")
length<-read.csv("./data/data-working/newman-length.csv")
catch<-read.csv("./data/data-working/catch_haul.csv")

# ---- load packages -----
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

# ---- format Dataframe -----
# fix date in age1pulse and length
age1pulse$date<-ymd(paste(age1pulse$year,age1pulse$month,age1pulse$day,sep="-"))
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))

# Format data for plotting
length0<-length%>%
  filter(age==0)%>%
  mutate(cohort=year)%>%
  select(-trip)
names(length0)
str(length0)
length0$pulse<-as.character(length0$pulse)
length1<-age1pulse%>%
  mutate(cohort=year-1)%>%
  select(-trip)
names(length1)
str(length1)
length1$pulse<-as.character(length1$pulse)
growth<-bind_rows(length0,length1)
#View(growth)
str(growth)
unique(growth$pulse)
growth$pulse<-as.integer(growth$pulse)

#format growth for plotting
growth1<-growth%>%
  group_by(cohort,age,pulse,date)%>%
  summarise(meanSL=mean(mmSL),min=min(mmSL),max=max(mmSL))%>%
  filter(!is.na(pulse))
#View(growth1) 

# ---- All years as function -----
cohort.graph<-function(growth1,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth1$cohort))
  
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(growth1,growth1$cohort==cohort_list[i]),
                 aes(x=date,y=meanSL,group=cohort,shape=factor(pulse)))+
      geom_point(size=2)+
      geom_errorbar(aes(ymin=min,ymax=max),width=0)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))
    print(plot)
  }
}
cohort.graph(growth1)

# ---- tables -----
catch2<-catch%>%
  select(year,month,trip,age,total_catch,total_measured,extrap_1,extrap_2,
         extrap_3,extrap_4,extrap_5,extrap_6,extrap_unknown,total)%>%
  gather(key="extrap",value = "catch_haul",c(7:13),na.rm = FALSE)%>%
  mutate(total_measured=replace(total_measured,total_catch==0,0))%>%
  mutate(catch_haul=replace(catch_haul,total_catch==0,0))%>%
  mutate(pulse=str_sub(extrap,start=8,end = 8))%>%
  mutate(pulse=replace(pulse,pulse=="u",NA))
View(catch2)

table1<-catch2%>%
  group_by(year,month,trip,age,pulse,total_catch,total_measured)%>%
  summarise(catch_per_haul=mean(catch_haul))

filter(catch2,year==2013 & age ==1 & month == 5)
