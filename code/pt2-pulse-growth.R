### Growth rates - Pulse data ###

# ---- set working directory ----
setwd("C:/Users/Emilie/Dropbox/Thesis/Research/CHONe-1.2.1/")

# ---- load packages ----
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# ---- load data -----
age0<-read.csv("./data/data-working/age-0-pulse-range.csv")
age1<-read.csv("./data/data-working/age-1-mixture-dist.csv")
count<-read.csv("./data/data-working/catch-pulse-newman.csv")
fullcount<-read.csv("./data/data-working/newman-catch.csv")
trips<-read.csv("./data/data-working/trip-dates-newman.csv")

# --- check data ----
# age 0
str(age0)
summary(age0)
head(age0)
dim(age0)
names(age0)

names(age0)<-c("year","cohort","month","trip","age","pulse","min_size","max_size","notes")

#age 1
str(age1)
dim(age1)
summary(age1)
head(age1)
names(age1)

#check the following line if data ever changes
age1<-select(age1,year,month,day,cohort,age,dummy_pulse,mu,mu.se,sigma,sigma.se)

#count
summary(count)
str(count)
dim(count)
head(count)
tail(count)
names(count)

# full count
summary(fullcount)
str(fullcount)
dim(fullcount)
head(fullcount)
names(fullcount)

names(fullcount)<-c("year","julian_date","site","species","age","count","notes","month","day")

# Trips
summary(trips)
str(trips)
dim(trips)
head(trips)
names(trips)

# ---- Date manipulation ----

# Trip Dates
trips$year<-as.numeric(str_sub(trips$Date,start = 1,end = 4))
trips$month<-as.numeric(str_sub(trips$Date,start=5,end=6))
trips$day<-as.numeric(str_sub(trips$Date,start=7,end=8))

tripdates<-trips%>%
  rename(trip=Trip)%>%
  select(trip,year,month,day)

# Add trip dates to age 0 data
age0<-left_join(age0,tripdates)

#format date
age0$date<-ymd(paste(age0$year,age0$month,age0$day,sep="-"))
age1$date<-ymd(paste(age1$year,age1$month,age1$day,sep="-"))
fullcount$date<-ymd(paste(fullcount$year,fullcount$month,fullcount$day,sep="-"))


#mean size for age 0
age0data<-age0%>%
  select(date,cohort,month,age,pulse,min_size,max_size)%>%
  mutate(mean_size=(min_size+max_size)/2)%>%
  rename(dummy_pulse=pulse)%>%
  data.frame()

#create min and max for age 1
# 2 standard deviations
age1data<-age1%>%
  select(date, cohort, month, age, dummy_pulse,mu,mu.se,sigma,sigma.se)%>%
  filter(mu<300)%>% # get rid of outliers (>300 mm SL)
  rename(mean_size=mu,stdev=sigma)%>%
  mutate(min_size=mean_size-(stdev),max_size=mean_size+(stdev))%>%
  select(-stdev,-sigma.se,-mu.se)

names(age0data)
names(age1data)
growth<-bind_rows(age0data,age1data)
unique(growth$dummy_pulse) # number of pulses present

growth$pulse<-as.factor(growth$dummy_pulse)
growth$age<-as.factor(growth$age)
growth$cohort<-as.factor(growth$cohort)

# ---- format count data -----
# go from wide format to long format
count2<-count%>%
  gather(key=p1,value=count,starts_with('count'))%>%
  gather(key=p2,value=extrap,starts_with('extrap'))%>%
  separate(p1,c("count2","pulse1"))%>%
  separate(p2,c("extrap2","pulse2"))

# make count df and extrap df
count_only<-count2%>%
  select(-pulse2,-extrap,-extrap2,-count2)
extrap_only<-count2%>%
  select(-pulse1,-count,-count2,-extrap2)
names(count_only)<-c("year","j_start_date","num_hauls","pulse","count")
names(extrap_only)<-c("year","j_start_date","num_hauls","pulse","extrap")

#combine adjusted df to be one complete 
count_long<-left_join(count_only,extrap_only)

# change NAs to 0 (confirmed that NAs are actually 0s)
count_long$count[is.na(count_long$count)]<-0

# ---- summary table of class-year strength -----
pulse_table<-count_long%>%
  group_by(year,pulse)%>%
  summarise(total_count=sum(count),avg_count=mean(count),total_extrap=sum(extrap),avg_extrap=mean(extrap))

adjusted_sums<-count_long%>%
  group_by(year,pulse)%>%
  summarise(total_extrap=sum(extrap))%>%
  spread(pulse,total_extrap)

adjusted_mean<-count_long%>%
  group_by(year,pulse)%>%
  summarise(avg_extrap=mean(extrap))%>%
  spread(pulse,avg_extrap)

# ---- age 0 and age 1 Totals -----
table_age0<-fullcount%>%
  filter(age==0)%>%
  filter(month>=10)%>%
  group_by(year,date,age,month)%>%
  summarise(count=sum(count))%>%
  ungroup()%>%
  group_by(year,month,age)%>%
  summarise(count=mean(count))
table_age1<-fullcount%>%
  filter(age==1)%>%
  filter(month<8)%>%
  group_by(year,date,age,month)%>%
  summarise(count=sum(count))%>%
  ungroup()%>%
  group_by(year,month,age)%>%
  summarise(count=mean(count)) 


# ---- Visualize data ----

growth%>%
  filter(cohort=="2010")%>%
  ggplot(aes(x=date,y=mean_size,group=cohort,shape=pulse))+
  geom_point(size=1)+
  geom_errorbar(aes(ymin=min_size,ymax=max_size),width=5)+
  theme_bw()

# ---- All years as function -----
cohort.graph<-function(growth,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(growth,growth$cohort==cohort_list[i]),
                 aes(x=date,y=mean_size,group=cohort,shape=pulse))+
      geom_point(size=2)+
      geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))
    print(plot)
  }
}
cohort.graph(growth)
