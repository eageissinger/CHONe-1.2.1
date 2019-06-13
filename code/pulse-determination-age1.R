### Pulse Determination for Age 1 ###

# ---- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

# ---- load packages ----
library(lubridate)
library(tidyverse)

# ---- load extra functions ----
source("./code/pulse_range_fct.R")

# ---- load data -----
age0<-read.csv("./data/data-working/pulse_range_0_final.csv")
age1<-read.csv("./data/data-working/age-1-mixture-dist.csv")
catch.haul<-read.csv("./data/data-working/catch_haul.csv")
fullcount<-read.csv("./data/data-working/newman-catch.csv")
length<-read.csv('./data/data-working/newman-length.csv')

# --- check data ----
# age 0
str(age0)
summary(age0)
head(age0)
dim(age0)
names(age0)

age0<-age0%>%
  mutate(cohort=year)
names(age0)

#age 1
str(age1)
dim(age1)
summary(age1)
head(age1)
names(age1)

# full count
summary(fullcount)
str(fullcount)
dim(fullcount)
head(fullcount)
names(fullcount)

trips<-length%>%
  select(year,julian.date,trip,month,day)%>%
  group_by(year,trip)%>%
  filter(julian.date==min(julian.date))%>%
  distinct(trip,year,julian.date,month,day)

# ---- Date manipulation ----

#format date
fullcount$date<-ymd(paste(fullcount$year,fullcount$month,fullcount$day,sep="-"))
age1$date<-ymd(paste(age1$year,age1$month,age1$day,sep="-"))
age1$julian.date<-yday(age1$date)

# add trips to age 0 to add date
age0<-left_join(age0,trips)
age0$date<-ymd(paste(age0$year,age0$month,age0$day, sep="-"))
head(age0)

#mean size for age 0
age0data<-age0%>%
  select(date,cohort,month,age,pulse,min,max)%>%
  mutate(meanSL=(min+max)/2)

#create min and max for age 1
# 2 standard deviations
age1data<-age1%>%
  select(date, cohort, month, age, dummy_pulse,mu,mu.se,sigma,sigma.se)%>%
  filter(mu<300)%>% # get rid of outliers (>300 mm SL)
  rename(meanSL=mu,stdev=sigma,pulse=dummy_pulse)%>%
  mutate(min=meanSL-stdev,max=meanSL+stdev)%>%
  select(-stdev,-sigma.se,-mu.se)

names(age0data)
names(age1data)
growth<-bind_rows(age0data,age1data)
unique(growth$pulse) # number of pulses present
growth<-growth%>%filter(meanSL<250)%>%
  filter(max<250)

# ---- Visualize data ----
cohort.graph<-function(growth,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  #pdf()
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(growth,growth$cohort==cohort_list[i]),
                 aes(x=date,y=meanSL,group=as.factor(cohort),shape=factor(pulse)))+
      geom_point(size=2)+
      geom_errorbar(aes(ymin=min,ymax=max),width=0)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))+
      ylim(c(15,250))
    print(plot)
  }
}
cohort.graph(growth)

# ---- format count data -----
# go from wide format to long format
catch.haul2<-catch.haul%>%
  gather(key=p1,value=count, starts_with('count'))%>%
  gather(key=p2,value=extrap, starts_with('extrap'))%>%
  separate(p1,c("count2","pulse1"))%>%
  separate(p2,c("extrap2","pulse2"))%>%
  distinct()%>%
  rename(pulse=pulse1)%>%
  select(-count2,-extrap2,-pulse2)%>%
  mutate(count=replace(count,is.na(count),0))%>%
  mutate(extrap=replace(extrap,count==0,0))

# ---- summary table of class-year strength -----
pulse_table<-catch.haul2%>%
  group_by(year,pulse)%>%
  summarise(total_count=sum(count),avg_count=mean(count),total_extrap=sum(extrap),avg_extrap=mean(extrap))


# ---- age 0 and age 1 Totals -----
table_full<-fullcount%>%
  group_by(year,age,month,trip)%>%
  summarise(count=sum(count),mean=mean(count))

# ---- Dummy pulse assignment ----
# Purpose: assign dummy pulses to length data for all age 1 cod

# make dataframe with min and max pulse ranges
# use +/- standard deviation for "min" and "max"

range_final<-pulse_range(age1)
# next step:
# create a data frame that fills in all possible length 
# possibilities for every pulse option

pulse_assign<-data.frame(trip=rep(range_final$trip,range_final$max-range_final$min+1),
                         year=rep(range_final$year,range_final$max-range_final$min+1),
                         cohort=rep(range_final$cohort,range_final$max-range_final$min+1),
                         pulse=rep(range_final$dummy_pulse,range_final$max-range_final$min+1),
                         mmSL=unlist(mapply(seq,range_final$min,range_final$max)))
# add additinal info such as date, cohort, pulse so that it is present in dataframe

#View(pulse_assign)
#is.na(pulse_assign)
summary(pulse_assign)
glimpse(pulse_assign)


# assign pulses to age 1 length data

#View(lengthtrip)
age1length<-length%>%filter(age==1)%>%select(-pulse)
View(age1length)
head(age1length)
# assign pulses to subsetted age1 length data

age1length_pulse<-left_join(age1length,pulse_assign)
head(age1length_pulse)
#View(age1length_pulse)

# check assignments
# create summary table of min and max for each assigned pulse by year

table1<-age1length_pulse%>%
  group_by(year,cohort,trip,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))
#View(table1)
table2<-pulse_range%>%
  group_by(year,cohort,date,dummy_pulse)%>%
  summarise(min=min_round,max=max_round)
#View(table2)

table3<-age1length_pulse%>%
  group_by(year,cohort,trip,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))%>%
  filter(is.na(pulse))
#View(table3)

write.csv(age1length_pulse,"./data/data-working/age1_dummypulse.csv",row.names = FALSE)

write.csv(range_final,"./data/data-working/age1-pulse-range.csv",row.names = FALSE)

# ---- Finalize pulse assignments ----

# Format data for plotting
length0<-length%>%
  filter(age==0)%>%
  mutate(cohort=year)%>%
  select(-trip)
names(length0)
str(length0)
length0$pulse<-as.character(length0$pulse)
length1<-age1length_pulse%>%
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
growth$date<-ymd(paste(growth$year,growth$month,growth$day,sep="-"))
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
catch2<-catch.haul%>%
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

# Use tables and figures to determine pulse assignments based on
# age 0 assignments and abundance
