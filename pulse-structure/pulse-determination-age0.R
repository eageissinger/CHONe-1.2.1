### Pulse determination ###

# ---- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

# ---- load packages ----
library(lubridate)
library(tidyverse)

# ---- load functions ----
source("./code/pulse_range_fct.R")

# ---- load data -----
length<-read.csv("./data/data-working/newman-length.csv")
age0pulse<-read.csv("./data/data-working/age-0-mixture-dist.csv")
count<-read.csv("./data/data-working/catch-pulse-newman.csv")
fullcount<-read.csv("./data/data-working/newman-catch.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")
catch<-read.csv("./data/data-working/catch_haul.csv")

# --- check data ----
# age 0
str(length)
summary(length)
head(length)
dim(length)
names(length)

#age 1
str(age0pulse)
dim(age0pulse)
summary(age0pulse)
head(age0pulse)
names(age0pulse)

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


# Trips
summary(trips)
str(trips)
dim(trips)
head(trips)
names(trips)

# ---- Date and data organization ----

#format date
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
age0pulse$date<-ymd(paste(age0pulse$year,age0pulse$month,age0pulse$day,sep="-"))
fullcount$date<-ymd(paste(fullcount$year,fullcount$month,fullcount$day,sep="-"))

# create pulse range for length
original<-length%>%
  filter(age==0)%>%
  group_by(year,trip,pulse,date)%>%
  summarise(mean_size=mean(mmSL),min_size=min(mmSL),max_size=max(mmSL))%>%
  mutate(cohort=year)%>%
  data.frame()
#create min and max for age 1
# 2 standard deviations
new<-age0pulse%>%
  select(date, cohort, month, age, dummy_pulse,mu,mu.se,sigma,sigma.se)%>%
  filter(mu<300)%>% # get rid of outliers (>300 mm SL)
  rename(mean_size=mu,stdev=sigma)%>%
  mutate(min_size=mean_size-(stdev),max_size=mean_size+(stdev))%>%
  select(-stdev,-sigma.se,-mu.se)%>%
  rename(pulse=dummy_pulse)%>%
  data.frame()

names(original)
names(new)

original$pulse<-as.factor(original$pulse)
original$cohort<-as.factor(original$cohort)

new$pulse<-as.factor(new$pulse)
new$cohort<-as.factor(new$cohort)


# ---- Visualize data ----

original%>%
  filter(cohort==2016)%>%
  ggplot(aes(x=date,y=mean_size,shape=factor(pulse)))+geom_point()+
  geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)

new%>%
  filter(cohort==2016)%>%
  ggplot(aes(x=date,y=mean_size,shape=factor(pulse)))+geom_point()+
  geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)

#  All years 
cohort.graph<-function(growth,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  #pdf()
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(growth,growth$cohort==cohort_list[i]),
                 aes(x=date,y=mean_size,group=cohort,shape=pulse))+
      geom_point(size=2)+
      geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)+
      ylim(20,150)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))
    print(plot)
  }
}
cohort.graph(original)
cohort.graph(new)

# create age 0 length dataframe with a date offset of 5
age0<-length%>%
  filter(age==0)%>%
  mutate(date2=date+3)%>%
  mutate(cohort=year)
#As a loop
cohort2.graph<-function(growth,length,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  
  #pdf("mixtures-length2.pdf")
  for (i in seq_along(cohort_list)) {
    plot<-ggplot()+
      geom_point(data=subset(growth,growth$cohort==cohort_list[i]),
                 aes(x=date,y=mean_size,group=cohort,shape=pulse),size=2)+
      geom_errorbar(data=subset(growth,growth$cohort==cohort_list[i]),
                    aes(x=date,ymin=min_size,ymax=max_size),width=0)+
      geom_jitter(data=subset(length,length$cohort==cohort_list[i]),
                  aes(x=date2,y=mmSL),colour='grey50',size=.25)+
      ylim(20,150)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))
    print(plot)
  }
  #dev.off()
}
cohort2.graph(new,age0)

# ---- Dummy pulses ----
# Purpose: assign dummy pulses to length data for all age 1 cod


# make dataframe with min and max pulse ranges
# use +/- standard deviation for "min" and "max"
# and incorporate the 'in between' ranges by splitint the 
# difference between min of pulse 1 and max of pulse 2, etc.
range_final<-pulse_range(age0pulse)
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

# add year back to pulse_assign (easier to do this now than to add it to above code)
pulse_assign<-pulse_assign%>%
  mutate(year=cohort)%>%
  data.frame()

# assign pulses to age 0 length data

#View(lengthtrip)
age0length<-length%>%filter(age==0)%>%select(-pulse)
#View(age0length)

# assign pulses to subsetted age0 length data

age0length_pulse<-left_join(age0length,pulse_assign)
#View(age0length_pulse)

# check assignments
# create summary table of min and max for each assigned pulse by year
table1<-age0length_pulse%>%
  group_by(year,cohort,trip,date,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))
#View(table1)
table2<-pulse_range%>%
  group_by(year,cohort,date,dummy_pulse)%>%
  summarise(min=min_round,max=max_round)
#View(table2)
table3<-age1length_pulse%>%
  group_by(year,cohort,trip,date,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))%>%
  filter(is.na(pulse))
#View(table3)

write.csv(age0length_pulse,"./data/data-working/age0_dummypulse.csv",row.names = FALSE)

write.csv(range_final,"./data/data-working/age0-pulse-range.csv",row.names = FALSE)


# ---- Pulse Assignment Final Sorting  -----
# Purpose: Finalize pulse assignments

# Format data for plotting

growth<-age0length_pulse%>%
  mutate(cohort=year)
unique(growth$pulse)
growth$pulse<-as.integer(growth$pulse)

#format growth for plotting
growth1<-growth%>%
  group_by(cohort,age,pulse,date,trip)%>%
  summarise(meanSL=mean(mmSL),min=min(mmSL),max=max(mmSL))%>%
  filter(!is.na(pulse))

#---- plot all years with min and max ----
cohort.graph<-function(growth1,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth1$cohort))
  #pdf()
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(growth1,growth1$cohort==cohort_list[i]),
                 aes(x=date,y=meanSL,group=cohort,shape=factor(pulse)))+
      geom_point(size=2)+
      geom_errorbar(aes(ymin=min,ymax=max),width=0)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      #scale_x_date(date_breaks="7 day",date_labels="%b")+
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
#View(catch2)

table4<-catch2%>%
  group_by(year,month,trip,age,pulse,total_catch,total_measured)%>%
  summarise(catch_per_haul=mean(catch_haul))


# use figures and abundance data to assign pulses appropriately