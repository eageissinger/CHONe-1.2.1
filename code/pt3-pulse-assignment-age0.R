# ---- Age 1 Pulses ----
# Purpose: assign dummy pulses to length data for all age 1 cod

# ---- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")
# ---- load packages ----
library(lubridate)
library(tidyverse)

source("./code/pulse_range_fct.R")
# ---- load data ----
length<-read.csv("./data/data-working/newman-length.csv")
age0pulse<-read.csv("./data/data-working/age-0-mixture-dist.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")

# ---- check data ----
str(length)
summary(length)
dim(length)
head(length)
names(length)

str(age0pulse)
summary(age0pulse)
dim(age0pulse)
head(age0pulse)
names(age0pulse)

# format dates
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
age0pulse$date<-ymd(paste(age0pulse$year,age0pulse$month,age0pulse$day,sep="-"))


# make dataframe with min and max pulse ranges
# use +/- standard deviation for "min" and "max"


# need to figure out where to go from here.....


#test<-freq_test1%>%
#  group_by(cohort,trip,pulse)%>%
#  filter(mmSL)


# incorporate the 'in between' ranges
# best way to do this is to split the difference between min of pulse 1 and max of pulse 2, etc.
range_final<-pulse_range(age0pulse)
# next step:
# need to create a data frame that fills in all possible length 
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
View(age0length)

# assign pulses to subsetted age1 length data

age0length_pulse<-left_join(age0length,pulse_assign)
#View(age1length_pulse)

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

# ----- Pulse Check -----
# check the frequency between pulses -> % overlap and % unaccounted for

str(age1length_pulse)

