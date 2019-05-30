# ---- Age 1 Pulses ----
# Purpose: assign dummy pulses to length data for all age 1 cod

# ---- set working directory ----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

# ---- load packages ----
library(lubridate)
library(tidyverse)

source("./code/pulse_range_fct.R")
# ---- load data ----
length<-read.csv("./data/data-working/newman-length.csv")
age1pulse<-read.csv("./data/data-working/age-1-mixture-dist.csv")
#tripdate<-read.csv("./data/data-working/newman-trips.csv")


# ---- check data ----
str(length)
summary(length)
dim(length)
head(length)
names(length)

str(age1pulse)
summary(age1pulse)
dim(age1pulse)
head(age1pulse)
names(age1pulse)


# format dates
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
age1pulse$date<-ymd(paste(age1pulse$year,age1pulse$month,age1pulse$day,sep="-"))

# make dataframe with min and max pulse ranges
# use +/- standard deviation for "min" and "max"

range_final<-pulse_range(age1pulse)
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

write.csv(age1length_pulse,"./data/data-working/age1_dummypulse.csv",row.names = FALSE)

write.csv(range_final,"./data/data-working/age1-pulse-range.csv",row.names = FALSE)

# ----- Pulse Check -----
# check the frequency between pulses -> % overlap and % unaccounted for

str(age1length_pulse)

