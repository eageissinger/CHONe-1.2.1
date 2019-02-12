# ---- Age 1 Pulses ----
# Purpose: assign dummy pulses to length data for all age 1 cod

# ---- set working directory ----
setwd("C:/Users/USER/Documents/Research/CHONe-1.2.1/")

# ---- load packages ----
library(lubridate)
library(tidyverse)

# ---- load data ----
length<-read.csv("./data/data-working/newman-length.csv")
age1pulse<-read.csv("./data/data-working/age-1-mixture-dist.csv")
tripdate<-read.csv("./data/data-working/newman-trips.csv")


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

str(tripdate)
summary(tripdate)
dim(tripdate)
head(tripdate)
names(tripdate)

# format dates
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
age1pulse$date<-ymd(paste(age1pulse$year,age1pulse$month,age1pulse$day,sep="-"))
tripdate$date<-ymd(paste(tripdate$year,tripdate$month,tripdate$day, sep="-"))


# make dataframe with min and max pulse ranges
# use +/- standard deviation for "min" and "max"
pulse_range<-age1pulse%>%
  group_by(date,dummy_pulse,year,cohort)%>%
  summarise(min=(mu-sigma),max=mu+sigma,sd=sigma)%>%
  mutate(min_round=floor(min),max_round=ceiling(max))# add rounded min and max to create an
# integer column. Min is rounded down, max is rounded up

# ---- Frequency check Round 1 -------
pulse_range<-left_join(pulse_range,tripdate)
freq_test1<-data.frame(trip=rep(pulse_range$trip,pulse_range$max_round-pulse_range$min_round+1),
                         date=rep(pulse_range$date,pulse_range$max_round-pulse_range$min_round+1),
                         cohort=rep(pulse_range$cohort,pulse_range$max_round-pulse_range$min_round+1),
                         pulse=rep(pulse_range$dummy_pulse,pulse_range$max_round-pulse_range$min_round+1),
                         mmSL=unlist(mapply(seq,pulse_range$min_round,pulse_range$max_round)))
freq_test1<-freq_test1%>%
  mutate(year=cohort+1)%>%
  select(-date)%>%
  data.frame()
l1<-select(length,-trip)
lt1<-left_join(l1,tripdate)
age1l<-lt1%>%filter(age==1)%>%select(-pulse)
freq_test1<-left_join(age1l,freq_test1)

# need to figure out where to go from here.....


test<-freq_test1%>%
  group_by(cohort,trip,pulse)%>%
  filter(mmSL)


# incorporate the 'in between' ranges
# best way to do this is to split the difference between min of pulse 1 and max of pulse 2, etc.
pulse_range2<-pulse_range%>%
  group_by(date)%>%
  mutate(diff=(lag(min_round)-max_round)/2)%>% # split difference between the pulse groups
  mutate(plusmax=floor(diff))%>% # round to prevent overlap
  mutate(minusmin=floor(lead(diff)))%>% # round to prevent overlap
  replace_na(list(plusmax=0,minusmin=0))%>% # change NAs to 0
  mutate(min_final=min_round-minusmin,max_final=max_round+plusmax)%>% # add/subtract appropriate terms
  mutate(lagmin=lag(min_final))%>% # create a lag column of min values to compare to max
  mutate(max1=replace(max_final,lagmin==max_final,1))%>% # any point where lagmin and lagmax match, put in a 1
  mutate(max1=replace(max1,max1==max_final,0))%>% # 0 for all others
  mutate(max_final2=max_final-max1)%>% # subtract 1 from max final (gets rid of any remaining overlap)
  select(-diff,-plusmax,-minusmin,-max_final,-lagmin,-max1)%>% #get rid of useless columns
  ungroup()
#View(pulse_range2)
# add 2sd above and below pulse range
pulse_range3<-pulse_range2%>%
  group_by(date)%>%
  mutate(adjustmax=ceiling(2*sd))%>%
  mutate(adjustmax=replace(adjustmax,dummy_pulse!=1,0))%>%
  mutate(adjustmin=ceiling(2*sd))%>%
  mutate(adjustmin=replace(adjustmin,dummy_pulse!=max(dummy_pulse),0))%>%
  mutate(max_final3=adjustmax+max_final2,
         min_final3=min_final-adjustmin)%>%
  select(date,dummy_pulse,year,cohort,min_final3,max_final3)%>%
  rename(min=min_final3,max=max_final3)


# combine trip data to pulse_range
## using trip data to ease pulse assignment later on, since ranges came
# from data across multiple days
pulse_trip<-left_join(pulse_range3,tripdate)
View(pulse_trip)
pulsetripNA<-pulse_trip%>%
  filter(is.na(trip)) # check for NAs
unique(pulsetripNA$date)

# Determine what the trip dates should be for the missing values
# these dates are not associated with a trip because the data is a 
# combination of two separate trips, with the date averaged between
# 1996, 1999, 2004, and 2015 are between trip 13 and 14
# 2012 is between trip 12 and 13

# create a dataframe to assign trips
range1<-pulse_trip%>%mutate(trip=replace(trip,date=="1996-07-24",13),
                           trip=replace(trip,date=="2004-07-24",13),
                           trip=replace(trip,date=="2012-07-10",12),
                           trip=replace(trip,date=="2015-07-21",13))
range2<-pulse_trip%>%mutate(trip=replace(trip,date=="1996-07-24",14),
                            trip=replace(trip,date=="2004-07-24",14),
                            trip=replace(trip,date=="2012-07-10",13),
                            trip=replace(trip,date=="2015-07-21",14))
range_final<-dplyr::union(range1,range2)

is.na(range_final)

# next step:
# need to create a data frame that fills in all possible length 
# possibilities for every pulse option

# next step: use the below functions and apply to entire dataset

pulse_assign<-data.frame(trip=rep(range_final$trip,range_final$max-range_final$min+1),
                         date=rep(range_final$date,range_final$max-range_final$min+1),
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
  mutate(year=cohort+1)%>%
  select(-date)%>%
  data.frame()

# assign pulses to age 1 length data

# select age 1 cod
lengthtrip<-left_join(length,tripdate)
#View(lengthtrip)
age1length<-lengthtrip%>%filter(age==1)%>%select(-pulse)
#View(age1length)

# assign pulses to subsetted age1 length data

age1length_pulse<-left_join(age1length,pulse_assign)
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

test<-age1length_pulse%>%
  group_by(cohort,age,trip,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))%>%
  spread(pulse,)
