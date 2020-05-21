# Survival and Pre- and post-winter condition
# Part 1: Data organization
# Purpose: Organize raw data for analysis
# files: condition-newman; newman-length; newman-catch; hauls; temperature; age1-pulse-range; trip dates
# and age1_dummypulse (for now)

# ----- set working directory -----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/condition-field/")


# ---- load packages ----
library(lubridate)
library(tidyverse)

# ---- load data ----
condition<-read.csv("../data/data-working/condition-newman.csv")
winter<-read.csv("../data/data-working/newman-winter-summary.csv")
catch_haul<-read.csv("../data/output/catch_haul.csv")
count.data<-read.csv("../data/data-working/newman-catch.csv")
SLfull<-read.csv("../data/data-working/newman-length-updated.csv")
pulse0<-read.csv("../data/output/pulse_range_0_final.csv")
pulse1<-read.csv("../data/output/pulse_range_age1_final.csv")
str(catch_haul)

# ---- SL data -----
names(SLfull)

SLfull$date<-ymd(paste(SLfull$year,SLfull$month,SLfull$day,sep="-"))


# ----- abundance data -----
abund<-catch_haul%>%
  dplyr::select(year,trip,age,total_catch,total_measured,julian.date,extrap_1,
                extrap_2,extrap_3,extrap_4,extrap_5,extrap_6)%>%
  gather(key="pulse1",value="extrap",-year,-trip,-age,-total_catch,-total_measured,
         -julian.date)%>%
  mutate(pulse=str_sub(pulse1,start=8,end=8))%>%
  mutate(pulse=replace(pulse,pulse=='u',NA))%>%
  dplyr::select(-pulse1)%>%
  mutate(count=ceiling(extrap))

# ----- condition data -----

# check data
dim(condition)
names(condition)

condition<-condition%>%
  dplyr::select(-Date,-fulton.k,-pulse) # take out blank column and fulton calc
str(condition)

# Fix weight - find value with comma
test<-condition
test$weight<-as.character(test$weight)
test$weight<-as.numeric(test$weight)
test%>%filter(is.na(weight)) # found the comma value
condition%>%filter(year==2015 & month == 8 & day == 26 & site== "Mount Stamford" &
                     mmSL==53) # find matching entry
condition$weight<-as.character(condition$weight)
condition<-condition%>%
  mutate(weight=replace(weight,weight=="1,226","1.266"))
condition$weight<-as.numeric(condition$weight)
str(condition)

# fix age
unique(condition$age)
condition$age<-as.character(condition$age)
condition<-condition%>%
  mutate(age=replace(age,age=="0+","0"))%>%
  mutate(age=replace(age,age=="0+ ","0"))%>%
  mutate(age=replace(age,age=="1+","1"))%>%
  mutate(age=replace(age,age=="0+/1+?",NA))%>%
  mutate(age=replace(age,age=="0+/1+",NA))# go back and figure out what proper age assignments are using min and max from revised data
unique(condition$age)
typeof(condition$age)
condition$age<-as.integer(condition$age)
str(condition)

# fix date
condition<-condition%>%
  mutate(month=replace(month,month==3 & day == 10 & year == 2017 & age ==0,10))%>%
  mutate(day=replace(day,month==10 & day == 10 & year == 2017 & age ==0,3))
condition$date<-ymd(paste(condition$year,condition$month,condition$day,sep="-"))
str(condition)
summary(condition)

# list of unassigned trips now assigned
trips2<-data.frame(year=c(1998,2002,2002,2003,2003,2003,2004,2005,2006,2009,2009,2011,2017,2017,2014),
                   month=c(8,6,11,7,8,10,11,8,10,8,11,9,7,10,05),
                   day=c(17,16,27,17,3,8,25,25,26,6,4,29,13,16,19),
                   trip=c(16,11,22,13,14,19,22,16,20,15,21,18,13,20,10))
# add trips
trips<-SLfull%>%
  dplyr::select(year,month,day,julian.date,trip,date)%>%
  bind_rows(trips2)%>%
  distinct()%>%
  select(-date)

condition2<-condition%>%
  mutate(ID=seq(1:4235))%>%
  left_join(trips,by=c('year','month','day'))%>%
  distinct(ID, .keep_all = TRUE)%>%
  mutate(fulton=(weight/((mmSL*.1)^3))*100)
SLfull%>%
  filter(species=="AC" & year == 2013 & month ==7 &  age ==0)%>%
  group_by(pulse,age)%>%
  summarise(min(mmSL),max(mmSL),mean(mmSL))
# need to deal with the following years:
# check the lab fish
## 2001
## 2013

summary(pulse0)
pulse_range0<-pulse0%>%
  filter(!is.na(pulse))%>%
  mutate(cohort=year)
# update pulse assignments for age 1
summary(pulse1)
pulse_range1<-pulse1%>%
  filter(!is.na(min))%>%
  mutate(age=1)
head(pulse_range0)
head(pulse_range1)

pulse.range.full<-bind_rows(pulse_range0,pulse_range1)
# adjust pulses for fish out of range
extrapulse<-data.frame(year=2002,
                       trip=10,
                       age=1,
                       pulse=1,
                       min=115,
                       max=129)
pulse.range.2<-pulse.range.full%>%
  mutate(min=replace(min,year==1999 & trip == 19 & age==0 & pulse ==2 & max==67,41))%>%
  mutate(min=replace(min,year==2000 & trip == 13 & age ==1 & pulse == 3 & max==98,69))%>%
  mutate(min=replace(min,year==2000 & trip ==22 & age ==0 & pulse ==3 & max == 62,52))%>%
  mutate(max=replace(max,year==2000 & trip == 22 & age == 0 & pulse ==3 & min==52,63))%>%
  mutate(min=replace(min,year==2000 & trip == 22 & age == 0 & pulse == 1 & max == 104,80))%>%
  mutate(max=replace(max,year==2000 & trip == 22 & age == 0 & pulse == 1 & min == 80,105))%>%
  mutate(max=replace(max,year==2000 & trip == 21 & age == 0 & pulse == 1 & min == 78,105))%>%
  mutate(min=replace(min,year==2000 & trip == 21 & age == 0 & pulse == 3 & max ==61,41))%>%
  mutate(min=replace(min,year==2000 & trip == 19 & age ==0 & pulse == 3 & max == 54,30))%>%
  mutate(max=replace(max,year==2000 & trip == 19 & age == 0 & pulse == 3 & min == 30,55))%>%
  mutate(min=replace(min,year==2001 & trip == 14 & age ==1 & pulse == 3 & max==91,57))%>%
  mutate(max=replace(max,year==2001 & trip == 14 & age == 1 & pulse == 3 & min == 57,92))%>%
  mutate(min=replace(min,year==2001 & trip == 14 & age == 1 & pulse == 2 & max == 125,93))%>%
  mutate(min=replace(min,year==2001 & trip == 22 & age == 0 & pulse ==4 & max ==54,39))%>%
  mutate(max=replace(max,year==2001 & trip == 22 & age == 0 & pulse == 4 & min==39,55))%>%
  mutate(min=replace(min,year==2001 & trip == 22 & age ==0 & pulse ==2 & max ==94,83))%>%
  mutate(max=replace(max,year==2001 & trip == 22 & age ==0 & pulse == 2 & min == 83,97))%>%
  mutate(min=replace(min,year==2001 & trip == 22 & age == 0 & pulse == 1 & max ==112,102))%>%
  mutate(max=replace(max,year==2001 & trip == 19 & age ==0 & pulse == 3 & min == 41,55))%>%
  mutate(min=replace(min,year==2001 & trip == 19 & age ==0 & pulse == 2 & max==68,59))%>%
  mutate(max=replace(max,year==2001 & trip == 19 & age ==0 & pulse == 2 & min == 59,69))%>%
  mutate(min=replace(min,year==2001 & trip == 19 & age == 0 & pulse == 1 & max == 87,70))%>%
  mutate(min=replace(min,year==2002 & trip == 10 & age == 1 & pulse == 2 & max ==107,75))%>%
  mutate(min=replace(min,year==2002 & trip == 10 & age == 1 & pulse == 4 & max== 53,35))%>%
  mutate(min=replace(min,year==2002 & trip == 20 & age == 0 & pulse == 1 & max == 90,44))%>%
  mutate(min=replace(min,year==2002 & trip == 19 & age == 0 & pulse == 1 & max == 82,39))%>%
  mutate(min=replace(min,year==2002 & trip == 22 & age ==0 & pulse == 2 & max == 63,47))%>%
  mutate(max=replace(max,year==2002 & trip == 21 & age == 0 & pulse == 2 & min == 41,57))%>%
  mutate(min=replace(min,year==2003 & trip == 19 & age == 0 & pulse == 1 & max ==89,72))%>%
  mutate(min=replace(min,year==2003 & trip == 19 & age == 0 & pulse == 3 & max ==40,37))%>%
  mutate(max=replace(max,year==2003 & trip == 20 & age ==0 & pulse == 3 & min ==35,55))%>%
  mutate(max=replace(max,year==2003 & trip == 20 & age == 0 & pulse == 1 & min == 77,103))%>%
  mutate(min=replace(min,year==2003 & trip == 21 & age == 0 & pulse == 1 & max == 79,76))%>%
  mutate(min=replace(min,year==2003 & trip == 21 & age== 0 & pulse == 3 & max ==51,39))%>%
  mutate(min=replace(min,year==2003 & trip == 22 & age ==0 & pulse == 3 & max==55,41))%>%
  mutate(min=replace(min,year==2003 & trip == 22 & age == 0 & pulse == 1 & max ==91,79))%>%
  mutate(min=replace(min,year==2003 & trip == 22 & age == 0 & pulse == 2 & max == 78,58))%>%
  mutate(min=replace(min,year==2004 & trip == 14 & age ==0 & pulse == 1 & max == 49,31))%>%
  mutate(min=replace(min,year==2005 & trip == 13 & age == 1 & pulse ==4 & max ==87,70))%>%
  mutate(min=replace(min,year==2005 & trip == 19 & age ==0 & pulse == 1 & max ==109,76))%>%
  mutate(min=replace(min,year==2005 & trip == 20 & age== 0 & pulse == 1 & max == 103,100))%>%
  mutate(min=replace(min,year==2005 & trip == 20 & age ==0 & pulse ==2 & max == 68,53))%>%
  mutate(max=replace(max,year==2005 & trip == 21 & age==0 & pulse == 3 & min==32,50))%>%
  mutate(min=replace(min,year==2005 & trip == 22 & age ==0 & pulse == 3 & max == 62,41))%>%
  mutate(min=replace(min,year==2006 & trip == 19 & age == 0 & pulse == 3 & max ==44,39))%>%
  mutate(min=replace(min,year==2006 & trip == 19 & age ==0 & pulse == 2 & max == 80,50))%>%
  mutate(min=replace(min,year==2006 & trip == 19 & age == 0 & pulse == 1 & max == 98,96))%>%
  mutate(min=replace(min,year==2006 & trip == 20 & age == 0 & pulse == 3 & max == 53,35))%>%
  mutate(min=replace(min,year==2006 & trip == 20 & age == 0 & pulse == 2 & max==91,78))%>%
  mutate(min=replace(min,year==2006 & trip == 21 & age ==0 & pulse == 3 & max ==52,39))%>%
  mutate(min=replace(min,year==2006 & trip == 21 & age == 0 & pulse == 4 & max == 34,28))%>%
  mutate(min=replace(min,year==2006 & trip == 22 & age == 0 & pulse == 4 & max ==49,38))%>%
  mutate(min=replace(min,year==2006 & trip == 22 & age == 0 & pulse == 3 & max == 73,52))%>%
  mutate(min=replace(min,year==2006 & trip == 9 & age == 1 & pulse == 3 & max == 71,40))%>%
  mutate(min=replace(min,year==2006 & trip == 14 & age == 0 & pulse == 1 & max ==55,35))%>%
  mutate(min=replace(min,year==2007 & trip == 9 & age == 1 & pulse == 4 & max==65,37))%>%
  mutate(min=replace(min,year==2007 & trip == 12 & age ==1 & pulse == 4 & max == 84,51))%>%
  mutate(min=replace(min,year==2007 & trip == 14 & age == 0 & pulse== 1 & max == 35,33))%>%
  mutate(min=replace(min,year==2007 & trip == 20 & age == 0 & pulse == 3 & max == 50,31))%>%
  mutate(min=replace(min,year==2007 & trip == 22 & age == 0 & pulse == 3 & max == 56,31))%>%
  mutate(min=replace(min,year==2008 & trip == 10 & age ==1 & pulse == 3 & max ==70,37))%>%
  mutate(min=replace(min,year==2008 & trip ==19 & age ==0 & pulse ==5 & max == 41, 34))%>%
  mutate(min=replace(min,year==2008 & trip == 19 & age ==0 & pulse == 3 & max==66,53))%>%
  mutate(min=replace(min,year==2008 & trip ==20 & age ==0 & pulse == 5 & max == 42,31))%>%
  mutate(max=replace(max,year==2008 & trip == 20 & age ==0 & pulse == 6 & min==27,28))%>%
  mutate(max=replace(max,year==2008 & trip ==20 & age ==0 & pulse == 3 & min == 60,71))%>%
  mutate(max=replace(max,year==2008 & trip == 21 & age ==0 & pulse == 6 & min==30,38))%>%
  mutate(min=replace(min,year==2008 & trip == 21 & age ==0 & pulse == 5 & max == 47,39))%>%
  mutate(min=replace(min,year==2009 & trip == 20 & age ==0 & pulse ==1 & max==107,80))%>%
  mutate(min=replace(min,year==2009 & trip ==20 & age ==0 & pulse == 3 & max == 52,41))%>%
  mutate(min=replace(min,year==2009 & trip == 21 & age == 0 & pulse ==1 & max==103,79))%>%
  mutate(min=replace(min,year==2009 & trip == 22 & age ==0 & pulse == 4 & max==54,27))%>%
  mutate(min=replace(min,year==2009 & trip == 14 & age==0 & pulse ==1 & max==46,25))%>%
  mutate(min=replace(min,year==2010 & trip == 19 & age==0 & pulse==3 & max==44,30))%>%
  mutate(max=replace(max,year==2010 & trip == 19 & age==0 & pulse==3 & min==30,46))%>%
  mutate(min=replace(min,year==2010 & trip == 19 & age ==0 & pulse==2 & max==62,47))%>%
  mutate(min=replace(min,year==2010 & trip == 20 & age==0 & pulse ==3 & max==55,27))%>%
  mutate(min=replace(min,year==2010 & trip == 21 & age ==0 & pulse == 3 & max == 54,30))%>%
  mutate(min=replace(min,year==2010 & trip == 22 & age ==0  & pulse==4 & max==37,28))%>%
  mutate(min=replace(min,year==2010 & trip==22 & age ==0 & pulse == 3 & max==58,40))%>%
  mutate(min=replace(min,year==2011 & trip == 10 & age ==1 & pulse == 3 & max==68,38))%>%
  mutate(min=replace(min,year==2011 & trip == 19 & age == 0 & pulse == 4 & max ==28,20))%>%
  mutate(min=replace(min,year==2011 & trip ==20 & age==0 & pulse == 4 & max==34,23))%>%
  mutate(min=replace(min,year==2011 & trip == 22 & age == 0 & pulse == 4 & max==53,33))%>%
  mutate(min=replace(min,year==2012 & trip==12 & age==1 & pulse == 4 & max==86,56))%>%
  mutate(max=replace(max,year==2012 & trip == 12 & age ==1 & pulse == 4 & min==56,87))%>%
  mutate(min=replace(min,year==2012 & trip==13 & age ==0 & pulse == 1 & max==39,21))%>%
  mutate(min=replace(min,year==2012 & trip == 22 & age ==0 & pulse == 3 & max==56,40))%>%
  mutate(min=replace(min,year==2013 & trip == 9 & age==1 & pulse ==3 & max==63,33))%>%
  mutate(min=replace(min,year==2013 & trip ==14 & age==0 & pulse ==1 & max==50,27))%>%
  mutate(min=replace(min,year==2014 & trip == 10 & age ==1 & pulse == 4 & max==59,28))%>%
  mutate(min=replace(min,year==2014 & trip == 13 & age==1 & pulse == 3 & max==99,70))%>%
  mutate(min=replace(min,year==2014 & trip == 14 & age==1 & pulse ==3 & max==108,82))%>%
  mutate(min=replace(min,year==2014 & trip == 14 & age == 1 & pulse == 4 & max==76,65))%>%
  mutate(min=replace(min,year==2014 & trip == 22 & age ==0 & pulse ==2 & max==80,60))%>%
  mutate(min=replace(min,year==2015 & trip == 13 & age ==1 & pulse == 4 & max==81,63))%>%
  mutate(min=replace(min,year==2015 & trip == 14 & age ==0 & pulse == 1 & max==53,33))%>%
  mutate(min=replace(min,year==2015 & trip == 22 & age ==0 & pulse ==3 & max==55,33))%>%
  mutate(min=replace(min,year==2017 & trip == 10 & age ==1 & pulse == 4 & max==61,34))%>%
  mutate(min=replace(min,year==2017 & trip == 10 & age == 1 & pulse == 2 & max==97,62))%>%
  mutate(min=replace(min,year==2017 & trip == 14 & age == 0 & pulse == 1 & max==39,23))%>%
  bind_rows(extrapulse)

pulses<-data.frame(trip=rep(pulse.range.2$trip,pulse.range.2$max-pulse.range.2$min+1),
                   year=rep(pulse.range.2$year,pulse.range.2$max-pulse.range.2$min+1),
                   cohort=rep(pulse.range.2$cohort,pulse.range.2$max-pulse.range.2$min+1),
                   age=rep(pulse.range.2$age,pulse.range.2$max-pulse.range.2$min+1),
                   pulse=rep(pulse.range.2$pulse,pulse.range.2$max-pulse.range.2$min+1),
                   mmSL=unlist(mapply(seq,pulse.range.2$min,pulse.range.2$max)))
head(pulses)
pulses<-pulses%>%
  distinct()

# assign pulses condition
condition3<-condition2%>%
  mutate(trip=replace(trip,year==2001 & trip ==13,14))%>% # need to check lab fish
  mutate(age=replace(age,year==2002 & trip == 20 & age ==1,0))%>% #need to check lab fish
  mutate(age=replace(age,year==2003 & trip == 13 & age == 0,1))%>% # need to check lab fish
  mutate(age=replace(age,year==2007 & trip ==14 & is.na(age),0))%>%
  mutate(age=replace(age,year==2017 & trip == 20 & is.na(age) & mmSL>100,1))%>% # fill in missing ages
  mutate(age=replace(age,year==2017 & trip == 20 & is.na(age),0))%>% # fill in missing ages
  left_join(pulses)

#Deal with unassinged fish
na.frame<-condition3%>%
  filter(is.na(pulse))%>%
  select(-notes,-site)%>%
  filter(month==10 | month == 11 | month == 5 | month ==7)%>%
  filter(year!=1998)

# ---- Winter Data ----
dim(winter)
names(winter)
str(winter)
summary(winter)
head(winter)

# ---- combine all data ----
nrow(condition3)
nrow(winter)
nrow(abund)

# determine pulse selection
# determine start year
summary(condition3)
summary(winter)
# start with 1999 cohort, end with 2016
cond<-condition3%>%
  filter(cohort<2017)%>%
  select(-ID)%>%
  mutate(ID=seq(1:3813))

winter<-winter%>% filter(cohort>1998 & cohort < 2017)

abund0<-abund%>%
  filter(age==0)%>%
  mutate(cohort=year)
abund1<-abund%>%
  filter(age==1)%>%
  mutate(cohort=year-1)
abundNA<-abund%>%
  filter(is.na(age))%>%
  mutate(cohort=NA)
abund2<-abund%>%
  filter(age==2)%>%
  mutate(cohort=year-2)
abund3<-abund%>%
  filter(age==3)%>%
  mutate(cohort=year-3)
abund<-bind_rows(abund0,abund1,abundNA,abund2,abund3)

abundance<-abund%>%
  filter(cohort>1998 & cohort<2017)%>%
  dplyr::select(year,trip,age,pulse,count,cohort)%>%
  distinct()
nrow(cond)
nrow(winter)
nrow(abundance)
summary(abundance)

str(abundance)
str(cond)
abundance$pulse<-as.integer(abundance$pulse)
cond_all1<-right_join(cond,abundance,by=c('year','trip','age','pulse','cohort'))%>%
  distinct(ID,.keep_all = TRUE)%>%
  filter(!is.na(ID))



# compare old condition with new condition
head(cond_all)
head(cond)

# use test for now
cond_all<-left_join(cond_all1,winter)
names(cond_all)
head(cond_all)
str(cond_all)

testpre<-cond_all%>%
  filter(month==10 | month == 11)%>%
  rename(preK=fulton,preCount=count,preMonth=month)%>%
  dplyr::select(cohort,preMonth,pulse,days_below_1,mean_temp,preK,preCount)

testpost<-cond_all%>%
  filter(month == 5 | month == 7)%>%
  rename(postK=fulton,postCount=count,postMonth=month)%>%
  select(cohort,postMonth,pulse,days_below_1,mean_temp,postK,postCount)
post.count<-testpost%>%
  select(cohort,pulse,postCount)
pre.count<-testpre%>%
  select(cohort,pulse,preCount)
preWinter<-right_join(testpre,post.count)%>%distinct()
postWinter<-right_join(testpost,pre.count)%>%distinct()


#cond_all<-cond_all%>%
# filter(!is.na(pulse))
# creat pre and post condition, and initial and final abundance
df.fall.all<-cond_all%>%
  filter(month==10)%>%
  group_by(cohort,pulse,days_below_1,mean_temp)%>%
  summarise(preK=mean(fulton),preCount=ceiling(mean(count)))%>%
  ungroup()%>%
  as.data.frame()

df.spring.K<-cond_all%>%
  filter(month==5)%>%
  select(-month,-date,-count)%>%
  mutate(season="spring")%>%
  group_by(cohort,pulse,season,days_below_1,mean_temp)%>%
  summarise(postK=mean(fulton))%>%
  ungroup()%>%
  as.data.frame()

df.spring.count<-cond_all%>%
  filter(month==7)%>%
  mutate(season="spring")%>%
  group_by(cohort,pulse,season,days_below_1,mean_temp)%>%
  summarise(postCount=ceiling(mean(count)))%>%
  ungroup()%>%
  as.data.frame()

df.spring.all<-full_join(df.spring.count,df.spring.K)%>%
  select(-season)


alldata<-full_join(df.fall.all,df.spring.all, by=c("cohort","pulse","days_below_1","mean_temp"))
alldata<-alldata%>%
  mutate(postCount=replace(postCount,is.na(postCount),0),
         preCount=replace(preCount,is.na(preCount),0))

head(alldata)

#Save data in output file
write.csv(alldata,"../data/output/condition-field-formatted.csv",row.names = FALSE)
