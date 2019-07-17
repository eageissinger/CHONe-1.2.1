# Survival and Pre- and post-winter condition
# Part 1: Data organization
# Purpose: Organize raw data for analysis
# files: condition-newman; newman-length; newman-catch; hauls; temperature; age1-pulse-range; trip dates
# and age1_dummypulse (for now)

# ----- set working directory -----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")


# ---- load packages ----
library(MASS)
library(tidyverse)
library(lubridate)
library(pscl)
library(boot)
library(car)
library(corrplot)
library(glmmTMB)
library(lme4)

# ---- load data ----
condition<-read.csv("./data/data-working/condition-newman.csv")
pulse_range1<-read.csv("./data/data-working/pulse_range_age1_final.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")
winter<-read.csv("./data/data-working/newman-winter-summary.csv")
catch_haul<-read.csv("./data/data-working/catch_haul.csv")
pulse_range0<-read.csv("./data/data-working/pulse_range_age0_final.csv")

# ----- abundance data -----
abund<-catch_haul%>%
  select(1:15)%>%
  gather(key="pulse1",value="count",-year,-trip,-age,-total_catch,-total_measured,
         -julian.date,-num_hauls,-weight)%>%
  mutate(pulse=str_sub(pulse1,start=7,end=7))%>%
  mutate(pulse=replace(pulse,pulse=='u',NA))%>%
  mutate(weight.count=count*weight)%>%
  mutate(count.adj=round(weight.count))%>%
  select(-pulse1)

# ----- condition data -----

# check data
dim(condition)
names(condition)

condition<-condition%>%
  select(-Date,-fulton.k,-pulse) # take out blank column and fulton calc
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
condition$date<-ymd(paste(condition$year,condition$month,condition$day,sep="-"))
str(condition)
summary(condition)

# add trips
condition<-left_join(condition,trips)%>%
  mutate(fulton=(weight/((mmSL*.1)^3))*100)



# update pulse assignments for age 0
summary(pulse_range0)
pulse_range0<-pulse_range0%>%
  filter(!is.na(pulse))

pulse_assign0<-data.frame(trip=rep(pulse_range0$trip,pulse_range0$max-pulse_range0$min+1),
                         year=rep(pulse_range0$year,pulse_range0$max-pulse_range0$min+1),
                         age=rep(pulse_range0$age,pulse_range0$max-pulse_range0$min+1),
                         pulse=rep(pulse_range0$pulse,pulse_range0$max-pulse_range0$min+1),
                         mmSL=unlist(mapply(seq,pulse_range0$min,pulse_range0$max)))
pulse_assign0<-pulse_assign0%>%
  mutate(cohort=year)

# update pulse assignments for age 1
summary(pulse_range1)
pulse_range1<-pulse_range1%>%
  filter(!is.na(min))

pulse_assign1<-data.frame(trip=rep(pulse_range1$trip,pulse_range1$max-pulse_range1$min+1),
                         year=rep(pulse_range1$year,pulse_range1$max-pulse_range1$min+1),
                         age=rep(pulse_range1$age,pulse_range1$max-pulse_range1$min+1),
                         pulse=rep(pulse_range1$pulse,pulse_range1$max-pulse_range1$min+1),
                         mmSL=unlist(mapply(seq,pulse_range1$min,pulse_range1$max)))
pulse_assign1<-pulse_assign1%>%
  mutate(cohort=year-1)%>%
  data.frame()

# combine age 0 and age 1 pulse ranges
pulses<-bind_rows(pulse_assign0,pulse_assign1)

# assign pulses condition
condition<-left_join(condition,pulses)

# ---- Winter Data ----
dim(winter)
names(winter)
str(winter)
summary(winter)
head(winter)

# ---- combine all data ----
nrow(condition)
nrow(winter)
nrow(abund)

# determine pulse selection
# determine start year
summary(condition)
# start with 1999 cohort, end with 2016 (for now)
cond<-condition%>%
  filter(cohort<2017)
winter<-winter%>% filter(cohort>1998 & cohort < 2017)

abund0<-abund%>%filter(age==0)%>%
  mutate(cohort=year)
abund1<-abund%>%filter(age==1)%>%
  mutate(cohort=year-1)
abundNA<-abund%>%filter(is.na(age))
abund2<-abund%>%filter(age>1)
abund<-bind_rows(abund0,abund1,abundNA,abund2)

abundance<-abund%>%
  filter(cohort>1998 & cohort<2017)
nrow(cond)
nrow(winter)
nrow(abund)
summary(abund)

str(cond)
str(abundance)
abundance$pulse<-as.integer(abundance$pulse)
abundance<-abundance%>%rename(weighting=weight)
cond_all<-left_join(cond,abundance)
cond_all<-left_join(cond_all,winter)
names(cond_all)
head(cond_all)
str(cond_all)
#cond_all<-cond_all%>%
 # filter(!is.na(pulse))
# creat pre and post condition, and initial and final abundance
df<-cond_all%>%
  filter(month==10 |month == 7)%>%
  mutate(season="fall")%>%
  mutate(season=replace(season,month==7,"spring"))%>%
  group_by(cohort,pulse,season,days_below_1,mean_temp,num_hauls)%>%
  summarise(K=mean(fulton),abund=mean(count.adj))%>%
  mutate(season2=season)%>%
  spread(key=season,value = K)%>%
  rename(preK=fall,postK=spring)%>%
  spread(key=season2,value=abund)%>%
  rename(preAbund=fall,postAbund=spring)%>%
  mutate(preCount=round(preAbund),postCount=round(postAbund))%>%
  select(-preAbund,-postAbund)%>%
  ungroup()%>%
  as.data.frame()
names(df)
prewinter<-df%>%
  select(cohort,pulse,days_below_1,mean_temp,preK,preCount,num_hauls)%>%
  filter(!is.na(preK))%>%
  filter(!is.na(preCount))
postwinter<-df%>%
  select(cohort,pulse,postK,postCount)%>%
  filter(!is.na(postK))%>%
  filter(!is.na(postCount))
dim(prewinter)
dim(postwinter)
alldata<-left_join(prewinter,postwinter)
alldata<-alldata%>%
  mutate(postCount=replace(postCount,is.na(postCount),0))
head(alldata)

# ---- model ----
# October and July model
summary(alldata)
str(alldata)
alldata<-alldata%>%filter(pulse!=4)

# raw count
m0<-glm(postCount~factor(pulse)+preCount+preK+postK+mean_temp+cohort,
        offset = num_hauls,data=alldata,family="poisson")
m0
hist(resid(m0))
plot(m0)
summary(m0)

m1<-glm(cbind(postCount,preCount)~cohort+factor(pulse)+preK+postK+mean_temp,
        offset=num_hauls,data=alldata,family="binomial")
logLik(m1)
exp(logLik(m1))

m1
hist(resid(m1))
plot(m1)
summary(m1)
Anova(m1,type="III")

# Liklihood is zero. back to drawing board
# deal with temporal autocorrelation?
# introduce interactions without breaking the model
# generalized additive model...



#---- collinearity check ----
mydata$cohort<-as.numeric(mydata$cohort)
mydata$pulse<-as.numeric(mydata$pulse)
mydata<-mydata%>%select(-ratio)
mat1<-as.matrix(mydata)
cor1<-cor.mtest(mydata)
corrplot(cor(mydata),method="shade",shade.col=NA,tl.col="black", tl.srt=45)

# ---- mixed effect model -----
mix1<-lmer(postCount~preCount+preK+factor(pulse)+days_below_1+(1|cohort),data = mydata)
plot(mix1)

#GLMM
mix2<-glmer(postCount~preK+factor(pulse)+days_below_1+(1|cohort),
            data=mydata,family = poisson(link = "logit"))

mix3<-glmer.nb(postCount~preCount+preK+factor(pulse)+days_below_1+(1|cohort),
               data=mydata,family=binomial(link = "log"))

