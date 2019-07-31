# Survival and Pre- and post-winter condition
# Part 1: Data organization
# Purpose: Organize raw data for analysis
# files: condition-newman; newman-length; newman-catch; hauls; temperature; age1-pulse-range; trip dates
# and age1_dummypulse (for now)

# ----- set working directory -----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")


# ---- load packages ----
library(MASS)
library(lubridate)
library(tidyverse)
library(pscl)
library(boot)
library(car)
library(corrplot)
library(glmmTMB)
library(lme4)
library(mgcv)
library(effects)

# ---- load data ----
condition<-read.csv("./data/data-working/condition-newman.csv")
pulse_range1<-read.csv("./data/data-working/pulse_range_age1_final.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")
winter<-read.csv("./data/data-working/newman-winter-summary.csv")
catch_haul<-read.csv("./data/data-working/catch_haul.csv")
pulse_range0<-read.csv("./data/data-working/pulse_range_age0_final.csv")
count.data<-read.csv("./data/data-working/newman-catch.csv")
str(catch_haul)
# ----- abundance data -----
abund<-catch_haul%>%
  select(year,trip,age,total_catch,total_measured,julian.date,extrap_1,
         extrap_2,extrap_3,extrap_4,extrap_5,extrap_6)%>%
  gather(key="pulse1",value="extrap",-year,-trip,-age,-total_catch,-total_measured,
         -julian.date)%>%
  mutate(pulse=str_sub(pulse1,start=8,end=8))%>%
  mutate(pulse=replace(pulse,pulse=='u',NA))%>%
  select(-pulse1)%>%
  mutate(count=ceiling(extrap))

#summary(count.data)
#count.data1<-count.data%>%
#  filter(age==1)%>%
#  mutate(cohort=year-1)
#count.data0<-count.data%>%
#  filter(age==0)%>%
#  mutate(cohort=year)
#count.data<-bind_rows(count.data0,count.data1)
  

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
  filter(cohort>1998 & cohort<2017)%>%
  select(year,trip,age,pulse,count,cohort)
nrow(cond)
nrow(winter)
nrow(abund)
summary(abund)

str(cond)
cond<-cond%>%
  mutate(ID=1:3524)
str(abundance)
abundance$pulse<-as.integer(abundance$pulse)
cond_all<-right_join(cond,abundance,by=c('year','trip','age','pulse','cohort'))


test<-cond_all%>%
  distinct()%>%
  filter(!is.na(ID))


# compare old condition with new condition
head(test)
head(cond)

test2<-test%>%
  select(year,month,day,site,species,age,mmSL,weight, notes, date, trip, fulton, pulse,cohort,ID)


# use test for now
cond_all<-left_join(test,winter)
names(cond_all)
head(cond_all)
str(cond_all)

testpre<-cond_all%>%
  filter(month==10 | month == 11)%>%
  rename(preK=fulton,preCount=count,preMonth=month)%>%
  select(cohort,preMonth,pulse,days_below_1,mean_temp,preK,preCount)

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

# ---- model ----
# October and July model
summary(alldata)
str(alldata)

# GLM: Bionomial
m0<-glm(cbind(postCount,preCount)~factor(pulse)+preK+postK+days_below_1+days_below_1+cohort,
        data=alldata,family=binomial)
plot(m0)
hist(resid(m0))
fit<-fitted(m0)
res=resid(m0)
plot(x=fit,y=res)
exp(logLik(m0))
summary(m0)
Anova(m0,type="III")
AIC(m0)

# GLM Poisson
m2<-glm(postCount~preCount+factor(pulse)+preK+postK+days_below_1+mean_temp,
        data=alldata,family=poisson)
plot(m2)
hist(resid(m2))
exp(logLik(m2))
summary(m2)
Anova(m2,type="III")


m3<-glm.nb(postCount~preCount+factor(pulse)+preK+postK+days_below_1+mean_temp,
           data=alldata)
plot(m3)
hist(resid(m3))
exp(logLik(m3))
summary(m3)
Anova(m3,type="III")


#GLMM binomial
alldata$pulse<-as.factor(alldata$pulse)
m4<-glmer(cbind(postCount,preCount)~pulse+postK+preK+scale(days_below_1)+
            (1|cohort),data=alldata,family=binomial)
par(mfrow=c(2,2))
plot(m4)
fit<-fitted(m4)
res<-resid(m4)
plot(x=fit,y=res)
hist(resid(m4))
qqnorm(resid(m4))
qqline(resid(m4),col='red')
logLik(m4)
exp(logLik(m4))
summary(m4)
Anova(m4,type="III")
par(mfrow=c(1,1))
plot(allEffects(m4))

null<-glmer(cbind(postCount,preCount)~1+(1|cohort),data=alldata,family=binomial)
plot(null)
hist(resid(null))
qqnorm(resid(null))
qqline(resid(null),col='red')
exp(logLik(null))
summary(null)

var1<-glmer(cbind(postCount,preCount)~scale(days_below_1)+
              (1|cohort),data=alldata,family=binomial)
var2<-glmer(cbind(postCount,preCount)~scale(days_below_1)+preK+
              (1|cohort),data=alldata,family=binomial)
var3<-glmer(cbind(postCount,preCount)~scale(days_below_1)+preK+postK+
              (1|cohort),data=alldata,family=binomial)
AIC(null)
AIC(var1)
AIC(var2)
AIC(var3)
AIC(m4)
# better than the null - doesn't explain all variation but is explaining some variation

# ---- first and last pulse ----
write.csv(alldata,"./data/data-working/condition-data-prepped.csv",row.names = FALSE)

settle<-read.csv("./data/data-working/condition-settlement.csv")

m5<-glmer(cbind(postCount,preCount)~settlement+preK+postK+
            (1|cohort),data=settle,family=binomial)
plot(m5)
hist(resid(m5))
exp(logLik(m5))
Anova(m5,type="III")
summary(m5)
plot(allEffects(m5))

