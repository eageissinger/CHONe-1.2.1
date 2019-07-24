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
library(mgcv)

# ---- load data ----
condition<-read.csv("./data/data-working/condition-newman.csv")
pulse_range1<-read.csv("./data/data-working/pulse_range_age1_final.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")
winter<-read.csv("./data/data-working/newman-winter-summary.csv")
catch_haul<-read.csv("./data/data-working/catch_haul.csv")
pulse_range0<-read.csv("./data/data-working/pulse_range_age0_final.csv")
count.data<-read.csv("./data/data-working/newman-catch.csv")

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

summary(count.data)
count.data1<-count.data%>%
  filter(age==1)%>%
  mutate(cohort=year-1)
count.data0<-count.data%>%
  filter(age==0)%>%
  mutate(cohort=year)
count.data<-bind_rows(count.data0,count.data1)
  

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
  select(year,trip,age,num_hauls,pulse,count.adj,cohort)
nrow(cond)
nrow(winter)
nrow(abund)
summary(abund)

str(cond)
cond<-cond%>%
  mutate(ID=1:3524)
str(abundance)
abundance$pulse<-as.integer(abundance$pulse)
abundance<-abundance%>%rename(weighting=weight)%>%
  select()
cond_all<-right_join(cond,abundance,by=c('year','trip','age','pulse','cohort'))


test<-cond_all%>%
  distinct()%>%
  filter(!is.na(ID))


# compare old condition with new condition
head(test)
head(cond)

test2<-test%>%
  select(year,month,day,site,species,age,mmSL,weight, notes, date, trip, fulton, pulse,cohort,ID)
compare_df(cond,test2,group_col = c("year","pulse","ID"))

# use test for now
cond_all<-left_join(test,winter)
names(cond_all)
head(cond_all)
str(cond_all)

testpre<-cond_all%>%
  filter(month==10 | month == 11)%>%
  rename(preK=fulton,preCount=count.adj,preMonth=month)%>%
  select(cohort,preMonth,pulse,num_hauls,days_below_1,mean_temp,preK,preCount)

testpost<-cond_all%>%
  filter(month == 5 | month == 7)%>%
  rename(postK=fulton,postCount=count.adj,postMonth=month)%>%
  select(cohort,postMonth,pulse,num_hauls,days_below_1,mean_temp,postK,postCount)
post.count<-testpost%>%
  select(cohort,pulse,postCount)
pre.count<-testpre%>%
  select(cohort,pulse,preCount)
preWinter<-right_join(testpre,post.count)%>%distinct()
postWinter<-right_join(testpost,pre.count)%>%distinct()

  
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

# GLM: Bionomial
m0<-glm(cbind(postCount,preCount)~factor(pulse)+preK+postK+days_below_1+offset(num_hauls),
        data=alldata,family=binomial)
plot(m0)
hist(resid(m0))
fit<-fitted(m0)
res=resid(m0)
plot(x=fit,y=res)
exp(logLik(m0))
summary(m0)
Anova(m0,type="III")


m1<-glm(cbind(postCount,preCount)~factor(pulse)+preK+postK+days_below_1+cohort+offset(num_hauls),
        data=alldata,family=binomial)
plot(m1)
hist(resid(m1))
exp(logLik(m1))
fit<-fitted(m1)
res=resid(m1)
plot(x=fit,y=res)
Anova(m1,type="III")
summary(m1)

# GLM Poisson
m2<-glm(postCount~preCount+factor(pulse)+preK+postK+days_below_1+offset(num_hauls),
        data=alldata,family=poisson)
plot(m2)
hist(resid(m2))
exp(logLik(m2))
summary(m2)
Anova(m2,type="III")


m3<-glm.nb(postCount~preCount+factor(pulse)+preK+postK+days_below_1+offset(num_hauls),
           data=alldata)
plot(m3)
hist(resid(m3))
exp(logLik(m3))
summary(m3)
Anova(m3,type="III")


#GLMM binomial
m4<-glmer(cbind(postCount,preCount)~factor(pulse)+preK+mean_temp+(1|cohort)+offset(num_hauls),
          data=alldata,family=binomial)

m5<-glmer(postCount~preCount+factor(pulse)+preK+mean_temp+(1|cohort)+offset(num_hauls),
          data=alldata,family=poisson)

m6<-glmer.nb(postCount~preCount+factor(pulse)+preK+mean_temp+(1|cohort)+offset(num_hauls),
          data=alldata)
plot(m6)
hist(resid(m6))
exp(logLik(m6))



#GLS
m7<-gls(postCount~factor(pulse)+preCount+preK+mean_temp,
        correlation = corAR1(form=~cohort|factor(pulse)),na.action = na.omit,data=alldata)

# Liklihood is zero. back to drawing board
# deal with temporal autocorrelation?
# introduce interactions without breaking the model
# generalized additive model...



# ---- reduced models ----

m8<-glm(cbind(postCount,preCount)~factor(pulse)+preK+mean_temp+offset(num_hauls),
        data=alldata,family=binomial)

plot(m8)
hist(resid(m8))

#---- survival as response ----
surv<-alldata%>%
  mutate(survival=postCount/preCount)
m.0<-lm(survival~factor(pulse)+preK+postK+mean_temp+offset(num_hauls),
        data=surv)
plot(m.0)
hist(resid(m.0))
summary(m.0)


m.1<-glm(survival~factor(pulse)+preK+postK+mean_temp+offset(num_hauls),
         data=surv,family=Gamma)

str(alldata)
#---- collinearity check ----
df<-alldata%>%
  select(cohort,postCount,preCount,pulse)
mat1<-as.matrix(df)
cor1<-cor.mtest(df)
corrplot(cor(df),method="shade",shade.col=NA,tl.col="black", tl.srt=45)

plot(alldata$cohort,alldata$preCount)


# further exploration

m0<-glm(cbind(postCount,preCount)~factor(pulse)+preK+postK+days_below_1+offset(num_hauls),
        data=alldata,family=binomial)
plot(m0)
hist(resid(m0))
fit<-fitted(m0)
res=resid(m0)
plot(x=fit,y=res)
exp(logLik(m0))
summary(m0)
Anova(m0,type="III")

m2<-glm(postCount~preCount+factor(pulse)+preK+postK+days_below_1+offset(num_hauls),
        data=alldata,family=poisson)
plot(m2)
hist(resid(m2))
fit<-fitted(m2)
res=resid(m2)
plot(x=fit,y=res)
exp(logLik(m2))
summary(m2)
Anova(m2,type="III")

# ---- preK only ----
m.0<-glm(cbind(postCount,preCount)~factor(pulse)+preK+days_below_1+factor(preMonth)+offset(num_hauls),
        data=preWinter,family=binomial)
plot(m.0)
hist(resid(m.0))
fit<-fitted(m.0)
res=resid(m.0)
plot(x=fit,y=res)
exp(logLik(m.0))
summary(m.0)
Anova(m.0,type="III")

#postK

m.1<-glm(cbind(postCount,preCount)~factor(pulse)+postK+days_below_1+factor(postMonth)+cohort+offset(num_hauls),
           data=postWinter,family = binomial)
plot(m.1)
hist(resid(m.1))
fit=(fitted(m.1))
res<-resid(m.1)
plot(x=fit,y=res)
exp(logLik(m.1))
summary(m.1)
Anova(m.1,type="III")



# explore figures
ggplot(preWinter,aes(x=pulse,y=postCount,colour=preCount))+geom_jitter()

ggplot(preWinter,aes(x=preCount,y=postCount,size=preK,colour=mean_temp,shape=factor(pulse)))+geom_jitter()

ggplot(postWinter,aes(x=preCount,y=postCount,size=postK,colour=mean_temp,shape=factor(pulse)))+geom_jitter()

ggplot(postWinter,aes(x=cohort,y=postCount,shape=factor(pulse),colour=mean_temp,size=postK))+geom_point()

ggplot(preWinter,aes(x=cohort,y=postCount,shape=factor(pulse),colour=mean_temp,size=preK))+geom_point()


ggplot(alldata,aes(y=postCount/preCount,x=cohort,size=mean_temp,colour=factor(pulse)))+geom_point()
