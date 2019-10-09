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
library(lmtest)
library(magclass)
# ---- load data ----
condition<-read.csv("./data/data-working/condition-newman.csv")
pulse_range1<-read.csv("./data/data-working/pulse_range_age1_final.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")
winter<-read.csv("./data/data-working/newman-winter-summary.csv")
catch_haul<-read.csv("./data/data-working/catch_haul.csv")
pulse_range0<-read.csv("./data/data-working/pulse_range_age0_final.csv")
count.data<-read.csv("./data/data-working/newman-catch.csv")
SLfull<-read.csv("./data/data-working/newman-length-all.csv")
str(catch_haul)

# ---- SL data -----
names(SLfull)
SLfull<-SLfull%>%
  select(-Wt...g.,-Time.1)

SLfull$date<-ymd(paste(SLfull$year,SLfull$month,SLfull$day,sep="-"))


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
summary(winter)
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
alldata<-alldata%>%filter(pulse<5)
alldata$pulse<-as.factor(alldata$pulse)
alldata$pulse<-droplevels(alldata$pulse)

m4<-glmer(cbind(postCount,preCount)~pulse+preK+postK+scale(days_below_1)+
            (1|cohort),data=alldata,family=binomial)

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
Anova(m4,type="III",test.statistic = c("LR"))
plot(allEffects(m4))

m0<-glmer(cbind(postCount,preCount)~1+(1|cohort),data=alldata,family=binomial)
m1<-glmer(cbind(postCount,preCount)~pulse+
            (1|cohort),data=alldata,family=binomial)
m2<-glmer(cbind(postCount,preCount)~pulse+preK+
            (1|cohort),data=alldata,family=binomial)
m3<-glmer(cbind(postCount,preCount)~pulse+preK+postK+
          (1|cohort),data=alldata,family=binomial)

exp(logLik(m0))
exp(logLik(m1))
exp(logLik(m2))
exp(logLik(m3))
exp(logLik(m4))

Anova(m4,type="III")
summary(m4)
plot(allEffects(m4))

# ---- predictions ----
# predict values
output.m4<-data.frame(pulse=rep(seq(c(1:4)),18),
                      postK=sample(seq(from=0.5,
                                to=0.9,
                                length=72)),
                      preK=sample(seq(from=0.5,
                               to=0.9,
                               length=72)),
                      days_below_1=sample(seq(from=min(alldata$days_below_1),
                                       to=max(alldata$days_below_1),
                                       length=72)),
                      cohort=c(rep(c(1999),4),rep(c(2000),4),rep(c(2001),4),rep(c(2002),4),rep(c(2003),4),rep(c(2004),4),
                               rep(c(2005),4),rep(c(2006),4),rep(c(2007),4),rep(c(2007),4),rep(c(2007),4),rep(c(2010),4),
                               rep(c(2011),4),rep(c(2012),4),rep(c(2013),4),rep(c(2014),4),rep(c(2015),4),rep(c(2016),4)))
output.m4$pulse<-as.factor(output.m4$pulse)

output.m4$predicted<-predict(m4,output.m4,allow.new.levels=TRUE,type='response')
# issues with predicting: issue incorporating factor
## also, i have randomly assigned all values randomly to pulses, however, the range for each pulse should
## likely remain the same for all sample data..... Don't know where to go from here

ggplot(alldata,aes(x=postK,y=postCount/preCount,colour=pulse))+geom_point()+
  geom_smooth(method="glm",alpha=0.15,aes(fill=pulse))
ggplot(alldata,aes(x=preK,y=postCount/preCount,colour=pulse))+geom_point()+
  geom_smooth(method='glm',alpha=0.15,aes(fill=pulse))


str(p0<-predict(m4))
str(p1<-predict(m4,re.form=NA))
newdata<-with(alldata,expand.grid(pulse=unique(pulse),postK=unique(postK),preK=unique(preK),
                                  days_below_1=unique(days_below_1),cohort=unique(cohort)))
str(p2<-predict(m4,newdata,allow.new.levels=TRUE))
str(p3<-predict(m4,newdata,re.form=NA,allow.new.levels=TRUE))
str(p4<-predict(m4,newdata,re.form= ~(1|cohort),allow.new.levels=TRUE))
stopifnot(identical(p2,p4))
summary(m4)

output.m4$pred<-predict(m4, newdata = output.m4, newparams = NULL,
        re.form = NULL,
        random.only=FALSE, terms = NULL,
        type = c("link", "response"), allow.new.levels = TRUE,
        na.action = na.pass)

ggplot(alldata,aes(y=postCount/preCount,x=days_below_1,colour=pulse))+geom_point()+
  geom_smooth(aes(x=days_below_1,y=predicted),data=output.m4,size=1)

# --- Further investegation ----
alldata%>%
  mutate(survival=postCount/preCount)%>%
  ggplot(aes(x=factor(pulse),y=survival))+geom_boxplot(outlier.shape = NA)+
  ylim(0,3)

alldata%>%
  mutate(survival=postCount/preCount)%>%
  ggplot(aes(x=postK,y=survival))+geom_smooth()

alldata%>%
  mutate(survival=postCount/preCount)%>%
  ggplot(aes(x=preK,y=survival))+geom_smooth()

alldata%>%
  mutate(survival=postCount/preCount)%>%
  ggplot(aes(x=days_below_1,y=survival))+geom_smooth()

ggplot(alldata,aes(x=cohort,y=postK))+geom_smooth()
ggplot(alldata,aes(x=cohort,y=preK))+geom_smooth()
ggplot(alldata,aes(x=cohort,y=days_below_1))+geom_smooth()
alldata%>%
  mutate(survival=postCount/preCount)%>%
  ggplot(aes(x=cohort,y=survival))+geom_smooth()

alldata%>%
  filter(pulse==1)%>%
  mutate(survival=postCount/preCount)%>%
  ggplot(aes(x=cohort,y=survival))+geom_smooth()
alldata%>%
  filter(pulse==2)%>%
  mutate(survival=postCount/preCount)%>%
  ggplot(aes(x=cohort,y=survival))+geom_smooth()
alldata%>%
  filter(pulse==3)%>%
  mutate(survival=postCount/preCount)%>%
  ggplot(aes(x=cohort,y=survival))+geom_smooth()
alldata%>%
  filter(pulse==4)%>%
  mutate(survival=postCount)%>%
  ggplot(aes(x=cohort,y=survival))+geom_smooth()


# pulse 1-3
alldata%>%
  mutate(survival=postCount/preCount)%>%
  filter(pulse==1 | pulse ==2 | pulse ==3)%>%
  ggplot(aes(x=postK,y=survival,colour=pulse))+geom_point()
alldata%>%
  mutate(survival=postCount/preCount)%>%
  filter(pulse==1 | pulse ==2 | pulse ==3)%>%
  ggplot(aes(x=preK,y=survival,colour=pulse))+geom_point()
alldata%>%
  mutate(survival=postCount/preCount)%>%
  filter(pulse==1 | pulse ==2 | pulse ==3)%>%
  ggplot(aes(x=days_below_1,y=survival,colour=pulse))+geom_point()

pulse123<-alldata%>%
  filter(pulse==1 | pulse ==2 | pulse ==3)

m5<-glmer(cbind(postCount,preCount)~pulse+postK+preK+scale(days_below_1)+
        (1|cohort),data=pulse123,family=binomial)
plot(m5)
hist(resid(m5))
qqnorm(resid(m5))
qqline(resid(m5),col='red')
exp(logLik(m5))
summary(m5)
Anova(m5,type="III")
plot(allEffects(m5))

m6<-glmer(cbind(postCount,preCount)~postK+preK+scale(days_below_1)+
            (1|cohort),data=alldata,family=binomial)
plot(m6)
hist(resid(m6))
qqnorm(resid(m6))
qqline(resid(m6))
exp(logLik(m6))
summary(m6)
Anova(m6,type="III")
plot(allEffects(m6))

m7<-glmer(cbind(postCount,preCount)~pulse+preK+scale(days_below_1)+
            (1|cohort),data=alldata,family=binomial)

plot(m7)
fit<-fitted(m7)
res<-resid(m7)
plot(x=fit,y=res)
hist(resid(m7))
qqnorm(resid(m7))
qqline(resid(m7),col='red')
logLik(m7)
exp(logLik(m7))
summary(m7)
Anova(m7,type="III")
plot(allEffects(m7))


# ---- pulse 1 only -----
p1<-alldata%>%
  filter(pulse==1)

pm1<-glmer(cbind(postCount,preCount)~preK+scale(days_below_1)+(1|cohort),data=p1,family = binomial(link="logit"))
plot(pm1)
hist(resid(pm1))
qqnorm(resid(pm1))
qqline(resid(pm1))
logLik(pm1)
Anova(pm1,type="III")
plot(allEffects(pm1))

# ---- pulse 2 only -----
p2<-alldata%>%
  filter(pulse==2)

pm2<-glmer(cbind(postCount,preCount)~preK+postK+scale(days_below_1)+(1|cohort),data=p2,family = binomial(link="logit"))
plot(pm2)
hist(resid(pm2))
qqnorm(resid(pm2))
qqline(resid(pm2))
logLik(pm2)
Anova(pm2,type="III")
plot(allEffects(pm2))

# ---- pulse 3 only -----
p3<-alldata%>%
  filter(pulse==3)

pm3<-glmer(cbind(postCount,preCount)~preK+postK+scale(days_below_1)+(1|cohort),data=p3,family = binomial(link="logit"))
plot(pm3)
hist(resid(pm3))
qqnorm(resid(pm3))
qqline(resid(pm3))
logLik(pm3)
Anova(pm3,type="III")
plot(allEffects(pm3))

# ---- pulse 4 only -----
p4<-alldata%>%
  filter(pulse==4)

pm4<-glmer(preCount~postK+scale(days_below_1)+(1|cohort),data=p4,family = poisson(link="log"))
plot(pm4)
hist(resid(pm4))
qqnorm(resid(pm4))
qqline(resid(pm4))
logLik(pm4)
Anova(pm4,type="III")
plot(allEffects(pm4))

# ---- condition vs growth (length) ----
# look at change in size from October (10) to May (05)
summary(SLfull)
SL0<-SLfull%>%
  filter(species=="AC")%>%
  filter(age==0)%>%
  mutate(cohort=year)%>%
  filter(month==10)%>%
  filter(pulse<5)%>%
  filter(cohort>1998)%>%
  filter(cohort<2017)%>%
  group_by(cohort,pulse)%>%
  summarise(mmSLpre=mean(mmSL))
SL1<-SLfull%>%
  filter(species=="AC")%>%
  filter(age==1)%>%
  mutate(cohort=year-1)%>%
  filter(month==5)%>%
  filter(pulse<5)%>%
  filter(cohort>1998)%>%
  filter(cohort<2017)%>%
  group_by(cohort,pulse)%>%
  summarise(mmSLpost=mean(mmSL))


SL<-left_join(SL0,SL1)

part2<-left_join(alldata,SL)%>%
  mutate(dK=postK-preK)%>%
  mutate(dSL=mmSLpost-mmSLpre)

part2$pulse<-as.factor(part2$pulse)
m5<-glmer(cbind(postCount,preCount)~pulse+dK+dSL+scale(days_below_1)+(1|cohort),data=part2,family = binomial)
            
summary(m5)
plot(m5)
plot(allEffects(m5))
exp(logLik(m5))
exp(logLik(m4))

summary(m4)
estimates<-c(9.4750,0.5738,2.0603,5.5028,12.2070,-31.7715,1.3438)
odds<-exp(estimates)
prob<-odds/(1+odds)
prob
source<-c("Intercept","pulse2","pulse3","pulse4","preK","postK","scale(days_below_1)")
prob.coeff<-as.data.frame(cbind(source,odds,prob))


# summary on todays work: calculated odds and probability, attempted predictions - doesn't seem to work
# must look at additional info for visualizing ANCOVA model..... Split up into four pulses maybe??
