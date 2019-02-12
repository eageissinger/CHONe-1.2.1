# Survival and Pre- and post-winter condition
# Part 1: Data organization
# Purpose: Organize raw data for analysis
# files: condition-newman; newman-length; newman-catch; hauls; temperature; age1-pulse-range; trip dates
# and age1_dummypulse (for now)

# ----- set working directory -----
setwd("C:/Users/USER/Documents/Research/CHONe-1.2.1/")


# ---- load packages ----
library(MASS)
library(tidyverse)
library(lubridate)
library(pscl)
library(boot)
library(car)
library(betareg)
library(corrplot)
library(GLMMadaptive)
library(glmmTMB)

# ---- load data ----
condition<-read.csv("./data/data-working/condition-newman.csv")
range_final<-read.csv("./data/data-working/age1-pulse-range.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")
winter<-read.csv("./data/data-working/newman-winter-summary.csv")
catch_haul<-read.csv("./data/data-working/catch_haul.csv")
pulse_range0<-read.csv("./data/data-working/pulse_range0.csv")

# ----- condition data -----

# check data
dim(condition)
names(condition)

condition<-condition%>%
  select(-Date,-fulton.k) # take out blank column and fulton calc
str(condition) # get rid of empty rows
condition%>%
  filter(is.na(mmSL)) # confirm that all NA for mmSL are blank rows
condition<-condition%>%
  filter(!is.na(mmSL))

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
  mutate(age=replace(age,age=="0+/1+?",NA))
unique(condition$age)
typeof(condition$age)
condition$age<-as.integer(condition$age)
str(condition)

# fix date
condition$date<-ymd(paste(condition$year,condition$month,condition$day,sep="-"))
str(condition)
summary(condition)
summary(pulse_range0)
# update pulse assignments for age 0
pulse_range0$date<-ymd(paste(pulse_range0$year,pulse_range0$month,pulse_range0$day,sep="-"))

pulse_assign0<-data.frame(trip=rep(pulse_range0$trip,pulse_range0$max-pulse_range0$min+1),
                         date=rep(pulse_range0$date,pulse_range0$max-pulse_range0$min+1),
                         year=rep(pulse_range0$year,pulse_range0$max-pulse_range0$min+1),
                         pulse=rep(pulse_range0$pulse,pulse_range0$max-pulse_range0$min+1),
                         mmSL=unlist(mapply(seq,pulse_range0$min,pulse_range0$max)))
pulse_assign0<-pulse_assign0%>%
  mutate(cohort=year)

# update pulse assignments for age 1
str(range_final)
range_final<-range_final%>%select(-date)
range_final$date<-ymd(paste(range_final$year,range_final$month,range_final$day,sep="-"))

pulse_assign1<-data.frame(trip=rep(range_final$trip,range_final$max-range_final$min+1),
                         year=rep(range_final$year,range_final$max-range_final$min+1),
                         month=rep(range_final$month,range_final$max-range_final$min+1),
                         cohort=rep(range_final$cohort,range_final$max-range_final$min+1),
                         pulse=rep(range_final$dummy_pulse,range_final$max-range_final$min+1),
                         mmSL=unlist(mapply(seq,range_final$min,range_final$max)))
pulse_assign1<-pulse_assign1%>%
  mutate(year=cohort+1)%>%
  data.frame()

# assign pulse to age 0 condition
age0<-condition%>%
  filter(age==0)%>%
  select(-pulse)%>%
  mutate(cohort=year)
age0pulse<-left_join(age0,pulse_assign0)
# assign pulse to age 1 condition
age1<-condition%>%
  filter(age==1)%>%
  select(-pulse)%>%
  mutate(cohort=year-1)
age1pulse<-left_join(age1,pulse_assign1)
str(age0pulse)
str(age1pulse)
age0pulse$pulse<-as.character(age0pulse$pulse)
age1pulse$pulse<-as.character(age1pulse$pulse)
condition<-bind_rows(age0pulse,age1pulse)

# average condition by trip
cond0<-condition%>%
  filter(age == 0)%>%
  filter(month == 10 | month ==11)%>%
  mutate(K=100*(weight/((mmSL*.1)^3)))%>%
  group_by(year,cohort,age,trip,pulse,month)%>%
  summarise(fulton=mean(K),sd=sd(K))
cond1<-condition%>%
  filter(age==1)%>%
  filter(month == 5 | month == 7)%>%
  mutate(K=100*(weight/((mmSL*.1)^3)))%>%
  group_by(year,cohort,age,trip,pulse,month)%>%
  summarise(fulton=mean(K),sd=sd(K))
cond<-bind_rows(cond0,cond1)

# ---- Winter Data ----
dim(winter)
names(winter)
str(winter)
summary(winter)
head(winter)

# ----- Abundance data ----
dim(catch_haul)
names(catch_haul)
str(catch_haul)
summary(catch_haul)
head(catch_haul)

catch<-catch_haul%>%
  select(year,month,trip,age,total_catch,total_measured,extrap_1,extrap_2,
         extrap_3,extrap_4,extrap_5,extrap_6,extrap_unknown,total)%>%
  gather(key="extrap",value = "catch_haul",c(7:13),na.rm = FALSE)%>%
  mutate(total_measured=replace(total_measured,total_catch==0,0))%>%
  mutate(catch_haul=replace(catch_haul,total_catch==0,0))%>%
  mutate(pulse=str_sub(extrap,start=8,end = 8))%>%
  mutate(pulse=replace(pulse,pulse=="u",NA))%>%
  select(-extrap)%>%
  rename(count=catch_haul)%>%
  group_by(year,month,age,trip,pulse)%>%
  summarise(count=sum(count))%>%
  filter(!is.na(month))%>%
  filter(!is.na(pulse))
View(catch)
catch0<-catch%>%
  filter(age==0)%>%
  filter(month == 10 | month ==11)%>%
  mutate(cohort=year)
catch1<-catch%>%
  filter(age==1)%>%
  filter(month == 5 | month ==7)%>%
  mutate(cohort=year-1)
abundance<-bind_rows(catch0,catch1)


# ---- combine all data ----
nrow(cond)
nrow(winter)
nrow(abundance)

# determine pulse selection
# determine start year
summary(condition)
summary(cond) # start with 1999 cohort, end with 2016 (for now)
cond<-cond%>%
  filter(cohort<2017)
winter<-winter%>% filter(cohort>1998 & cohort < 2017)
abundance<-abundance%>%
  filter(cohort>1998 & cohort<2017)
nrow(cond)
nrow(winter)
nrow(abundance)
summary(abundance)

cond_all<-left_join(cond,abundance)
cond_all<-left_join(cond_all,winter)
names(cond_all)
head(cond_all)
str(cond_all)
#cond_all<-cond_all%>%
 # filter(!is.na(pulse))
# creat pre and post condition, and initial and final abundance
df<-cond_all%>%
  filter(month==10 |month==11 | month == 5 | month == 7)%>%
  mutate(season="fall")%>%
  mutate(season=replace(season,month==5,"spring"))%>%
  mutate(season=replace(season,month==7,"spring"))%>%
  group_by(cohort,pulse,season,days_below_1,mean_temp)%>%
  summarise(K=mean(fulton),abund=mean(count))%>%
  mutate(season2=season)%>%
  spread(key=season,value = K)%>%
  rename(preK=fall,postK=spring)%>%
  spread(key=season2,value=abund)%>%
  rename(preAbund=fall,postAbund=spring)%>%
  mutate(preCount=ceiling(preAbund),postCount=ceiling(postAbund))%>%
  ungroup()
names(df)
prewinter<-df%>%
  select(cohort,pulse,days_below_1,mean_temp,preK,preCount)%>%
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
  mutate(postCount=replace(postCount,is.na(postCount),0))%>%
  mutate(ratio=postCount/preCount)
names(alldata)
str(alldata)
summary(alldata)

ggplot(alldata,aes(x=cohort,y=ratio,colour=preK,size=days_below_1,shape=pulse))+
  geom_point()+theme_bw()+
  scale_colour_gradient(low = "red", 
                        high = "green",
                        name = "PreWinter Condition")+
  ylab("Abundance Ratio")+xlab("Year")


# ---- model ----
mydata<-as.data.frame(alldata)
mydata<-mydata%>%filter(pulse!=6)


# raw count
m0<-glm(postCount~preCount+preK+cohort+factor(pulse)+days_below_1,
             data=mydata,family=poisson(link = "log"))
m0
hist(resid(m0))
plot(m0)

m1<-glm.nb(postCount~preCount+preK+cohort+factor(pulse)+days_below_1,
        data=mydata,link="log")
m4
plot(m4)
res<-resid(m4)
fit<-fitted(m4)
plot(x=fit,y=res)
hist(resid(m4))
qqnorm(resid(m4))
summary(m4)
anova(m4)


#---- collinearity check ----
mydata$cohort<-as.numeric(mydata$cohort)
mydata$pulse<-as.numeric(mydata$pulse)
mydata<-mydata%>%select(-ratio)
mat1<-as.matrix(mydata)
cor1<-cor.mtest(mydata)
corrplot(cor(mydata),method="shade",shade.col=NA,tl.col="black", tl.srt=45)


# ---- zero inflated ----

ggplot(data=mydata,aes(x=cohort,y=postCount))+geom_point()
ggplot(data=mydata,aes(x=cohort,y=preCount))+geom_point()

m1<-zeroinfl(postCount~preK+factor(pulse)+days_below_1+cohort|
              preCount,data=mydata,dist = "poisson")
res1<-resid(m1)
fit1<-fitted(m1)
plot(x=fit1,y=res1)
hist(resid(m1))
qqnorm(resid(m1))     
summary(m1)
exp(logLik(m1))
df1<-as.data.frame(cbind(res1,fit1))
ggplot(df1,aes(x=fit1,y=res1))+geom_point()+
  xlab("Fitted Values")+ylab("Residuals")+
  geom_hline(yintercept = 0,colour='red',linetype='dashed')+
  theme_classic()
ggplot(df1,aes(x=lag(res1),y=res1))+geom_point()+
  xlab("Lagged Residuals")+ylab("Residuals")+
  theme_classic()
summary(m1)
Anova(m1,type="III")

# neg binomial with mean temp
m2<-zeroinfl(postCount~preK+factor(pulse)+mean_temp+cohort|
               preCount,data=mydata,dist = "negbin")
summary(m2)
Anova(m2,type="III")
res2<-resid(m2)
fit2<-fitted(m2)
df2<-as.data.frame(cbind(res2,fit2))

ggplot(df2,aes(x=fit2,y=res2))+geom_point()+
  xlab("Fitted Values")+ylab("Residuals")+
  geom_hline(yintercept = 0,colour='red',linetype='dashed')+
  theme_classic()
hist(resid(m2))
qqnorm(resid(m2))

ggplot(df2,aes(x=lag(res2),y=res2))+geom_point()+
  xlab("Lagged Residuals")+ylab("Residuals")+
  theme_classic()
exp(logLik(m1))
exp(logLik(m2))

# model 1 is better

# zero inflated mixed effect model
m3<-mixed_model(postCount~preCount+cohort,
                random=~1|cohort,data=mydata,
                family=zi.poisson(),zi_fixed = ~preCount)
res<-resid(m3)
fit<-fitted(m3)
plot(x=res,y=fit)

hist(resid(m3))

m4<-mixed_model(postCount~preCount+mean_temp+cohort,
                random=~1|cohort,data=mydata,
                family=zi.poisson(),zi_fixed = ~preCount)
res<-resid(m4)
fit<-fitted(m4)
plot(x=res,y=fitted)
hist(resid(m4))
qqnorm(resid(m4))

summary(m4)

m4<-mixed_model(postCount~preCount+mean_temp+factor(pulse)+cohort,
                random=~1|cohort,data=mydata,
                family=zi.poisson(),zi_fixed = ~preCount)


m.null<-update(m6,.~1)
anova(m6,m.null)
logLik(m.null)


restotal<-mydata$postCount-mean(mydata$postCount)
SStotal<-sum(restotal^2)
SStotal

SSresid

LR<-(SSresid/SStotal)^(-57/2)
LR
exp(logLik(m6))

m0<-update(m6,.~1)
pchisq(2*(logLik(m6)-logLik(m0)),df=5,lower.tail=FALSE)

# ---- Beta Regression -----
# condition~temperature+year+age+pulse
View(condition)
summary(cond)
summary(cond_all)
#add temperature data
temp<-read.csv("./data/data-working/daily-temp-corrected-newman.csv")


temp<-temp%>%
  group_by(year,month)%>%
  summarise(mean_temp=mean(daily_temp_C))

full<-left_join(cond_all,temp)

# Beta regression model
min(full$fulton)
max(full$fulton)
summary(full)
check<-full[full$fulton>1,]
# take out everything that is greater than 1.10
full<-full%>%
  filter(fulton<1)
summary(full)
m1<-betareg(fulton~year*age*mean_temp,data = full)
plot(m1)
hist(resid(m1))
qqnorm(resid(m1))

m2<-lm(fulton~year*age*mean_temp,data=full)
plot(m2)

exp(logLik(m1))
exp(logLik(m2))
