# Survival and Pre- and post-winter condition
# Part 2: Analysis

# ---- working directory -----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/condition-field/")
# ---- Packages -----
#library(pscl)
library(car)
#library(boot)
#library(corrplot)
#library(glmmTMB)
library(lme4)
#library(mgcv)
library(effects)
#library(lmtest)
#library(magclass)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(MASS)
#library(lubridate)
library(tidyverse)
library(ggpubr)

# ---- data ----
#data<-read.csv("../data/output/condition-field-formatted.csv")
SLfull<-read.csv("../data/data-working/newman-length-updated.csv")
condition<-read.csv("../data/output/condition-field-clean.csv")
data<-read.csv("../data/output/condition-field-formatted_outlier.csv")

head(data)
str(data)

# ---- Graphical model ----
alldata<-data%>%filter(pulse<5)
alldata$pulse<-as.factor(alldata$pulse)
alldata$pulse<-droplevels(alldata$pulse)
# Survival vs. preK for all pulses
ggplot(alldata)+geom_point(aes(y=postCount/preCount,x=preK))+
  facet_grid(~pulse)+
  ylim(0,17)
# Survival vs. postK for all pulses
ggplot(alldata)+geom_point(aes(y=postCount/preCount,x=postK))+
  facet_grid(~pulse)+
  ylim(0,15)
# Survival vs. degree days for all pulses
ggplot(alldata)+geom_point(aes(y=postCount/preCount,x=days_below_1))+
  facet_grid(~pulse)+
  ylim(0,17)
# Survival vs. year for all pulses
ggplot(alldata)+geom_point(aes(y=postCount/preCount,x=year))+
  facet_grid(~pulse)+
  ylim(0,17)
# ---- model attempts ----
# October and July model
summary(alldata)
str(alldata)
alldata<-alldata%>%
  mutate(pulse.f=as.factor(pulse))

# GLM: Bionomial
m0<-glm(cbind(postCount,preCount)~pulse.f+preK+postK+days_below_1+cohort,
        data=alldata,family=binomial(link = "logit"))
plot(m0)
hist(resid(m0))
fit<-fitted(m0)
res=resid(m0)
plot(x=fit,y=res)
summary(m0)
Anova(m0,type="III")
47.323/12 #RD


plot(allEffects(m0))

plot_model(m0,show.intercept = TRUE)
plot_model(m0)



m1<-glm(cbind(postCount,preCount)~pulse.f+preK+postK+days_below_1+days_below_1+year,
    data=alldata,family=binomial)
plot(m1)
hist(resid(m1))
summary(m1)
49.045/13

# GLM Poisson
m2<-glm(postCount~preCount+pulse.f+preK+postK+days_below_1+mean_temp,
        data=alldata,family=poisson)
plot(m2)
hist(resid(m2))
summary(m2)
44.041/11
Anova(m2,type="III")


m3<-glm.nb(postCount~preCount+pulse.f+preK+postK+days_below_1+mean_temp,
           data=alldata)
plot(m3)
hist(resid(m3))
summary(m3)
19.958/11
Anova(m3,type="III")


# ---- GLMM binomial -----


m4<-glmer(cbind(postCount,preCount)~pulse+preK+postK+scale(days_below_1)+
            (1|cohort),data=alldata,family=binomial)
#convergence warning
#model.fit.all<-lme4::allFit(m4)
#ss<-summary(model.fit.all)
#try different optimizers
#m4<-glmer(cbind(postCount,preCount)~pulse+preK+postK+scale(days_below_1)+
#              (1|cohort),data=alldata,family=binomial,control = glmerControl(optimizer = "nlminbwrap"))

plot(m4)
fit<-fitted(m4)
res<-resid(m4)
plot(x=fit,y=res)
hist(resid(m4))
qqnorm(resid(m4))
qqline(resid(m4),col='red')
summary(m4)

Anova(m4,type="III")
A<-Anova(m4,type="III")
A%>%
  mutate(LR=exp(Chisq/2))
plot(allEffects(m4))

# ---- Model with Settlement day -----

m5<-glmer(cbind(postCount,preCount)~pulse+preK+postK+scale(days_below_1)+scale(settle.day)+
            (1|cohort),data=alldata,family=binomial,control = glmerControl(optimizer = "nlminbwrap"))
plot(m5)
hist(resid(m5))
qqnorm(resid(m5))
qqline(resid(m5))
summary(m5)
Anova(m5,type="III")
m6<- glmer(cbind(postCount,preCount)~preK+postK+scale(days_below_1)+scale(settle.day)+
             (1|cohort),data=alldata,family=binomial)
plot(m6)
hist(resid(m6))
qqnorm(resid(m6))
qqline(resid(m6))
summary(m6)
Anova(m6,type="III")

# ----- Model 4 ------
ggplot(data=alldata,aes(x=cohort,y=postCount/preCount))+geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~pulse)

ggplot(alldata,aes(x=cohort,y=preCount))+geom_point()+
  geom_smooth(method="glm")+
  facet_wrap(~pulse)
ggplot(alldata,aes(x=cohort,y=postCount))+geom_point()+
  geom_smooth(method="glm")+
  facet_wrap(~pulse)
ggplot(alldata,aes(x=preK,y=postCount))+geom_point()+
  geom_smooth(method="glm")+
  facet_wrap(~pulse)
ggplot(alldata,aes(x=postK,y=postCount))+geom_point()+
  geom_smooth(method="glm")+
  facet_wrap(~pulse)
ggplot(alldata,aes(x=days_below_1,y=postCount))+geom_point()+
  geom_smooth(method="glm")+
  facet_wrap(~pulse)
ggplot(alldata,aes(x=pulse,y=postCount/preCount))+geom_point()+
  geom_boxplot(outlier.shape = NA)

plot_model(m4)
plot_model(m4,show.intercept=TRUE,order.terms = c(1,2,3,4,5,6))
plot_model(m4,type="std")
plot_model(m4,show.intercept = TRUE)
plot_model(m4,transform = "plogis",show.intercept = TRUE)

plot_model(
  m4, 
  type="std",
  title = "Survival",
  colors = "black", 
  show.values = TRUE,
  value.offset = .4,
  value.size = 4,
  dot.size = 3,
  line.size = 1.5,
  vline.color = "#a8ddb5"
)


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

output.m4$predicted<-predict(m4.1,output.m4,allow.new.levels=TRUE,type='response')
# issues with predicting: issue incorporating factor
## also, i have randomly assigned all values randomly to pulses, however, the range for each pulse should
## likely remain the same for all sample data..... Don't know where to go from here

ggplot(alldata,aes(x=postK,y=postCount/preCount,colour=pulse))+geom_point()+
  geom_smooth(method="glm",alpha=0.15,aes(fill=pulse))
ggplot(alldata,aes(x=preK,y=postCount/preCount,colour=pulse))+geom_point()+
  geom_smooth(method='glm',alpha=0.15,aes(fill=pulse))


str(p0<-predict(m4.1))
str(p1<-predict(m4.1,re.form=NA))
newdata<-with(alldata,expand.grid(pulse=unique(pulse),postK=unique(postK),preK=unique(preK),
                                  days_below_1=unique(days_below_1),cohort=unique(cohort)))
str(p2<-predict(m4.1,newdata,allow.new.levels=TRUE))
str(p3<-predict(m4.1,newdata,re.form=NA,allow.new.levels=TRUE))
str(p4<-predict(m4.1,newdata,re.form= ~(1|cohort),allow.new.levels=TRUE))
stopifnot(identical(p2,p4))
summary(m4.1)

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
  ylim(0,1)

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
pulse123<-alldata%>%
  filter(pulse==1 | pulse ==2 | pulse ==3)

m5<-glmer(cbind(postCount,preCount)~pulse+postK+preK+scale(days_below_1)+
        (1|cohort),data=pulse123,family=binomial)
plot(m5)
hist(resid(m5))
qqnorm(resid(m5))
qqline(resid(m5),col='red')
summary(m5)
Anova(m5,type="III")
plot(allEffects(m5))

#no pulse
m6<-glmer(cbind(postCount,preCount)~postK+preK+scale(days_below_1)+
            (1|cohort),data=alldata,family=binomial)
plot(m6)
hist(resid(m6))
qqnorm(resid(m6))
qqline(resid(m6))
summary(m6)
Anova(m6,type="III")
plot(allEffects(m6))

# no post K
m7<-glmer(cbind(postCount,preCount)~pulse+preK+scale(days_below_1)+
            (1|cohort),data=alldata,family=binomial)

plot(m7)
fit<-fitted(m7)
res<-resid(m7)
plot(x=fit,y=res)
hist(resid(m7))
qqnorm(resid(m7))
qqline(resid(m7),col='red')
summary(m7)
Anova(m7,type="III")
plot(allEffects(m7))

#stick with model 4 (full)
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


# ---- Odds -----
summary(m4)
estimates<-c(11.6029,1.6686,2.9090,5.5253,15.8653,-39.6424,1.6552)
std.error<-c(11.0987,1.8674,1.8860,2.2990,6.2025,14.6772,0.7468)
odds<-exp(estimates)
odds.error<-exp(std.error)
#prob<-odds/(1+odds)
source<-c("Intercept","pulse2","pulse3","pulse4","preK","postK","scale(days_below_1)")
effect.size<-as.data.frame(cbind(source,odds,odds.error))
effect.size

# summary: attempted predictions - doesn't seem to work
# must look at additional info for visualizing ANCOVA model..... Split up into four pulses maybe??

# ---- what is the evidence -----
null<-glmer(cbind(postCount,preCount)~1+
              (1|cohort),data=alldata,family=binomial)
deviance(null)
deviance(m4)
deviance(null)-deviance(m4)
exp(259.1085/2)
Anova(m4,type="III")

# ---- goodness of fit measure-----
## ?????


# post analysis
# evaluate trends in high vs low abundance years
abund<-alldata%>%
  group_by(cohort)%>%
  summarise(preAbund=sum(preCount),postAbund=sum(postCount))%>%
  mutate(surv=postAbund/preAbund)%>%
  filter(surv!='Inf')
summary(abund)
# high abundance years
# 1999
# 2014
# 2013
# 2012
# 2015

high<-alldata%>%
  filter(year == 2013 | year == 2014 | year == 1999 | year == 2012 | year == 2015)
summary(high)
summary(alldata)

# high survival years
high.surv<-alldata%>%
  filter(cohort==2008 | cohort == 2001 | cohort == 2006 | cohort == 2009 | cohort== 2016 | cohort == 2000)

summary(high.surv)

# low survival years
low.surv<-alldata%>%
  filter(cohort!=2008 | cohort != 2001 | cohort != 2006 | cohort != 2009 | cohort!= 2016 | cohort != 2000)
summary(low.surv)
plot(allEffects(m4))


# --- pre analysis figures -----
# number of pulses per year

ggplot(data)+geom_(aes(y=pulse,x=year))
SLfull%>%
  filter(age==1)%>%
  ggplot()+geom_point(aes(y=pulse,x=year))
# winter temperature ranges and degree days
# condition pre/post by pulse

# tie it all together!
SLfull%>%
  filter(age==0 & pulse == 6)%>%
  distinct(year)

# pre and post K
summary(condition)
precond<-condition%>%
  filter(month==10 & age==0)%>%
  filter(fulton<1.76)
postcond<-condition%>%
  filter(month==5 & age ==1)
summary(precond)
summary(postcond)
