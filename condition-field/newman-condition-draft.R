# Survival and Pre- and post-winter condition
# Part 2: Analysis

# ---- working directory -----
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/condition-field/")

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
library(broom.mixed)

# ---- data ----
#data<-read.csv("../data/output/condition-field-formatted.csv")
SLfull<-read.csv("../data/data-working/newman-length.csv")
condition<-read.csv("../data/output/condition-field-clean.csv")
data<-read.csv("../data/output/condition-field-formatted_outlier.csv")
settle<-read.csv("../data/output/settlement-day.csv")
temp<-read.csv("../data/data-working/newman-temp-to-2017.csv")
#mort<-read.csv("../data/output/mortalities.csv") # need to redo mortality calculations

#check data
head(data)
str(data)
summary(data)

# ---- Graphical model ----

alldata<-data%>%filter(pulse<5) # remove pulse 5 and 6 from working data
alldata$pulse<-as.factor(alldata$pulse) # convert pulse to factor
alldata$pulse<-droplevels(alldata$pulse) # remove 5 and 6 from factor levels
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
ggplot(alldata)+geom_point(aes(y=postCount/preCount,x=cohort))+
  facet_grid(~pulse)+
  ylim(0,17)

#calculate overwinter mortality rate
# 275 days
winter.mort.pulse<-alldata%>%
  mutate(mortality=(log(postCount/preCount)/-275)*100)%>%
  mutate(mortality=replace(mortality,preCount==0 |postCount==0,NA))
winter.mort.full<-alldata%>%
  group_by(cohort,days_below_1,mean_temp)%>%
  summarise(preCount=sum(preCount),postCount=sum(postCount))%>%
  mutate(mortality=(log(postCount/preCount)/-275)*100)%>%
  mutate(mortality=replace(mortality,preCount==0 |postCount==0,NA))

mean(winter.mort.pulse$mortality,na.rm=TRUE)
winter.mort.pulse%>%
  filter(pulse!="4")%>%
  ggplot()+
  geom_hline(yintercept = 0.3134413,linetype='dashed')+
  geom_point(aes(x=cohort,y=mortality))+
  facet_wrap(~pulse)


mean(winter.mort.full$mortality,na.rm=TRUE)
ggplot(data=winter.mort.full)+
  geom_hline(yintercept = 0.4260159,linetype='dashed')+
  geom_point(aes(x=cohort,y=mortality))

#winter mortality and temp
winter.m0<-lm(mortality~mean_temp+cohort,data=winter.mort.full)
plot(winter.m0)
qqnorm(resid(winter.m0))
qqline(resid(winter.m0),col='red')
hist(resid(winter.m0))
summary(winter.m0)
Anova(winter.m0,type="III")


# Figure 2
ggplot(data=winter.mort.full)+
  geom_hline(yintercept = 0.4260159,linetype='dashed')+
  geom_point(aes(x=cohort,y=mortality),size=2)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab('Mortality Rate % ??? ' ~d^-1)+
  xlab('Cohort')+
  scale_x_continuous(breaks = seq(1998,2016,by=2))


# ---- model attempts ----
# October and July model
summary(alldata)
glimpse(alldata)

# GLM: Bionomial, everything except pulse 4
m0<-glm(cbind(postCount,preCount)~pulse+preK+days_below_1+cohort,
        data=filter(alldata,pulse!="4"),family=binomial(link = "logit"))
plot(m0)
hist(resid(m0))
fit<-fitted(m0)
res=resid(m0)
plot(x=fit,y=res)
qqnorm(resid(m0))
qqline(resid(m0),col='red')
summary(m0)
Anova(m0,type="III")
A<-Anova(m0,type="III")
A%>%rename(Chisq='LR Chisq')%>%
  mutate(LR=exp(Chisq/2))

plot(allEffects(m0))

plot_model(m0,show.intercept = TRUE)
plot_model(m0)


# exclude pulse 4 and temperature
m0.1<-glmer(cbind(postCount,preCount)~pulse+preK+postK+(1|cohort),
            data=filter(alldata,pulse!="4"),family=binomial)
plot(m0.1)
qqnorm(resid(m0.1))
qqline(resid(m0.1),col='red')
hist(resid(m0.1))
summary(m0.1)
Anova(m0.1,type="III")

plot(allEffects(m0.1))
# model m0.1 not as good

# everything (all pulses, all variables)
m1<-glmer(cbind(postCount,preCount)~pulse+preK+postK+scale(days_below_1)+(1|cohort),
          data=alldata,family=binomial)
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1),col='red')
hist(resid(m1))
summary(m1)
Anova(m1,type="III")
plot(allEffects(m1))

# everything except for pulse 4
m2<-glmer(cbind(postCount,preCount)~pulse+preK+postK+scale(days_below_1)+(1|cohort),
          data=filter(alldata,pulse!="4"),family=binomial)
plot(m2)
qqnorm(resid(m2))
qqline(resid(m2),col='red')
hist(resid(m2))
Anova(m2,type="III")
summary(m2)
plot(allEffects(m2))

# ----- Model 1 full model ------
ggplot(data=alldata,aes(x=cohort,y=postCount/preCount))+geom_point()+
  geom_smooth()+
  facet_wrap(~pulse)

ggplot(data=alldata,aes(x=pulse,y=postCount/preCount))+
  geom_boxplot(outlier.shape = NA)+ylim(0,1)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab('Abundance Ratio')

ggplot(winter.mort.pulse)+
  geom_boxplot(aes(x=pulse,y=mortality))



plot_model(
  m1, 
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

# ---- Odds -----
model.df1 <- tidy(m1)%>%
  filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation
model.df1

e1<-model.df1 %>% 
  mutate(odds = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(m1)),  # Variance of each coefficient
         odds.se = sqrt(odds^2 * var.diag))%>%  # Odds-ratio adjusted
  mutate(p = odds/(1+odds))
e1

# variance explained
am1<-Anova(m1,type="III")


model.df2 <- tidy(m2)%>%
  filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation
model.df2

e2<-model.df2 %>% 
  mutate(odds = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(m2)),  # Variance of each coefficient
         odds.se = sqrt(odds^2 * var.diag))  # Odds-ratio adjusted
e2


# summary: attempted predictions - doesn't seem to work
# must look at additional info for visualizing ANCOVA model..... Split up into four pulses maybe??

# ---- what is the evidence -----
null<-glmer(cbind(postCount,preCount)~1+
              (1|cohort),data=alldata,family=binomial)
deviance(null)
deviance(m1)
deviance(null)-deviance(m1)
exp(98.34164/2)
Anova(m1,type="III")
# strong evidence for the model


deviance(null)-deviance(m2)
exp(195.4787/2)
Anova(m2,type="III")


# ---- prewinter and survival-----

# ----- prewinter condition -----
head(settle)
head(condition)
settle<-settle%>%
  rename(cohort=Year,pulse=Pulse)
cond2<-left_join(condition,settle,by=c("cohort","pulse"))%>%
  left_join(temp)

# plot age 0 cod in october
condition%>%
  filter(age==0 & month==10 & fulton<1.6)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton,fill=factor(pulse)))

# look at the effect of pre-winter condition on abundance ratio
Km1<-glmer(fulton~scale(settle.week)+(1|cohort),
           data=filter(cond2,age==0 & month==10 & fulton<1.6 & pulse<5),
           family=Gamma(link="log"))
plot(Km1)
hist(resid(Km1))
Anova(Km1,type="III")
summary(Km1)
# no effect

# effect of pulse on condition
Km2<-glmer(fulton~factor(pulse)+(1|cohort),
           data=filter(condition,age==0 & month==10 & fulton<1.6 & pulse<5),
           family=Gamma(link="log"))
plot(Km2)
hist(resid(Km2))
Anova(Km2,type="III")
summary(Km2)
# no effect


# effect of pulse and settelement week on condition
Km3<-glmer(fulton~factor(pulse)+scale(settle.week)+(1|cohort),
           data=filter(cond2,age==0 & month==10 & fulton<1.6 & pulse<5),
           family=Gamma(link="log"))
plot(Km3)
hist(resid(Km3))
Anova(Km3,type="III")
summary(Km3)
# pulse and settle time have no effect on pre-winter K


# temperature effect and pulse
Km4<-glmer(fulton~pulse+scale(daily_temp_C)+(1|cohort),
           data=filter(cond2,age==0&month==10&fulton<1.6&pulse<5),
           family=Gamma(link = "log"))
plot(Km4)
hist(resid(Km4))
Anova(Km4,type="III")
# temperature potential effect


# look at pre and post winter pulse during years where pulses are present before and after winter

# for preK and postK look at:
# 2001 pulse 2 and 3
# 2007 both pulses (pulse 2 and 3?)
# 2009 pulse 2 and 3
# 2011 pulse 2, 3, and 4
# 2012 pulse 2 and 3
# 2013 pulse 2 and 3
# 2015 pulse 2 and 3
ggplot(alldata)+
  geom_boxplot(aes(x=factor(cohort),y=preK,fill=pulse))

K1<-cond2%>%
  filter(pulse==2 | pulse ==3)%>%
  filter(cohort==2001 | cohort == 2007 | cohort == 2009 | cohort == 2011 | cohort == 2012 |
           cohort == 2013 | cohort == 2015)%>%
  filter(month==10)%>%
  mutate(season="fall")%>%
  dplyr::select(fulton,cohort,pulse,season)
K2<-cond2%>%
  filter(pulse==2 | pulse ==3)%>%
  filter(cohort==2001 | cohort == 2007 | cohort == 2009 | cohort == 2011 | cohort == 2012 |
           cohort == 2013 | cohort == 2015)%>%
  filter(month==5)%>%
  mutate(season="spring")%>%
  dplyr::select(fulton,cohort,pulse,season)

K<-bind_rows(K1,K2)%>%
  left_join(dplyr::select(alldata, cohort, days_below_1),by=c("cohort"))%>%
  distinct()%>%
  filter(fulton<1.6)%>%
  mutate(pulse=as.factor(pulse))



Km5<-lmer(fulton~season+factor(pulse)+scale(days_below_1)+(1|cohort),data=K)

plot(Km5)
qqnorm(resid(Km5))
qqline(resid(Km5),col='red')
hist(resid(Km5))
Anova(Km5,type="III")
summary(Km5)


ggplot(K)+
  geom_boxplot(aes(x=factor(cohort),y=fulton,fill=season))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Fulton's K")+
  xlab("Cohort")+
  ylim(0.25,1.2)

# prewinter K only
Km6<-lmer(fulton~pulse+scale(days_below_1)+(1|cohort),data=filter(K,season=="fall"))

plot(Km6)
qqnorm(resid(Km6))
qqline(resid(Km6),col='red')
hist(resid(Km6))
Anova(Km6,type="III")
summary(Km6)
plot(allEffects(Km6))

# no effect

# postwinter K
Km7<-lmer(fulton~pulse+scale(days_below_1)+(1|cohort),data=filter(K,season=="spring"))

plot(Km7)
qqnorm(resid(Km7))
qqline(resid(Km7),col='red')
hist(resid(Km7))
Anova(Km7,type="III")
summary(Km7)
plot(allEffects(Km7))

# large amount of annual variation

# ---- length/weight and settlement ----
head(SLfull)
SLcod<-SLfull%>%
  filter(Species=="AC" & Age == 0 & Month>8)
head(SLcod)

SLcod%>%
  filter(Month==9)%>%
  ggplot()+
  geom_histogram(aes(x=mmSL,fill=factor(Pulse)))+
  facet_wrap(~Year)
SLcod%>%
  filter(Month==10)%>%
  ggplot()+
  geom_histogram(aes(x=mmSL,fill=factor(Pulse)))+
  facet_wrap(~Year)
SLcod%>%
  filter(Month==11)%>%
  ggplot()+
  geom_histogram(aes(x=mmSL,fill=factor(Pulse)))+
  facet_wrap(~Year)


head(condition)
summary(condition)

condition%>%
  filter(fulton<1.5)%>%
  filter(month==9)%>%
  ggplot()+
  geom_jitter(aes(x=log(mmSL),y=log(weight),colour=factor(pulse)))
condition%>%
  filter(fulton<1.5)%>%
  filter(month==10)%>%
  ggplot()+
  geom_jitter(aes(x=log(mmSL),y=log(weight),colour=factor(pulse)))
condition%>%
  filter(fulton<1.5)%>%
  filter(month==11)%>%
  ggplot()+
  geom_jitter(aes(x=log(mmSL),y=log(weight),colour=factor(pulse)))


# settlement day and growth


pre.m4<-lmer(log(weight)~log(mmSL)+settle.yday+month+(1|cohort),
             data=filter(cond2,age==0 & pulse <5 & fulton<1.5))
plot(pre.m4)
Anova(pre.m4,type="III")
summary(pre.m4)


# does it vary by year?
# is that reflected in condition?

pre.m5<-lmer(log(weight)~log(mmSL)+factor(pulse)+(1|cohort),
             data=filter(cond2,month==10 & age == 0 &pulse<5 & fulton<1.5))
plot(pre.m5)
Anova(pre.m5)
summary(pre.m5)
pre.m6<-lmer(log(weight)~log(mmSL)+settle.week+(1|cohort),
             data=filter(cond2,month==10 & age == 0 &pulse<5 & fulton<1.5))
plot(pre.m6)
Anova(pre.m6)
summary(pre.m6)
pre.m7<-lmer(log(weight)~log(mmSL)+settle.week+(1|cohort),
             data=filter(cond2,month==11 & age == 0 &pulse<5 & fulton<1.5 &fulton>0.1))
plot(pre.m7)
Anova(pre.m7)
summary(pre.m7)

pre.m8<-lmer(log(weight)~log(mmSL)+factor(pulse)+(1|cohort),
             data=filter(cond2,month==11 & age == 0 &pulse<5 & fulton<1.5 &fulton>0.1))
plot(pre.m8)
Anova(pre.m8)
summary(pre.m8)
# Settlement day does not influence weight/length relationship until Nov
# is pulse 4 more prevalent in November?
table1<-condition%>%
  filter(age==0)%>%
  filter(!is.na(pulse))%>%
  group_by(month,pulse)%>%
  summarise(n=n())
# pulse 3 and 4 more abundant in November
# related to late settlement?
# The change in weight/length relationship in November related to late pulses
# or is it related to temperature changes?

### does temp leading up to winter influence weight/length relationship?
pre.m9<-lmer(log(weight)~log(mmSL)+scale(settle.week)+scale(daily_temp_C)+(1|cohort),
             data=filter(cond2,month==9 & age == 0 &pulse<5 & fulton<1.5 &fulton>0.1))
plot(pre.m9)
Anova(pre.m9)
summary(pre.m9)
pre.m10<-lmer(log(weight)~log(mmSL)+scale(settle.week)+scale(daily_temp_C)+(1|cohort),
              data=filter(cond2,month==10 & age == 0 &pulse<5 & fulton<1.5 &fulton>0.1))
plot(pre.m10)
Anova(pre.m10)
summary(pre.m10)
pre.m11<-lmer(log(weight)~log(mmSL)+(1|cohort),
              data=filter(cond2,month==10 & age == 0 &pulse<5 & fulton<1.5))
plot(pre.m11)
Anova(pre.m11)
summary(pre.m11)

#pre.m5, pre.m8
pred1<-predict(pre.m11)


ggplot(data=cbind(filter(cond2,month==10 & age == 0 & pulse <5 & fulton <1.5),
                  pred1),
       aes(x=log(mmSL),y=log(weight),color=factor(pulse)))+geom_point(alpha=0.7)+
  geom_smooth(aes(y=pred1),method="lm",colour='black')+
  theme_bw()+
  annotate("text",x=4.3,y=-2.8,label="y = -11.71 + 2.966?SL")+
  xlab("log(SL)")+
  ylab("log(weight)")+
  scale_fill_discrete(name="Pulse")


# ---- change in condition ----
dK<-alldata%>%
  mutate(deltaK=postK-preK)

ggplot(dK)+
  geom_point(aes(x=preK,y=deltaK,colour=pulse))+
  geom_smooth(aes(x=preK,y=deltaK), method='lm')+
  theme_bw()


ggplot(dK)+
  geom_point(aes(x=cohort,y=deltaK,colour=factor(pulse)))+
  geom_hline(yintercept = 0)

# ---- interaction between pre and postK `----

a<-condition%>%
  filter(fulton<1.5)%>%
  filter(month==10)%>%
  filter(pulse<5)%>%
  filter(cohort>2000)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton,fill=factor(pulse)))+
  theme_bw()+
  xlab("Cohort")+
  ylab("Fulton's K")+
  scale_fill_discrete(name="Pulse")+
  theme(axis.text.x = element_text(angle=45))
condition%>%
  filter(fulton<1.5)%>%
  filter(month==11)%>%
  filter(pulse<5)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton,fill=factor(pulse)))+
  theme(axis.text.x = element_text(angle=45))
b<-condition%>%
  filter(fulton<1.5)%>%
  filter(month==5)%>%
  filter(pulse<5)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton,fill=factor(pulse)))+
  theme_bw()+
  xlab("Cohort")+
  ylab("Fulton's K")+
  scale_fill_discrete(name="Pulse")+
  theme(axis.text.x = element_text(angle=45))
ggarrange(a,b,nrow = 2,ncol=1,common.legend = TRUE)



m.K<-glm(cbind(postCount,preCount)~preK+postK+days_below_1,
         data=alldata,family=binomial)
plot(m.K)
hist(resid(m.K))
Anova(m.K)
plot(allEffects(m.K))

m.K1<-glmer(postK~preK+(1|cohort),
            data=alldata,family=Gamma(link = "log"))
plot(m.K1)
hist(resid(m.K1))
Anova(m.K1,type="III")
summary(m.K1)

ggplot(alldata)+
  geom_point(aes(x=preK,y=postK,colour=factor(cohort)))

m.K2<-glmer(postK~preK+factor(pulse)+scale(days_below_1)+(1|cohort),
            data=alldata,family=Gamma(link = "log"))
plot(m.K2)
hist(resid(m.K2))
Anova(m.K2)

m.K3<-glmer(postK~preK+scale(days_below_1)+(1|cohort),
            data=alldata,family=Gamma(link = "log"))
plot(m.K3)
hist(resid(m.K3))
Anova(m.K3)

m.K4<-glmer(postK~preK+factor(pulse)+(1|cohort),
            data=alldata,family=Gamma(link = "log"))
plot(m.K4)
hist(resid(m.K4))
Anova(m.K4)


m.K5<-lmer(deltaK~factor(pulse)+scale(days_below_1)+(1|cohort),
           data=dK)
plot(m.K5)
hist(resid(m.K5))
Anova(m.K5,type="III")

#found relationship between pre and post condition. and it does NOT depend
# on pulse structure or degree days
# something else is influencing the change in condition
# WINTER RESOURCES

# ---- post K -----

m.K3<-glm(postK~days_below_1+cohort,
          data=alldata,family=Gamma(link = "log"))
plot(m.K3)
Anova(m.K3)
summary(m.K3)
m.K4<-glm(postK~days_below_1+factor(pulse),
          data=alldata,family=Gamma(link = "log"))
plot(m.K4)
Anova(m.K4)
summary(m.K4)

# looking at postK alone (without preK), degree days do influence postK
# what does this mean, exactly
# leads back to winter resources?

# ---- The closer look ----
# look at years with pulses 2, 3, and 4
# look at contrasting abundance years

# pulse 2, 3, and 4

late.pulse<-alldata%>%
  filter(pulse!=1)%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2,pulse==4 & preCount==0 & postCount!=0,1))%>% # convert no measurements for pulse 4 to 1 for calcuation purposes
  mutate(preCount2=replace(preCount2,pulse==3 & preCount==0 & postCount!=0,1))
late.pulse$pulse<-droplevels(late.pulse$pulse) # remove 5 and 6 from factor levels
glimpse(late.pulse)
summary(late.pulse)
late.pulse%>%distinct(cohort)

m3<-glmer(cbind(postCount,preCount)~pulse+preK+postK+scale(days_below_1)+(1|cohort),
          data=late.pulse,family=binomial)
plot(m3)
hist(resid(m3))
qqnorm(resid(m3))
qqline(resid(m3),col='red')

Anova(m3,type="III")
plot(allEffects(m3))

ggplot(data=late.pulse)+
  geom_boxplot(aes(y=postCount/preCount,x=pulse),outlier.shape = NA)+
  ylim(0,1)
ggplot(data=late.pulse)+
  geom_boxplot(aes(y=preCount,x=pulse),outlier.shape = NA)+
  ylim(0,30)
ggplot(data=late.pulse)+
  geom_boxplot(aes(y=postCount,x=pulse),outlier.shape=NA)

ggplot(data=late.pulse)+
  geom_point(aes(x=preK,y=postK,colour=pulse))

ggplot(data=late.pulse)+
  geom_point(aes(y=postCount/preCount2,x=preK,colour=pulse))
ggplot(data=late.pulse)+
  geom_point(aes(y=postCount/preCount2,x=postK,colour=pulse))
ggplot(data=late.pulse)+
  geom_point(aes(y=postCount/preCount2,x=days_below_1,colour=pulse))+
  ylim(0,1)

# --- The Closer Look ----
# determine years to look at

# years with pulse 2, 3, and 4
alldata%>%
  filter(pulse!="1")

m3<-glmer(cbind(postCount,preCount)~pulse+postK+scale(days_below_1)+(1|cohort),
          data=filter(alldata,pulse!="1"),family=binomial)
plot(m3)
qqnorm(resid(m3))
qqline(resid(m3),col='red')
hist(resid(m3))
Anova(m3,type="III")
summary(m3)
plot(allEffects(m3))

model.df3 <- tidy(m3)%>%
  filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation

e3<-model.df3 %>% 
  mutate(odds = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(m3)),  # Variance of each coefficient
         odds.se = sqrt(odds^2 * var.diag))%>%  # Odds-ratio adjusted
  mutate(p = odds/(1+odds))
e3

#select years above the mean age-0 abundance level
# 1998, 1999, 2007, 2012, 2013, 2014, 2015, 2016, 2017

high.year<-alldata%>%
  filter(cohort== 1998 | cohort == 1999 | cohort ==2007 | cohort >= 2013 & cohort & cohort <=2017)


# parse out smaller components

ggplot(high.year)+
  geom_point(aes(y=postCount/preCount, x=preK))

ggplot(high.year)+
  geom_point(aes(y=postCount/preCount, x = postK))

ggplot(high.year)+
  geom_point(aes(y=postCount/preCount, x = days_below_1))

ggplot(year.set)+
  geom_boxplot(aes(y=postCount/preCount, x = pulse))

# pulse 4 less present in these high abundance years

m4<-glmer(cbind(postCount,preCount)~pulse+(1|cohort),
          data=filter(high.year,pulse!="4"),family=binomial)
plot(m4)
qqnorm(resid(m4))
qqline(resid(m4),col='red')
hist(resid(m4))
Anova(m4,type="III")
summary(m4)
plot(allEffects(m4))


model.df4 <- tidy(m4)%>%
  filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation
model.df4

e4<-model.df4 %>% 
  mutate(odds = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(m4)),  # Variance of each coefficient
         odds.se = sqrt(odds^2 * var.diag))%>%  # Odds-ratio adjusted
  mutate(p = odds/(1+odds))
e4
# pulse the only factor when looking at high abundance years

# low abundance years
# 2000 to 2005, 2006 to 2011
low.year<-alldata%>%
  filter(cohort>=2000 & cohort<=2005 | cohort >= 2006 & cohort & cohort <=2011)

# parse out smaller components

ggplot(low.year)+
  geom_point(aes(y=postCount/preCount, x=preK))

ggplot(low.year)+
  geom_point(aes(y=postCount/preCount, x = postK))

ggplot(low.year)+
  geom_point(aes(y=postCount/preCount, x = days_below_1))

low.year%>%
  filter(pulse!="4")%>%
  ggplot()+
  geom_boxplot(aes(y=postCount/preCount, x = pulse))+ylim(0,1)

# pulse 4 less present in these high abundance years

m5<-glmer(cbind(postCount,preCount)~pulse+preK+scale(days_below_1)+(1|cohort),
          data=filter(low.year,pulse!="4"),family=binomial)
plot(m5)
qqnorm(resid(m5))
qqline(resid(m5),col='red')
hist(resid(m5))
Anova(m5,type="III")
summary(m5)
plot(allEffects(m5))

#none of our factors explain low abundance years... BUT pulse 2 and pulse 4 show high "survival"

# ---- pulse structure and winter survival ----

# settlement timing
ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.yday,colour=pulse,size=postCount/preCount))

ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.week,colour=pulse,size=postCount))

ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.week,colour=pulse,size=preCount))

m6<-glm(cbind(postCount,preCount)~settle.week*pulse+cohort,data=alldata,family = binomial(link="logit"))
plot(m6)
qqnorm(resid(m6))
qqline(resid(m6),col='red')
hist(resid(m6))
Anova(m6,type="III")
summary(m6)
plot(allEffects(m6))

model.df6 <- tidy(m6)%>%
  filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation

e6<-model.df6 %>% 
  mutate(odds = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(m6)),  # Variance of each coefficient
         odds.se = sqrt(odds^2 * var.diag))%>%  # Odds-ratio adjusted
  mutate(p = odds/(1+odds))
e6

m7<-glm(settle.week~preK,data=alldata,family=poisson)
plot(m7)
qqnorm(resid(m7))
qqline(resid(m7),col='red')
hist(resid(m7))
Anova(m7,type="III")
summary(m7)
plot(allEffects(m7))
