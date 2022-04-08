# Survival and Pre- and post-winter condition
# Part 2: Analysis

# # ---- working directory -----
# setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/condition-field/")

# ---- Packages -----
library(car)
library(lme4)
library(effects)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(MASS)
library(tidyverse)
library(ggpubr)
library(broom.mixed)
library(DescTools)

# ---- data ----
SLfull<-read.csv("./data/data-working/newman-length.csv")
condition<-read.csv("./data/output/condition-field-clean.csv")
data<-read.csv("./data/output/condition-field-formatted.csv")
settle<-read.csv("./data/output/settlement-day.csv")
temp<-read.csv("./data/data-working/newman-temp-to-2019.csv")
winter<-read.csv("./data/output/newman-winter-summary.csv")
mort<-read.csv("./data/output/age-0-mortality-R.csv") # need to redo mortality calculations

#check data
head(data)
str(data)
summary(data)

# format data
# ---- Graphical model ----

alldata<-data%>%filter(pulse<4)%>% # remove pulse 5 and 6 from working data
  filter(preCount!=0) # remove years where there are no fish present in October
alldata$pulse<-as.factor(alldata$pulse) # convert pulse to factor
alldata$pulse<-droplevels(alldata$pulse) # remove 5 and 6 from factor levels

# ---- preliminary models -----

# Winter characterisation and analysis:
winter.m0<-lm(mean_temp~days_below_1+cohort, data=winter)
plot(winter.m0)
qqnorm(resid(winter.m0))
qqline(resid(winter.m0),col='red')
hist(resid(winter.m0))
summary(winter.m0)
Anova(winter.m0,type="III")

Fig4<-winter%>%
  filter(cohort>1998)%>%
  ggplot()+geom_line(aes(x=cohort,y=mean_temp),size=1)+
  geom_ribbon(aes(ymin=(mean_temp-sd_temp),ymax=(mean_temp+sd_temp),x=cohort),alpha=0.3)+
  geom_text(aes(x=cohort, y=mean_temp,label=as.character(days_below_1)),vjust=2.25,size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  scale_x_continuous(breaks=seq(1999,2016,by=1))+
  theme(axis.text.x = element_text(angle=40,hjust=1,size=10))+
  ylab("Winter Temperature (?C)")+xlab("Cohort")

# ggsave(file="./figures/Fig4.png",plot=Fig4,width=168,height=84,units="mm")

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
winter.m1<-lm(mortality~cohort,data=winter.mort.full)
plot(winter.m1)
qqnorm(resid(winter.m1))
qqline(resid(winter.m1),col='red')
hist(resid(winter.m1))
summary(winter.m1)
Anova(winter.m1,type="III")

winter.m2<-lmer(mortality~days_below_1+pulse+(1|cohort),data=winter.mort.pulse)
plot(winter.m2)
qqnorm(resid(winter.m2))
qqline(resid(winter.m2),col='red')
hist(resid(winter.m2))
summary(winter.m2)
Anova(winter.m2,type="III")
plot(allEffects(winter.m2))
# indications of pulse-structure mortality, but weak signal
# add more variables (condition)

# run model with mean size as explanatory variable
pulse.size<-SLfull%>%
  filter(Species=="AC" & Age == 0)%>%
  group_by(Year,Pulse)%>%
  summarise(meanSL=mean(mmSL))%>%
  rename(pulse=Pulse,cohort=Year)%>%
  filter(pulse<4)%>%
  mutate(pulse=as.factor(pulse))

winter.mort.pulse<-left_join(winter.mort.pulse,pulse.size)

winter.m3<-lmer(mortality~days_below_1+meanSL+(1|cohort),data=winter.mort.pulse)
plot(winter.m3)
qqnorm(resid(winter.m3))
qqline(resid(winter.m3),col='red')
hist(resid(winter.m3))
summary(winter.m3)
Anova(winter.m3,type="III")
plot(allEffects(winter.m3))

# Figure 2
ggplot(data=winter.mort.full)+
  geom_hline(yintercept = 0.4260159,linetype='dashed')+
  geom_point(aes(x=cohort,y=mortality),size=2)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab('Mortality Rate % ??? ' ~d^-1)+
  xlab('Cohort')+
  scale_x_continuous(breaks = seq(1998,2016,by=2))+
  ylim(-1,2)

winter.mort.pulse%>%
  ggplot()+
  geom_hline(yintercept = 0.3134413,linetype='dashed')+
  geom_point(aes(x=cohort,y=mortality,colour=factor(pulse)),size=2)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab('Mortality Rate % ??? ' ~d^-1)+
  xlab('Cohort')+
  scale_x_continuous(breaks = seq(1998,2016,by=2))+
  #ylim(-1,2)+
  theme(legend.position = "top")

# ---- full model ----
# October and July model
summary(alldata)
glimpse(alldata)

# everything (all pulses, all variables)
m1<-glm(cbind(postCount,preCount)~pulse+preK+postK+days_below_1,
          data=alldata,family=binomial)
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1),col='red')
hist(resid(m1))
summary(m1)
Anova(m1,type="III")
plot(allEffects(m1),residuals=TRUE)
allEffects(m1)
a<-plot(effect("pulse",m1),
        main=list(label="",cex=1.5),
        ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5),
        xlab=list(label="Pulse",cex=1.5))
b<-plot(effect("preK",m1),
        xlab=list(label="Prewinter K"),
        main=list(label="",cex=1.5),
        ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5))
c<-plot(effect("postK",m1),
        xlab=list(label="Postwinter K"),
        main=list(label="",cex=1.5),
        ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5))
d<-plot(effect("days_below_1",m1),
        xlab=list(label="Days Below 1"),
        main=list(label="",cex=1.5),
        ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5))
ggarrange(a,b,c,d)

plot(predictorEffects(m1))

exp(13.3618)/2
exp(6.5428)/2
exp(7.2952)/2
exp(4.9128)/2
# ----- graphical model ------
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

# summary: attempted predictions - doesn't seem to work
# must look at additional info for visualizing ANCOVA model..... Split up into four pulses maybe??

# ---- what is the evidence -----
null<-glmer(cbind(postCount,preCount)~1+
              (1|cohort),data=alldata,family=binomial)
deviance(null)
deviance(m1)
deviance(null)-deviance(m1)
exp(144.6086/2)
Anova(m1,type="III")
# strong evidence for the model

# --- model 2: pulse and preK only ----
# everything (all pulses, all variables)
m2<-glmer(cbind(postCount,preCount)~pulse*preK+(1|cohort),
          data=alldata,family=binomial)
plot(m2)
qqnorm(resid(m2))
qqline(resid(m2),col='red')
hist(resid(m2))
summary(m2)
Anova(m2,type="III")
plot(allEffects(m2),residuals=TRUE)
allEffects(m2)
a<-plot(effect("pulse",m2),
        main=list(label="",cex=1.5),
        ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5),
        xlab=list(label="Pulse",cex=1.5))
b<-plot(effect("preK",m2),
        xlab=list(label="Prewinter K"),
        main=list(label="",cex=1.5),
        ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5))
ggarrange(a,b)

plot(predictorEffects(m2))

exp(13.3618)/2
exp(6.5428)/2
exp(7.2952)/2
exp(4.9128)/2
# ----- graphical model ------

ggplot(data=alldata,aes(x=pulse,y=postCount/preCount))+
  geom_boxplot(outlier.shape = NA)+ylim(0,1)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab('Abundance Ratio')

ggplot(winter.mort.pulse)+
  geom_boxplot(aes(x=pulse,y=mortality))



plot_model(
  m2, 
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
model.d2 <- tidy(m2)%>%
  filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation
model.df1

e1<-model.df1 %>% 
  mutate(odds = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(m1)),  # Variance of each coefficient
         odds.se = sqrt(odds^2 * var.diag))%>%  # Odds-ratio adjusted
  mutate(p = odds/(1+odds))
e1

# summary: attempted predictions - doesn't seem to work
# must look at additional info for visualizing ANCOVA model..... Split up into four pulses maybe??

# ---- what is the evidence -----
null<-glmer(cbind(postCount,preCount)~1+
              (1|cohort),data=alldata,family=binomial)
deviance(null)
deviance(m1)
deviance(null)-deviance(m1)
exp(144.6086/2)

# ---- evidence between model 1 and model 2 -----
deviance(m1)
deviance(m2)
deviance(m2)-deviance(m1)
exp(102.894/2)

# ---- winter ----
alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=days_below_1),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Days Below 1? C")

alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=days_below_1),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Abundance Ratio")+xlab("Days Below 1? C")+
  facet_wrap(~pulse)

mw<-glm(cbind(postCount,preCount)~days_below_1,data=alldata,family=binomial)
plot(mw)
hist(resid(mw))
Anova(mw,type="III")
summary(mw)


mp<-glmer(cbind(postCount,preCount)~days_below_1+factor(pulse)+(1|cohort),data=alldata,family=binomial)
plot(mp)
hist(resid(mp))
Anova(mp,type="III")
summary(mp)


# ---- prewinter and survival-----
head(settle)
head(condition)
settle<-settle%>%
  rename(cohort=Year,pulse=Pulse)%>%
  filter(cohort<2019)
cond2<-left_join(condition,settle,by=c("cohort","pulse"))%>%
  left_join(temp)

# ----- condition -----
condition%>%
  filter(fulton<1.5)%>%
  filter(month==10)%>%
  filter(pulse<5)%>%
  filter(cohort>2000 & cohort<2019)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton),size=1)+
  xlab("Cohort")+
  ylab("Fulton's K")+
  theme(axis.text.x = element_text(angle=45))+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))

condition%>%
  filter(fulton<1.5)%>%
  filter(month==5)%>%
  filter(pulse<5)%>%
  filter(cohort>2000 & cohort<2019)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton),size=1)+
  xlab("Cohort")+
  ylab("Fulton's K")+
  theme(axis.text.x = element_text(angle=45))+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))
# look at pre and post winter pulse during years where pulses are present before and after winter



K1<-cond2%>%
  filter(pulse==1 | pulse==2 | pulse ==3)%>%
  # filter(cohort==2001 | cohort == 2007 | cohort == 2009 | cohort == 2011 | cohort == 2012 |
  #          cohort == 2013 | cohort == 2015)%>%
  filter(month==10)%>%
  mutate(season="fall")%>%
  dplyr::select(fulton,cohort,pulse,season)%>%
  filter(cohort>=2001)
K2<-cond2%>%
  filter(pulse==1 | pulse==2 | pulse ==3)%>%
  # filter(cohort==2001 | cohort == 2007 | cohort == 2009 | cohort == 2011 | cohort == 2012 |
  #          cohort == 2013 | cohort == 2015)%>%
  filter(month==5)%>%
  mutate(season="spring")%>%
  dplyr::select(fulton,cohort,pulse,season)

K<-bind_rows(K1,K2)%>%
  left_join(dplyr::select(alldata, cohort, days_below_1),by=c("cohort"))%>%
  distinct()%>%
  filter(fulton<1.6)%>%
  mutate(pulse=as.factor(pulse))%>%
  filter(cohort<2019)



Km1<-lmer(fulton~season+factor(pulse)+scale(days_below_1)+(1|cohort),data=K)

plot(Km1)
qqnorm(resid(Km1))
qqline(resid(Km1),col='red')
hist(resid(Km1))
Anova(Km1,type="III")
summary(Km1)


ggplot(K)+
  geom_boxplot(aes(x=factor(cohort),y=fulton,fill=season))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Fulton's K")+
  xlab("Cohort")+
  ylim(0.25,1.2)

# prewinter K only
Km2<-lmer(fulton~pulse+(1|cohort),data=filter(K,season=="fall"))

plot(Km2)
qqnorm(resid(Km2))
qqline(resid(Km2),col='red')
hist(resid(Km2))
Anova(Km2,type="III")
summary(Km2)
plot(allEffects(Km2))

# condition does not change with pulse


km3<-glmer(cbind(postCount,preCount)~preK+(1|cohort),family=binomial(link="logit"),data=alldata)
plot(km3)
hist(resid(km3))
Anova(km3,type="III")
summary(km3)

km4<-glmer(cbind(postCount,preCount)~preK*pulse+(1|cohort),family=binomial(link="logit"),data=alldata)
plot(km4)
hist(resid(km4))
Anova(km4,type="III")
summary(km4)

cond2%>%
  filter(fulton<1.6 & pulse<5 & month == 10)%>%
  ggplot()+
  geom_boxplot(aes(y=fulton,x=factor(pulse)),size=1)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Pre-winter K")+xlab("Pulse")

# postwinter K
Km5<-lmer(fulton~scale(days_below_1)+pulse+(1|cohort),data=filter(K,season=="spring"))

plot(Km5)
qqnorm(resid(Km5))
qqline(resid(Km5),col='red')
hist(resid(Km5))
Anova(Km5,Km5="III")
summary(Km5)


alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=postK),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Postwinter Fulton's K")

# K3<-cond2%>%
#   filter(month==5)%>%
#   dplyr::select(fulton,cohort,pulse)%>%
#   left_join(dplyr::select(alldata, cohort, days_below_1),by=c("cohort"))%>%
#   distinct()%>%
#   filter(fulton<1.6)%>%
#   mutate(pulse=as.factor(pulse))%>%
#   filter(!is.na(cohort))


km6<-glmer(cbind(postCount,preCount)~postK+(1|cohort),data=alldata,family=binomial(link = "logit"))
plot(km6)
hist(resid(km6))
Anova(km6,type="III")
summary(km6)

cond2%>%
  filter(fulton<1.6 & pulse<5 & month == 5)%>%
  ggplot()+
  geom_boxplot(aes(y=fulton,x=factor(pulse)),size=2)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  #scale_x_continuous(breaks=seq(1999,2016,by=1))+
  theme(axis.text.x = element_text(angle=40,hjust=1,size=10))+
  ylab("Pre-winter K")+xlab("pulse")
# abundance ratio~pulse+K+winter with select years

# data.pulse<-alldata%>%
#   filter(cohort==2001 | cohort == 2007 | cohort == 2009 | cohort == 2011 | cohort == 2012 |
#            cohort == 2013 | cohort == 2015)%>%
#   filter(pulse==2 | pulse ==3)
# 
# km8<-glmer(cbind(postCount,preCount)~pulse+preK+postK+scale(days_below_1)+(1|cohort),
#            data=data.pulse,family=binomial)
# plot(km8)
# qqnorm(resid(km8))
# qqline(resid(km8),col='red')
# hist(resid(km8))
# Anova(km8,type="III")
# summary(km8)
# plot(allEffects(km8))

# ---- Pulse settlement ----

pre.m1<-lmer(log(weight)~log(mmSL)+(1|cohort),
              data=filter(cond2,month==10 & age == 0 &pulse<4 & fulton<1.5))
plot(pre.m1)
Anova(pre.m1)
summary(pre.m1)

pred1<-predict(pre.m1)


ggplot(data=cbind(filter(cond2,month==10 & age == 0 & pulse <4 & fulton <1.5),
                  pred1),
       aes(x=log(mmSL),y=log(weight),color=factor(pulse)))+geom_point(alpha=0.8,size=2)+
  geom_smooth(aes(y=pred1),method="lm",colour='black')+
  annotate("text",x=4.2,y=-2.8,label="y = -11.71 + 2.966x",size=5)+
  xlab("log(SL) (mm)")+
  ylab("log(weight) (g)")+
  scale_color_discrete(name="Pulse")+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))

ggplot(data=cbind(filter(cond2,month==10 & age == 0 & pulse <4 & fulton <1.5),
                  pred1),
       aes(x=(mmSL),y=(weight),color=factor(pulse)))+geom_point(alpha=0.8,size=2)
# --- The Closer Look ----
# # determine years to look at
# 
# #select years above the mean age-0 abundance level
# # 1998, 1999, 2007, 2012, 2013, 2014, 2015, 2016, 2017
# 
# high.year<-alldata%>%
#   filter(cohort== 1998 | cohort == 1999 | cohort ==2007 | cohort >= 2013 & cohort & cohort <=2017)
# 
# summary(high.year)
# # pulse 4 less present in these high abundance years
# 
# m4<-glmer(cbind(postCount,preCount)~pulse+preK+scale(days_below_1)+(1|cohort),
#           data=filter(high.year,pulse!="4"),family=binomial)
# plot(m4)
# qqnorm(resid(m4))
# qqline(resid(m4),col='red')
# hist(resid(m4))
# Anova(m4,type="III")
# summary(m4)
# plot(allEffects(m4))
# 
# 
# model.df4 <- tidy(m4)%>%
#   filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation
# model.df4
# 
# e4<-model.df4 %>% 
#   mutate(odds = exp(estimate),  # Odds ratio/gradient
#          var.diag = diag(vcov(m4)),  # Variance of each coefficient
#          odds.se = sqrt(odds^2 * var.diag))%>%  # Odds-ratio adjusted
#   mutate(p = odds/(1+odds))
# e4
# # pulse the only factor when looking at high abundance years
# 
# # low abundance years
# # 2000 to 2005, 2006 to 2011
# low.year<-alldata%>%
#   filter(cohort>=2000 & cohort<=2005 | cohort >= 2006 & cohort & cohort <=2011)
# 
# # parse out smaller components
# 
# ggplot(low.year)+
#   geom_point(aes(y=postCount/preCount, x=preK))
# 
# ggplot(low.year)+
#   geom_point(aes(y=postCount/preCount, x = postK))
# 
# ggplot(low.year)+
#   geom_point(aes(y=postCount/preCount, x = days_below_1))
# 
# low.year%>%
#   filter(pulse!="4")%>%
#   ggplot()+
#   geom_boxplot(aes(y=postCount/preCount, x = pulse))+ylim(0,1)
# 
# # pulse 4 less present in these high abundance years
# 
# m5<-glmer(cbind(postCount,preCount)~pulse+preK+scale(days_below_1)+(1|cohort),
#           data=filter(low.year,pulse!="4"),family=binomial)
# plot(m5)
# qqnorm(resid(m5))
# qqline(resid(m5),col='red')
# hist(resid(m5))
# Anova(m5,type="III")
# summary(m5)
# plot(allEffects(m5))

#none of our factors explain low abundance years... BUT pulse 2 and pulse 4 show high "survival"

# ---- pulse structure and winter survival ----

pm1<-glmer(cbind(postCount,preCount)~pulse+(1|cohort),data=alldata,family=binomial(link = "logit"))
plot(pm1)
hist(resid(pm1))
Anova(pm1,type="III")
summary(pm1)

ggplot(alldata)+
  geom_boxplot(aes(y=postCount,x=factor(pulse)),size=1,outlier.shape = NA)+
  theme_bw(base_rect_size = 1)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Post-winter Abundance (catch/haul)")+xlab("Pulse")+
  ylim(0,15)

alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_boxplot(aes(y=postCount/preCount2,x=pulse),size=1,outlier.shape = NA)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Pulse")+
  ylim(0,4.55)

# settlement timing
ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.yday,colour=pulse,size=postCount/preCount))

ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.week,colour=pulse,size=postCount))

ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.week,colour=pulse,size=preCount))

#post winter abundance and settlement by pulse
ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.week,size=postCount))+
  facet_wrap(~pulse)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Settlement Week")+xlab("Cohort")+
  labs(size="Post-winter Abundance")+
  theme(legend.position = "top")+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=12))








m6<-glmer(cbind(postCount,preCount)~scale(settle.week)+pulse+(1|cohort),data=alldata,family = binomial(link="logit"))
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


#prediction plot

pred6<-predict(m6)


ggplot(data=cbind(alldata,pred6),
       aes(x=settle.week,y=cbind(postCount,preCount),color=pulse))+geom_point(alpha=0.8,size=2)+
  geom_smooth(aes(y=pred6),method="glm",colour='black')+
  scale_color_discrete(name="Pulse")+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  facet_wrap(~pulse)

alldata%>%
  filter(postCount!=16)%>%
  ggplot()+
  geom_point(aes(x=settle.week,y=postCount/preCount,colour=pulse))

m6.1<-glmer(cbind(postCount,preCount)~scale(settle.week)+(1|cohort),data=alldata,family = binomial(link="logit"))
plot(m6.1)
qqnorm(resid(m6.1))
qqline(resid(m6.1),col='red')
hist(resid(m6.1))
Anova(m6.1,type="III")
summary(m6.1)
plot(allEffects(m6.1))
# ---- Settlement timing and condition -----
m7<-glmer(preK~scale(settle.week)+(1|cohort),data=alldata,family=Gamma(link="log"))
plot(m7)
qqnorm(resid(m7))
qqline(resid(m7),col='red')
hist(resid(m7))
Anova(m7,type="III")
summary(m7)
plot(allEffects(m7))


# ---- settlement timing vs pulse structure ----
alldata%>%
  filter(postCount!=16)%>% # remove outlier for datavis
  ggplot()+
  geom_jitter(aes(x=settle.week,y=postCount/preCount,colour=pulse))
alldata%>%
  filter(postCount!=16)%>% # remove outlier for datavis
  ggplot()+
  geom_point(aes(x=settle.week,y=postCount/preCount))+
  facet_wrap(~pulse)

ggplot(alldata)+
  geom_point(aes(x=settle.week,y=preK,colour=pulse))

m8<-glmer(cbind(postCount,preCount)~pulse+(1|cohort),data=alldata,family=binomial(link = "logit"))
plot(m8)
hist(resid(m8))
qqnorm(resid(m8))
qqline(resid(m8))
Anova(m8,type="III")
plot(allEffects(m8))

# look at how settlement time changes for each pulse over time in relation to size
ggplot(alldata)+
  geom_point(aes(x=settle.week,y=fallSL,size=postCount/preCount,colour=pulse))

alldata%>%
  #filter(postCount!= 16)%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount,x=fallSL))

#---- mortality leading to winter----

#fall mort
head(mort)

mort%>%
  filter(Pulse!=4)%>%
  ggplot()+
  geom_point(aes(x=Year,y=M))+
  facet_wrap(~Pulse)

# fall mortality and settlement time
# create new dataframe with settle week andfall M
fall<- mort%>%
  rename(cohort=Year,pulse=Pulse)%>%
  filter(pulse<4 & cohort<2019)%>%
  mutate(pulse=as.factor(pulse))%>%
  left_join(alldata)
head(fall)

# model
m7<- lm(M~settle.week,data=fall)
plot(m7)
qqnorm(resid(m7))
qqline(resid(m7))
hist(resid(m7))
Anova(m7, type="III")
summary(m7)

