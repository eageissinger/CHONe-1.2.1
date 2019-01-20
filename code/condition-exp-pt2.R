#----Overwinter Condition Analysis-----

#load working directory
setwd("C:/Users/Emilie/Dropbox/Thesis/Research/CHONe-1.2.1/")

#----load data-----
condition<-read.csv("./data/data-working/condition-exp.csv",header=TRUE)
length_weight<-read.csv("./data/data-working/length-weight-exp.csv",header=TRUE)
tank_survival<-read.csv("./data/data-working/tank-survival-exp.csv",header=TRUE)
temp<-read.csv("./data/data-working/temperature-exp.csv",header=TRUE)
tanks<-read.csv("./data/data-working/tank-assignments-exp.csv")

# ----load pacakges -----
library(car)
library(MASS)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(stats)
library(lattice)
library(ggpmisc)
library(lme4)
library(nlme)
library(numDeriv)
library(glmmTMB)
library(psych)
library(betareg)
library(lmtest)
library(rcompanion)
library(multcompView)
library(emmeans)
library(grid)



#source("ggplot_smooth_func.R")

# --- check data -----
#length-weight
str(length_weight)
summary(length_weight)
names(length_weight)
head(length_weight)
tail(length_weight)
dim(length_weight)

#condition
str(condition)
summary(condition)
names(condition)
head(condition)
tail(condition)
dim(condition)

#tank survival
str(tank_survival)
summary(tank_survival)
names(tank_survival)
head(tank_survival)
tail(tank_survival)
dim(tank_survival)

#temperature
str(temp)
summary(temp)
names(temp)
head(temp)
tail(temp)
dim(temp)

# --- format data -----

# ---- Fix date ------

# length-weight
names(length_weight)<-c('year','month','day','tank','size','ration','length_mm','weight_g',
                        'fish_number','notes')
length_weight$date<-ymd(paste(length_weight$year,length_weight$month,length_weight$day,sep="-"))
length_weight$julian_date<-yday(length_weight$date)

#condition
names(condition)<-c("year",'month','day','time','tank','num','sl_mm','wet_total_weight_g',
                    'wet_liver_g','wet_evis_g','dry_liver_mg','dry_evis_g','mortality','notes',
                    'drying_notes')
condition$date<-ymd(paste(condition$year,condition$month,condition$day,sep="-"))
condition$julian_date<-yday(condition$date)

#survival
names(tank_survival)<-c('year','month','day','tank','size',
                        'ration','number')
tank_survival$date<-ymd(paste(tank_survival$year,tank_survival$month,tank_survival$day,sep="-"))
tank_survival$julian_date<-yday(tank_survival$date)

#temperature
names(temp)<-c('year','month','day','time','tank','temperature','notes')
temp$date<-ymd(paste(temp$year,temp$month,temp$day,sep="-"))
temp$julian_date<-yday(temp$date)

# ----- calculations and summary -----

# Temperature
temp<-left_join(temp,tanks)

tempdata<-temp%>%
  unite(group,size,ration,sep=' ')%>%
  group_by(group,julian_date,date)%>%
  summarise(temperature=mean(temperature))%>%
  ungroup()%>%
  data.frame()

tempdata<-separate(tempdata,group,c("size","ration"),sep=' ', convert=TRUE)

tempdata<-tempdata%>%
  mutate(julian_date=replace(julian_date,julian_date>365,0))%>%
  data.frame()

#Lenght-weight
#Specific growth rate
#SGR=(lnwwt2-lnwwt1)/(t2-t1)x100

lw<-length_weight%>%
  mutate(julian_date=replace(julian_date,julian_date>364,0))%>%
  group_by(tank,julian_date)%>%
  summarise(mean_weight=mean(weight_g),sd_w=sd(weight_g),se_w=sd_w/sqrt(n()),
            mean_sl=mean(length_mm),sd_sl=sd(length_mm),se_sl=sd_sl/sqrt(n()))%>%
  ungroup()

#combine survival data with lw summary
tank_survival<-tank_survival%>%mutate(julian_date=replace(julian_date,julian_date>364,0))
lw<-left_join(tank_survival,lw,by=NULL)
lw<-lw%>%
  filter(julian_date%in%c(0,30,31,58,59,86,87))
lw<-na.omit(lw)

lw<-lw%>%
  group_by(tank)%>%
  mutate(sgr_w=(log(mean_weight)-log(lag(mean_weight)))/(julian_date-lag(julian_date))*100,
         sgr_sl=(log(mean_sl)-log(lag(mean_sl)))/(julian_date-lag(julian_date))*100)%>%
  ungroup()%>%
  data.frame()


lw<-left_join(lw,tanks)

#Condition Summary

cond<-condition%>%
  mutate(kwet=100*wet_total_weight_g/(sl_mm*.1)^3)%>%
  mutate(kdry=1000*dry_evis_g/(sl_mm*.1)^3)%>%
  mutate(HSI=((dry_liver_mg*.001)/dry_evis_g)*1000)

head(cond)
summary(cond)
#add size and rations

cond<-left_join(cond,tanks)

# need to add day-0 information
# new column labeling as day 0
# add size group

day0<-cond%>%
  filter(date=="2016-12-16")%>%
  data.frame()

day0$size[day0$sl_mm<=84]<-"small"
day0$size[day0$sl_mm>=89]<-"large"
day0$tank<-0

cond<-cond%>%
  filter(date>"2016-12-16")%>%
  data.frame()

fishcond<-bind_rows(day0,cond)
str(fishcond)

#Survival
#max for large = 27, max for small = 54
#make data frame with start values to attach to new data

tank_survival$start[tank_survival$size=="small"]<-54
tank_survival$start[tank_survival$size=="large"]<-27

survival<-tank_survival%>%
  unite(group,size,ration,sep=' ')%>%
  group_by(group,julian_date,date,start)%>%
  summarise(total=sum(number))%>%
  mutate(exp_surv=(total/start)*100)%>%
  ungroup()%>%
  data.frame()

survival<-separate(survival,group,c("size","ration"), sep=' ', convert=TRUE)


survival<-left_join(survival,tempdata)




# ---- Live condition data ------
#data summary 
# format data to have early terminations in as well....
aprilcond<-fishcond%>%
  filter(month==4)%>%
  filter(day==24)%>%
  data.frame()

earlyterm<-fishcond%>%
  filter(month==3)%>%
  filter(day%in%c(21,25))%>%
  data.frame()


names(aprilcond)
names(earlyterm)
names(length_weight)

livecond<-length_weight%>%
  mutate(julian_date=replace(julian_date,julian_date>364,0))%>%
  mutate(k=100*weight_g/(length_mm*.1)^3)%>%
  select(-length_mm,-weight_g,-fish_number,-notes)%>%
  data.frame()
aprilcond<-aprilcond%>%
  mutate(k=kwet)%>%
  select(year,month,day,tank,size,ration,date,julian_date,k)%>%
  data.frame()
earlyterm<-earlyterm%>%
  mutate(k=kwet)%>%
  select(year,month,day,tank,size,ration,date,julian_date,k)%>%
  data.frame()
glimpse(livecond)
glimpse(earlyterm)
glimpse(aprilcond)
names(livecond)
names(earlyterm)
names(aprilcond)
livecond<-union_all(livecond,earlyterm)
livecond<-union_all(livecond,aprilcond)


# add temperature data
str(livecond)
str(tempdata)

tempdata$size<-as.factor(tempdata$size)
tempdata$ration<-as.factor(tempdata$ration)

livecond<-left_join(livecond,tempdata)

#visualize data
ggplot(livecond,aes(x=julian_date,y=k))+
  geom_point()
ggplot(livecond,aes(x=julian_date,y=k,colour=ration))+
  geom_point()
ggplot(livecond,aes(x=julian_date,y=k,colour=size))+
  geom_point()

xyplot(k~julian_date|tank,data=livecond)

head(livecond)
str(livecond)
livecond$tank<-as.factor(livecond$tank)

# create new df for analysis
# ration -> percent food 

lc<-livecond%>%
  group_by(tank,julian_date,ration,size)%>%
  summarise(kavg=mean(k),sdk=sd(k),sek=sdk/sqrt(n()))%>%
  data.frame()
lc$tank<-as.factor(lc$tank)
str(lc)
summary(lc)
lc$julian_date<-as.integer(lc$julian_date)
unique(lc$julian_date)
# select measurement days only
# 0 , 30, 58, 84, 86, 114, small day 80
str(lc)
lc<-lc%>%
  filter(julian_date == 80 & size == "small" |julian_date == 0 |
           julian_date==58 | julian_date == 84 | julian_date == 86 |
           julian_date==114)

# ---- LC Models ----
lc.m1<-lmer(kavg~ration*size*julian_date+(ration|tank)+(size|tank),data=lc)
lc.m1
plot(lc.m1)
summary(lc.m1)
av1<-anova(lc.m1)
anova(lc.m1)
resid<-resid(lc.m1)
SSresid<-sum(resid^2)
resid
SSresid
SStotal<-sum(av1$`Sum Sq`)+SSresid
SStotal
LR<-SSresid/SStotal
LR

ggplot(lc.m1,aes(x=fitted(lc.m1),y=resid(lc.m1)))+geom_point()
hist(resid(lc.m1))
qqnorm(resid(lc.m1))

lc.m2<-glmer(kavg~ration*size*julian_date+(ration|tank)+(size|tank),
             data=lc,family = Gamma(link = "log"))
lc.m2
plot(lc.m2)
hist(resid(lc.m2))
qqnorm(resid(lc.m2))
summary(lc$kavg)

# ---- LC model Beta regression ----
lc.m3<-glmmTMB(kavg~ration*size*julian_date+(ration|tank)+(size|tank),
        data=lc,family = list(family="beta",link="logit"))
plot(lc.m3)

lc.m4<-betareg(kavg~ration*size*julian_date,data=lc)
lc.m4$model
plot(lc.m4)
hist(resid(lc.m4))
qqnorm(resid(lc.m4))

joint_tests(lc.m4)
lrtest(lc.m4)
summary(lc.m4)
plot(x=fitted(lc.m4),y=residuals(lc.m4))
marginal<-emmeans(lc.m4,~ration)
pairs(marginal,adjust="tukey")
exp(logLik(lc.m4))

lc.m4.5<-betareg(kavg~ration*julian_date,data=lc)
plot(lc.m4.5)
hist(resid(lc.m4.5))
qqnorm(resid(lc.m4.5))

joint_tests(lc.m4.5)
lrtest(lc.m4.5)
summary(lc.m4.5)
plot(x=fitted(lc.m4.5),y=residuals(lc.m4.5))
marginal<-emmeans(lc.m4.5,~ration)
pairs(marginal,adjust="tukey")
exp(logLik(lc.m4.5))
exp(logLik(lc.m4))
lc.m5<-betareg(kavg~size+ration*julian_date,data=lc)
plot(lc.m4)
hist(resid(lc.m5))
qqnorm(resid(lc.m5))
joint_tests(lc.m5)
lrtest(lc.m5)
summary(lc.m5)

logLik(lc.m4)
logLik(lc.m5)
exp(logLik(lc.m4))
exp(logLik(lc.m5))
#plot without size

limitse<-aes(ymin=kavg-sek,ymax=kavg+sek)
lc%>%
  rename(Ration=ration)%>%
  ggplot(aes(x=julian_date,y=kavg,colour=Ration))+
  geom_jitter(aes(shape=Ration,colour=Ration,fill=Ration),size=3)+
  stat_smooth(method="lm",linetype='dashed',size=1,se=FALSE)+
  geom_errorbar(limitse,width=1)+
  theme_bw(base_rect_size = 2)+
  ylab("Condition factor (K)")+xlab("Day of experiment")+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=16,face='bold'))+
  ylim(0.6,1.0)+
  theme(axis.title=element_text(size=20))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(legend.position=c(0.85,0.26))+
  scale_colour_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_fill_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_shape_manual(values=c(22:25))+
  theme(panel.grid=element_blank())+
  theme(legend.text = element_text(size=16))+
  theme(plot.margin = unit(c(0.75,0.75,0.75,0.75),"cm"))


livecond.small<-lc%>%
  filter(size=="small")%>%
  data.frame()

livecond.large<-lc%>%
  filter(size=="large")%>%
  filter(julian_date!=80)%>%
  data.frame()


#small
livecond.small.plot<-ggplot(livecond.small,aes(x=julian_date,y=kavg,colour=ration))+
  geom_jitter(aes(shape=ration,colour=ration,fill=ration),size=3)+
  stat_smooth(method="lm",linetype='dashed',size=1,se=FALSE)+
  geom_errorbar(limitse,width=1)+
  xlab("Day of Experiment")+ylab("Condition factor (K)")+
  theme_classic()+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=18,face='bold'))+
  ylim(0.6,1.0)+
  theme(axis.title=element_text(size=20))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(legend.key=element_blank())+
  theme(legend.position=c(0.9,0.19))+
  scale_colour_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_fill_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_shape_manual(values=c(22:25))
livecond.small.plot

#large
livecond.large.plot<-ggplot(livecond.large,aes(x=julian_date,y=kavg,colour=ration))+
  geom_point(aes(shape=ration,colour=ration,fill=ration),size=3)+
  stat_smooth(method="lm",linetype='dashed',size=1,se=FALSE)+
  geom_errorbar(limitse,width=1)+
  theme_classic()+
  ylim(0.6,1.0)+
  theme(axis.title=element_text(size=20))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(legend.key=element_blank())+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(legend.position=c(0.9,0.19))+
  scale_colour_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_fill_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_shape_manual(values=c(22:25))
livecond.large.plot

# ----- Final Condition analysis ------
summary(fishcond)
dim(fishcond)
View(fishcond)

# take out day 0 condition
finalcond<-fishcond%>%
  filter(notes!="day 0")
   
summary(finalcond)
View(finalcond)

finalK<-finalcond%>%
  group_by(tank,ration,size,percent)%>%
  summarise(dry=mean(kdry),wet=mean(kwet),
            sedry=(sd(kdry)/sqrt(n())),
            sewet=(sd(kwet)/sqrt(n())))
View(finalK)
# add fish # for each tank
finalcond<-finalcond%>%
  group_by(tank)%>%
  mutate(fish=seq(1:9))

# Dry condition model analysis

ggplot(data=finalcond,aes(x=percent,y=kdry))+geom_point()
ggplot(data=finalcond,aes(x=percent,y=kdry,colour=size))+geom_point()


#model dry condition
d.m0<-lmer(kdry~percent*size+(percent|tank)+(size|tank),data=finalcond)
plot(d.m0)
hist(resid(d.m0))
qqnorm(resid(d.m0))
exp(logLik(d.m0))

# not linear....

# GLMM

d.m1<-glmer(kdry~percent*size+(percent|tank)+(size|tank),data=finalcond,
            family=Gamma(link = "log"))
d.m1
summary(d.m1)
plot(d.m1)
hist(resid(d.m1))
qqnorm(resid(d.m1))
anova(d.m1)
Anova(d.m1,type = "III")
exp(logLik(d.m1))

d.m2<-glmer(kdry~percent+size+(percent|tank)+(size|tank),data=finalcond,
            family=Gamma(link = "log"))
d.m2
summary(d.m2)
plot(d.m2)
hist(resid(d.m2))
qqnorm(resid(d.m2))
plot(x=fitted(d.m2),y=resid(d.m2))
anova(d.m2)
Anova(d.m2,type = "III")
exp(logLik(d.m1))
exp(logLik(d.m2))

# use averaged data


d.m3<-lm(dry~percent*size,data=finalK)
plot(d.m3)
hist(resid(d.m3))
qqnorm(resid(d.m3))
plot(x=fitted(d.m3),y=resid(d.m3))
logLik(d.m3)

d.m4<-glm(dry~percent*size,data=finalK,family=Gamma(link = "log"))
plot(d.m4)
hist(resid(d.m4))
qqnorm(resid(d.m4))
plot(x=fitted(d.m4),y=resid(d.m4))
exp(logLik(d.m4))


finalK%>%
  mutate(percent_adj=percent-.05)%>%
  mutate(percent_adj=replace(percent_adj,size=="large",percent+0.05))%>%
  rename(Size=size)%>%
  ggplot(aes(x=percent_adj,y=dry,fill=Size))+
  geom_pointrange(aes(shape=Size,ymin=dry-sedry,ymax=dry+sedry),size=.75)+
  theme_bw(base_rect_size = 2)+
  xlab("Food ration (% body weight)")+ylab("Condition factor (K)")+
  theme(axis.title=element_text(size=20))+
  theme(legend.key=element_blank())+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(legend.position=c(0.88,0.21))+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:23))+
  theme(panel.grid = element_blank())+
  theme(legend.text = element_text(size=16))+
  theme(legend.key.size=unit(1,"cm"))

# Wet condition factor

w.m1<-lmer(kwet~percent*size+(percent|tank)+(size|tank),data=finalcond)
w.m1
summary(w.m1)
plot(w.m1)
hist(resid(w.m1))
qqnorm(resid(w.m1))
Anova(w.m1,type = "III")

#GLMM
w.m2<-glmer(kwet~percent*size+(percent|tank)+(size|tank),data=finalcond,
            family = Gamma(link = "log"))
plot(w.m2)
hist(resid(w.m2))
qqnorm(resid(w.m2))

# use averaged data
w.m3<-lm(kwet~percent*size,data=finalK)
plot(w.m3)
hist(resid(w.m3))


w.m4<-glm(kwet~percent*size,data=finalK,family=Gamma(link = "log"))
plot(w.m4)
hist(resid(w.m4))
qqnorm(resid(w.m4))


# ----- Day 0 HSI -------
d0HSI<-day0%>%
  select(sl_mm,HSI,size)%>%
  data.frame()
day0%>%
  group_by(size)%>%
  summarise(mean=mean(HSI))

# ---- HSI -----

hsi.m1<-lmer(HSI~percent*size+(percent|tank)+(size|tank),data=finalcond)
hsi.m1
summary(hsi.m1)
plot(hsi.m1)
hist(resid(hsi.m1))
qqnorm(resid(hsi.m1))
Anova(hsi.m1,type = "III")

hsi.m2<-lmer(HSI~percent+(percent|tank),data=finalcond)
plot(hsi.m2)
hist(resid(hsi.m2))
qqnorm(resid(hsi.m2))
summary(hsi.m2)
Anova(hsi.m2,type = "III")

hsi.m3<-lmer(HSI~size+(size|tank),data=finalcond)
plot(hsi.m3)
hist(resid(hsi.m3))
qqnorm(resid(hsi.m3))
Anova(hsi.m3,type = "III")


# Model 1 is best


# use terminated only
unique(finalcond$julian_date)
endcond<-finalcond%>%
  filter(julian_date==80 | julian_date == 84 | julian_date == 114)

hsi.m4<-lmer(kdry~percent*size+(percent|tank)+(size|tank),data=endcond)
plot(hsi.m4)
hist(resid(hsi.m4))
qqnorm(resid(hsi.m4))

hsi.m5<-glmer(kdry~percent*size+(percent|tank)+(size|tank),data=endcond,
              family = Gamma(link="log"))
plot(hsi.m5)
# ---- Survival -----

#Cacluate survival rate by tank
#group by treatment with st.error
survival<-tank_survival%>%
  group_by(tank,julian_date)%>%
  mutate(survival=(number/9))%>%
  ungroup()%>%
  group_by(tank,size,ration,julian_date)%>%
  summarise(perc_surv=mean(survival))

# Visualize data
survival%>%
  filter(size=="small")%>%
  ggplot(aes(x=julian_date,y=perc_surv,linetype=ration))+geom_smooth()
survival%>%
  filter(size=="large")%>%
  ggplot(aes(x=julian_date,y=perc_surv,linetype=ration))+geom_smooth()

# ----- survival models ----
# split into increments of 30 days
# survival at day 20, 40, 60, 80
S<-survival%>%
  filter(julian_date==0 | julian_date==20 | julian_date==40 |
           julian_date ==60 | julian_date ==80)%>%
  mutate(percent=0.0)%>%
  mutate(percent=replace(percent,ration=="0.5%",0.5))%>%
  mutate(percent=replace(percent,ration=="1.0%",1.0))%>%
  mutate(percent=replace(percent,ration=="2.0%",2.0))
typeof(S$julian_date)
S$julian_date<-as.factor(S$julian_date)

s.m1<-lm(perc_surv~ration*size*factor(julian_date),data=S)
plot(s.m1)
hist(resid(s.m1))
qqnorm(resid(s.m1))
anova(s.m1)
Anova(s.m1,type = "III")
logLik(s.m1)
exp(logLik(s.m1))
exp(logLik(s.m2))

s.m2<-lmer(perc_surv~ration*size*factor(julian_date)+(size|tank)+
              (ration|tank),data=S)

plot(s.m2)
hist(resid(s.m2))
qqnorm(resid(s.m2))

s.m3<-glmer(perc_surv~ration*size*julian_date+(size|tank)+
              (ration|tank),data=survival,family = Gamma(link = 'log'))


survival%>%
  filter(size=="small")%>%
  rename(Ration=ration)%>%
  ggplot()+
  geom_smooth(aes(x=julian_date,y=perc_surv*100,col=Ration),size=1.5,se=FALSE)+
  theme_bw(base_rect_size = 2)+
  ylab("% Survival")+xlab("Day of experiment")+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=16,face='bold'))+
  theme(axis.title=element_text(size=20))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  scale_colour_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_fill_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_shape_manual(values=c(22:25))+
  theme(panel.grid=element_blank())+
  theme(legend.text = element_text(size=16))+
  theme(plot.margin = unit(c(0.75,0.75,0.75,0.75),"cm"))+
  theme(legend.position=c(0.85,0.26))+
  ylim(0,102)

survival%>%
  filter(size=="large")%>%
  rename(Ration=ration)%>%
  ggplot()+
  geom_smooth(aes(x=julian_date,y=perc_surv*100,col=Ration),size=1.5,se=FALSE)+
  theme_bw(base_rect_size = 2)+
  ylab("% Survival")+xlab("Day of experiment")+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=16,face='bold'))+
  theme(axis.title=element_text(size=20))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  scale_colour_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_fill_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_shape_manual(values=c(22:25))+
  theme(panel.grid=element_blank())+
  theme(legend.text = element_text(size=16))+
  theme(plot.margin = unit(c(0.75,0.75,0.75,0.75),"cm"))+
  theme(legend.position=c(0.85,0.26))+
  ylim(0,102)

# ---- Growth ------
sgrsum<-lw%>%
  filter(julian_date>0)%>%
  group_by(tank,size,ration,percent)%>%
  summarise(sgr_length=mean(sgr_sl),sgr_sd_sl=sd(sgr_sl),sgr_se_sl=sgr_sd_sl/sqrt(n()),
            sgr_weight=mean(sgr_w),sgr_sd_w=sd(sgr_sl),sgr_se_w=sgr_sd_w/sqrt(n()))%>%
  data.frame()

sgr_all<-lw%>%
  filter(julian_date>0)

ggplot(data=sgr_all,aes(x=percent,y=sgr_w,colour=size))+geom_point()

#---- SGR Models -----
# weight
sgrw.m1<-lmer(sgr_w~percent+size+(percent|tank)+(size|tank),data=sgr_all)
sgrw.m1
summary(sgrw.m1)
plot(sgrw.m1)
hist(resid(sgrw.m1))
qqnorm(resid(sgrw.m1))
Anova(sgrw.m1,type="III")
anova(sgrw.m1)
exp(logLik(sgrw.m1))
# take out size
sgrw.m2<-lmer(sgr_w~percent+(percent|tank),data=sgr_all)
sgrw.m2
summary(sgrw.m2)
plot(sgrw.m2)
hist(resid(sgrw.m2))
qqnorm(resid(sgrw.m2))
Anova(sgrw.m2,type="III")
anova(sgrw.m2)
exp(logLik(sgrw.m2))

anova(sgrw.m1,sgrw.m2)

# Length
sgrl.m1<-lmer(sgr_sl~percent*size+(1|tank),data=sgr_all)
sgrl.m1
summary(sgrl.m1)
plot(sgrl.m1)
Anova(sgrl.m1)

# take out size
sgrl.m2<-lmer(sgr_sl~percent+(1|tank),data=sgr_all)
sgrl.m2
summary(sgrl.m2)
plot(sgrl.m2)
Anova(sgrl.m2)

anova(sgrl.m1,sgrl.m2)

# model 2 is better!
# Try GLMM to see if there are improvements
# don't know what distribution to use

#when size=large, add .1 to percent
#when size=small, subtract .1 from percent

ggplot(weight,aes(x=percent_adj,y=sgr_weight,fill=size))+
  geom_pointrange(aes(shape=size,ymin=sgr_weight-sgr_se_w,ymax=sgr_weight+sgr_se_w),size=.75)+
  theme_classic()+
  ggtitle("SGR: Weight")+
  ylab("Specifci growth rate")+xlab("Ration (% body weight)")+
  theme(axis.title=element_text(size=20))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(legend.key=element_blank())+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(legend.position=c(0.9,0.15))+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:23))


sgrlarge<-sgrsum%>%
  filter(size=="large")%>%
  mutate(percent_adj=percent+0.05)%>%
  data.frame()

sgrsmall<-sgrsum%>%
  filter(size=="small")%>%
  mutate(percent_adj=percent-0.05)%>%
  data.frame()

sgr_adjusted<-bind_rows(sgrlarge,sgrsmall)

ggplot(sgr_adjusted,aes(x=percent_adj,sgr_weight,fill=size))+
  geom_pointrange(aes(shape=size,ymin=sgr_weight-sgr_se_w,ymax=sgr_weight+sgr_se_w),size=.75)+
  theme_bw(base_rect_size = 2)+
  xlab("Food ration (% body weight)")+ylab("Specific growth rate")+
  theme(axis.title=element_text(size=20))+
  theme(legend.key=element_blank())+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(legend.position=c(0.88,0.23))+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:23))+
  theme(panel.grid = element_blank())+
  theme(legend.text = element_text(size=16))+
  theme(legend.key.size=unit(1,"cm"))+
  theme(plot.title = element_text(size=20,face='bold',hjust=0.5))

ggplot(sgr_adjusted,aes(x=percent_adj,sgr_length,fill=size))+
  geom_pointrange(aes(shape=size,ymin=sgr_length-sgr_se_sl,ymax=sgr_length+sgr_se_sl),size=.75)+
  theme_bw(base_rect_size = 2)+
  xlab("Food ration (% body weight)")+ylab("Specific growth rate")+
  theme(axis.title=element_text(size=22,face='bold'))+
  theme(legend.key=element_blank())+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(legend.position=c(0.88,0.23))+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:23))+
  theme(panel.grid = element_blank())+
  theme(legend.text = element_text(size=16))+
  theme(legend.key.size=unit(1,"cm"))+
  ggtitle("Length")+
  theme(plot.title = element_text(size=20,face='bold',hjust=0.5))
  
  
  
  
  
weight.m1<-lm(sgr_w~ration*size,data=lw)
summary(weight.m1)
par(mfrow=c(2,2))
plot(weight.m1)

weight.m2<-lm(sgr_w~ration+size,data=lw)
summary(weight.m2)
plot(weight.m2)



length.m1<-lm(sgr_sl~ration*size,data=lw)
summary(length.m1)
plot(length.m1)

length.m2<-lm(sgr_sl~ration+size,data=lw)
summary(length.m2)
plot(length.m2)




#Relative Rate of increase
names(lw)
lw2<-length_weight%>%
  mutate(julian_date=replace(julian_date,julian_date>364,0))%>%
  group_by(tank,julian_date)%>%
  summarise(mean_weight=mean(weight_g),sd_w=sd(weight_g),se_w=sd_w/sqrt(n()),
            mean_sl=mean(length_mm),sd_sl=sd(length_mm),se_sl=sd_sl/sqrt(n()))%>%
  ungroup()
lw2<-left_join(lw2,tanks)
RRI<-lw2%>%
  group_by(tank)%>%
  mutate(RRI_weight=(mean_weight-lag(mean_weight))/lag(mean_weight),
         RRI_length=(mean_sl-lag(mean_sl))/lag(mean_sl))%>%
  ungroup()

ggplot(RRI,aes(x=julian_date,y=RRI_weight))+geom_point()

RRIweight.m1<-lm(RRI_weight~julian_date+ration+size,data=RRI)
summary(weight.m1)
plot(weight.m1)

RRIlenght.m1<-lm(RRI_length~julian_date*ration+size,data=RRI)
summary(RRIlenght.m1)
plot(RRIlenght.m1)

# ---- daily growth rate ----
tank36<-filter(length_weight,tank==36)

growth<-mutate(length_weight,julian_date=replace(julian_date,julian_date>364,0))


length_weight$tank<-as.factor(length_weight$tank)
ggplot(tank36,aes(x=julian_date,y=length_mm))+
  stat_smooth_func(geom="text",method="lm",hjust=-0.5,vjust=-4,parse=TRUE)+
  geom_smooth(method='lm')+
  geom_point()

# plot in loop

growth.graph<-function(growth,na.rm=TRUE, ...){
  
  tank_list<-unique(growth$tank)
  
  for (i in seq_along(tank_list)) {
    plot<-ggplot(subset(growth,growth$tank==tank_list[i]),
                 aes(x=julian_date,y=length_mm,group=tank))+
      stat_smooth_func(geom="text",method="lm",hjust=-0.5,vjust=-4,parse=TRUE)+
      geom_smooth(method='lm')+
      geom_point()+
      theme_bw()+
      ggtitle(paste('tank',tank_list[i]))
    print(plot)
  }
}
growth.graph(growth)

# Tank averages
growth2<-growth%>%
  group_by(tank,julian_date,size,ration)%>%
  summarise(sl=mean(length_mm),sd=sd(length_mm),se=sd/sqrt(n()))%>%
  ungroup()%>%
  data.frame()

ggplot(growth2,aes(x=julian_date,y=sl,colour=ration,shape=size))+geom_point()
growth2%>%
  filter(tank==1)%>%
  ggplot(aes(x=julian_date,y=sl,colour=ration,shape=size))+
  geom_pointrange(aes(ymin=sl-se,ymax=sl+se),size=.75)

growth2%>%
  filter(ration=="1.0%")%>%
  filter(size=="small")%>%
  ggplot(aes(x=julian_date,y=sl,shape=size))+
  geom_pointrange(aes(ymin=sl-se,ymax=sl+se),size=.75)

# all plots grouped by tank
growth2.graph<-function(growth2,na.rm=TRUE, ...){
  
  tank_list<-unique(growth$tank)
  
  for (i in seq_along(tank_list)) {
    plot<-ggplot(subset(growth2,growth2$tank==tank_list[i]),
                 aes(x=julian_date,y=sl,group=tank))+
      geom_pointrange(aes(ymin=sl-se,ymax=sl+se),size=.75)+
      stat_smooth_func(geom="text",method="lm",hjust=-0.5,vjust=-4,parse=TRUE)+
      geom_smooth(method='lm')+
      geom_point()+
      theme_bw()+
      ggtitle(paste('tank',tank_list[i]))
    print(plot)
  }
}
growth2.graph(growth2)


### summary of daily growth rate
exp_growth<-read.csv("experiment_growth.csv")
sum_table<-exp_growth%>%
  group_by(ration,size)%>%
  summarise(avg=mean(rate),sd=sd(rate))%>%
  ungroup()%>%
  data.frame()


# ----- non-linear regression -----
head(finalcond)
View(finalcond)
par(mfrow=c(1,1))
plot(kdry~percent*size,data=finalcond,
     ylab="Condition",
     xlab="Ration")
plot(kdry~size,data=finalcond,
     ylab="Condition",
     xlab="Ration")

# ----- BETA REGRESSION
# Live condition: lc or livecond
str(lc)
summary(lc)
max(lc$kavg)
max(livecond$k)
# General linear model
m1<-lm(kavg~ration*size*julian_date,data=lc)
plot(m1)
hist(resid(m1))
qqnorm(resid(m1))
plot(x=fitted(m1),y=resid(m1))



m3<-betareg(kavg~ration*size*julian_date,data=lc)
plot(m3)
hist(resid(m3))
qqnorm(resid(m3))
plot(x=fitted(m3),y=resid(m3))

exp(logLik(m1))
exp(logLik(m2))
exp(logLik(m3))


# Try with survival data?
summary(survival)
m4<-betareg(perc_surv~julian_date,data=survival)
