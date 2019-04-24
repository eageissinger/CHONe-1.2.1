#----Overwinter Condition Analysis-----

#load working directory
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

#----load data-----
condition<-read.csv("./data/data-working/condition-exp.csv",header=TRUE)
length_weight<-read.csv("./data/data-working/length-weight-exp.csv",header=TRUE)
tank_survival<-read.csv("./data/data-working/tank-survival-exp.csv",header=TRUE)
temp<-read.csv("./data/data-working/temperature-exp.csv",header=TRUE)
tanks<-read.csv("./data/data-working/tank-assignments-exp.csv")

# ----load pacakges -----
library(car)
library(MASS)
library(lattice)
library(lubridate)
library(stats)
library(lme4)
library(lmtest)
library(rcompanion)
library(multcompView)
library(emmeans)
library(strucchange)
library(tidyverse)
library(reshape2)
library(ggpubr)

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
  summarise(kavg=mean(k),sdk=sd(k),sek=sdk/sqrt(n()),temp=mean(temperature))%>%
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
           julian_date==30 | julian_date==58 | julian_date == 84 | 
           julian_date == 86 | julian_date==114)

# ---- LC Models ----
lc.m1<-lm(kavg~ration*size*julian_date,data=lc)
lc.m1
plot(lc.m1)
summary(lc.m1)
Anova(lc.m1,type="III")

# get rid of three way interaction
lc.m2<-lm(kavg~(ration+size+julian_date)^2,data=lc)
lc.m2
plot(lc.m2)
summary(lc.m2)
Anova(lc.m2,type="III")

hist(resid(lc.m2))

lc.m3<-lm(kavg~ration+size+julian_date+ration*julian_date+size*julian_date,
          data=lc)

lc.m3
plot(lc.m3)
hist(resid(lc.m3))
summary(lc.m3)
Anova(lc.m3,type="III")

lc.m4<-lmer(kavg~ration+size+ration*julian_date+size*julian_date+(1|tank),data=lc)
lc.m4
plot(lc.m4)
hist(resid(lc.m4))
qqnorm(resid(lc.m4))
summary(lc.m4)
Anova(lc.m4,type="III")
table1<-lc%>%group_by(julian_date,ration)%>%
  summarise(mean=mean(kavg))

# ---- Live condition figures -----
limitse<-aes(ymin=kavg-sek,ymax=kavg+sek)
LCA<-lc%>%
  rename(Ration=ration)%>%
  filter(size=='small')%>%
  ggplot(aes(x=julian_date,y=kavg,colour=Ration))+
  geom_jitter(aes(shape=Ration,colour=Ration,fill=Ration))+
  stat_smooth(method="lm",se=FALSE,linetype='dashed')+
  geom_errorbar(limitse,width=1)+
  theme_bw(base_rect_size = 1)+
  ylab("Condition factor (K)")+xlab("Day of experiment")+
  scale_colour_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_fill_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_shape_manual(values=c(22:25))+
  theme(panel.grid=element_blank())+
  ylim(0.55,1.05)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
LCB<-lc%>%
  rename(Ration=ration)%>%
  filter(size=='large')%>%
  ggplot(aes(x=julian_date,y=kavg,colour=Ration))+
  geom_jitter(aes(shape=Ration,colour=Ration,fill=Ration))+
  stat_smooth(method="lm",se=FALSE,linetype='dashed')+
  geom_errorbar(limitse,width=1)+
  theme_bw(base_rect_size = 1)+
  ylab("Condition factor (K)")+xlab("Day of experiment")+
  scale_colour_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_fill_manual(values=c('grey0','grey25','grey39','grey64'))+
  scale_shape_manual(values=c(22:25))+
  theme(panel.grid=element_blank())+
  ylim(0.55,1.05)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))



ggarrange(LCA,LCB+theme(axis.title.y=element_text(colour='white')), labels=c("A","B"),ncol=2,nrow=1,
          common.legend=TRUE,legend='top')

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
  summarise(dry=mean(kdry),wet=mean(kwet),hsi=mean(HSI),
            sedry=(sd(kdry)/sqrt(n())),
            sewet=(sd(kwet)/sqrt(n())),
            sehsi=sd(HSI/sqrt(n())))
View(finalK)


# Dry condition model analysis

#model dry condition
d.m0<-lm(dry~percent*size,data=finalK)
plot(d.m0)
hist(resid(d.m0))
qqnorm(resid(d.m0))
exp(logLik(d.m0))

# not linear....

# GLMM

d.m1<-glm(dry~percent*size,data=finalK,
            family=Gamma(link = "log"))
d.m1
summary(d.m1)
plot(d.m1)
hist(resid(d.m1))
qqnorm(resid(d.m1))
Anova(d.m1,type = "III")
exp(logLik(d.m1))

d.m2<-glm(dry~ration+size,data=finalK,
            family=Gamma(link = "log"))

d.m2
summary(d.m2)
plot(d.m2)
hist(resid(d.m2))
qqnorm(resid(d.m2))
Anova(d.m2,type="III")

table2<-finalK%>%
  group_by(ration)%>%
  summarise(mean(dry),mean(wet))



# model d.m2, glm with gamma distribution and loglink

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

limitse.dryK<-aes(ymin=dry-sedry,ymax=dry+sedry)
finalK%>%
  mutate(percent_adj=percent-0.05)%>%
  mutate(percent_adj=replace(percent_adj,size=="large",percent+0.05))%>%
  rename(Size=size)%>%
  ggplot(aes(x=percent_adj,y=dry,fill=Size))+
  geom_errorbar(limitse.dryK,width=.025)+
  geom_pointrange(aes(shape=Size,ymin=dry-sedry,ymax=dry+sedry),size=.5)+
  theme_bw(base_rect_size = 1)+
  ylab("Condition factor (K)")+xlab("Food ration (% body weight)")+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:25))+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))


# Wet condition factor

w.m1<-lm(wet~percent*size,data=finalK)
w.m1
summary(w.m1)
plot(w.m1)
hist(resid(w.m1))
qqnorm(resid(w.m1))
Anova(w.m1,type = "III")

#GLMM
w.m2<-glm(wet~percent*size,data=finalK,
            family = Gamma(link = "log"))
plot(w.m2)
hist(resid(w.m2))
qqnorm(resid(w.m2))

#use dry condition for results (proven to be more reliaable)

# ----- Day 0 HSI -------
d0HSI<-day0%>%
  select(sl_mm,HSI,size)%>%
  data.frame()
day0%>%
  group_by(size)%>%
  summarise(mean=mean(HSI))

# ---- HSI -----

hsi.m1<-lm(hsi~percent*size,data=finalK)
hsi.m1
plot(hsi.m1)
summary(hsi.m1)

hist(resid(hsi.m1))
qqnorm(resid(hsi.m1))
Anova(hsi.m1,type = "III")


# Model 1 is best


# use terminated only
unique(finalcond$julian_date)
endcond<-finalcond%>%
  filter(julian_date==80 | julian_date == 84 | julian_date == 114)%>%
  group_by(tank,ration,size,percent)%>%
  summarise(hsi=mean(HSI),hsise=sd(HSI)/sqrt(n()))


hsi.m2<-lm(hsi~percent*size,data=endcond)
plot(hsi.m2)
hist(resid(hsi.m2))
qqnorm(resid(hsi.m4))

hsi.m3<-glm(hsi~percent*size,data=endcond,
              family = Gamma(link="log"))
plot(hsi.m3)
hist(resid(hsi.m3))


# hsi.m1! figure out how to interpret the interaction!

# ---- Survival -----

#Cacluate survival rate by tank
#group by treatment with st.error
survival<-tank_survival%>%
  group_by(tank,julian_date)%>%
  mutate(survival=(number/9))%>%
  ungroup()%>%
  group_by(tank,size,ration,julian_date)%>%
  summarise(perc_surv=mean(survival))%>%
  filter(perc_surv!=0)

# Visualize data
survival%>%
  filter(size=="small")%>%
  ggplot(aes(x=julian_date,y=perc_surv,linetype=ration))+geom_smooth()
survival%>%
  filter(size=="large")%>%
  ggplot(aes(x=julian_date,y=perc_surv,linetype=ration))+geom_smooth()

large.surv0<-survival%>%filter(size=="large" & ration == "0.0%")%>%
  as.data.frame()

l0ts<-ts(large.surv0,start=1,end=114,frequency=1,
                  deltat=1)
l2<-window(l0ts,start=1,end=114)
coint.res<-residuals(lm(perc_surv~1,data=l2))
coint.res<-stats::lag(ts(coint.res,start=1,end=114,freq=1),k=-1)
l2<-cbind(l0ts,diff(l2),coint.res)
l2<-window(l2,start=20,end=114)
colnames(l2)<-c("tank","size","ration","perc_surv",
                "julian_date","diff.tank","diff.size","diff.ration",
                  "diff.perc_surv","diff.julian_date", "coint.res")
ecm.model<-diff.perc_surv~coint.res+1

l3<-window(l2,start=20,end=30)
me.efp<-efp(ecm.model,type="ME",data=l2,h=.5)
me.mefp<-mefp(me.efp,alpha=0.05)
l3<-window(l2,start=20,end=30)
me.mefp<-monitor(me.mefp)
plot(me.mefp)
me.mefp
temp3<-window(l2,start=20,end=40)
me.mefp<-monitor(me.mefp)
plot(me.mefp)
temp3<-window(l2,start=20,end=50)
me.mefp<-monitor(me.mefp)
plot(me.mefp)
temp3<-window(l2,start=20,end=60)
me.mefp<-monitor(me.mefp)
plot(me.mefp)
temp3<-window(l2,start=20,end=70)
me.mefp<-monitor(me.mefp)
plot(me.mefp)
temp3<-window(l2,start=20,end=80)
me.mefp<-monitor(me.mefp)
plot(me.mefp)

# ----- survival models ----

# GLS
s.m1<-gls(perc_surv~size*ration*julian_date,data=survival)
plot(s.m1)

s.m2<-lm(perc_surv~size*ration*julian_date,data=survival)
plot(s.m2)

s.m3<-glm(perc_surv~size*ration*julian_date,data=survival,family=Gamma(link="log"))
plot(s.m3)
glimpse(survival)
survival$tank<-as.factor(survival$tank)
s.m4<-glmer(perc_surv~size*ration*julian_date+(ration|tank)+(size|tank),
            family=Gamma(link="log"),data=survival)
plot(s.m4)


# split into increments of 30 days
# survival at day 20, 40, 60, 80
S<-survival%>%
  filter(julian_date==0 | julian_date==23 | 
           julian_date==46 | 
           julian_date ==69| julian_date == 114)%>%
  mutate(percent=0.0)%>%
  mutate(percent=replace(percent,ration=="0.5%",0.5))%>%
  mutate(percent=replace(percent,ration=="1.0%",1.0))%>%
  mutate(percent=replace(percent,ration=="2.0%",2.0))
typeof(S$julian_date)
S$julian_date<-as.factor(S$julian_date)

s.m5<-lm(perc_surv~ration+size+factor(julian_date),data=S)
plot(s.m5)
hist(resid(s.m5))
qqnorm(resid(s.m5))
Anova(s.m5,type = "III")

s.m6<-glm(perc_surv~ration+size+factor(julian_date),data=S,
          family=Gamma(link = "log"))
plot(s.m6)
hist(resid(s.m6))
Anova(s.m6,type="III")

S<-survival%>%
  filter(julian_date==0 | julian_date==30 | 
           julian_date==60 | 
           julian_date ==90| julian_date == 114)%>%
  mutate(percent=0.0)%>%
  mutate(percent=replace(percent,ration=="0.5%",0.5))%>%
  mutate(percent=replace(percent,ration=="1.0%",1.0))%>%
  mutate(percent=replace(percent,ration=="2.0%",2.0))
typeof(S$julian_date)
S$julian_date<-as.factor(S$julian_date)

s.m5<-lm(perc_surv~ration+size+factor(julian_date),data=S)
plot(s.m5)
hist(resid(s.m5))
qqnorm(resid(s.m5))
Anova(s.m5,type = "III")

s.m6<-glm(perc_surv~ration+size+factor(julian_date),data=S,
          family = Gamma(link = "log"))
plot(s.m6)
hist(resid(s.m6))
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
sgrw.m1<-lm(sgr_w~ration*size,data=sgr_all)

plot(sgrw.m1)
hist(resid(sgrw.m1))
qqnorm(resid(sgrw.m1))
Anova(sgrw.m1,type="III")
sgrw.m2<-lm(sgr_w~ration+size,data=sgr_all)
plot(sgrw.m2)
hist(resid(sgrw.m2))
Anova(sgrw.m2,type="III")
summary(sgrw.m2)

sgrw.m3<-lm(sgr_w~ration,data=sgr_all)
plot(sgrw.m3)
hist(resid(sgrw.m3))
Anova(sgrw.m3,type="III")
summary(sgrw.m3)

exp(logLik(sgrw.m1))
exp(logLik(sgrw.m2))
exp(logLik(sgrw.m3))

dfsubset<-sgr_all%>%filter(ration!="0.0%")
sgrw.m4<-lm(sgr_w~ration+size,data=dfsubset)
plot(sgrw.m4)
Anova(sgrw.m4,type="III")
summary(sgrw.m4)

# Length
sgrl.m1<-lm(sgr_sl~ration*size,data=sgr_all)
sgrl.m1
summary(sgrl.m1)
plot(sgrl.m1)
Anova(sgrl.m1,type="III")

# take out size
sgrl.m2<-lm(sgr_sl~ration+size,data=sgr_all)

plot(sgrl.m2)
hist(resid(sgrl.m2))
Anova(sgrl.m2,type="III")
summary(sgrl.m2)




# model 2 is better!


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

sgr_adjusted%>%
  mutate(percent_adj=percent-0.05)%>%
  mutate(percent_adj=replace(percent_adj,size=="large",percent+0.05))%>%
  rename(Size=size)%>%
  ggplot(aes(x=percent_adj,y=sgr_weight,fill=Size))+
  geom_pointrange(aes(shape=Size,ymin=sgr_weight-sgr_se_w,ymax=sgr_weight+sgr_se_w),size=.5)+
  theme_bw(base_rect_size = 1)+
  ylab("Condition factor (K)")+xlab("Food ration (% body weight)")+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:25))+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
sgr_adjusted%>%
  mutate(percent_adj=percent-0.05)%>%
  mutate(percent_adj=replace(percent_adj,size=="large",percent+0.05))%>%
  rename(Size=size)%>%
  ggplot(aes(x=percent_adj,y=sgr_length,fill=Size))+
  geom_pointrange(aes(shape=Size,ymin=sgr_length-sgr_se_sl,ymax=sgr_length+sgr_se_sl),size=.5)+
  theme_bw(base_rect_size = 1)+
  ylab("Condition factor (K)")+xlab("Food ration (% body weight)")+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:25))+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))

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
