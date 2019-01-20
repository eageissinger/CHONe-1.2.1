#----Overwinter Growth and Condition Part 1-----

# Purpose
# Preliminary analysis with a focus on visual representation of the data

#load working directory
setwd("C:/Users/Emilie/Dropbox/Thesis/Research/CHONe-1.2.1/")

#----load data-----
condition<-read.csv("./data/data-working/condition-exp.csv",header=TRUE)
length_weight<-read.csv("./data/data-working/length-weight-exp.csv",header=TRUE)
tank_survival<-read.csv("./data/data-working/tank-survival-exp.csv",header=TRUE)
temp<-read.csv("./data/data-working/temperature-exp.csv",header=TRUE)
tanks<-read.csv("./data/data-working/tank-assignments-exp.csv")

# ----load pacakges -----
library(lubridate)
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)
library(stats)
library(lattice)
library(ggpmisc)

# --- check data -----
#length-weight
str(length_weight)
summary(length_weight)
head(length_weight)
tail(length_weight)
dim(length_weight)
names(length_weight) # need to fix name of first column
length_weight<-length_weight%>%rename(year=ï..year)

#condition
str(condition)
summary(condition)
head(condition)
tail(condition)
dim(condition)
names(condition)
condition<-condition%>%rename(year=ï..year)

#tank survival
str(tank_survival)
summary(tank_survival)
head(tank_survival)
tail(tank_survival)
dim(tank_survival)
names(tank_survival)
tank_survival<-tank_survival%>%rename(year=ï..year)

#temperature
str(temp)
summary(temp)
head(temp)
tail(temp)
dim(temp)
names(temp)
temp<-temp%>%rename(year=ï..year,notes=X)

# ---- Format date ------

# length-weight
length_weight$date<-ymd(paste(length_weight$year,length_weight$month,length_weight$day,sep="-"))
length_weight$julian_date<-yday(length_weight$date)

#condition

condition$date<-ymd(paste(condition$year,condition$month,condition$day,sep="-"))
condition$julian_date<-yday(condition$date)

#survival

tank_survival$date<-ymd(paste(tank_survival$year,tank_survival$month,tank_survival$day,sep="-"))
tank_survival$julian_date<-yday(tank_survival$date)

#temperature

temp$date<-ymd(paste(temp$year,temp$month,temp$day,sep="-"))
temp$julian_date<-yday(temp$date)

# ----- calculations and summary -----

# Temperature
temp<-left_join(temp,tanks) # combine tank assignments to temperature data

#summary table
tempdata<-temp%>%
  unite(group,size,ration,sep=' ')%>%
  group_by(group,julian_date,date)%>%
  summarise(temperature=mean(temperature))%>%
  ungroup()%>%
  data.frame()

tempdata<-separate(tempdata,group,c("size","ration"),sep=' ', convert=TRUE)

tempdata<-tempdata%>%
  mutate(julian_date=replace(julian_date,julian_date>365,0))%>% # set Dec 31 as day 0
  data.frame()

#Lenght-weight
# Cacluate Specific growth rate
# SGR=(lnwwt2-lnwwt1)/(t2-t1)x100

lw<-length_weight%>%
  mutate(julian_date=replace(julian_date,julian_date>364,0))%>%
  group_by(tank,julian_date)%>%
  summarise(mean_weight=mean(weight_g),sd_w=sd(weight_g),se_w=sd_w/sqrt(n()),
            mean_sl=mean(length_mm),sd_sl=sd(length_mm),se_sl=sd_sl/sqrt(n()))%>%
  ungroup()

#combine survival data with lw summary
tank_survival<-tank_survival%>%
  mutate(julian_date=replace(julian_date,julian_date>364,0)) # set dec31 as day 0
lw<-left_join(tank_survival,lw)
lw<-lw%>%
  filter(julian_date%in%c(0,30,31,58,59,86,87))
lw<-na.omit(lw)

# calculate SGR for weight and SL
# use mean weight and length for each tank
lw<-lw%>%
  group_by(tank)%>%
  mutate(sgr_w=(log(mean_weight)-log(lag(mean_weight)))/(julian_date-lag(julian_date))*100,
         sgr_sl=(log(mean_sl)-log(lag(mean_sl)))/(julian_date-lag(julian_date))*100)%>%
  ungroup()%>%
  data.frame()

lw<-left_join(lw,tanks) # add tank groupings to lw df

# Condition Summary
# caculate wet and dry condition factors
cond<-condition%>%
  mutate(kwet=100*wet_total_weight_g/(sl_mm*.1)^3)%>%
  mutate(kdry=1000*dry_evis_g/(sl_mm*.1)^3)%>%
  mutate(HSI=((dry_liver_mg*.001)/dry_evis_g)*1000)

#add size and rations
cond<-left_join(cond,tanks)

# Day 0 calculations
# new column labeling as day 0
day0<-cond%>%
  filter(date=="2016-12-16")%>%
  data.frame()
# add size group
day0$size[day0$sl_mm<=84]<-"small" # assign to small size class
day0$size[day0$sl_mm>=89]<-"large" # assign to large size class
day0$tank<-0

cond<-cond%>%
  filter(date>"2016-12-16")%>%
  data.frame()

fishcond<-bind_rows(day0,cond)
str(fishcond)

#Survival
#total starting number for large = 27, max for small = 54
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
#data summary - final condition for fish lasting until april
aprilcond<-fishcond%>%
  select(year,month,day,tank,size,ration,sl_mm,wet_total_weight_g,date,julian_date)%>%
  filter(month==4)%>%
  filter(day==24)%>%
  data.frame()
# condition for early terminated fish (mortalities)
earlyterm<-fishcond%>%
  select(year,month,day,tank,size,ration,sl_mm,wet_total_weight_g,date,julian_date)%>%
  filter(month==3)%>%
  filter(day%in%c(21,25))%>%
  data.frame()

names(aprilcond)
names(length_weight)
aprilcond<-rename(aprilcond,weight_g=wet_total_weight_g,length_mm=sl_mm)

names(earlyterm)
earlyterm<-rename(earlyterm,weight_g=wet_total_weight_g,length_mm=sl_mm)

livecond<-length_weight%>%
  select(year,month,day,tank,size,ration,length_mm,weight_g,date,julian_date)

# check that column names line up
names(aprilcond)
names(livecond)
names(earlyterm)

#combine to single dataframe
livecond<-rbind(livecond,earlyterm,aprilcond)

livecond<-livecond%>%
  mutate(julian_date=replace(julian_date,julian_date>364,0))%>% # day 364/365 = day 0
  mutate(k=100*weight_g/(length_mm*.1)^3)%>% # calculate condition
  group_by(tank,size,ration,julian_date)%>%
  summarise(kavg=mean(k),sd=sd(k),se=sd/sqrt(n()))%>% # average by tank
  ungroup()%>%
  data.frame()

# add temperature data
str(livecond)
str(tempdata)

tempdata$size<-as.factor(tempdata$size)
tempdata$ration<-as.factor(tempdata$ration)

livecond<-left_join(livecond,tempdata)
  
#visualize data
ggplot(livecond,aes(x=julian_date,y=kavg))+
         geom_point()
ggplot(livecond,aes(x=julian_date,y=kavg,colour=ration))+
  geom_point()
ggplot(livecond,aes(x=julian_date,y=kavg,colour=size))+
  geom_point()

#plot without size

limitse<-aes(ymin=kavg-se,ymax=kavg+se)

livecond.plot<-ggplot(livecond,aes(x=julian_date,y=kavg,colour=ration,linetype=size))+
  geom_jitter(aes(shape=ration,colour=ration,fill=ration),size=3)+
  stat_smooth(method="lm",size=1,se=FALSE)+
  geom_errorbar(limitse,width=1)+
  theme_classic()+
  ggtitle("Condition over time: All")+
  ylab("Condition")+xlab("Day of Experiment")+
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
livecond.plot

# plot by size
#small
livecond.small<-livecond%>%
  filter(size=="small")%>%
  ggplot(aes(x=julian_date,y=kavg,colour=ration))+
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
  scale_shape_manual(values=c(22:25))+
  ggtitle("Small Size Class")
livecond.small

#large
livecond.large<-livecond%>%
  filter(size=="large")%>%
  filter(julian_date!=80)%>%
  ggplot(aes(x=julian_date,y=kavg,colour=ration))+
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
  scale_shape_manual(values=c(22:25))+
  ggtitle("Large Size Class")
livecond.large

# ----- Final Condition analysis ------

#Summarise Data
finalcond<-fishcond%>%
  select(tank,sl_mm,date,kwet,kdry,HSI,size,ration,percent)%>%
  group_by(tank,percent,size)%>%
  summarise(kwet=mean(kwet),kdry=mean(kdry),hsi=mean(HSI),
            sd_kwet=sd(kwet),sd_kdry=sd(kdry),sd_hsi=sd(HSI),
            se_kwet=sd_kwet/sqrt(n()),se_kdry=sd_kdry/sqrt(n()),se_hsi=sd_hsi/sqrt(n()))%>%
  ungroup()%>%
  data.frame()

finalcond<-finalcond%>%
  filter(!is.na(percent))%>%
  data.frame()

# Dry condition model analysis

# Dry condition factor

dry<-fishcond%>%
  select(tank,size,percent,kdry)%>%
  filter(tank>0)%>%
  group_by(tank,size,percent)%>%
  summarise(kavg=mean(kdry),sd=sd(kdry),se=sd/sqrt(n()))%>%
  ungroup()

dry$size<-relevel(dry$size,"small")
levels(dry$size)

ggplot(dry,aes(x=percent,y=kavg))+
  geom_point()

ggplot(dry,aes(x=percent,y=kavg,colour=size))+
  geom_point()

#model dry condition
dry.m1<-lm(kavg~percent,data=dry)
summary(dry.m1)

plot(dry.m1)

#add size

dry.m2<-lm(kavg~percent*size,data=dry)
summary(dry.m2)

plot(dry.m2)

#SIZE INSIGNIFICANT

#let's try nonparametric regression.....








#################################
drylarge<-dry%>%
  filter(size=="large")%>%
  mutate(percent_adj=percent+0.1)%>%
  data.frame()

drysmall<-dry%>%
  filter(size=="small")%>%
  mutate(percent_adj=percent-0.1)%>%
  data.frame()

dry_adjusted<-bind_rows(drylarge,drysmall)

#when size=large, add .1 to percent

Fig3<-ggplot(dry_adjusted,aes(x=percent_adj,kavg,fill=size))+
  geom_pointrange(aes(shape=size,ymin=kavg-se,ymax=kavg+se),size=.75)+
  theme_classic()+
  xlab("Food ration (% body weight)")+ylab("Condition factor (K)")+
  theme(axis.title=element_text(size=22,face='bold'))+
  theme(legend.key=element_blank())+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(legend.position=c(0.9,0.15))+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:23))+
  ggtitle("Dry Condition")
Fig3


ggplot(dry,aes(x=percent,y=kavg))+
  geom_point()+theme_classic()+
  ggtitle("Dry weight")


xyplot(kavg~percent|size,data=dry,
       xlab="Ration",
       ylab="Condition Factor (K)")

dry.m1<-lm(kavg~percent*size,data=dry)
summary(dry.m1)

dry.m2<-lm(kavg~percent+size,data=dry)
summary(dry.m2)

plot(dry.m2)

TukeyHSD(dry.m2)

# Wet condition factor

wet<-fishcond%>%
  select(tank,size,ration,kwet)%>%
  filter(tank>0)%>%
  group_by(tank,size,ration)%>%
  summarise(kavg=mean(kwet),sd=sd(kwet),se=sd/sqrt(n()))%>%
  ungroup()%>%
  data.frame()

Fig3.1<-ggplot(wet,aes(x=ration,kavg,fill=size))+
  geom_boxplot(position='dodge')+
  theme_bw()+
  xlab("Ration")+
  ylab("Condition factor (K)")+
  ggtitle("Condition factor (K) - wet weight")+
  theme(axis.title=element_text(size=14))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(legend.key=element_blank())

dry.m1<-lm(kavg~ration*size,data=dry)
summary(dry.m1)

dry.m2<-lm(kavg~ration+size,data=dry)
summary(dry.m2)

plot(dry.m2)




# ----- Day 0 HSI -------
d0HSI<-day0%>%
  select(sl_mm,HSI,size)%>%
  data.frame()
day0%>%
  group_by(size)%>%
  summarise(mean=mean(HSI))

#HSI
hsi<-fishcond%>%
  select(tank,size,percent,HSI,ration)%>%
  filter(tank>0)%>%
  group_by(tank,size,percent,ration)%>%
  summarise(HSIavg=mean(HSI),sd=sd(HSI),se=sd/sqrt(n()))%>%
  ungroup()%>%
  data.frame() 

hsi$size<-relevel(hsi$size,"small")
levels(hsi$size)

hsi_large<-hsi%>%
  filter(size=="large")%>%
  mutate(percent_adj=percent+0.1)%>%
  data.frame()

hsi_small<-hsi%>%
  filter(size=="small")%>%
  mutate(percent_adj=percent-0.1)%>%
  data.frame()

hsi_adjusted<-bind_rows(hsi_large,hsi_small)


Fig4<-ggplot(hsi_adjusted,aes(x=percent_adj,y=HSIavg,shape=size))+
  geom_pointrange(aes(shape=size,fill=size,ymin=HSIavg-se,ymax=HSIavg+se),size=1)+
  theme_classic()+
  xlab("Food ration (% body weight)")+
  ylab("HSI")+
  ggtitle("Hepatosomatic Index")+
  theme(axis.title=element_text(size=22,face='bold'))+
  theme(legend.key=element_blank())+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(legend.title=element_text(size=18))+
  theme(legend.position=c(0.9,0.9))+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:23))
Fig4

Fig4.1<-ggplot(hsi,aes(x=ration,y=HSIavg,fill=size))+
  geom_boxplot(position='dodge')+
  xlab("Ration")+
  ylab("HSI")+
  ggtitle("Hepatosomatic Index")+
  theme_bw()+
  theme(axis.title=element_text(size=14))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(legend.key=element_blank())

hsi.m1<-lm(HSIavg~percent*size,data=hsi)
summary(hsi.m1)
plot(hsi.m1)

# ---- Survival -----

#Cacluate survival rate by tank
#group by treatment with st.error
survival<-tank_survival%>%
  group_by(tank,julian_date)%>%
  mutate(survival=(number/9)*100)%>%
  group_by(size,trt,julian_date)%>%
  summarise(avg_surv=mean(survival),
            se_surv=sd(survival)/sqrt(n()))



#create dataframe with tank averages
tanksurvival<-tank_survival%>%
  group_by(tank,julian_date)%>%
  mutate(survival=(number/9)*100)%>%
  group_by(tank,size,ration,julian_date)%>%
  summarise(avg_surv=mean(survival),
            se_surv=sd(survival)/sqrt(n()))

tank_by_trt<-tanksurvival%>%
  group_by(julian_date,ration,size)%>%
  summarise(surv=mean(avg_surv),se=sd(avg_surv)/sqrt(n()))%>%
  data.frame()

#create new data frames divided by size
mort_small<-tank_by_trt%>%
  filter(size=="small")%>%
  as.data.frame()

mort_large<-tank_by_trt%>%
  filter(size=="large")%>%
  as.data.frame()



#plot without model

mortality_small<-ggplot(data=mort_small, aes(x=julian_date, y=surv,linetype=ration,col=ration))+
  geom_smooth(size=1.5,se=FALSE)+
  theme_classic()+
  xlab("Day of Experiment")+ylab("% Survival")+
  ggtitle("Survival of small group")+
  ylim(0,105)+
  theme(legend.position=c(0.9,0.19))+
  theme(plot.title=element_text(size=20,face='bold'))+
  theme(axis.text.x=element_text(size=16,face='bold'))+
  theme(axis.text.y=element_text(size=16,face='bold'))+
  theme(legend.title=element_text(size=16,face='bold'))+
  theme(axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=NULL)+xlim(-.5,120)+
  scale_colour_manual(values=c('grey0','grey25','grey39','grey64'))
mortality_small

mortality_large<-ggplot(data=mort_large,aes(x=julian_date,y=surv,colour=ration,linetype=ration))+
  geom_smooth(size=1.5,se=FALSE)+
  theme_classic()+
  xlab("Day of Experiment")+ylab("% Survival")+
  ggtitle("Survival of large group")+
  theme(plot.title=element_text(size=20,face='bold'))+
  theme(axis.text.x=element_text(size=16,face='bold'))+
  theme(axis.text.y=element_text(size=16,face='bold'))+
  theme(legend.title=element_text(size=16,face='bold'))+
  theme(axis.title=element_text(size=18,face='bold'))+
  ylim(0,105)+
  theme(legend.position=c(0.9,.19))+
  theme(legend.key=element_blank())+
  scale_x_continuous(breaks=NULL)+xlim(-.5,120)+
  scale_colour_manual(values=c('grey0','grey25','grey39','grey64'))
mortality_large

summarise(tempdata,min=min(temperature),max=max(temperature))



# ---- Growth ------
sgrsum<-lw%>%
  filter(julian_date>0)%>%
  group_by(tank,size,percent)%>%
  summarise(sgr_length=mean(sgr_sl),sgr_sd_sl=sd(sgr_sl),sgr_se_sl=sgr_sd_sl/sqrt(n()),
            sgr_weight=mean(sgr_w),sgr_sd_w=sd(sgr_sl),sgr_se_w=sgr_sd_w/sqrt(n()))%>%
  data.frame()


sgrsum$size<-relevel(sgrsum$size,"small")
levels(sgrsum$size)

sgrlarge<-sgrsum%>%
  filter(size=="large")%>%
  mutate(percent_adj=percent+0.1)%>%
  data.frame()

sgrsmall<-sgrsum%>%
  filter(size=="small")%>%
  mutate(percent_adj=percent-0.1)%>%
  data.frame()

sgr_adjusted<-bind_rows(sgrlarge,sgrsmall)

#when size=large, add .1 to percent
#when size=small, subtract .1 from percent

Fig5<-ggplot(sgr_adjusted,aes(x=percent_adj,sgr_weight,fill=size))+
  geom_pointrange(aes(shape=size,ymin=sgr_weight-sgr_se_w,ymax=sgr_weight+sgr_se_w),size=.75)+
  theme_classic()+
  ggtitle("SGR: Weight")+
  ylab("SGR")+xlab("Ration (% body weight)")+
  theme(axis.title=element_text(size=20))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(legend.key=element_blank())+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(legend.position=c(0.9,0.15))+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:23))
Fig5

Fig6<-ggplot(sgr_adjusted,aes(x=percent_adj,sgr_length,fill=size))+
  geom_pointrange(aes(shape=size,ymin=sgr_length-sgr_se_sl,ymax=sgr_length+sgr_se_sl),size=.75)+
  theme_classic()+
  ggtitle("SGR: Length")+
  ylab("SGR")+xlab("Ration (% body weight)")+
  theme(axis.title=element_text(size=20))+
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5))+
  theme(axis.text.x=element_text(size=20,face='bold'))+
  theme(axis.text.y=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=18, face='bold'))+
  theme(legend.position=c(0.9,0.15))+
  scale_fill_manual(values=c('grey0','grey64'))+
  scale_shape_manual(values=c(22:23))
Fig6

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
