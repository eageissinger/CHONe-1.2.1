#### Food consumption ####

# --- set working directory ----
setwd("C:/Users/USER/Documents/Research/CHONe-1.2.1/")

# ---- load data ----
total<-read.csv("./data/data-working/diet_feeding-exp.csv")
leftover<-read.csv("./data/data-working/diet_leftover-exp.csv")
temp<-read.csv("./data/data-working/temperature-exp.csv")
lw<-read.csv("./data/data-working/length-weight-exp.csv")
condition<-read.csv("./data/data-working/condition-exp.csv")
tanks<-read.csv("./data/data-working/tank-assignments-exp.csv")

# ---- packages ----
library(MASS)
library(strucchange)
library(timeSeries)
library(pscl)
library(boot)
library(car)
library(scales)
library(ggthemes)
library(lubridate)
library(tidyverse)
library(ggpubr)

# ---- data check ----
#total
str(total)
summary(total)
dim(total)
head(total)
names(total)

names(total)<-c('year','month','day','tank','size','trt','total_food','notes')

#leftover
str(leftover)
dim(leftover)
summary(leftover)
head(leftover)
names(leftover)

names(leftover)<-c('year','month','day','tank','size','trt','leftover_food')

#temp
str(temp)
summary(temp)
dim(temp)
names(temp)
head(temp)

names(temp)<-c('year','month','day','time','tank','temperature','notes')

#length-weight
str(lw)
summary(lw)
dim(lw)
names(lw)
head(lw)

names(lw)<-c('year','month','day','tank','size','ration','sl','weight','fish_num','notes')

#condition
str(condition)
summary(condition)
dim(condition)
names(condition)
head(condition)

names(condition)<-c("year",'month','day','time','tank','num','sl_mm','wet_total_weight_g',
                    'wet_liver_g','wet_evis_g','dry_liver_mg','dry_evis_g','mortality','notes',
                    'drying_notes')

# --- format date -----
#total
total$date<-ymd(paste(total$year,total$month,total$day,sep="-"))
total$julian_date<-yday(total$date)

#leftover
leftover$date<-ymd(paste(leftover$year,leftover$month,leftover$day,sep="-"))
leftover$julian_date<-yday(leftover$date)

#temp
temp$date<-ymd(paste(temp$year,temp$month,temp$day,sep="-"))
temp$julian_date<-yday(temp$date)

#lw
lw$date<-ymd(paste(lw$year,lw$month,lw$day,sep="-"))
lw$julian_date<-yday(lw$date)

lw<-lw%>%
  mutate(julian_date=replace(julian_date,julian_date>364,0))

condition$date<-ymd(paste(condition$year,condition$month,condition$day,sep="-"))
condition$julian_date<-yday(condition$date)
# ---- format data ----
# average weight/length by tank and day
lw<-lw%>%
  group_by(tank,ration,size,julian_date)%>%
  summarise(mean(sl),sd(sl),mean(weight),sd(weight),biomass=sum(weight))%>%
  rename(weight='mean(weight)',sl='mean(sl)',sl.sd='sd(sl)',weight.sd='sd(weight)')

# add final measurement from condition to lw
finalw<-condition%>%
  filter(julian_date==114 | julian_date == 84 | julian_date ==80)%>%
  group_by(tank,julian_date)%>%
  summarise(sl=mean(sl_mm),sl.sd=sd(sl_mm),weight=mean(wet_total_weight_g),weight.sd=sd(wet_total_weight_g),biomass=sum(wet_total_weight_g))
finalw<-left_join(finalw,tanks)
head(lw)
head(finalw)

lw_all<-bind_rows(lw,finalw)%>%rename(trt=ration)

#combine diet dataframes
diet<-left_join(total,leftover)
# create totals based on days measured
unique(lw_all$julian_date)
day30<-diet%>%filter(julian_date<=30)%>%
  group_by(tank,trt,size)%>%
  summarise(total=sum(total_food),leftover=sum(leftover_food))%>%
  mutate(julian_date=30)
day31<-diet%>%filter(julian_date<=31)%>%
  group_by(tank,trt,size)%>%
  summarise(total=sum(total_food),leftover=sum(leftover_food))%>%
  mutate(julian_date=31)
day58<-diet%>%filter(julian_date>31 & julian_date<=58)%>%
  group_by(tank,trt,size)%>%
  summarise(total=sum(total_food),leftover=sum(leftover_food))%>%
  mutate(julian_date=58)
day59<-diet%>%filter(julian_date>31 & julian_date<=59)%>%
  group_by(tank,trt,size)%>%
  summarise(total=sum(total_food),leftover=sum(leftover_food))%>%
  mutate(julian_date=59)
day80s<-diet%>%filter(julian_date>59 & julian_date<=80)%>%
  group_by(tank,trt,size)%>%
  summarise(total=sum(total_food),leftover=sum(leftover_food))%>%
  mutate(julian_date=80)
day84s<-diet%>%filter(julian_date>59 & julian_date<=84)%>%
  group_by(tank,trt,size)%>%
  summarise(total=sum(total_food),leftover=sum(leftover_food))%>%
  mutate(julian_date=84)
day86<-diet%>%filter(julian_date>59 & julian_date<=86)%>%
  group_by(tank,trt,size)%>%
  summarise(total=sum(total_food),leftover=sum(leftover_food))%>%
  mutate(julian_date=86)
day87<-diet%>%filter(julian_date>59 & julian_date<=87)%>%
  group_by(tank,trt,size)%>%
  summarise(total=sum(total_food),leftover=sum(leftover_food))%>%
  mutate(julian_date=87)
day114<-diet%>%filter(julian_date>87)%>%
  group_by(tank,trt,size)%>%
  summarise(total=sum(total_food),leftover=sum(leftover_food))%>%
  mutate(julian_date=114)


diet_sum<-bind_rows(day30,day31,day58,day86,day31,day59,day87,day114,day80s,day84s)
diet<-left_join(diet_sum,lw_all)%>%
  select(-percent)
day0<-filter(lw_all,julian_date==0)%>%
  select(-percent)
diet<-na.omit(diet)
diet<-bind_rows(day0,diet)
#create 'consumed' column
# consumed = total-(leftover*1.09)

#change trt to percent
diet$trt<-as.character(diet$trt)
diet$ration[diet$trt=="2.0%"]<-"0.02"
diet$ration[diet$trt=="1.0%"]<-"0.01"
diet$ration[diet$trt=="0.5%"]<-"0.005"
diet$ration[diet$trt=="0.0%"]<-"0.00"

diet$ration<-as.numeric(diet$ration)

diet<-diet%>%
  mutate(consumed=total-(leftover*1.09))%>%
  mutate(percent_consumed=((consumed/total)*ration)*100)%>%
  mutate(ration=ration*100)%>%
  data.frame()

diet$percent_consumed[diet$percent_consumed<0]<-0

diet$ration<-as.numeric(diet$ration)
str(diet$ration)

# combine temperature
# first, filter temperature days to start on january 1
temp1<-temp%>%
  filter(julian_date<300)%>%
  group_by(tank,julian_date,date)%>%
  summarise(mean_temp=mean(temperature))%>%
  group_by(date,julian_date)%>%
  summarise(daily_temp=mean(mean_temp))%>%
  ungroup()%>%
  data.frame()

temp_avg<-temp%>%
  filter(julian_date<300)%>%
  group_by(julian_date)%>%
  summarise(daily_temp=mean(temperature))%>%
  data.frame()

#combine data sets


diet_final<-left_join(diet,temp_avg)


#create new column % difference
diet_final<-diet_final%>%
  mutate(diff=ration-percent_consumed)%>%
  data.frame()



# ---- Visualize data ----


diet2.0<-diet_final%>%
  filter(trt=="2.0%")%>%
  ggplot(aes(x=julian_date,y=percent_consumed))+geom_point()+
  geom_line(aes(x=julian_date,y=daily_temp),size=1,col='red')
diet2.0

diet1.0<-diet_final%>%
  filter(trt=="1.0%")%>%
  ggplot(aes(x=julian_date,y=percent_consumed))+geom_point()+
  geom_line(aes(x=julian_date,y=daily_temp),size=1,col='red')
diet1.0

diet0.5<-diet_final%>%
  filter(trt=="0.5%")%>%
  ggplot(aes(x=julian_date,y=percent_consumed))+geom_point()+
  geom_line(aes(x=julian_date,y=daily_temp),size=1,col='red')
diet0.5



diet_final%>%
  filter(trt=="2.0%")%>%
  group_by(julian_date,daily_temp)%>%
  summarise(consumed=mean(percent_consumed))%>%
  ggplot(aes(x=julian_date,y=consumed))+geom_line()+
  geom_line(aes(x=julian_date,y=daily_temp),size=1,col='red')

diet_final%>%
  filter(trt=="1.0%")%>%
  group_by(julian_date,daily_temp)%>%
  summarise(consumed=mean(percent_consumed))%>%
  ggplot(aes(x=julian_date,y=consumed))+geom_line()+
  geom_line(aes(x=julian_date,y=daily_temp),size=1,col='red')

diet_final%>%
  filter(trt=="0.5%")%>%
  group_by(julian_date,daily_temp)%>%
  summarise(consumed=mean(percent_consumed))%>%
  ggplot(aes(x=julian_date,y=consumed))+geom_line()+
  geom_line(aes(x=julian_date,y=daily_temp),size=1,col='red')



# ---- Further analysis -----
# calculate all rations as percent differenc from 0.5%
diet2<-diet_final%>%
  mutate(food_eaten=percent_consumed-0.5)

diet2%>%
  filter(trt!="0.0%")%>%
  ggplot(aes(y=food_eaten,x=julian_date,colour=trt))+geom_point()+
  ylim(-.5,1.5)+
  xlab("Julian date")+
  ylab("Change from maintenance level (0.5%)")+
  geom_line(data=temp_avg,aes(x=julian_date,y=daily_temp),size=1,colour='blue')+
  theme_classic()+
  theme(axis.title.x=element_text(size=14,face='bold'))+
  theme(axis.title.y=element_text(size=14,face='bold'))+
  theme(axis.text.x=element_text(size=14,face='bold'))+
  theme(axis.text.y=element_text(size=14,face='bold'))

m1<-lm(percent_consumed~julian_date*ration*daily_temp,data=diet2)
summary(m1)
m1
plot(m1)

m2<-glm(percent_consumed~julian_date*ration*daily_temp,data = diet2,family=poisson)
summary(m2)
plot(m2)
Anova(m2)

m3<-glm(percent_consumed~julian_date+ration+daily_temp,data=diet2,family=poisson)
summary(m3)
plot(m3)
Anova(m3)

m4<-glmer(percent_consumed~julian_date*ration*daily_temp+(1+julian_date|tank),
          data=diet2,family = Gamma(link = log))

m4<-glm(percent_consumed~julian_date*daily_temp+ration,data=diet2,family=poisson)
summary(m4)
plot(m4)
Anova(m4)

anova(m2,m3,m4)
anova(m2)


# ---- breaking points----
# start with temperature (temp1)

# 2.0% ration
diet2<-diet_final%>%
  filter(trt=="2.0%")%>%
  select(date,tank,size,percent_consumed,daily_temp)%>%
  filter(!is.na(percent_consumed))%>%
  filter(!is.na(daily_temp))%>%
  group_by(date,daily_temp)%>%
  summarise(consumed=mean(percent_consumed))%>%
  ungroup()
diet2$day<-yday(diet2$date)
diet2temp<-diet2%>%
  select(daily_temp,consumed)

names(diet2.0)

temp_ts<-ts(diet2temp,start=1,end=112,frequency=1,
            deltat=1)

temp2<-window(temp_ts,start=1,end=112)
coint.res<-residuals(lm(consumed~daily_temp,data=temp2))
coint.res<-stats::lag(ts(coint.res,start=1,frequency = 1),k=-1)
temp2<-cbind(temp2,diff(temp2),coint.res)
temp2<-window(temp2,start=20,end=112)
colnames(temp2)<-c("temp","consumed","diff.temp","diff.consumed","coint.res")
ecm.model<-diff.consumed~coint.res+diff.temp

temp3<-window(temp2,start=20,end=50)
me.efp<-efp(ecm.model,type="ME",data=temp3,h=0.5)
me.mefp<-mefp(me.efp,alpha = 0.05)
temp3<-window(temp2,start=20,end=70)
me.mefp<-monitor(me.mefp)
plot(me.mefp)
me.mefp
me.plot2.0<-plot(me.mefp)
me.mefp2<-me.mefp

# 1.0% ration
diet1<-diet_final%>%
  filter(trt=="1.0%")%>%
  select(date,tank,size,percent_consumed,daily_temp)%>%
  filter(!is.na(percent_consumed))%>%
  filter(!is.na(daily_temp))%>%
  group_by(date,daily_temp)%>%
  summarise(consumed=mean(percent_consumed))%>%
  ungroup()
diet1$day<-yday(diet1$date)
diet1temp<-diet1%>%
  select(daily_temp,consumed)

temp_ts<-ts(diet1temp,start=1,end=112,frequency=1,
            deltat=1)
temp2<-window(temp_ts,start=1,end=112)
coint.res<-residuals(lm(consumed~daily_temp,data=temp2))
coint.res<-stats::lag(ts(coint.res,start=1,frequency = 1),k=-1)
temp2<-cbind(temp2,diff(temp2),coint.res)
temp2<-window(temp2,start=20,end=112)
colnames(temp2)<-c("temp","consumed","diff.temp","diff.consumed","coint.res")
ecm.model<-diff.consumed~coint.res+diff.temp

temp3<-window(temp2,start=20,end=50)
me.efp<-efp(ecm.model,type="ME",data=temp3,h=0.5)
me.mefp<-mefp(me.efp,alpha = 0.05)
temp3<-window(temp2,start=20,end=70)
me.mefp<-monitor(me.mefp)
plot(me.mefp)
me.mefp
me.plot1.0<-plot(me.mefp)
me.mefp1<-me.mefp

# 0.5% ration
diet05<-diet_final%>%
  filter(trt=="0.5%")%>%
  select(date,tank,size,percent_consumed,daily_temp)%>%
  filter(!is.na(percent_consumed))%>%
  filter(!is.na(daily_temp))%>%
  group_by(date,daily_temp)%>%
  summarise(consumed=mean(percent_consumed))%>%
  ungroup()
diet05$day<-yday(diet05$date)
diet05temp<-diet05%>%
  select(daily_temp,consumed)

temp_ts<-ts(diet05temp,start=1,end=112,frequency=1,
            deltat=1)
temp2<-window(temp_ts,start=1,end=112)
coint.res<-residuals(lm(consumed~daily_temp,data=temp2))
coint.res<-stats::lag(ts(coint.res,start=1,frequency = 1),k=-1)
temp2<-cbind(temp2,diff(temp2),coint.res)
temp2<-window(temp2,start=20,end=112)
colnames(temp2)<-c("temp","consumed","diff.temp","diff.consumed","coint.res")
ecm.model<-diff.consumed~coint.res+diff.temp

temp3<-window(temp2,start=20,end=50)
me.efp<-efp(ecm.model,type="ME",data=temp3,h=0.5)
me.mefp<-mefp(me.efp,alpha = 0.05)
temp3<-window(temp2,start=20,end=70)
me.mefp<-monitor(me.mefp)
me.mefp
plot(me.mefp)

me.mefp5<-me.mefp
me.plot05<-plot(me.mefp)

# All
diet00<-diet_final%>%
  filter(trt!="0.0%")%>%
  select(date,tank,size,percent_consumed,daily_temp)%>%
  filter(!is.na(percent_consumed))%>%
  filter(!is.na(daily_temp))%>%
  group_by(date,daily_temp)%>%
  summarise(consumed=mean(percent_consumed))%>%
  ungroup()
diet00$day<-yday(diet00$date)
diettemp<-diet00%>%
  select(daily_temp,consumed)

temp_ts<-ts(diettemp,start=1,end=112,frequency=1,
            deltat=1)
temp2<-window(temp_ts,start=1,end=112)
coint.res<-residuals(lm(consumed~daily_temp,data=temp2))
coint.res<-stats::lag(ts(coint.res,start=1,frequency = 1),k=-1)
temp2<-cbind(temp2,diff(temp2),coint.res)
temp2<-window(temp2,start=20,end=112)
colnames(temp2)<-c("temp","consumed","diff.temp","diff.consumed","coint.res")
ecm.model<-diff.consumed~coint.res+diff.temp

temp3<-window(temp2,start=20,end=50)
me.efp<-efp(ecm.model,type="ME",data=temp3,h=0.5)
me.mefp<-mefp(me.efp,alpha = 0.05)
temp3<-window(temp2,start=20,end=70)
me.mefp<-monitor(me.mefp)
me.mefp
plot(me.mefp)

me.mefp5<-me.mefp
me.plot05<-plot(me.mefp)

# ---- more analyses ----
diet3<-diet_final%>%
  filter(trt!="0.0%")%>%
  mutate(percent_consumed=replace(percent_consumed,percent_consumed=="NaN",0))
str(diet3)
diet3$trt<-as.factor(diet3$trt)
m1<-lm(percent_consumed~daily_temp*julian_date*size*trt,data=diet3)
m1
plot(m1)
hist(resid(m1))
summary(m1)
Anova(m1,type="III")


# 32, 866, 875
diet3[32,]
diet3[866,]
diet3[875,]
# hungry fish....

drop1(m1,test="Chisq")

m2<-lm(percent_consumed~daily_temp*size*trt,data=diet3)
plot(m2)
hist(resid(m2))
summary(m2) 
Anova(m2,type="III")
levels(diet3$trt)
drop1(m2,test="Chisq")

exp(logLik(m1))
logLik(m2)

m3<-lm(percent_consumed~daily_temp*trt,data=diet3)
plot(m3)
hist(resid(m3))
summary(m3)
Anova(m3,type="III")

m4<-lm(percent_consumed~daily_temp*size,data=diet3)
plot(m4)
hist(resid(m4))
summary(m4)
Anova(m4,type="III")

m5<-lm(percent_consumed~size,data=diet3)
plot(m5)
hist(resid(m5))
summary(m5)
Anova(m5,type="III")

m6<-lm(percent_consumed~daily_temp*trt,data=diet3)
plot(m6)
hist(resid(m6))
summary(m6)
Anova(m6,type="III")

m7<-lm(percent_consumed~julian_date*daily_temp*trt,data=diet3)
plot(m7)
hist(resid(m7))
summary(m7)
Anova(m7,type="III")


logLik(m1)
logLik(m2)
logLik(m3)
logLik(m4)
logLik(m5)
logLik(m6)
logLik(m7)

diet3%>%
  filter(trt=="0.5%")%>%
  filter(size=="small")%>%
  ggplot(aes(x=julian_date,y=percent_consumed,
                      colour=daily_temp))+
  geom_point()+
  geom_smooth(method='lm')



# ---- scale temperature data -----
summary(diet3)
diet3<-diet3%>%
  mutate(temp_scale=daily_temp+1)
summary(diet3)
min(diet3$temp_scale)
levels(diet3$trt)

diet4<-filter(diet3,!is.na(daily_temp))
min(diet4$percent_consumed)
m2<-glm(percent_consumed~temp_scale*size*trt,data=diet4,family=Gamma(link = "log"))

# manually do a hurdle model.....
diet4<-diet4%>%
  mutate(non_zero=ifelse(percent_consumed>0,1,0))

names(diet4)

ggplot(diet4,aes(y=percent_consumed,x=daily_temp,
                 colour=as.factor(non_zero)))+geom_point()
m1<-glm(non_zero~daily_temp*julian_date*trt*size,data=diet4,family=binomial(link=logit))
m2<-glm(percent_consumed~daily_temp*trt*size,data=subset(diet4,non_zero==1),family=Gamma(link=log))

plot(m1)
hist(resid(m2))
plot(m2)
hist(resid(m2))

# Final figures
diet_final%>%
  filter(trt!="0.0%")%>%
  ggplot(aes(x=julian_date,y=percent_consumed))+
  geom_smooth(method="lm")+
  geom_point(aes(x=daily_temp,y=percent_consumed))

diet_final%>%
  filter(trt!="0.0%")%>%
  ggplot(aes(x=julian_date,y=percent_consumed,colour=ration,shape=size))+
  geom_smooth(method='lm')

diet_final%>%
  filter(trt!="0.0%")%>%
  ggplot(aes(x=julian_date,y=percent_consumed))+geom_point()+
  facet_grid(.~size*trt*daily_temp)+stat_smooth(method='lm')



data.p1<-diet_final%>%
  filter(trt!="0.0%")%>%
  group_by(daily_temp,size,trt)%>%
  summarise(consumed=mean(percent_consumed),se=sd(percent_consumed)/sqrt(n()))%>%
  rename(Ration=trt)


ggplot(data.p1)+
  geom_jitter(aes(x=daily_temp,y=consumed,colour=Ration,shape=size),size=4)+
  geom_smooth(aes(x=daily_temp,y=consumed),method="lm",se=F,colour='red',
              size=1.5)+
  theme_bw(base_rect_size = 2)+
  scale_colour_manual(values=c('grey25','grey39','grey64'))+
  ylab("Food Consumed \n(% Body Weight)")+xlab("Daily Temperature (??C)")+
  theme(axis.text=element_text(size=20,face='bold'))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(axis.title=element_text(size=20))+
  theme(legend.position = c(0.15,0.7))+
  theme(legend.text = element_text(size=16))+
  theme(legend.key.size=unit(1,"cm"))+
  theme(panel.grid = element_blank())


blue.text<-element_text(colour='blue',size=20)
red.text<-element_text(colour='red',size=20)

diet_final%>%
  filter(trt=="2.0%")%>%
  group_by(julian_date,daily_temp)%>%
  summarise(consumed=mean(percent_consumed))%>%
  ggplot()+
  geom_line(aes(x=julian_date,y=consumed),size=1.5,col='red')+
  geom_line(aes(x=julian_date,y=daily_temp),size=1.5,col='blue')+
  theme_bw(base_rect_size = 2)+
  scale_y_continuous("Food consumed \n(% body weight)",sec.axis=sec_axis(~.*1,name="Temperature (??C)"))+
  xlab("Day of experiment")+
  theme(axis.text=element_text(size=18))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(axis.title=element_text(size=20))+
  theme(axis.title.y.left = red.text)+
  theme(axis.title.y.right=blue.text)+
  theme(axis.text = element_text(size=20,face='bold'))+
  theme(panel.grid = element_blank())+
  ggtitle("2.0%")+
  theme(plot.title = element_text(size=22,face='bold',hjust=0.5))


diet_final%>%
  filter(trt=="1.0%")%>%
  group_by(julian_date,daily_temp)%>%
  summarise(consumed=mean(percent_consumed))%>%
  ggplot()+
  geom_line(aes(x=julian_date,y=consumed),size=1.5,col='red')+
  geom_line(aes(x=julian_date,y=daily_temp),size=1.5,col='blue')+
  theme_bw(base_rect_size = 2)+
  scale_y_continuous("Food consumed \n(% body weight)",sec.axis=sec_axis(~.*1,name="Temperature (??C)"))+
  xlab("Day of experiment")+
  theme(axis.text=element_text(size=18))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(axis.title=element_text(size=20))+
  theme(axis.title.y.left = red.text)+
  theme(axis.title.y.right=blue.text)+
  theme(axis.text = element_text(size=20,face='bold'))+
  theme(panel.grid = element_blank())+
  ggtitle("1.0%")+
  theme(plot.title = element_text(size=22,face='bold',hjust=0.5))

diet_final%>%
  filter(trt=="0.5%")%>%
  group_by(julian_date,daily_temp)%>%
  summarise(consumed=mean(percent_consumed))%>%
  ggplot()+
  geom_line(aes(x=julian_date,y=consumed),size=1.5,col='red')+
  geom_line(aes(x=julian_date,y=daily_temp),size=1.5,col='blue')+
  theme_bw(base_rect_size = 2)+
  scale_y_continuous("Food consumed \n(% body weight)",sec.axis=sec_axis(~.*1,name="Temperature (??C)"))+
  xlab("Day of experiment")+
  theme(axis.text=element_text(size=18))+
  theme(legend.title=element_text(size=18,face='bold'))+
  theme(axis.title=element_text(size=20))+
  theme(axis.title.y.left = red.text)+
  theme(axis.title.y.right=blue.text)+
  theme(axis.text = element_text(size=20,face='bold'))+
  theme(panel.grid = element_blank())+
  ggtitle("0.5%")+
  theme(plot.title = element_text(size=22,face='bold',hjust=0.5))


diet3%>%
  filter(trt=="0.5%")%>%
  ggplot(aes(x=julian_date,y=percent_consumed,colour=daily_temp,shape=size))+
  geom_point(size=2)+
  geom_smooth(aes(linetype=size),method=lm,se=F,colour='black',
              size=1.2)+
  scale_colour_gradient(low = "blue", 
                        high = "grey",
                        name = "Temperature (??C)")+
  theme_bw(base_rect_size = 2)+
  theme(panel.grid = element_blank())+
  xlab("Day of experiment")+ylab("Food consumed \n(% body weight)")+
  theme(axis.text=element_text(size=18))+
  theme(legend.title=element_text(size=14,face='bold'))+
  theme(legend.text = element_text(size=12))+
  theme(axis.title=element_text(size=20))+
  theme(axis.text = element_text(size=20,face='bold'))+
  ggtitle("0.5%")+
  theme(plot.title = element_text(size=22,face='bold',hjust=0.5))


ggplot(diet3,aes(x=daily_temp,y=percent_consumed,shape=size,fill=size))+
  geom_point(size=2)+
  geom_smooth(aes(colour=size),method=lm,se=F,
              size=1.5)+
  facet_grid(~trt)+
  theme_bw(base_rect_size = 2)+
  theme(panel.grid = element_blank())+
  xlab("Temperature (??C)")+ylab("Food consumed \n(% body weight)")+
  theme(axis.text=element_text(size=18))+
  theme(legend.title=element_text(size=14,face='bold'))+
  theme(legend.text = element_text(size=12))+
  theme(axis.title=element_text(size=20))+
  theme(axis.text = element_text(size=20,face='bold'))+
  ggtitle("Food consumption")+
  theme(plot.title = element_text(size=22,face='bold',hjust=0.5))+
  theme(strip.text=element_text(size=14,face='bold'))+
  scale_fill_manual(values=c('grey55','lightskyblue'))+
  scale_colour_manual(values=c('grey0','dodgerblue4'))+
  scale_shape_manual(values=c(22:23))+
  theme(legend.position = c(0.1,.8))


# ---- Bioenergetics -----

# Total feed consumption (Ct)
# Ct=total feed supplied - total remaining feed

# Daily feeding rate (F%)
# F% = 100[C/((B1+B2)/2)](t2-t1)^-1
# C is feed consumption
# B1 and B2 are fish biomass on days t1 (start) and t2 (final)
# 100[consumption at time2/(time one weight+time 2 weight)/2](time2-time1)^-1

# Feed conversion efficiency (FCE)
# FCE = (B2-B1)/C


# Specific Growth rate (SGR)
# SGR = (e^g - 1)*100

# g is instantaneous growth coefficient
# g = (lnW2 - lnW1)(t2-t1)^-1

energy<-diet_final%>%
  group_by(tank)%>%
  mutate(F.rate=(100*(consumed/((lag(biomass)+biomass)/2))*(julian_date-lag(julian_date))^-1))%>%
  mutate(FCE=(biomass-lag(biomass))/consumed)%>%
  mutate(g=(log(weight)-log(lag(weight)))*(julian_date-lag(julian_date))^-1)%>%
  mutate(sgr=(exp(g)-1)*100)%>%
  ungroup()%>%
  as.data.frame()

energy%>%
  filter(trt=="0.0%")%>%
  ggplot(aes(x=julian_date,y=F.rate,colour=size))+geom_point() #tank 21 is weird due to mort

energy%>%
  filter(trt=="0.5%")%>%
  ggplot(aes(x=julian_date,y=F.rate,colour=size))+geom_point()

energy%>%
  filter(trt=="1.0%")%>%
  ggplot(aes(x=julian_date,y=F.rate,colour=size))+geom_point()#somethign is off

energy%>%
  filter(trt=="2.0%")%>%
  ggplot(aes(x=julian_date,y=F.rate,colour=size))+geom_point()

energy%>%
  filter(trt=="0.0%")%>%
  ggplot(aes(x=julian_date,y=FCE,colour=size))+geom_point()
energy%>%
  filter(trt=="0.5%")%>%
  ggplot(aes(x=julian_date,y=FCE,colour=size))+geom_point()
energy%>%
  filter(trt=="1.0%")%>%
  ggplot(aes(x=julian_date,y=FCE,colour=size))+geom_point()
energy%>%
  filter(trt=="2.0%")%>%
  ggplot(aes(x=julian_date,y=FCE,colour=size))+geom_point()

energy%>%
  filter(trt=="0.0%")%>%
  ggplot(aes(x=julian_date,y=sgr,colour=size))+geom_point()
energy%>%
  filter(trt=="0.5%")%>%
  ggplot(aes(x=julian_date,y=sgr,colour=size))+geom_point()
energy%>%
  filter(trt=="1.0%")%>%
  ggplot(aes(x=julian_date,y=sgr,colour=size))+geom_point()
energy%>%
  filter(trt=="2.0%")%>%
  ggplot(aes(x=julian_date,y=sgr,colour=size))+geom_point()  

#Feed conversion efficiency
# remove -Inf and NaNs
energy.0<-energy%>%
  filter(trt!="0.0%")%>%
  mutate(percent2=0.0)%>%
  mutate(percent2=replace(percent2,trt=="0.5%",2.5))%>%
  mutate(percent2=replace(percent2,trt=="1.0%", 4.9))%>%
  mutate(percent2=replace(percent2,trt=="2.0%", 9.8))
names(energy)
energy$FCE[which(is.nan(energy$FCE))] = NA
energy$FCE[which(is.infinite(energy$FCE))] = NA


m1<-lm(FCE~size+factor(trt)+daily_temp,data=energy.0)
plot(m1)
hist(resid(m1))
summary(m1)
Anova(m1,type="III")


# Feed Conversion Efficiency
head(energy.0)
ggplot(energy.0,aes(y=FCE,x=julian_date,shape=as.factor(ration),colour=size))+
  geom_point()

ggplot(energy.0,aes(y=FCE,x=julian_date,shape=as.factor(ration),colour=size))+
  geom_point()+
  facet_wrap(vars(as.factor(ration)))

# remove outliers
fce<-energy.0%>%
  filter(FCE>-2)

m1.2<-lm(FCE~0+factor(trt)+size+daily_temp,data=fce)
plot(m1.2)
hist(resid(m1.2))
summary(m1.2)
Anova(m1.2,type="III")

# take % out of treatment label
fce2<-fce%>%
  mutate(percent=as.numeric(str_sub(trt,start = 1,end = 3)))%>%
  mutate(percent2=0.0)%>%
  mutate(percent2=replace(percent2,trt=="0.5%",2.5))%>%
  mutate(percent2=replace(percent2,trt=="1.0%", 4.9))%>%
  mutate(percent2=replace(percent2,trt=="2.0%", 9.8))

ggplot(fce,aes(y=FCE,x=daily_temp,colour=size))+
  geom_point()+
  facet_wrap(vars(ration))
F1<-ggplot(fce2)+
  geom_boxplot(aes(y=FCE,x=as.factor(percent2)),
               outlier.shape = NA)+
  geom_jitter(aes(y=FCE,x=as.factor(percent2),colour=daily_temp),width=.15)+
  facet_wrap(vars(size=factor(size,levels=c("small","large"))))+
  ylab("Feed conversion efficiency (g · "~g^-1*")")+xlab("Ration (% body weight)")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_color_gradient(low="blue",high="red")+
  labs(colour="Temp. (°C)")+
  theme(axis.title = element_text(size=12))+
  theme(axis.text = element_text(size=10))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=15)))
F1

energy.0$F.rate[which(is.nan(energy.0$F.rate))] = NA
energy.0$F.rate[which(is.infinite(energy.0$F.rate))] = NA

m2<-lm(F.rate~0+factor(trt)+size+daily_temp,data=energy.0)
plot(m2)
hist(resid(m2))
summary(m2)
Anova(m2,type="III")

energy.02<-energy.0%>%
  mutate(percent=0.0)%>%
  mutate(percent=replace(percent,trt=="0.5%",2.5))%>%
  mutate(percent=replace(percent,trt=="1.0%", 4.9))%>%
  mutate(percent=replace(percent,trt=="2.0%", 9.8))
F2<-ggplot(energy.02)+
  geom_boxplot(aes(y=F.rate,x=as.factor(percent),colour=daily_temp))+
  geom_jitter(aes(y=F.rate,x=as.factor(percent),colour=daily_temp),width=.15)+
  facet_wrap(vars(size=factor(size,levels=c("small","large"))))+
  ylab("Feeding rate (% body weight · "~day^-1*")")+xlab("Ration (% body weight)")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_color_gradient(low="blue",high="red")+
  labs(colour="Temp. (°C)")+
  theme(axis.title = element_text(size=12))+
  theme(axis.text = element_text(size=10))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=15)))
F2
ggarrange(F2+theme(axis.title.x=element_text(colour='white')),F1, labels=c("A","B"),ncol=1,nrow=2,
          common.legend=TRUE,legend='right')


dflarge<-energy.0%>%
  filter(size=="large")
m3<-lm(F.rate~factor(trt)+daily_temp,data=dflarge)
plot(m3)
hist(resid(m3))
summary(m3)
Anova(m3,type="III")


dfsmall<-energy.0%>%
  filter(size=="small")
m4<-lm(F.rate~factor(trt)+daily_temp,data=dfsmall)
plot(m4)
hist(resid(m4))
summary(m4)
Anova(m4,type="III")


# ---- presentation figures ----
mid<-mean(fce$daily_temp)
B<-F1+
  theme(axis.title = element_text(size=14, face='bold'))+
  theme(axis.text = element_text(size=12,face='bold'))+
  theme(axis.text.x = element_text(angle=30,hjust=.7))+
  theme(legend.title=element_text(size=14,face='bold'))+
  theme(legend.text = element_text(size=12,face='bold'))+
  labs(colour="Temp. (°C)")

A<-F2+
  theme(axis.title = element_text(size=14, face='bold'))+
  theme(axis.text = element_text(size=12,face='bold'))+
  theme(axis.text.x = element_text(angle=30,hjust=.7))+
  theme(legend.title=element_text(size=14,face='bold'))+
  theme(legend.text = element_text(size=12,face='bold'))+
  labs(colour="Temp. (°C)")

ggarrange(A+
            theme(strip.text = element_blank()),
          B+
            theme(strip.text = element_blank()),
          ncol=2,nrow=1,
          common.legend=TRUE,legend='right')
