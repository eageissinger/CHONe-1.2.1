# Post analysis for newman condition
# Use full dataset to evaluate relationship between pre-winter K
# and pulse structure, settlement time, and size

# set working directory
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/condition-field/")
# load data
condition<-read.csv("../data/output/condition-field-clean.csv")
settletime<-read.csv("../data/output/settlement-day.csv")
winter<-read.csv("../data/output/winter-start-end.csv")
winter.sum<-read.csv("../data/output/newman-winter-summary.csv")

#load packages
library(lme4)
library(tidyverse)
library(car)
library(ggpubr)
library(lubridate)
#check data
head(condition)
summary(condition)


head(settletime)


condition2<-condition%>%
  mutate(fulton=replace(fulton,ID==1615,0.7912681))%>%
  filter(fulton<1.7)%>%
  filter(fulton>0.09)%>%
  filter(pulse<5)%>%
  left_join(settletime)
summary(condition2)
# K on pulse, size, and settlement day and month
K.pulse<-lmer(fulton~mmSL+(1|cohort),
              data=filter(condition2,month==10 & age == 0))
plot(K.pulse)
hist(resid(K.pulse))
qqnorm(resid(K.pulse))
qqline(resid(K.pulse),col='red')

Anova(K.pulse)
summary(K.pulse)

a<-condition2%>%
  filter(month==10 & age==0)%>%
  ggplot()+
  geom_smooth(aes(x=year,y=fulton,colour=factor(pulse)))+
  theme_bw()+
  ggtitle("October")

b<-condition2%>%
  filter(month==11 & age==0)%>%
  ggplot()+
  geom_smooth(aes(x=year,y=fulton,colour=factor(pulse)))+
  theme_bw()+
  ggtitle("November")
ggarrange(a,b,ncol=1,nrow=2,common.legend = TRUE)

c<-condition2%>%
  filter(month==5 & age==1)%>%
  ggplot()+
  geom_smooth(aes(x=year,y=fulton,colour=factor(pulse)))+
  theme_bw()+
  ggtitle("May")

d<-condition2%>%
  filter(month==7 & age==1)%>%
  ggplot()+
  geom_smooth(aes(x=year,y=fulton,colour=factor(pulse)))+
  theme_bw()+
  ggtitle("July")
ggarrange(c,d,ncol=1,nrow=2,common.legend = TRUE)

# investigate sampling artefacts
# sampling date
# days since end of winter
# sample size

head(condition2)
summary(condition2)
head(winter)

#format start and end data
winter2<-winter%>%
  mutate(start.date=ymd(paste(year.start,month.start,day.start,sep="-")),
         end.date=ymd(paste(year.end,month.end,day.end,sep="-")))%>%
  select(year,start.date,end.date)%>%
  mutate(end.julian=as.integer(yday(end.date)))


may<-condition2%>%
  filter(month==5 & age == 1)%>%
  select(year,month,pulse,fulton,date)%>%
  group_by(year,pulse,date)%>%
  summarise(fulton.mean=mean(fulton),sd=sd(fulton),n=n())%>%
  left_join(winter2)%>%
  mutate(j.date=as.integer(yday(date)))%>%
  mutate(days.since.winter=j.date-end.julian)%>%
  select(year,pulse,date,fulton.mean,sd,n,days.since.winter)
View(may)  

july<-condition2%>%
  filter(month==7 & age == 1)%>%
  select(year,month,pulse,fulton,date)%>%
  group_by(year,pulse,date)%>%
  summarise(fulton.mean=mean(fulton),sd=sd(fulton),n=n())%>%
  left_join(winter2)%>%
  mutate(j.date=as.integer(yday(date)))%>%
  mutate(days.since.winter=j.date-end.julian)%>%
  select(year,pulse,date,fulton.mean,sd,n,days.since.winter)
View(july)   



ggplot()+
  geom_smooth(data=filter(condition2,month==5 & age == 1),aes(x=year,y=fulton,colour=factor(pulse)))+
  geom_jitter(data=may,aes(x=year,y=days.since.winter/50,colour=factor(pulse)))+
  theme_bw()+
  ggtitle("May")

ggplot()+
  geom_point(data=filter(condition2,pulse==1 & month==5 & age == 1),aes(x=year,y=fulton),colour='red')+
  geom_point(data=filter(may, pulse==1),aes(x=year,y=days.since.winter/50),colour='blue')+
  theme_bw()+
  ggtitle("May")
ggplot()+
  geom_jitter(data=filter(condition2, month==5 & age == 1),
             aes(x=year,y=fulton,colour=factor(pulse)))+
  theme_bw()+
  ggtitle("May")
ggplot()+
  geom_boxplot(data=filter(condition2, month==5 & age == 1),
              aes(x=factor(year),y=fulton,fill=factor(pulse)))+
  theme_bw()+
  ggtitle("May")

ggplot()+
  geom_point(data=filter(condition2,pulse==1 & month==7 & age == 1),aes(x=year,y=fulton),colour='red')+
  theme_bw()+
  ggtitle("July")

#does pulse have an effect?
m1<-glm(fulton~factor(pulse)+year,data=filter(condition2,month==5 & age==1),
        family = Gamma(link = "log"))
plot(m1)
Anova(m1,type="III")

m2<-glm(fulton~factor(pulse)*year,data=filter(condition2,month==10 & age ==0),
        family=Gamma(link = "log"))
plot(m2)
Anova(m2,type='III')
m3<-glm(fulton~factor(pulse)*year,data=filter(condition2,month==11 & age==0),
        family=Gamma(link = "log"))
plot(m3)
Anova(m3,type="III")

# pulse matters in october
# pulse DOES NOT MATTER in May

ggplot()+
  geom_point(data=filter(condition2, month==5 & age == 1),
              aes(x=year,y=fulton))+
  theme_bw()+
  ggtitle("May")
ggplot()+
  geom_boxplot(data=filter(condition2, month==5 & age == 1),
             aes(x=factor(year),y=fulton))+
  theme_bw()+
  ggtitle("May")

condition2%>%
  filter(month== 10 & age ==0 )%>%
  ggplot()+
  geom_boxplot(aes(x=factor(year),y=fulton,fill=factor(pulse)))+
  theme_bw()+
  ggtitle("October")
condition2%>%
  filter(month== 5 &age==1)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(year),y=fulton,fill=factor(pulse)))+
  theme_bw()+
  ggtitle("May")


# winter and post-winter K
head(winter.sum)
head(condition2)

data2<-left_join(condition2,winter.sum)

m4<-glm(fulton~days_below_1+cohort,
        data=filter(data2,month==5),
        family=Gamma(link = "log"))
plot(m4)
Anova(m4,type="III")
summary(m4)

data2%>%
  filter(month== 5 &age==1)%>%
  ggplot()+
  geom_point(aes(x=cohort,y=fulton))+
  geom_line(aes(x=cohort,y=days_below_1/130),colour='blue')+
  theme_bw()+
  ggtitle("May")
data2%>%
  filter(month== 5 &age==1)%>%
  ggplot()+
  geom_point(aes(x=cohort,y=fulton))+
  geom_line(aes(x=cohort,y=mean_temp+.5),colour='blue')+
  theme_bw()+
  ggtitle("May")



ggplot(data2)+
  geom_line(aes(x=cohort,y=days_below_1/100),colour='red')+
  geom_line(aes(x=cohort,y=mean_temp+1),colour='blue')
