# emigration analysis
# brainstorm:
# determine if there is a length range between post-winter settlement
# determine if there is a temperature signal influencin post-witner settlement
# how to determine the signal:
# temperature signal: do age-1's come in in july when water is warmer?
# size signal: is there a size limit on May and July....

# --- packages ----
library(tidyverse)
library(lubridate)
library(MASS)
library(car)
library(lme4)
library(effects)
# load in data
length<-read.csv("./data/data-working/newman-length.csv")%>%
  mutate(date=ymd(paste(Year,Month,Day,sep = "-")))%>%
  filter(Species=="AC" & Age ==1)

tripdates<-length%>%
  dplyr::select(Year,Month,Day,Trip)
catch<-read.csv("./data/output/catch_haul.csv")%>%
  left_join(tripdates)%>%
  mutate(date=ymd(paste(Year,Month,Day,sep="-")))%>%
  filter(Age == 1)


catch.long<-catch%>%
  dplyr::select(Year,Trip,Age,total_catch,total_measured,
                Julian.Date,num_hauls,extrap_1,extrap_2,extrap_3,
                extrap_4,extrap_5,extrap_6,extrap_unknown,total,date)%>%
  gather(Pulse,count,8:13)%>%
  mutate(Pulse=as.integer(str_sub(Pulse,start=8,end=8)))
t<-read.csv("./data/data-working/newman-temp-to-2019.csv")%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  rename(Year=year,Month=month,Day=day)

# length signal
length%>%
  filter(Month==5)%>%
  ggplot()+
  geom_point(aes(x=Year,y=mmSL))

length%>%
  filter(Month==7)%>%
  ggplot()+
  geom_point(aes(x=Year,y=mmSL))

# catch
catch%>%
  filter(Month==5)%>%
  ggplot()+
  geom_point(aes(x=Year,y=total))

catch%>%
  filter(Month==7)%>%
  ggplot()+
  geom_point(aes(x=Year,y=total))
# temperature
t%>%
  filter(Month==5)%>%
  ggplot()+
  geom_point(aes(x=Year,y=daily_temp_C))

t%>%
  filter(Month==7)%>%
  ggplot()+
  geom_point(aes(x=Year,y=daily_temp_C))

# combine temperature and catch
tc<-catch%>%
  left_join(t)%>%
  dplyr::select(Year,Julian.Date,Trip,Month,date,Age,total_catch,total,extrap_1,daily_temp_C)%>%
  mutate(total=round(total))%>%
  mutate(extrap_1=round(extrap_1))
summary(tc)
names(tc)
tc.factor<-tc%>%
  mutate(Month=as.factor(Month))%>%
  filter(!is.na(extrap_1))

m0<-glmer.nb(extrap_1~scale(daily_temp_C)*Month+(1|Year),data=tc.factor)
plot(x=fitted(m0),y=resid(m0))
qqnorm(resid(m0))
qqline(resid(m0),col='red')
hist(resid(m0))
Anova(m0,type="III")
plot(allEffects(m0))

m1<-glmer.nb(extrap_1~daily_temp_C+(1|Year),data=filter(tc.factor,Month==5))
plot(x=fitted(m1),y=resid(m1))
qqnorm(resid(m1))
qqline(resid(m1))
hist(resid(m1))
Anova(m1,type="III")
summary(m1)
plot(allEffects(m1))

m2<-glmer.nb(extrap_1~daily_temp_C+(1|Year),data=filter(tc,Month==7))
plot(x=fitted(m2),y=resid(m2))
qqnorm(resid(m2))
qqline(resid(m2))
hist(resid(m2))
Anova(m2,type="III")
summary(m2)
plot(allEffects(m2))

m3<-glmer.nb(extrap_1~daily_temp_C+(1|Year),data=filter(tc,Month==8))
plot(x=fitted(m3),y=resid(m3))
qqnorm(resid(m3))
qqline(resid(m3))
hist(resid(m3))
Anova(m3,type="III")
summary(m3)
plot(allEffects(m3))

m4<-glmer.nb(total~daily_temp_C+(1|Year),data=filter(tc,Month==9))
plot(x=fitted(m4),y=resid(m4))
qqnorm(resid(m4))
qqline(resid(m4))
hist(resid(m4))
Anova(m4,type="III")
summary(m4)
plot(allEffects(m4))


# catch rates are lower during warm years (high temp = low catch rate for age 1)

# size analysis
tl<-length%>%
  left_join(t)

m3<-lm(mmSL~daily_temp_C,data=filter(tl,Month==5)) 
plot(x=fitted(m3),y=resid(m3))
qqnorm(resid(m3))
qqline(resid(m3))
hist(resid(m3))

Anova(m3,type="III")
summary(m3)
plot(allEffects(m3))

m4<-lm(mmSL~daily_temp_C,data=filter(tl,Month==7))
plot(x=fitted(m4),y=resid(m4))
qqnorm(resid(m4))
qqline(resid(m4),col='red')
hist(resid(m4))
Anova(m4,type="III")
summary(m3)
plot(allEffects(m4))

# catch and length
# create a dataframe with mean length and catch per haul to look
# at effect of tmperature and length on catch rate in May and July

head(tl)
data2<-length%>%
  group_by(Year,Trip,Pulse)%>%
  summarise(mmSL=mean(mmSL))%>%
  left_join(catch.long)%>%
  left_join(t)%>%
  mutate(Pulse=as.factor(Pulse))%>%
  mutate(count=round(count))%>%
  mutate(Month=as.factor(Month))
head(data2)

m5<-glmer(count~scale(mmSL)+Month+(1|Year),family=poisson(link="log"),
          data=data2)
plot(m5)
hist(resid(m5))
Anova(m5,type="III")
plot(allEffects(m5))

data2%>%
  filter(Month==7)%>%
  ggplot()+
  geom_point(aes(x=daily_temp_C,y=count,colour=factor(Pulse)))+
  facet_wrap(~Year)

data2%>%
  filter(Month==7)%>%
  ggplot()+
  geom_point(aes(x=mmSL,y=count,colour=factor(Pulse)))+
  facet_wrap(~Year)
# polynomial temperature fit?

m6<-glmer(count~scale(mmSL)*scale(daily_temp_C)+(1|Year),family = poisson(link="log"),data=filter(data2,Month==7))
plot(m6)
hist(resid(m6))
Anova(m6,type="III")
plot(allEffects(m6))

# ------- Visual assessment -----
length<-read.csv("./data/data-working/newman-length.csv")%>%
  mutate(date=ymd(paste(Year,Month,Day,sep = "-")))
head(length)

# add cohort to length data
age0<-length%>%
  filter(Species=="AC" & Age ==0)%>%
  mutate(cohort=Year)
age1<-length%>%
  filter(Species=="AC" & Age == 1)%>%
  mutate(cohort=Year-1)
cod<-bind_rows(age0,age1)

cod%>%
  filter(cohort==2018)%>%
  ggplot()+
  geom_point(aes(x=date,y=mmSL,colour=factor(Pulse)))+
  scale_x_date(breaks='2 months',date_labels = "%d %b %Y")+
  theme(axis.text.x = element_text(angle=40,hjust=1))+
  geom_rect(data=filter(collection.range,cohort==2018),
            aes(xmin=min(date),
                xmax=max(date),
                ymin=-Inf,ymax=Inf),
            inherit.aes = FALSE, alpha=0.4,fill='lightgrey')

collection.range1<-cod%>%
  filter(Age==1)%>%
  dplyr::select(cohort,Year)%>%
  distinct()%>%
  mutate(month=7,
         day=1)%>%
  mutate(date=ymd(paste(Year,month,day,sep="-")))
collection.range2<-cod%>%
  filter(Age==1)%>%
  dplyr::select(cohort,Year)%>%
  distinct()%>%
  mutate(month=7,
         day=31)%>%
  mutate(date=ymd(paste(Year,month,day,sep="-")))
collection.range<-bind_rows(collection.range1,collection.range2)

cohort.list<-rev(unique(cod$cohort))
cohort.list<-unique(cod$cohort)
cohort.list<-cohort.list[order(cohort.list)]
cohort.list
for (i in seq_along(cohort.list)) {
 plot <- cod%>%
    filter(cohort==cohort.list[i])%>%
    ggplot()+
   geom_rect(data=filter(collection.range,cohort==cohort.list[i]),
             aes(xmin=min(date),
                 xmax=max(date),
                 ymin=-Inf,ymax=Inf),
             inherit.aes = FALSE, alpha=0.4,fill='lightgrey')+
    geom_point(aes(x=date,y=mmSL,colour=factor(Pulse)))+
    scale_x_date(breaks='2 months', date_labels = '%d %b %Y')+
    theme(axis.text.x = element_text(angle=40,hjust=1))+
    ggtitle(cohort.list[i])+
   scale_color_brewer(palette = "Dark2")
 print(plot)
}
