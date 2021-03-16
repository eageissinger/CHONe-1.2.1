# Settlement days
# determine settlement day for each pulse
# use methods from Ings et al. 2008

# set working directory
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/condition-field/")

# load packages 
library(tidyverse)
library(car)

# read in data
SL<-read.csv("../data/data-working/newman-length-updated.csv")

# check data
head(SL)

# select age-0 Atlantic cod
cod<-SL%>%
  filter(species=="AC")%>%
  filter(age==0)

# regression analysis
m1<-lm(mmSL~julian.date+factor(pulse)+factor(year),data=cod)
plot(m1)
hist(resid(m1))
summary(m1)
Anova(m1,type="III")

# minimum length by year and pulse
size<-cod%>%
  group_by(year,trip,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL))%>%
  ungroup()%>%
  group_by(year,pulse)%>%
  summarise(min=min(min))
# summary of length by year, trip, and pulse.
size.all<-cod%>%
  group_by(year,trip,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL))

# dataframe of the first trip each pulse settled with length stats
size2<-left_join(size,size.all)%>%
  select(year,trip,pulse,min,max,mean)

write.csv(size2,"../data/output/settlement.csv",na="",row.names=FALSE)

### determine timing

# settlement size is 39 mm SL (Ings et al. 2008)
#1996 pulse 1

m1$coefficients
test<-as.data.frame(m1$coefficients)
co<-data.frame(coefficient=c('Intercept','julian.date','pulse2','pulse3','pulse4','pulse5','pulse6',
      '1998','1999','2000','2001','2002','2003','2004','2005','2006','2007',
      '2008','2009','2010','2011','2012','2013','2014','2015','2016','2017',
      '2018'))
results<-bind_cols(co,test)%>%
  rename(estimate="m1$coefficients")%>%
  add_row(coefficient='pulse1',estimate=0, .before=3)%>%
  add_row(coefficient='1996',estimate=0, .before=9)

pulses<-data.frame(pulse=c(1,2,3,4,5,6),
                   Bpulse=c(0,-18.478996,-35.315267,-46.230024,-51.245488,-62.875015))%>%
  slice(rep(row_number(),22))

years<-results[9:30,]%>%
  rename(year=coefficient,Byear=estimate)%>%
  slice(rep(1:n(),each=6))
full<-bind_cols(pulses,years)%>%
  mutate(Intercept=-63.701036,
         JDay=0.434873,
         res=7.033)%>%
  select(Intercept,JDay,pulse,Bpulse,year,Byear,res)%>%
  mutate(settle=(Intercept+Bpulse+Byear-39)/(-JDay))
View(full)

settlement<-full%>%
  mutate(settle.yday=round(settle))%>%
  select(year,pulse,settle.yday)%>%
  mutate(year=as.integer(year))%>%
  mutate(leap=leap_year(year))%>%
  mutate(days.in.year=365)%>%
  mutate(days.in.year=replace(days.in.year,leap==TRUE,366))%>%
  mutate(decimal.day=settle.yday/days.in.year)%>%
  mutate(decimal.date=year+decimal.day)%>%
  mutate(date=date_decimal(decimal.date))%>%
  mutate(settle.year=str_sub(date,start=1,end=4),
         settle.month=str_sub(date,start=6,end=7),
         settle.day=str_sub(date,start=9,end=10))%>%
  mutate(settle.week=week(date))%>%
  select(year,pulse,settle.yday,settle.year,settle.month,settle.day,settle.week)
  
  
write.csv(settlement,"../data/output/settlement-day.csv",row.names=FALSE)
