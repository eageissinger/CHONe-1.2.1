# Settlement days
# determine settlement day for each pulse
# use methods from Ings et al. 2008

# set working directory
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/condition-field/")

# load packages 
library(tidyverse)
library(car)

# read in data
SL<-read.csv("../data/data-working/newman-length.csv")

# check data
head(SL)

# select age-0 Atlantic cod
cod<-SL%>%
  filter(Species=="AC" & Age == 0)

# regression analysis
m1<-lm(mmSL~Julian.Date+factor(Pulse)+factor(Year),data=cod)
plot(m1)
hist(resid(m1))
summary(m1)
Anova(m1,type="III")

# minimum length by year and pulse
size<-cod%>%
  group_by(Year,Trip,Pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL))%>%
  ungroup()%>%
  group_by(Year,Pulse)%>%
  summarise(min=min(min))
# summary of length by year, trip, and pulse.
size.all<-cod%>%
  group_by(Year,Trip,Pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL))

# dataframe of the first trip each pulse settled with length stats
size2<-left_join(size,size.all)%>%
  select(Year,Trip,Pulse,min,max,mean)

write.csv(size2,"../data/output/settlement.csv",na="",row.names=FALSE)

### determine timing

# settlement size is 39 mm SL (Ings et al. 2008)
#1996 pulse 1

m1$coefficients
test<-as.data.frame(m1$coefficients)
co<-data.frame(coefficient=c('Intercept','Julian.Date','Pulse2','Pulse3','Pulse4','Pulse5','Pulse6',
      '1998','1999','2000','2001','2002','2003','2004','2005','2006','2007',
      '2008','2009','2010','2011','2012','2013','2014','2015','2016','2017',
      '2018','2019','2020'))
results<-bind_cols(co,test)%>%
  rename(estimate="m1$coefficients")%>%
  add_row(coefficient='Pulse1',estimate=0, .before=3)%>%
  add_row(coefficient='1996',estimate=0, .before=9)

pulses<-data.frame(Pulse=c(1,2,3,4,5,6),
                   B.Pulse=c(0,-18.9473924,-35.5566506,-46.9063645,-51.5125139,-63.1677266))%>%
  slice(rep(row_number(),24)) # repeat for each year

years<-as.data.frame(results[9:32,])%>%
  rename(Year=coefficient,B.Year=estimate)%>%
  slice(rep(1:n(),each=6))
full<-bind_cols(pulses,years)%>%
  mutate(Intercept=-64.0214789,
         JDay=0.4371169,
         res=7.11)%>%
  select(Intercept,JDay,Pulse,B.Pulse,Year,B.Year,res)%>%
  mutate(settle=(Intercept+B.Pulse+B.Year-39)/(-JDay))
View(full)

settlement<-full%>%
  mutate(settle.yday=round(settle))%>%
  select(Year,Pulse,settle.yday)%>%
  mutate(Year=as.integer(Year))%>%
  mutate(leap=leap_year(Year))%>%
  mutate(days.in.year=365)%>%
  mutate(days.in.year=replace(days.in.year,leap==TRUE,366))%>%
  mutate(decimal.day=settle.yday/days.in.year)%>%
  mutate(decimal.date=Year+decimal.day)%>%
  mutate(Date=date_decimal(decimal.date))%>%
  mutate(settle.year=as.integer(str_sub(Date,start=1,end=4)),
         settle.month=as.integer(str_sub(Date,start=6,end=7)),
         settle.day=as.integer(str_sub(Date,start=9,end=10)))%>%
  mutate(settle.week=week(Date))%>%
  select(Year,Pulse,settle.yday,settle.year,settle.month,settle.day,settle.week)
  
  
write.csv(settlement,"../data/output/settlement-day.csv",row.names=FALSE,na="")
