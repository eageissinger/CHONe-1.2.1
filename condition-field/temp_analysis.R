#Temperature exploratory analysis for Condition paper
# output of code is winter summary statistics for condition analysis

# --- set working directory ----
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/condition-field/")

# --- load packages ----
library(tidyverse)
library(lubridate)
library(gridExtra)

# --- load data -----
temp<-read.csv("../data/data-working/newman-temp-to-2019.csv")

# ---data check -----
summary(temp)
names(temp)
dim(temp)
str(temp)
head(temp)
tail(temp)


#----Fix dates -----
temp<-temp%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))


#----Visualize data -----
ggplot(temp,aes(x=date,y=daily_temp_C))+
  geom_line()

#Average monthly temperatures
monthly<-temp%>%
  group_by(month,year)%>%
  summarise(mean_temp=mean(daily_temp_C),stdev=sd(daily_temp_C),sterror=stdev/sqrt(n()))

sumstats<-monthly%>%
  filter(month%in%c(1,2,3,4))%>%
  group_by(year)%>%
  summarise(mean=mean(mean_temp),sd=sd(mean_temp),se=sd/sqrt(n()))%>%
  ungroup()

tempplot<-ggplot(monthly,aes(x=month,y=mean_temp))+
  geom_point()+
  geom_hline(yintercept=4,linetype="dashed",colour='red')+
  geom_hline(yintercept=1,linetype="dashed",colour='red')+
  theme_classic()

tempplot

#-----Determine winter duration by year-----
# steps for each year:
# 1. create a dataframe for the specified eyar
# 2. plot data
# 3. determine days below 1 degree
# 4. select start and end of winter. More than 3 consecutive days above 1 = end of winter
# 5. Using the start and end dates, calculate the average temperature of winter
# 6. Using the start and end dates, calculate number of days below 1 degree Celsius
# 7. repeat for each year


summary(temp)

#----1995-----
yr95<-temp%>%
  filter(year==1995)%>%
  data.frame()
plot95<-ggplot(yr95,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("1995")+
  theme_classic()+
  scale_x_date(date_breaks="1 month",date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle=20,vjust=.5))
plot95

#determine days below 1 degree
temp%>%
  filter(year==1995)%>%
  filter(daily_temp_C<1)%>%
  data.frame()
#Winter start = January 4, 1995
#winter end = June 20, 1995
#1995 was a clear year, no hovering around 1 degree

avg95<-temp%>%
  filter(date>="1995-01-04")%>%
  filter(date<"1995-06-20")%>%
  summarise(cohort=1994,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

days95<-temp%>%
  filter(date>="1995-01-04")%>%
  filter(date<"1995-06-20")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=1994,days_below_1=n())%>%
  data.frame()

min.max<-yr95%>%
  filter(date>="1995-01-20")%>%
  filter(date<"1995-06-20")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=1995)

# ----- 1996 ------
yr96<-temp%>%
  filter(date>="1995-10-1")%>%
  filter(date<="1996-9-30")%>%
  data.frame()
plot96<-ggplot(yr96,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("1996")+
  theme_classic()
plot96
  
#days below 1 degree
filter(yr96,daily_temp_C<1)
#winter start = January 1, 1996
#winter end =  April 1, 1996
#more than 3 consecutive days above 1 = end of winter

days96<-yr96%>%
  filter(date>="1996-1-1")%>%
  filter(date<"1996-4-1")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=1995,days_below_1=n())%>%
  data.frame()


avg96<-yr96%>%
  filter(date>="1996-1-1")%>%
  filter(date<"1996-4-1")%>%
  summarise(cohort=1995,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr96%>%
  filter(date>="1996-1-1")%>%
  filter(date<"1996-4-1")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=1996)%>%
  bind_rows(min.max)->min.max

# ----- 1997 -----
yr97<-temp%>%
  filter(date>="1996-10-01")%>%
  filter(date<="1997-9-30")%>%
  data.frame()
plot97<-ggplot(yr97,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("1997")+
  theme_classic()
plot97

filter(yr97,daily_temp_C<1)
#winter start = 4 January 1997
#winter end = 15 May 1997

days97<-yr97%>%
  filter(date>="1997-1-4")%>%
  filter(date<"1997-5-15")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=1996,days_below_1=n())%>%
  data.frame()

avg97<-yr97%>%
  filter(date>="1997-1-4")%>%
  filter(date<"1997-5-15")%>%
  summarise(cohort=1996,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr97%>%
  filter(date>="1997-1-4")%>%
  filter(date<"1997-5-15")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=1997)%>%
  bind_rows(min.max)->min.max


# ---- 1998 -----
yr98<-temp%>%
  filter(date>="1997-10-1")%>%
  filter(date<="1998-09-30")%>%
  data.frame()
plot98<-ggplot(yr98,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("1998")+
  theme_classic()
plot98

#no 1998 winter data

# ---- 1999 ------
yr99<-temp%>%
  filter(date>="1998-10-1")%>%
  filter(date<="1999-09-30")%>%
  data.frame()
plot99<-ggplot(yr99,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("1999")+
  theme_classic()
plot99

filter(yr99,daily_temp_C<1)
#winter start = 1 January 1999
#winter end = 26 March 1999
#there was a cold week but then 10  days of warm temps before it fully dropped
# for the end, I stuck with the more than 3 consecutive days above 1

days99<-yr99%>%
  filter(date>="1999-1-1")%>%
  filter(date<"1999-3-26")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=1998,days_below_1=n())%>%
  data.frame()

avg99<-yr99%>%
  filter(date>="1999-1-1")%>%
  filter(date<"1999-3-26")%>%
  summarise(cohort=1998,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr99%>%
  filter(date>="1999-1-1")%>%
  filter(date<"1999-3-26")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=1999)%>%
  bind_rows(min.max)->min.max

# ----- 2000 -----
yr00<-temp%>%
  filter(date>="1999-10-1")%>%
  filter(date<="2000-09-30")%>%
  data.frame()
plot00<-ggplot(yr00,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2000")+
  theme_classic()
plot00

filter(yr00,daily_temp_C<1)
#winter start = 29 December 1999
#winter end = 13 April 2000

days00<-yr00%>%
  filter(date>="1999-12-29")%>%
  filter(date<"2000-4-13")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=1999,days_below_1=n())%>%
  data.frame()

avg00<-yr00%>%
  filter(date>="1999-12-29")%>%
  filter(date<"2000-4-13")%>%
  summarise(cohort=1999,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr00%>%
  filter(date>="1999-12-29")%>%
  filter(date<"2000-4-13")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2000)%>%
  bind_rows(min.max)->min.max

# ----- 2001 -----
yr01<-temp%>%
  filter(date>="2000-10-1")%>%
  filter(date<="2001-09-30")%>%
  data.frame()
plot01<-ggplot(yr01,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2001")+
  theme_classic()
plot01

filter(yr01,daily_temp_C<1)
#winter start = 13 January 2001
#winter end = 20 May 2001

days01<-yr01%>%
  filter(date>="2001-1-13")%>%
  filter(date<"2001-5-20")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2000,days_below_1=n())%>%
  data.frame()

avg01<-yr01%>%
  filter(date>="2001-1-13")%>%
  filter(date<"2001-5-20")%>%
  summarise(cohort=2000,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr01%>%
  filter(date>="2001-1-29")%>%
  filter(date<"2001-5-20")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2001)%>%
  bind_rows(min.max)->min.max

# ----- 2002 -----
yr02<-temp%>%
  filter(date>="2001-10-1")%>%
  filter(date<="2002-09-30")%>%
  data.frame()
plot02<-ggplot(yr02,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2002")+
  theme_classic()
plot02

filter(yr02,daily_temp_C<1)
#winter start = 5 January 2002
#winter end = 15 April 2002

days02<-yr02%>%
  filter(date>="2002-1-5")%>%
  filter(date<"2002-4-15")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2001,days_below_1=n())%>%
  data.frame()

avg02<-yr02%>%
  filter(date>="2002-1-5")%>%
  filter(date<"2002-4-15")%>%
  summarise(cohort=2001,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr02%>%
  filter(date>="2002-1-5")%>%
  filter(date<"2002-4-15")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2002)%>%
  bind_rows(min.max)->min.max

# ----- 2003 ----
yr03<-temp%>%
  filter(date>="2002-10-1")%>%
  filter(date<="2003-09-30")%>%
  data.frame()
plot03<-ggplot(yr03,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2003")+
  theme_classic()
plot03

filter(yr03,daily_temp_C<1)
#winter start = 2 January 2003
#winter end = 3 May 2003

days03<-yr03%>%
  filter(date>="2003-1-2")%>%
  filter(date<"2003-5-3")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2002,days_below_1=n())%>%
  data.frame()

avg03<-yr03%>%
  filter(date>="2003-1-2")%>%
  filter(date<"2003-5-3")%>%
  summarise(cohort=2002,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr03%>%
  filter(date>="2003-1-2")%>%
  filter(date<"2003-5-3")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2003)%>%
  bind_rows(min.max)->min.max

# ---- 2004 -----
yr04<-temp%>%
  filter(date>="2003-10-1")%>%
  filter(date<="2004-09-30")%>%
  data.frame()
plot04<-ggplot(yr04,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2004")+
  theme_classic()
plot04

filter(yr04,daily_temp_C<1)
#winter start = 9 January 2004
#winter end = 3 April 2004

days04<-yr04%>%
  filter(date>="2004-1-9")%>%
  filter(date<"2004-4-3")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2003,days_below_1=n())%>%
  data.frame()

avg04<-yr04%>%
  filter(date>="2004-1-9")%>%
  filter(date<"2004-4-3")%>%
  summarise(cohort=2003,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr04%>%
  filter(date>="2004-1-9")%>%
  filter(date<"2004-5-3")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2004)%>%
  bind_rows(min.max)->min.max

# ----- 2005 -----
yr05<-temp%>%
  filter(date>="2004-10-1")%>%
  filter(date<="2005-09-30")%>%
  data.frame()
plot05<-ggplot(yr05,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2005")+
  theme_classic()
plot05

filter(yr05,daily_temp_C<1)
#winter start = 5 January 2005
#winter end = 28 April 2005

days05<-yr05%>%
  filter(date>="2005-1-5")%>%
  filter(date<"2005-4-28")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2004,days_below_1=n())%>%
  data.frame()

avg05<-yr05%>%
  filter(date>="2005-1-5")%>%
  filter(date<"2005-4-28")%>%
  summarise(cohort=2004,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr05%>%
  filter(date>="2005-1-5")%>%
  filter(date<"2005-4-28")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2005)%>%
  bind_rows(min.max)->min.max

# ----- 2006 -----
yr06<-temp%>%
  filter(date>="2005-10-1")%>%
  filter(date<="2006-09-30")%>%
  data.frame()
plot06<-ggplot(yr06,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2006")+
  theme_classic()
plot06

filter(yr06,daily_temp_C<1)
#winter start = 11 January 2006
#winter end = 1 April 2006
#need consulation on the end here.....

days06<-yr06%>%
  filter(date>="2006-1-11")%>%
  filter(date<"2006-4-1")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2005,days_below_1=n())%>%
  data.frame()

avg06<-yr06%>%
  filter(date>="2006-1-11")%>%
  filter(date<"2006-4-1")%>%
  summarise(cohort=2005,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr06%>%
  filter(date>="2006-1-11")%>%
  filter(date<"2006-4-1")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2006)%>%
  bind_rows(min.max)->min.max

# ----- 2007 -----
yr07<-temp%>%
  filter(date>="2006-10-1")%>%
  filter(date<="2007-09-30")%>%
  data.frame()
plot07<-ggplot(yr07,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2007")+
  theme_classic()
plot07

filter(yr07,daily_temp_C<1)

#winter start = 15 January 2007 - need to check on this one
#winter end = 20 April 2007

days07<-yr07%>%
  filter(date>="2007-1-15")%>%
  filter(date<"2007-4-20")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2006,days_below_1=n())%>%
  data.frame()

avg07<-yr07%>%
  filter(date>="2007-1-15")%>%
  filter(date<"2007-4-20")%>%
  summarise(cohort=2006,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()
  
yr07%>%
  filter(date>="2007-1-15")%>%
  filter(date<"2007-4-20")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2007)%>%
  bind_rows(min.max)->min.max

# ----- 2008 -----
yr08<-temp%>%
  filter(date>="2007-10-1")%>%
  filter(date<="2008-09-30")%>%
  data.frame()
plot08<-ggplot(yr08,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2008")+
  theme_classic()
plot08

filter(yr08,daily_temp_C<1)
#winter start = 14 Decemnber 2007
#winter end = 23 April 2008 

days08<-yr08%>%
  filter(date>="2007-12-14")%>%
  filter(date<"2008-4-23")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2007,days_below_1=n())%>%
  data.frame()

avg08<-yr08%>%
  filter(date>="2007-12-14")%>%
  filter(date<"2008-4-23")%>%
  summarise(cohort=2007,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr08%>%
  filter(date>="2007-12-14")%>%
  filter(date<"2008-4-23")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2008)%>%
  bind_rows(min.max)->min.max

# ---- 2009 ----
yr09<-temp%>%
  filter(date>="2008-10-1")%>%
  filter(date<="2009-09-30")%>%
  data.frame()
plot09<-ggplot(yr09,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2009")+
  theme_classic()
plot09

filter(yr09,daily_temp_C<1)
#winter start = 13 January 2009
#winter end = 21 April 2009 #### this one is clustered
# around 1 degree for a few days.... need consult

days09<-yr09%>%
  filter(date>="2009-1-13")%>%
  filter(date<"2009-4-21")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2008,days_below_1=n())%>%
  data.frame()

avg09<-yr09%>%
  filter(date>="2009-1-13")%>%
  filter(date<"2009-4-21")%>%
  summarise(cohort=2008,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr09%>%
  filter(date>="2009-1-13")%>%
  filter(date<"2009-4-21")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2009)%>%
  bind_rows(min.max)->min.max

# ----- 2010 -----
yr10<-temp%>%
  filter(date>="2009-10-1")%>%
  filter(date<="2010-09-30")%>%
  data.frame()
plot10<-ggplot(yr10,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2010")+
  theme_classic()
plot10

filter(yr10,daily_temp_C<1)
# Winter start = 10 January 2010  #def need to check on this one
# winter end = 26 March 2010

days10<-yr10%>%
  filter(date>="2010-1-10")%>%
  filter(date<"2010-3-26")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2009,days_below_1=n())%>%
  data.frame()

avg10<-yr10%>%
  filter(date>="2010-1-10")%>%
  filter(date<"2010-3-26")%>%
  summarise(cohort=2009,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr10%>%
  filter(date>="2010-1-10")%>%
  filter(date<"2010-3-26")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2010)%>%
  bind_rows(min.max)->min.max

# ------ 2011 --------
yr11<-temp%>%
  filter(date>="2010-10-1")%>%
  filter(date<="2011-09-30")%>%
  data.frame()
plot11<-ggplot(yr11,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2011")+
  theme_classic()
plot11

filter(yr11,daily_temp_C<1)
# winter start = 26 January 2011
# winter end = 1 April 2011

days11<-yr11%>%
  filter(date>="2011-1-26")%>%
  filter(date<"2011-4-1")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2010,days_below_1=n())%>%
  data.frame()

avg11<-yr11%>%
  filter(date>="2011-1-26")%>%
  filter(date<"2011-4-1")%>%
  summarise(cohort=2010,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr11%>%
  filter(date>="2011-1-26")%>%
  filter(date<"2011-4-1")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2011)%>%
  bind_rows(min.max)->min.max

# ----- 2012 ------
yr12<-temp%>%
  filter(date>="2011-10-1")%>%
  filter(date<="2012-09-30")%>%
  data.frame()
plot12<-ggplot(yr12,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2012")+
  theme_classic()
plot12

filter(yr12,daily_temp_C<1)
# winter start = 11 January 2012
# winter end = 9 April 2012

days12<-yr12%>%
  filter(date>="2012-1-11")%>%
  filter(date<"2012-4-9")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2011,days_below_1=n())%>%
  data.frame()

avg12<-yr12%>%
  filter(date>="2012-1-11")%>%
  filter(date<"2012-4-9")%>%
  summarise(cohort=2011,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr12%>%
  filter(date>="2012-1-11")%>%
  filter(date<"2012-4-9")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2012)%>%
  bind_rows(min.max)->min.max

#-----2013 ------
yr13<-temp%>%
  filter(date>="2012-10-1")%>%
  filter(date<="2013-9-30")%>%
  data.frame()
plot13<-ggplot(yr13,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2013")+
  theme_classic()
plot13

filter(yr13,daily_temp_C<1)
#winter start = 3 January 2013
# winter end = 14 April 2013

days13<-yr13%>%
  filter(date>="2013-1-3")%>%
  filter(date<"2013-4-14")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2012,days_below_1=n())%>%
  data.frame()

avg13<-yr13%>%
  filter(date>="2013-1-3")%>%
  filter(date<"2013-4-14")%>%
  summarise(cohort=2012,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr13%>%
  filter(date>="2013-1-3")%>%
  filter(date<"2013-4-14")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2013)%>%
  bind_rows(min.max)->min.max

# ----- 2014 ------
yr14<-temp%>%
  filter(date>="2013-10-1")%>%
  filter(date<="2014-9-30")%>%
  data.frame()
plot14<-ggplot(yr14,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2014")+
  theme_classic()
plot14

filter(yr14,daily_temp_C<1)
#winter start = 22 December 2013
# winter end = 22 April 2014

days14<-yr14%>%
  filter(date>="2013-12-22")%>%
  filter(date<"2014-4-22")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2013,days_below_1=n())%>%
  data.frame()

avg14<-yr14%>%
  filter(date>="2013-12-22")%>%
  filter(date<"2014-4-22")%>%
  summarise(cohort=2013,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr14%>%
  filter(date>="2013-12-22")%>%
  filter(date<"2014-4-22")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2014)%>%
  bind_rows(min.max)->min.max

# ---- 2015 ------
yr15<-temp%>%
  filter(date>="2014-10-1")%>%
  filter(date<="2015-9-30")%>%
  data.frame()

plot15<-ggplot(yr15,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2015")+
  theme_classic()
plot15

filter(yr15,daily_temp_C<1)
# winter start = 1 January 2015
# winter end = 25 April 2015

days15<-yr15%>%
  filter(date>="2015-1-1")%>%
  filter(date<"2015-4-25")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2014,days_below_1=n())%>%
  data.frame()

avg15<-yr15%>%
  filter(date>="2015-1-1")%>%
  filter(date<"2015-4-25")%>%
  summarise(cohort=2014,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr15%>%
  filter(date>="2015-1-1")%>%
  filter(date<"2015-4-25")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2015)%>%
  bind_rows(min.max)->min.max

# ----- 2016 ------
yr16<-temp%>%
  filter(date>="2015-10-1")%>%
  filter(date<="2016-9-30")%>%
  data.frame()

plot16<-ggplot(yr16,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2016")+
  theme_classic()
plot16

filter(yr16,daily_temp_C<1)
# winter start = 28 December 2015
# winter end = 16 April 2016

days16<-yr16%>%
  filter(date>="2015-12-28")%>%
  filter(date<"2016-4-16")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2015,days_below_1=n())%>%
  data.frame()

avg16<-yr16%>%
  filter(date>="2015-12-28")%>%
  filter(date<"2016-4-16")%>%
  summarise(cohort=2015,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr16%>%
  filter(date>="2015-12-28")%>%
  filter(date<"2016-4-26")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2016)%>%
  bind_rows(min.max)->min.max

# ----- 2017 ------
yr17<-temp%>%
  filter(date>="2016-10-1")%>%
  filter(date<="2017-9-30")%>%
  data.frame()

plot17<-ggplot(yr17,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2017")+
  theme_classic()
plot17

filter(yr17,daily_temp_C<1)
# winter start = 3 January 2017
# winter end = 6 May 2017

days17<-yr17%>%
  filter(date>="2017-01-03")%>%
  filter(date<"2017-05-06")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2016,days_below_1=n())%>%
  data.frame()

avg17<-yr17%>%
  filter(date>="2017-01-03")%>%
  filter(date<"2017-05-06")%>%
  summarise(cohort=2016,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr17%>%
  filter(date>="2017-01-03")%>%
  filter(date<"2017-05-06")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2017)%>%
  bind_rows(min.max)->min.max

# ----- 2018 ------
yr18<-temp%>%
  filter(date>="2017-10-1")%>%
  filter(date<="2018-9-30")%>%
  data.frame()

plot18<-ggplot(yr18,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2018")+
  theme_classic()
plot18

filter(yr18,daily_temp_C<1)
# winter start = 27 December 2017
# winter end = 28 April 2018

days18<-yr18%>%
  filter(date>="2017-12-27")%>%
  filter(date<"2018-04-28")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2017,days_below_1=n())%>%
  data.frame()

avg18<-yr18%>%
  filter(date>="2017-12-27")%>%
  filter(date<"2018-04-28")%>%
  summarise(cohort=2017,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr18%>%
  filter(date>="2017-12-27")%>%
  filter(date<"2018-04-28")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2018)%>%
  bind_rows(min.max)->min.max

# ----- 2019 ------
yr19<-temp%>%
  filter(date>="2018-10-1")%>%
  filter(date<="2019-9-30")%>%
  data.frame()

plot19<-ggplot(yr19,aes(x=date,y=daily_temp_C))+
  geom_point()+
  geom_hline(yintercept=1,linetype='dashed',col='red')+
  ggtitle("2019")+
  theme_classic()
plot19

filter(yr19,daily_temp_C<1)
# winter start = 25 December 2018
# winter end = 5 May 2019

days19<-yr19%>%
  filter(date>="2018-12-25")%>%
  filter(date<"2019-05-05")%>%
  filter(daily_temp_C<1)%>%
  summarise(cohort=2018,days_below_1=n())%>%
  data.frame()

avg19<-yr19%>%
  filter(date>="2018-12-25")%>%
  filter(date<"2019-05-05")%>%
  summarise(cohort=2018,mean_temp=mean(daily_temp_C),
            sd_temp=sd(daily_temp_C),
            min=min(daily_temp_C),
            max=max(daily_temp_C))%>%
  data.frame()

yr19%>%
  filter(date>="2018-12-25")%>%
  filter(date<"2019-05-05")%>%
  summarise(min(daily_temp_C),max(daily_temp_C))%>%
  mutate(year=2019)%>%
  bind_rows(min.max)->min.max




# ----- create winter summary tables------
winterdays<-bind_rows(days95,days96,days97,days99,days00,days01,days02,
                      days03,days04,days05,days06,days07,days08,days09,
                      days10,days11,days12,days13,days14,days15,days16,days17,days18,days19)

meantemp<-bind_rows(avg95,avg96,avg97,avg99,avg00,avg01,avg02,avg03,
                    avg04,avg05,avg06,avg07,avg08,avg09,avg10,avg11,avg12,
                    avg13,avg14,avg15,avg16,avg17,avg18,avg19)

min.max%>%
  rename(mintemp='min(daily_temp_C)',maxtemp='max(daily_temp_C)')%>%
  summarise(min(mintemp),max(maxtemp))

# ----- load winter duration data ------
winterlength<-read.csv("../data/data-working/newman-winter-duration.csv",header=TRUE,sep=",")

#check data
summary(winterlength)
str(winterlength)
head(winterlength)
tail(winterlength)
names(winterlength)
dim(winterlength)

#format date
winterlength$date<-ymd(paste(winterlength$year,winterlength$month,winterlength$day,sep="-"))

# ---- num. days in winter ------
duration<-winterlength%>%
  group_by(cohort)%>%
  mutate(total_days=date-lag(date))%>%
  ungroup()%>%
  data.frame()
duration<-na.omit(duration)
duration<-duration%>%
  select(cohort,total_days)


#merge winterdays, meantemp, duration 
winter_data<-left_join(winterdays,meantemp)
winter_data<-left_join(duration,winter_data)

#save summary file
write.csv(winter_data,file="../data/output/newman-winter-summary.csv",row.names=FALSE)

# ----- Organize graphs onto one page ------
#use gridExtra

page1<-grid.arrange(plot95,plot96,plot97,plot98,plot99,plot00)
page2<-grid.arrange(plot01,plot02,plot03,plot04,plot05,plot06)
page3<-grid.arrange(plot07,plot08,plot09,plot10,plot11, plot12)
page4<-grid.arrange(plot13,plot14,plot15,plot16,plot17)

