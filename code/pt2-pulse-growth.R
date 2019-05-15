### Growth rates - Pulse data ###

# ---- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

# ---- load packages ----
library(lubridate)
library(tidyverse)

# ---- load data -----
age0<-read.csv("./data/data-working/pulse_range_0_final.csv")
age1<-read.csv("./data/data-working/age-1-mixture-dist.csv")
catch.haul<-read.csv("./data/data-working/catch_haul.csv")
fullcount<-read.csv("./data/data-working/newman-catch.csv")
length<-read.csv('./data/data-working/newman-length.csv')

# --- check data ----
# age 0
str(age0)
summary(age0)
head(age0)
dim(age0)
names(age0)

age0<-age0%>%
  mutate(cohort=year)
names(age0)

#age 1
str(age1)
dim(age1)
summary(age1)
head(age1)
names(age1)

# full count
summary(fullcount)
str(fullcount)
dim(fullcount)
head(fullcount)
names(fullcount)

trips<-length%>%
  select(year,julian.date,trip,month,day)%>%
  group_by(year,trip)%>%
  filter(julian.date==min(julian.date))%>%
  distinct(trip,year,julian.date,month,day)

# ---- Date manipulation ----

#format date
fullcount$date<-ymd(paste(fullcount$year,fullcount$month,fullcount$day,sep="-"))
age1$date<-ymd(paste(age1$year,age1$month,age1$day,sep="-"))
age1$julian.date<-yday(age1$date)

# add trips to age 0 to add date
age0<-left_join(age0,trips)
age0$date<-ymd(paste(age0$year,age0$month,age0$day, sep="-"))
head(age0)

#mean size for age 0
age0data<-age0%>%
  select(date,cohort,month,age,pulse,min,max)%>%
  mutate(meanSL=(min+max)/2)

#create min and max for age 1
# 2 standard deviations
age1data<-age1%>%
  select(date, cohort, month, age, dummy_pulse,mu,mu.se,sigma,sigma.se)%>%
  filter(mu<300)%>% # get rid of outliers (>300 mm SL)
  rename(meanSL=mu,stdev=sigma,pulse=dummy_pulse)%>%
  mutate(min=meanSL-stdev,max=meanSL+stdev)%>%
  select(-stdev,-sigma.se,-mu.se)

names(age0data)
names(age1data)
growth<-bind_rows(age0data,age1data)
unique(growth$pulse) # number of pulses present
growth<-growth%>%filter(meanSL<250)%>%
  filter(max<250)

# ---- Visualize data ----

growth%>%
  filter(cohort==2018)%>%
  ggplot(aes(x=month,y=meanSL,shape=factor(pulse)))+
  geom_point(size=1)

# ---- All years as function -----
cohort.graph<-function(growth,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  #pdf()
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(growth,growth$cohort==cohort_list[i]),
                 aes(x=date,y=meanSL,group=as.factor(cohort),shape=factor(pulse)))+
      geom_point(size=2)+
      geom_errorbar(aes(ymin=min,ymax=max),width=0)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))+
      ylim(c(15,250))
    print(plot)
  }
}
cohort.graph(growth)
str(growth)
growth$cohort<-as.integer(as.character(growth$cohort))
str(growth)

# ---- format count data -----
# go from wide format to long format
catch.haul2<-catch.haul%>%
  gather(key=p1,value=count, starts_with('count'))%>%
  gather(key=p2,value=extrap, starts_with('extrap'))%>%
  separate(p1,c("count2","pulse1"))%>%
  separate(p2,c("extrap2","pulse2"))%>%
  distinct()%>%
  rename(pulse=pulse1)%>%
  select(-count2,-extrap2,-pulse2)%>%
  mutate(count=replace(count,is.na(count),0))%>%
  mutate(extrap=replace(extrap,count==0,0))

# ---- summary table of class-year strength -----
pulse_table<-catch.haul2%>%
  group_by(year,pulse)%>%
  summarise(total_count=sum(count),avg_count=mean(count),total_extrap=sum(extrap),avg_extrap=mean(extrap))


# ---- age 0 and age 1 Totals -----
table_full<-fullcount%>%
  group_by(year,age,month,trip)%>%
  summarise(count=sum(count),mean=mean(count))
