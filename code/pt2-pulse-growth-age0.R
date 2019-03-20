### Growth rates - Pulse data ###

# ---- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

# ---- load packages ----
library(lubridate)
library(tidyverse)

# ---- load data -----
length<-read.csv("./data/data-working/newman-length.csv")
age0.new<-read.csv("./data/data-working/age-0-mixture-dist.csv")
count<-read.csv("./data/data-working/catch-pulse-newman.csv")
fullcount<-read.csv("./data/data-working/newman-catch.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")

# --- check data ----
# age 0
str(length)
summary(length)
head(length)
dim(length)
names(length)

#age 1
str(age0.new)
dim(age0.new)
summary(age0.new)
head(age0.new)
names(age0.new)

#count
summary(count)
str(count)
dim(count)
head(count)
tail(count)
names(count)

# full count
summary(fullcount)
str(fullcount)
dim(fullcount)
head(fullcount)
names(fullcount)

names(fullcount)<-c("year","julian_date","site","species","age","count","notes","month","day")

# Trips
summary(trips)
str(trips)
dim(trips)
head(trips)
names(trips)

# ---- Date manipulation ----

# Add trip dates to age 0 data
length<-left_join(length,trips)

#format date
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
age0.new$date<-ymd(paste(age0.new$year,age0.new$month,age0.new$day,sep="-"))
fullcount$date<-ymd(paste(fullcount$year,fullcount$month,fullcount$day,sep="-"))

# create pulse range for length
original<-length%>%
  filter(age==0)%>%
  group_by(year,trip,pulse,date)%>%
  summarise(mean_size=mean(mmSL),min_size=min(mmSL),max_size=max(mmSL))%>%
  mutate(cohort=year)%>%
  data.frame()
#create min and max for age 1
# 2 standard deviations
new<-age0.new%>%
  select(date, cohort, month, age, dummy_pulse,mu,mu.se,sigma,sigma.se)%>%
  filter(mu<300)%>% # get rid of outliers (>300 mm SL)
  rename(mean_size=mu,stdev=sigma)%>%
  mutate(min_size=mean_size-(stdev),max_size=mean_size+(stdev))%>%
  select(-stdev,-sigma.se,-mu.se)%>%
  rename(pulse=dummy_pulse)%>%
  data.frame()

names(original)
names(new)

original$pulse<-as.factor(original$pulse)
original$cohort<-as.factor(original$cohort)

new$pulse<-as.factor(new$pulse)
new$cohort<-as.factor(new$cohort)

# ---- format count data -----
# go from wide format to long format
count2<-count%>%
  gather(key=p1,value=count,starts_with('count'))%>%
  gather(key=p2,value=extrap,starts_with('extrap'))%>%
  separate(p1,c("count2","pulse1"))%>%
  separate(p2,c("extrap2","pulse2"))

# make count df and extrap df
count_only<-count2%>%
  select(-pulse2,-extrap,-extrap2,-count2)
extrap_only<-count2%>%
  select(-pulse1,-count,-count2,-extrap2)
names(count_only)<-c("year","j_start_date","num_hauls","pulse","count")
names(extrap_only)<-c("year","j_start_date","num_hauls","pulse","extrap")

#combine adjusted df to be one complete 
count_long<-left_join(count_only,extrap_only)

# change NAs to 0 (confirmed that NAs are actually 0s)
count_long$count[is.na(count_long$count)]<-0

# ---- summary table of class-year strength -----
pulse_table<-count_long%>%
  group_by(year,pulse)%>%
  summarise(total_count=sum(count),avg_count=mean(count),total_extrap=sum(extrap),avg_extrap=mean(extrap))

adjusted_sums<-count_long%>%
  group_by(year,pulse)%>%
  summarise(total_extrap=sum(extrap))%>%
  spread(pulse,total_extrap)

adjusted_mean<-count_long%>%
  group_by(year,pulse)%>%
  summarise(avg_extrap=mean(extrap))%>%
  spread(pulse,avg_extrap)

# ---- age 0 and age 1 Totals -----
table_age0<-fullcount%>%
  filter(age==0)%>%
  filter(month>=10)%>%
  group_by(year,date,age,month)%>%
  summarise(count=sum(count))%>%
  ungroup()%>%
  group_by(year,month,age)%>%
  summarise(count=mean(count))
table_age1<-fullcount%>%
  filter(age==1)%>%
  filter(month<8)%>%
  group_by(year,date,age,month)%>%
  summarise(count=sum(count))%>%
  ungroup()%>%
  group_by(year,month,age)%>%
  summarise(count=mean(count)) 


# ---- Visualize data ----

original%>%
  filter(cohort==2016)%>%
  ggplot(aes(x=date,y=mean_size,shape=factor(dummy_pulse)))+geom_point()+
  geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)

new%>%
  filter(cohort==2016)%>%
  ggplot(aes(x=date,y=mean_size,shape=factor(dummy_pulse)))+geom_point()+
  geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)

# ---- All years as function -----
cohort.graph<-function(growth,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  #pdf()
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(growth,growth$cohort==cohort_list[i]),
                 aes(x=date,y=mean_size,group=cohort,shape=pulse))+
      geom_point(size=2)+
      geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)+
      ylim(20,150)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))
    print(plot)
  }
}
cohort.graph(original)
cohort.graph(new)

write.csv(new,"./data/data-working/age0-trial.csv",row.names = FALSE)
write.csv(original,"./data/data-working/age0-original.csv",row.names=FALSE)
