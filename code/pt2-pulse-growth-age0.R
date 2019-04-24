### Growth rates - Pulse data ###

# ---- set working directory ----
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office//")

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


# Trips
summary(trips)
str(trips)
dim(trips)
head(trips)
names(trips)

# ---- Date manipulation ----

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


# ---- Visualize data ----

original%>%
  filter(cohort==2016)%>%
  ggplot(aes(x=date,y=mean_size,shape=factor(pulse)))+geom_point()+
  geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)

new%>%
  filter(cohort==2016)%>%
  ggplot(aes(x=date,y=mean_size,shape=factor(pulse)))+geom_point()+
  geom_errorbar(aes(ymin=min_size,ymax=max_size),width=0)

# ---- All years as function -----
cohort.graph<-function(growth,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  pdf()
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
#cohort.graph(original)
cohort.graph(new)

write.csv(new,"./data/data-working/age0-trial.csv",row.names = FALSE)
write.csv(original,"./data/data-working/age0-original.csv",row.names=FALSE)

# plot model and points on scatter
df1<-new%>%
  filter(cohort==2016)
df2<-length%>%
  filter(age==0)%>%
  filter(year==2016)%>%
  mutate(date2=date+5)

ggplot()+
  geom_point(data=df1,aes(x=date,y=mean_size,shape=pulse),size=2)+
  geom_errorbar(data=df1,aes(x=date,ymin=min_size,ymax=max_size),width=0)+
  geom_jitter(data=df2,aes(x=date2,y=mmSL))

# create age 0 length dataframe with a date offset of 5
age0<-length%>%
  filter(age==0)%>%
  mutate(date2=date+3)%>%
  mutate(cohort=year)
#As a loop
cohort2.graph<-function(growth,length,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  
  pdf("mixtures-length.pdf")
  for (i in seq_along(cohort_list)) {
    plot<-ggplot()+
      geom_point(data=subset(growth,growth$cohort==cohort_list[i]),
                 aes(x=date,y=mean_size,group=cohort,shape=pulse),size=2)+
      geom_errorbar(data=subset(growth,growth$cohort==cohort_list[i]),
                    aes(x=date,ymin=min_size,ymax=max_size),width=0)+
      geom_jitter(data=subset(length,length$cohort==cohort_list[i]),
                  aes(x=date2,y=mmSL),colour='grey50',size=.75)+
      ylim(20,150)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))
    print(plot)
  }
  dev.off()
}
cohort2.graph(new,age0)

