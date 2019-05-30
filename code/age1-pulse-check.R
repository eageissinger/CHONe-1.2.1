# Assign remaining pulses

# set working directory
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

# ---- load packages ----
library(tidyverse)
library(lubridate)

# --- load data ----
range_final<-read.csv("./data/data-working/pulse_range_age1_final.csv")
catch_haul<-read.csv("./data/data-working/catch_haul.csv")
length<-read.csv("./data/data-working/newman-length.csv")
mixtures<-read.csv("./data/data-working/age-1-mixture-dist.csv")

head(mixtures)

# --- Mixture models ----
mixtures<-mixtures%>%
  rename(mean=mu,pulse=dummy_pulse)%>%
  mutate(min=mean-sigma,max=mean+sigma)%>%
  select(mean,min,max,year,month,day,trip,pulse,cohort,age)

mix2<-mixtures%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip == 10 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 13 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 17 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 17 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 17 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 20 & pulse == 4,5))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 20 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 20 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 21 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 21 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 21 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 14 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 19 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 19 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 20 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 21 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 21 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 4,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 5,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 10 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 10 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 10 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 11 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 11 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 12 & pulse == 4,5))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 12 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 12 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 13 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 13 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 13 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 14 & pulse == 2,5))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 14 & pulse == 1,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 14 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 14 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 16 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 16 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 16 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 18 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 18 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & pulse == 4,5))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 9 & pulse == 4,5))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 9 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 9 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 9 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & pulse == 2,3))%>% 
  mutate(pulse=replace(pulse,cohort==2004 & trip == 18 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2005 & trip == 9,3))%>%
  mutate(pulse=replace(pulse,cohort==2006 & trip == 9 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2006 & trip == 9 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2006 & trip == 9 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2006 & trip == 12 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2006 & trip == 12 & pulse == 1,4))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 10 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 12 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & pulse == 5,4))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 18 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 18 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 18 & pulse == 5,4))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 9 & pulse == 3,6))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 9 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 9 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 10 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 13 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 13 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 10 & pulse == 1,2))%>% 
  mutate(pulse=replace(pulse,cohort==2010 & trip == 12 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 15 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 15 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 10 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 20 & pulse < 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 9 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 9 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 20 & pulse == 3,NA))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 20 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 21 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 21 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 21 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 10 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 14 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 14 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 22 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 10 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 10 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 14 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 14 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 15 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 16 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 16 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 17 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 16 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 9 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 14 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 18 & pulse == 4,5))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 18 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 18 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 19 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 19 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 20 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 20 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 20 & pulse == 1,3))

cohort2.graph<-function(growth,length,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  growth$date<-ymd(paste(growth$year,growth$month,growth$day,sep="-"))
  length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
  length<-length%>%filter(age==1)%>%
    mutate(date2=date+3)%>%
    mutate(cohort=year-1)
  #pdf("mixtures-length-age1.pdf")
  for (i in seq_along(cohort_list)) {
    plot<-ggplot()+
      geom_point(data=subset(growth,growth$cohort==cohort_list[i]),
                 aes(x=date,y=mean,group=cohort,shape=as.factor(pulse)),size=2)+
      geom_errorbar(data=subset(growth,growth$cohort==cohort_list[i]),
                    aes(x=date,ymin=min,ymax=max),width=0)+
      geom_jitter(data=subset(length,length$cohort==cohort_list[i]),
                  aes(x=date2,y=mmSL),colour='grey50',size=0.25)+
      ylim(20,250)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))
    print(plot)
  }
  #dev.off()
}
cohort2.graph(mix2,length)

# ---- pulse assigned to length data ----

# update pulse assignments for age 1
str(range_final)
summary(range_final)
range_final<-range_final%>%
  filter(!is.na(min), !is.na(max))

pulse_assign1<-data.frame(trip=rep(range_final$trip,range_final$max-range_final$min+1),
                          year=rep(range_final$year,range_final$max-range_final$min+1),
                          pulse=rep(range_final$pulse,range_final$max-range_final$min+1),
                          mmSL=unlist(mapply(seq,range_final$min,range_final$max)))

# Add to age 1 length data
glimpse(length)
glimpse(pulse_assign1)
length1<-length%>%
  filter(age==1)%>%
  select(-pulse)

length_pulse<-left_join(length1,pulse_assign1)
head(length_pulse)
length_pulse$date<-ymd(paste(length_pulse$year,length_pulse$month,length_pulse$day,sep="-"))
length_pulse<-length_pulse%>%
  mutate(cohort=year-1)



# Data visualization

length_pulse%>%
  filter(cohort==1999)%>%
  ggplot(aes(x=date,y=mmSL,colour=factor(pulse)))+geom_jitter(size=.75)+
  theme_bw()+
  ylim(c(20,225))

length_pulse<-length_pulse%>%
  arrange(desc(cohort))
# ---- All years as function -----
cohort.graph<-function(length_pulse,na.rm=FALSE, ...){
  
  cohort_list<-rev(unique(length_pulse$cohort))
  #pdf("age1-full-test.pdf")
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(length_pulse,length_pulse$cohort==cohort_list[i]),
                 aes(x=date,y=mmSL,group=cohort,colour=factor(pulse)))+
      geom_jitter(size=1)+
      theme_bw()+
      ylim(c(25,250))+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
    print(plot)
  }
}
cohort.graph(length_pulse)
