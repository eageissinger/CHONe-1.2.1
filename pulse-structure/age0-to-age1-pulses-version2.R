# Age 0 to Age 1 Pulse structure Version 1

setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

library(tidyverse)
library(lubridate)

range0<-read.csv("./data/output/pulse_range_0_final.csv")
range1<-read.csv("./data/output/pulse_range_age1_final.csv")
length<-read.csv("./data/data-working/newman-length.csv")

head(range0)
head(range1)
str(range0)
range0<-range0%>%
  mutate(cohort=year)
range1<-range1%>%
  mutate(age=1)
pulse.range<-bind_rows(range0,range1)%>%
  mutate(month=str_sub(date,start=6,end=7),
         day=str_sub(date,start=9,end=10))%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  mutate(date2=date-8)%>%
  filter(!is.na(max))%>%
  filter(!is.na(pulse))
str(pulse.range)

summary(length)
length0<-length%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  filter(age==0)%>%
  mutate(cohort=year)
length1<-length%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  filter(age==1)%>%
  mutate(cohort=year-1)

  
pulse.assign<-data.frame(age=rep(pulse.range$age,pulse.range$max-pulse.range$min+1),
                         year=rep(pulse.range$year,pulse.range$max-pulse.range$min+1),
                         trip=rep(pulse.range$trip,pulse.range$max-pulse.range$min+1),
                         pulse=rep(pulse.range$pulse,pulse.range$max-pulse.range$min+1),
                         mmSL=unlist(mapply(seq,pulse.range$min,pulse.range$max)))

length2<-bind_rows(length0,length1)%>%
  select(-pulse)%>%
  left_join(pulse.assign)
# add additinal info such as date, cohort, pulse so that it is present in dataframe  

cohort.graph<-function(growth,length,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  pdf("./output/age0-age1-v2.pdf")
  for (i in seq_along(cohort_list)) {
    plot<-ggplot()+
      geom_jitter(data=subset(length,length$cohort==cohort_list[i]),
                  aes(x=date,y=mmSL,colour=factor(pulse)),size=1,alpha=0.25)+
      geom_point(data=subset(growth,growth$cohort==cohort_list[i]),
                 aes(x=date2,y=(min+max)/2,group=as.factor(cohort),shape=factor(pulse)),size=2)+
      geom_errorbar(data=subset(growth,growth$cohort==cohort_list[i]),
                    aes(x=date2,ymin=min,ymax=max),width=0)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))+
      ylim(c(15,250))+
      scale_colour_manual(values=c("#009E73","#E69F00", "#0072B2","#CC79A7","#56B4E9","#F0E442","#D55E00"))
    print(plot)
  }
}
cohort.graph(pulse.range,length2)
dev.off()
