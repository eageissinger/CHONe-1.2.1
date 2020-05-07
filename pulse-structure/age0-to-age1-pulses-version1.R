# Age 0 to Age 1 Pulse structure Version 1

setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

range0<-read.csv("./output/age0_range-v1.csv")
range1<-read.csv("./output/age1_range_v1.csv")
length<-read.csv("./data/data-working/newman-length.csv")

head(range0)
head(range1)
str(range0)
range0<-range0%>%
  mutate(cohort=year)

pulse.range<-bind_rows(range0,range1)%>%
  mutate(month=str_sub(date,start=6,end=7),
         day=str_sub(date,start=9,end=10))%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  mutate(date2=date-10)
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
length2<-bind_rows(length0,length1)
  

cohort.graph<-function(growth,length,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(growth$cohort))
  pdf("./output/age0-age1-v1.pdf")
  for (i in seq_along(cohort_list)) {
    plot<-ggplot()+
      geom_point(data=subset(growth,growth$cohort==cohort_list[i]),
                 aes(x=date2,y=(min+max)/2,group=as.factor(cohort),shape=factor(pulse)),size=2)+
      geom_errorbar(data=subset(growth,growth$cohort==cohort_list[i]),
                    aes(x=date2,ymin=min,ymax=max),width=0)+
      geom_jitter(data=subset(length,length$cohort==cohort_list[i]),
                  aes(x=date,y=mmSL),colour='grey39',size=1,alpha=0.5)+
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
cohort.graph(pulse.range,length2)
