library(tidyverse)
library(lubridate)

file_list<-list.files(path="C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/length",
                      pattern="*.csv")

setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/length/")
length<-lapply(file_list,read.csv)


file_list2<-list.files(path="C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/lab-measured/",
                       pattern = "*.csv")
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/lab-measured/")
lab<-lapply(file_list2,read.csv)

lab.all<-lapply(lab,bind_rows)
length.all<-lapply(length,bind_rows)

mydata<-bind_rows(lab.all,length.all)
str(mydata)
distinct(mydata,Species)

cod<-mydata%>%
  filter(Species=="AC")%>%
  mutate(month=as.integer(str_sub(Date,start=5,end=6)))%>%
  mutate(date=ymd(paste(Year,month,Day,sep="-")))%>%
  select(-Month,-Day,-Date,-Time,-Site,-Species,-Weighting,-Notes,-Wt...g.,-Time.1)%>%
  mutate(date2=date+10)%>%
  rename(pulse=Pulse)
  
trip.date<-cod%>%
  select(Trip,Year,date)%>%
  group_by(Year,Trip)%>%
  summarise(date2=min(date))%>%
  rename(date=date2)
final_range<-cod%>%
  group_by(Year,Age,Trip,Pulse)%>%
  summarise(SL=mean(mmSL),max=max(mmSL),min=min(mmSL))

final_range<-left_join(final_range,trip.date)

cohort2.graph<-function(final,final_range,na.rm=FALSE, ...){
  
  cohort_list<-rev(unique(final$Year))
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(final,final$Year==cohort_list[i]))+
      geom_jitter(aes(x=date2,y=mmSL,colour=as.character(pulse),shape=as.character(Age)),
                  alpha=0.5,size=1)+
      geom_point(data=subset(final_range,final_range$Year==cohort_list[i]),
                 aes(x=date,y=(min+max)/2,shape=factor(Pulse)),size=2)+
      geom_errorbar(data=subset(final_range,final_range$Year==cohort_list[i]),
                    aes(x=date,ymin=min,ymax=max),width=0)+
      theme_bw()+
      ggtitle(paste("Age 1", cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))+
      scale_colour_manual(values=c("#009E73","#E69F00", "#0072B2","#CC79A7","#56B4E9","#F0E442","#D55E00",
                                   na.value="grey40"))
    print(plot)
  }
}
cohort2.graph(cod,final_range)



cohort.graph<-function(final,na.rm=FALSE, ...){
  
  cohort_list<-rev(unique(final$Year))
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(final,final$Year==cohort_list[i]))+
      geom_jitter(aes(x=date2,y=mmSL,colour=as.character(pulse),shape=as.character(Age)),
                  alpha=0.5,size=1)+
      theme_bw()+
      ggtitle(paste("Age 1", cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))+
      scale_colour_manual(values=c("#009E73","#E69F00", "#0072B2","#CC79A7","#56B4E9","#F0E442","#D55E00"),na.value="grey39")
    print(plot)
  }
}
cohort.graph(cod)

cod%>%
  filter(Year==1996)%>%
  ggplot()+
  geom_jitter(aes(x=date,y=mmSL,colour=as.character(pulse),shape=as.character(Age)),
              alpha=0.75,size=1)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))+
  scale_colour_manual(values=c("#009E73","#E69F00", "#0072B2","#CC79A7","#56B4E9","#F0E442","#D55E00"),na.value="grey40")
