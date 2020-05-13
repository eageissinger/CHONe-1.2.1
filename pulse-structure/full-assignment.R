# Full Pulse Assignment
# 1995-2018

library(lubridate)
library(tidyverse)


setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")
pulses0<-read.csv("./data/output/pulse_range_0_final.csv")
pulses1<-read.csv("./data/output/pulse_range_age1_final.csv")

# ---- Pulse structure ----
head(pulses0)
head(pulses1)
pulses1<-pulses1%>%
  mutate(age=1)%>%
  dplyr::select(-cohort)

pulses<-bind_rows(pulses0,pulses1)%>%
  filter(!is.na(pulse))%>%
  filter(!is.na(min))
summary(pulses)
pulse.assign<-data.frame(Trip=rep(pulses$trip,pulses$max-pulses$min+1),
                         Age=rep(pulses$age,pulses$max-pulses$min+1),
                         Year=rep(pulses$year,pulses$max-pulses$min+1),
                         Pulse=rep(pulses$pulse,pulses$max-pulses$min+1),
                         mmSL=unlist(mapply(seq,pulses$min,pulses$max)))
pulse.assign2<-pulse.assign%>%
  mutate(Species="AC")%>%
  distinct()

# --- Field Fish ----
files<-list.files(path="C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/length",
                  full.names = FALSE, include.dirs = FALSE)
head(files)
file_names<-list.files(path="C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/length")
head(file_names)

for(i in 1:length(file_names)){
  setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/length/")
  length<-read.csv(files[i])
  length1<-length%>%
    dplyr::select(1:14)%>%
    mutate(Species=replace(Species,Species=="AC ","AC"))%>%
    #dplyr::select(-Time)%>%
    dplyr::select(-Pulse)%>%
    left_join(pulse.assign2)%>%
    dplyr::select(1:11,Pulse,12:13)
    
  setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/length/") #writenewfilestohere
  write.csv(x=length1,row.names=FALSE, file = paste("revised",file_names[i],sep = "_"),na="")
} 

# ---- Lab Fish -----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/")
lab1<-read.csv("./lab-measured/TNNP1996a.csv")
lab2<-read.csv("./lab-measured/TNNP1998a.csv")
lab3<-read.csv("./lab-measured/TNNP1999a.csv")

lab33<-lab3%>%
  dplyr::select(-Time)%>%
  rename(Time=Time.1)%>%
  dplyr::select(-Pulse)%>%
  left_join(pulse.assign2)
lab22<-lab2%>%
  dplyr::select(-Pulse)%>%
  left_join(pulse.assign2)
lab11<-lab1%>%
  dplyr::select(-Pulse)%>%
  left_join(pulse.assign2)
write.csv(lab11,"./output/lab-measured/revised_TNNP1996a.csv",row.names = FALSE)
write.csv(lab22, "./output/lab-measured/revised_TNNP1998a.csv",row.names=FALSE)
write.csv(lab33, "./output/lab-measured/revised_TNNP1999a.csv",row.names=FALSE)

# ---- Quality check -----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/temp/")
file_list<-list.files(path="C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/temp")
length<-lapply(file_list,read.csv)

length.all<-bind_rows(length)

setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/lab-measured/")

file_list2<-list.files(path="C:/Users/user/Documents/Research/CHONe-1.2.1/data/TNNP Revised Data/TNNP/output/lab-measured/")
lab<-lapply(file_list2,read.csv)

lab.all<-bind_rows(lab)


mydata<-bind_rows(lab.all,length.all)
glimpse(mydata)
distinct(mydata,Species)

cod<-mydata%>%
  filter(Species=="AC")%>%
  mutate(month=as.integer(str_sub(Date,start=5,end=6)))%>%
  mutate(date=ymd(paste(Year,month,Day,sep="-")))%>%
  select(-Month,-Day,-Date,-Time,-Site,-Species,-Weighting,-Notes,-Weighting,-Wt...g.)%>%
  mutate(date2=date+10)

cod0<-cod%>%
  filter(Age==0)%>%
  mutate(cohort=Year)
cod1<-cod%>%
  filter(Age==1)%>%
  mutate(cohort=Year-1)
cod2<-cod%>%
  filter(Age==2)%>%
  mutate(cohort=Year-2)
cod3<-cod%>%
  filter(Age==3)%>%
  mutate(cohort=Year-3)
codNA<-cod%>%
  filter(is.na(Age))%>%
  mutate(cohort=NA)

cod<-bind_rows(cod0,cod1,cod2,cod3,codNA)
trip.date<-cod%>%
  select(Trip,Year,cohort,date)%>%
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
      geom_jitter(aes(x=date2,y=mmSL,colour=as.character(Pulse),shape=as.character(Age)),
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


setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")
cohort.graph<-function(final,na.rm=FALSE, ...){
  
  cohort_list<-rev(unique(final$Year))
  for (i in seq_along(cohort_list)) {
    #pdf("./output/test.pdf")
    plot<-ggplot(subset(final,final$Year==cohort_list[i]))+
      geom_jitter(aes(x=date2,y=mmSL,colour=as.character(Pulse),shape=as.character(Age)),
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
cod01<-cod%>%
  filter(Age<2)
cohort.graph(cod01)

select.years<-cod01%>%
  filter(Year== 1999 | Year == 2000 | Year ==2001 | Year == 2004 | Year == 2005 | Year == 2012)
cohort.graph(select.years)
cod%>%
  filter(Year==1996)%>%
  ggplot()+
  geom_jitter(aes(x=date,y=mmSL,colour=as.character(Pulse),shape=as.character(Age)),
              alpha=0.75,size=1)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels="%b")+
  theme(axis.text.x=element_text(angle=40))+
  scale_colour_manual(values=c("#009E73","#E69F00", "#0072B2","#CC79A7","#56B4E9","#F0E442","#D55E00"),na.value="grey40")
