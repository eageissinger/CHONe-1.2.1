# set working directory 
setwd("C:/Users/eageissinger/Documents/Emilie-Lab-comp/")

# ---- load packages ----
library(dplyr)
library(lubridate)

# load data
pulserange<-read.csv("./data/data-working/age1-pulse-range.csv")

str(pulserange)
# fix date
pulserange$date<-ymd(paste(pulserange$year,pulserange$month,pulserange$day,sep="-"))

# 2000
mydata<-pulserange%>%
  mutate(pulse=dummy_pulse)%>%
  mutate(pulse=replace(pulse,cohort==2000 & month==7 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & month ==7 & pulse == 1, 2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & month == 5 & pulse == 3, 5))%>%
  mutate(pulse=replace(pulse,cohort==2001 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & month == 5 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & month == 7 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & month == 7 & pulse == 1, 3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & month ==5 & pulse == 3, 4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & month == 5 & pulse == 2, 3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & month == 5 & pulse == 1, 2))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 5 & pulse == 3,5))%>%
  mutate(pulse= replace(pulse,cohort==2004 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 5 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 7 & pulse == 3, 5))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 7 & pulse== 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & month == 7 & pulse == 1,3))%>%
  mutate(pulse=replace(pulse,cohort==2005 & month == 5,3))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month ==5 & pulse == 4,6))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 5 & pulse == 3, 5))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 5 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 7 & pulse == 3, 6))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 7 & pulse == 2,5))%>%
  mutate(pulse=replace(pulse,cohort==2006 & month == 7 & pulse == 1,4))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip==12 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2008 & month ==5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2008 & month == 5 & pulse ==2, 4))%>%
  mutate(pulse=replace(pulse,cohort==2008 & month == 5 & pulse ==1,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 5 & pulse==3,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 5 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 5 & pulse == 1, 2))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 7 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 7 & pulse ==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & month == 7 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010& month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2010& month == 5 & pulse == 1,2))%>% #this one is not fully correct
  mutate(pulse=replace(pulse,cohort==2010& trip ==12 & pulse ==2,4))%>%
  mutate(pulse=replace(pulse,cohort==2010& trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2011 & month ==5 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2011 & month == 5 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & month == 5 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & month == 5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2012 & month == 5 & pulse == 2, 4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 5 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 5 & pulse == 1,"2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 7 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 7 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & month == 7 & pulse == 1,"2/3"))%>%
  mutate(pulse=replace(pulse,cohort==2015 & month == 5 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & month == 5 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & month ==5 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip==12 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip ==12 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip ==13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & month == 5 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2016 & month == 5 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & pulse ==4,5))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016& trip == 14 & pulse ==3,5))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse == 2,4))

filter(mydata,cohort==2000 & month == 7)         
filter(mydata,cohort==2001 & month == 5)
filter(mydata,cohort==2001 & month ==7)
filter(mydata,cohort==2002 & month ==5)
filter(mydata,cohort==2004 & month ==5)
filter(mydata,cohort==2004 & month ==7)
filter(mydata,cohort==2005 & month ==5)
filter(mydata,cohort==2006 & month == 5)
filter(mydata,cohort==2006 & month == 7)
filter(mydata,cohort==2007 & month == 7)
filter(mydata,cohort==2008 & month ==5)
filter(mydata,cohort==2009 & month == 5)
filter(mydata,cohort==2009 & month == 7)
# need to fix 2010 (2 is actually likely 1,2,3)
filter(mydata,cohort==2010 & month == 5)
filter(mydata,cohort==2010 & month == 7)
filter(mydata,cohort==2011 & month == 5)
filter(mydata,cohort==2011 & month == 7)
filter(mydata,cohort==2012 & month ==5)
filter(mydata,cohort==2013 & month == 5)
filter(mydata,cohort==2013 & month == 7)
filter(mydata,cohort==2015 & month ==5)
filter(mydata,cohort==2015 & month ==7)
filter(mydata,cohort==2016 & month ==5)
filter(mydata,cohort==2016 & month == 7)

# only updated for May and July
write.csv(mydata,"./data/data-working/pulse_range_mayjuly.csv")

cohort.graph(mydata)

test<-mydata%>%
  mutate(mean=(min+max)/2)
test$cohort<-as.factor(test$cohort)
test$pulse<-as.integer(test$pulse)
glimpse(test)

# plotting

test%>%
  filter(cohort=="2010")%>%
  ggplot(aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
  geom_point(size=1)+
  geom_errorbar(aes(ymin=min,ymax=max),width=5)+
  theme_bw()

cohort.graph<-function(test,na.rm=TRUE, ...){
  
  cohort_list<-rev(unique(test$cohort))

  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(test,test$cohort==cohort_list[i]),
                 aes(x=date,y=mean,group=cohort,shape=factor(pulse)))+
      geom_point(size=2)+
      geom_errorbar(aes(ymin=min,ymax=max),width=0)+
      theme_bw()+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_x_date(date_breaks="1 month",
                   date_labels="%b")+
      theme(axis.text.x=element_text(angle=40))
    print(plot)
  }
}
cohort.graph(test)
glimpse(test)
