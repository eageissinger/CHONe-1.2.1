# set working directory 
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

# ---- load packages ----
library(tidyverse)
library(lubridate)

# load data
pulserange<-read.csv("./data/data-working/age0-pulse-range.csv")
length<-read.csv("./data/data-working/newman-length.csv")

str(pulserange)

mydata<-pulserange%>%
  rename(pulse=dummy_pulse)%>%
  mutate(pulse=replace(pulse,cohort==1996 & trip == 19 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==1996 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==1996 & trip == 20 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==1996 & trip == 21 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 18 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 18 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 19 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 19 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 21 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 22 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 22 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2003 & trip == 19 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 17 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 21 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 22 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 22 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 18 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 18 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 19 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 20 & pulse == 3,5))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 19 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 22 & pulse == 1,4))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 20 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 21 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 21 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 17 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 18 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 18 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 18 & pulse == 4,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 19 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 19 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2018 & trip == 22 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2018 & trip == 22 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2018 & trip == 22 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2018 & trip == 23 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2018 & trip == 23 & pulse == 2,3))



# 2004
# take out trip 19, pulse 2; trip 20
# 2008
# take out trip 19
# 2010
# take out trip 18
# 2018
# take out trip 19

#mydata2<-mydata%>%
#  filter(cohort== 1999 & !trip %in% 18:21 | cohort == 2000 & !trip %in% 19:22 |
#           cohort %in% 2001:2006 | cohort==2007 & !trip %in% 16:18 | cohort %in% 2008:2011 |
#           cohort == 2012 & !trip %in% 17:22 | cohort==2013 & !trip %in% 21:22 |
#           cohort ==2014 | cohort == 2015 & !trip %in% 16:21 | cohort == 2016 & !trip %in% 17:20)
#View(mydata2)
#mydata2


# as complete as it can be
#write.csv(mydata,"./data/data-working/pulse_range_age0_round1.csv",row.names = FALSE)

#---- Assignments part 2 ----
# deal with the outliers
# start by combining pulse range to length data

pulse_assign0<-data.frame(trip=rep(mydata$trip,mydata$max-mydata$min+1),
                          year=rep(mydata$year,mydata$max-mydata$min+1),
                          month=rep(mydata$month,mydata$max-mydata$min+1),
                          cohort=rep(mydata$cohort,mydata$max-mydata$min+1),
                          pulse=rep(mydata$pulse,mydata$max-mydata$min+1),
                          mmSL=unlist(mapply(seq,mydata$min,mydata$max)))
length2<-length%>%select(-pulse)
pulse.length<-left_join(length2,pulse_assign0)%>%select(-cohort)

# assign the unassigned fish
final<-pulse.length%>%
  filter(age==0)%>%
  mutate(pulse=replace(pulse,year==1996 & trip <18 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==1996 & trip == 18 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,year==1996 & trip == 18 & mmSL>50,1))%>%
  mutate(pulse=replace(pulse,year == 1996 & trip == 19 & mmSL>60,1))%>%
  mutate(pulse=replace(pulse,year == 1996 & trip == 20 & mmSL>60,1))%>%
  mutate(pulse=replace(pulse,year==1998 & month == 7 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==1998 & month == 9 & is.na(pulse) & mmSL>50,1))%>%
  mutate(pulse=replace(pulse,year==1998 & month == 9 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,year==1998 & month == 10 & is.na(pulse) & mmSL>80,1))%>%
  mutate(pulse=replace(pulse,year==1998 & month == 10 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,year==1998 & month == 11 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,year==1998 & month == 11 & mmSL >49,2))%>%
  mutate(pulse=replace(pulse,year==1998 & month ==12 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,year==1998 & month == 12 & mmSL > 80,2))%>%
  mutate(pulse=replace(pulse,year==1999 & trip<18 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==1999 & trip == 17 & mmSL<35,2))%>%
  mutate(pulse=replace(pulse,year==1999 & trip == 18 & is.na(pulse) & mmSL>62,1))%>%
  mutate(pulse=replace(pulse,year==1999 & trip == 18 & is.na(pulse) & mmSL <63,2))%>%
  mutate(pulse=replace(pulse,year==1999 & trip == 22 & mmSL<45,4))%>%
  mutate(pulse=replace(pulse,year==1999 & mmSL>100,1))%>%
  mutate(pulse=replace(pulse,year==2000 & trip == 15, 1))%>%
  mutate(pulse=replace(pulse,year==2000 & trip == 17 & mmSL<55,2))%>%
  mutate(pulse=replace(pulse,year==2000 & trip == 19 & pulse==1 & mmSL< 70,2))%>%
  mutate(pulse=replace(pulse,year==2000 & trip == 20 & pulse == 1 & mmSL <73,2))%>%
  mutate(pulse=replace(pulse,year==2000 & trip == 22 & mmSL >80,1))%>%
  mutate(pulse=replace(pulse,year==2000 & trip == 22 & pulse == 3 & mmSL >62,2))%>%
  mutate(pulse=replace(pulse,year==2001 & trip==15,1))%>%
  mutate(pulse=replace(pulse,year==2001 & trip == 17 & mmSL>48,1))%>%
  mutate(pulse=replace(pulse,year==2001 & trip == 18 & mmSL<50,3))%>%
  mutate(pulse=replace(pulse,year==2001 & trip == 19 & pulse ==1 & mmSL<70,2))%>%
  mutate(pulse=replace(pulse,year==2001 & trip == 20 & pulse == 1 & mmSL<80,2))%>%
  mutate(pulse=replace(pulse,year==2001 & trip == 21 & pulse == 1 & mmSL <90,2))%>%
  mutate(pulse=replace(pulse,year==2001 & trip == 22 & pulse == 1 & mmSL<100,2))%>%
  mutate(pulse=replace(pulse,year==2002 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==2003 & is.na(pulse) & month <9,1))%>%
  mutate(pulse=replace(pulse,year==2003 & trip == 17 & mmSL<50,2))%>%
  mutate(pulse=replace(pulse,year==2003 & trip == 18 & mmSL<50,2))%>%
  mutate(pulse=replace(pulse,year==2003 & trip == 19 & mmSL<45,3))%>%
  mutate(pulse=replace(pulse,year==2003 & trip ==21 & mmSL<60,3))%>%
  mutate(pulse=replace(pulse,year==2003 & trip == 21 &is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==2003 & trip == 22,3))%>%
  mutate(pulse=replace(pulse,year==2003 & trip ==22 & mmSL>60,2))%>%
  mutate(pulse=replace(pulse,year==2003 & trip ==22 &mmSL>79,1))%>%
  mutate(pulse=replace(pulse,year==2004 & trip <17,1))%>%
  mutate(pulse=replace(pulse,year==2004 & trip == 17 & mmSL<50,2))%>%
  mutate(pulse=replace(pulse,year==2004 & trip == 18,2))%>%
  mutate(pulse=replace(pulse,year==2004 & trip == 18 & mmSL>90,1))%>%
  mutate(pulse=replace(pulse,year==2004 & trip == 18 & mmSL<45,3))%>%
  mutate(pulse=replace(pulse,year==2004 & trip == 20 & mmSL<40,4))%>%
  mutate(pulse=replace(pulse,year==2004 & trip > 20,3))%>%
  mutate(pulse=replace(pulse,year==2004 & trip == 21 & mmSL<50,4))%>%
  mutate(pulse=replace(pulse,year==2004 & trip == 22 & mmSL <60,4))%>%
  mutate(pulse=replace(pulse,year==2004 & mmSL>90,1))%>%
  mutate(pulse=replace(pulse,year==2005 & trip == 16 & mmSL<41,2))%>%
  mutate(pulse=replace(pulse,year==2005 & trip == 20,2))%>%
  mutate(pulse=replace(pulse,year==2005 & trip ==22,3))%>%
  mutate(pulse=replace(pulse,year==2005 & trip >19 &mmSL<50,3))%>%
  mutate(pulse=replace(pulse,year==2005 & mmSL>100,1))%>%
  mutate(pulse=replace(pulse,year==2006 & trip ==13,1))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 15 & mmSL<45,2))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 17 & mmSL<60,2))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 17 & mmSL>60,1))%>%
  mutate(pulse=replace(pulse,year==2006 & trip ==18,3))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 18 & mmSL>40,2))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 18 & mmSL>78,1))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 19,3))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 19 & mmSL>50,2))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 19 & mmSL>80,1))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 20,3))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 20 & mmSL>75,2))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 20 & mmSL>120,1))%>%
  mutate(pulse=replace(pulse,year==2006 & trip>20,3))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 21 & mmSL<40,4))%>%
  mutate(pulse=replace(pulse,year==2006 & trip == 22 & mmSL<50,4))%>%
  mutate(pulse=replace(pulse,year==2007 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==2008 & trip ==13,1))%>%
  mutate(pulse=replace(pulse,year==2008 & trip ==15 & mmSL<39,2))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 17 & mmSL <38,4))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 18 & pulse==1 & mmSL<81,2))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 18 & pulse == 3 & mmSL>62,2))%>%
  mutate(pulse=replace(pulse,year==2008 & mmSL>85,1))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 19 & pulse == 3 & mmSL>66,2))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 19 & mmSL<53,4))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 19 & mmSL<44,5))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 20 & pulse==5 & mmSL>44,4))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 20 &pulse ==3 & mmSL<60,4))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 20 & pulse == 3 & mmSL>70,2))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 20 & mmSL<30,6))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 21,6))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 21 & mmSL>39,5))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 21 & mmSL>50,4))%>%
  mutate(pulse=replace(pulse,year==2008 & trip == 21 & mmSL>65,3))%>%
  mutate(pulse=replace(pulse,year==2009 & trip ==16 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==2009 & trip == 16 & mmSL< 42,2))%>%
  mutate(pulse=replace(pulse,year==2009 & trip == 19 & pulse == 1 & mmSL<71,2))%>%
  mutate(pulse=replace(pulse,year==2009 & trip == 19 & pulse ==3 & mmSL>55,2))%>%
  mutate(pulse=replace(pulse,year==2009 & trip == 19 & mmSL<30,4))%>%
  mutate(pulse=replace(pulse,year==2009 & trip == 20 & mmSL<60,3))%>%
  mutate(pulse=replace(pulse,year==2009 & trip == 21,4))%>%
  mutate(pulse=replace(pulse,year==2009 & trip == 21 & mmSL>50,3))%>%
  mutate(pulse=replace(pulse,year==2009 & trip == 21 & mmSL>80,1))%>%
  mutate(pulse=replace(pulse,year==2009 & trip == 22 & mmSL>55,3))%>%
  mutate(pulse=replace(pulse,year==2009 & mmSL>80,1))%>%
  mutate(pulse=replace(pulse,year==2010 & trip<18,1))%>%
  mutate(pulse=replace(pulse,year==2010 & trip == 17 & mmSL<50,2))%>%
  mutate(pulse=replace(pulse,year==2010 & trip == 18 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,year==2010 & trip == 18 & pulse==1 & mmSL<60,2))%>%
  mutate(pulse=replace(pulse,year==2010 & trip == 19,3))%>%
  mutate(pulse=replace(pulse,year==2010 & trip == 19 & mmSL>45,2))%>%
  mutate(pulse=replace(pulse,year==2010 & trip ==21 & mmSL<60,3))%>%
  mutate(pulse=replace(pulse,year==2010 & trip == 22 & mmSL<40,4))%>%
  mutate(pulse=replace(pulse,year==2010 & trip == 22 & mmSL>40,3))%>%
  mutate(pulse=replace(pulse,year==2011 & trip <15,1))%>%
  mutate(pulse=replace(pulse,year==2011 & trip == 19 & mmSL<60,2))%>%
  mutate(pulse=replace(pulse,year==2011 & trip == 20 & mmSL>70,1))%>%
  mutate(pulse=replace(pulse,year==2011 & trip>20,2))%>%
  mutate(pulse=replace(pulse,year==2012 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==2012 & trip == 16 & mmSL<35,2))%>%
  mutate(pulse=replace(pulse,year==2012 & trip == 17 & mmSL<45,2))%>%
  mutate(pulse=replace(pulse,year==2012 & trip == 19 & mmSL< 35,3))%>%
  mutate(pulse=replace(pulse,year==2013 & trip == 21 &mmSL> 126,NA))%>%
  mutate(pulse=replace(pulse,year==2013 & trip == 16 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==2013 & trip == 16 & mmSL< 40,2))%>%
  mutate(pulse=replace(pulse,year==2013 & trip == 17 & mmSL<49,2))%>%
  mutate(pulse=replace(pulse,year==2013 & trip == 18 & mmSL<55,2))%>%
  mutate(pulse=replace(pulse,year==2013 & trip == 19 & mmSL<61,2))%>%
  mutate(pulse=replace(pulse,year==2013 & trip == 20 & pulse == 3 & mmSL>47,2))%>%
  mutate(pulse=replace(pulse,year==2013 & trip==22 & mmSL>90,1))%>%
  mutate(pulse=replace(pulse,year==2013 & trip == 22 & pulse==3,2))%>%
  mutate(pulse=replace(pulse,year==2013 & pulse==4,3))%>%
  mutate(pulse=replace(pulse,year==2014 & trip<17,1))%>%
  mutate(pulse=replace(pulse,year==2014 & trip==16 & mmSL<43,2))%>%
  mutate(pulse=replace(pulse,year==2014 & trip == 18 & mmSL<35,3))%>%
  mutate(pulse=replace(pulse,year==2014 & trip == 20 & pulse==3 & mmSL>50,2))%>%
  mutate(pulse=replace(pulse,year==2014 & trip == 21 & mmSL>78,1))%>%
  mutate(pulse=replace(pulse,year==2015 & trip == 17 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,year==2015 & trip == 19 & mmSL<65,2))%>%
  mutate(pulse=replace(pulse,year==2015 & trip == 19 & mmSL<38,3))%>%
  mutate(pulse=replace(pulse,year==2015 & mmSL>95,1))%>%
  mutate(pulse=replace(pulse,year==2015 & trip ==22 & pulse == 1 & mmSL<88,2))%>%
  mutate(pulse=replace(pulse,year==2016 & trip<18,1))%>%
  mutate(pulse=replace(pulse,year==2016 & trip==17 & mmSL<41,2))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 19 & mmSL<45,3))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 20 & mmSL<40,4))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 21 & mmSL<43,4))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 21 & pulse == 2 & mmSL<54,3))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 22 & mmSL<50,4))%>%
  mutate(pulse=replace(pulse,year==2016 & trip == 22 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==2017 & trip <20 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==2017 & trip == 19 & mmSL<51,3))%>%
  mutate(pulse=replace(pulse,year==2017 & trip == 21 & mmSL>83,1))%>%
  mutate(pulse=replace(pulse,year==2017 & trip == 21 & mmSL<46,4))%>%
  mutate(pulse=replace(pulse,year==2017 & trip == 21 & is.na(pulse) & mmSL<68,3))%>%
  mutate(pulse=replace(pulse,year==2017 & trip == 21 & is.na(pulse) & mmSL>68,2))%>%
  mutate(pulse=replace(pulse,year==2017 & trip == 22 & mmSL>95,1))%>%
  mutate(pulse=replace(pulse,year==2018 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 18 & mmSL<55,2))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 19 & mmSL<69,2))%>%
  mutate(pulse=replace(pulse,year==2018 & trip==19 & mmSL<53,3))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 20 & pulse == 1 & mmSL<74,2))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 20 & mmSL<58,3))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 20 & mmSL<43,4))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 21 & pulse == 1 & mmSL<83,2))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 21 & pulse == 2 & mmSL<59,3))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 21 & mmSL<45,4))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 22 & mmSL>93,1))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 22 & pulse == 3 & mmSL>68,2))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 23 & pulse == 3 & mmSL>76,2))%>%
  mutate(pulse=replace(pulse,year==2018 & trip == 23 & pulse == 1 & mmSL<100, 2))
#final%>%
#  filter(year==2018 & age == 0)%>%
#  ggplot(aes(y=mmSL,x=trip,colour=factor(pulse)))+geom_point(size=1)

#final$date<-ymd(paste(final$year,final$month,final$day,sep="-"))
final%>%
  filter(year==2018)%>%
  ggplot(aes(x=date,y=mmSL,colour=factor(pulse)))+geom_jitter(size=1)

final_range<-final%>%
  group_by(year,trip,age,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))

write.csv(final_range,"./data/data-working/pulse_range_0_final.csv",row.names = FALSE)
write.csv(final,"./data/data-working/length_pulse_0_final.csv",row.names=FALSE)
