# set working directory 
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

# ---- load packages ----
library(tidyverse)
library(lubridate)

# load data
pulserange<-read.csv("./data/data-working/age1-pulse-range.csv")
length<-read.csv("./data/data-working/newman-length.csv")

str(pulserange)

mydata<-pulserange%>%
  rename(pulse=dummy_pulse)%>%
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


#---- Assignments part 2 ----
# deal with the outliers
# start by combining pulse range to length data

pulse_assign1<-data.frame(trip=rep(mydata$trip,mydata$max-mydata$min+1),
                          year=rep(mydata$year,mydata$max-mydata$min+1),
                          month=rep(mydata$month,mydata$max-mydata$min+1),
                          cohort=rep(mydata$cohort,mydata$max-mydata$min+1),
                          pulse=rep(mydata$pulse,mydata$max-mydata$min+1),
                          mmSL=unlist(mapply(seq,mydata$min,mydata$max)))
length2<-length%>%filter(age==1)%>%select(-pulse)
pulse.length<-left_join(length2,pulse_assign1)%>%
  select(-cohort)%>%
  mutate(cohort=year-1)

final<-pulse.length%>%
  mutate(pulse=replace(pulse,cohort==1995 & mmSL<188,1))%>%
  mutate(pulse=replace(pulse,cohort==1995 & mmSL>188,NA))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip==13 & mmSL<100,3))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip==14 & mmSL<100,3))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip==17 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip==18 & mmSL<175,3))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip == 19 & mmSL <175,3))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip==20 & mmSL<180,3))%>%
  mutate(pulse=replace(pulse,cohort==1998 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip==13 &mmSL<75,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip==14,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip==14 & mmSL>80,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip==14 & mmSL>105,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 14 & mmSL<90,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip ==15,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 15 & mmSL<91,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip==16,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 16 & mmSL<125,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 16 & mmSL<101,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 19,5))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip ==19 & mmSL>118,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 19 & mmSL>140,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 20 & pulse==5 & mmSL>120,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 20 & pulse == 2 & mmSL<175,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 21 & mmSL>180,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 22,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 22 & mmSL<165,4))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 22 & mmSL<135,5))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==15,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==15 & mmSL<100,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip==16,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==16 & mmSL>125,1))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==16 & mmSL<100,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip==17,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip==17 & mmSL<120,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip==18,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip==18 & mmSL<125,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==19 & mmSL<126,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==20 & mmSL<135,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 21 & mmSL<138,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==21 & mmSL >138,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 21 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & mmSL<145,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & mmSL>180,1))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip==11 & pulse==1,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip ==11 & mmSL<60,5))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip ==13 & pulse==3 & mmSL<95,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 14 & mmSL>104,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip ==14 & pulse==5 & mmSL>76,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip==15,5))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 15 & mmSL>80,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 15 & mmSL>115,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip ==16,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip ==16 & mmSL<85,5))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip >16,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip>19,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip==10 & mmSL<50,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 10 & is.na(pulse),1))%>%  
  mutate(pulse=replace(pulse,cohort==2002 & trip == 13 & mmSL<74,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 14 & mmSL>128,1))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 14 & pulse == 3 & mmSL>106,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip==15,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip ==15 & mmSL>86,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 15 & mmSL>110,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 15 & mmSL>130,1))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 16 & mmSL>144,1))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 17 & pulse == 4 & mmSL>112,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 18 & pulse == 4 & mmSL>115,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 18 & mmSL>138,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip ==18 & mmSL>160,1))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & pulse==5,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & pulse==4 & mmSL>124,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & mmSL>149,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & mmSL>174,1))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 20 & pulse==4,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 20 & pulse ==3 &mmSL>150,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 20 & mmSL<126,4))%>%
  mutate(pulse=replace(pulse,cohort==2002 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & mmSL<74,5))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & mmSL>120,2))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 14,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip ==14 & mmSL<75,5))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 17 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 17 & mmSL>150,2))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 18 & mmSL<125,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 18 & mmSL>174,2))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 19 & mmSL<150,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 19 & mmSL>175,2))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 19 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & pulse==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 21 & mmSL==175,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2005 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2006 & trip ==12 & mmSL>78,3))%>%
  mutate(pulse=replace(pulse,cohort==2006 & is.na(pulse) & mmSL>90,3))%>%
  mutate(pulse=replace(pulse,cohort==2006 & is.na(pulse),4))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip ==10 & mmSL>94,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 13 & pulse == 1 & mmSL<112,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 13 & pulse == 3 & mmSL>80,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 15,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 15 & mmSL<90,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & mmSL<100,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & mmSL>100,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & mmSL>149,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 17 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 17 & mmSL>150,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 18 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & pulse==4,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 19,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip ==19 & mmSL<125,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 19 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 20,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 20 & mmSL<176,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 20 & mmSL<135,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2008 & pulse ==4 & mmSL>75,2))%>%
  mutate(pulse=replace(pulse,cohort==2008 & is.na(pulse) & mmSL<75,6))%>%
  mutate(pulse=replace(pulse,cohort==2008 & is.na(pulse),4))%>%
  mutate(pulse=replace(pulse,cohort==2008 & mmSL>149,2))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip >13 & mmSL<90,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip >13& mmSL>90,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & mmSL>120,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & pulse ==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip ==13 & pulse==2 & mmSL<99,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip ==14 & mmSL<110,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip ==14 & mmSL>110,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & is.na(pulse) & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip ==10 & mmSL<58,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 12 & mmSL>86,1))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 12 & mmSL<65,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip==13,2))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip ==13 & mmSL<75,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 13 & mmSL >98,1))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 18 & mmSL>148,1))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 18 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip==19,1))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip ==19 & mmSL<151,2))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 19 & mmSL<135,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip ==20 & mmSL>160,1))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 20 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2011 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & is.na(pulse) & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip ==21 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,cohort==2011 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip ==9 & mmSL>100,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 16 & mmSL>125,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 16 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 17 & mmSL<125,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 18,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 18 & mmSL<145,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 18 & mmSL<126,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 19 & mmSL<153,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 19 & mmSL<130,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 20 & mmSL>160,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 21 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 22 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip ==10 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 13 & mmSL>150,1))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 13 & mmSL<70,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip ==13 & pulse ==2 & mmSL<100,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & is.na(pulse) & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip==16 & mmSL<90,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 17 & mmSL<102,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 17 & pulse==2 & mmSL<125,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 17 & pulse == 1 & mmSL<170,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 18 & pulse == 2 & mmSL<134,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 18 & pulse == 3 & mmSL<105,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 18 & pulse ==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 19 & mmSL<107,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 19 & pulse==2 & mmSL<138,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 19 & pulse == 1 & mmSL<175,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 20 & mmSL<111,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 20 & pulse == 2 & mmSL<145,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 20 & pulse ==1 & mmSL<175,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 21 & mmSL<112,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 21 & pulse==2 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 21 & mmSL>180,1))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip ==22 & pulse == 2 & mmSL<157,3))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 10 & pulse == 3 & mmSL<59,4))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 13 & mmSL<85,4))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 13 & mmSL>85,3))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 13 & mmSL>110,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 14,4))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 14 & mmSL>95,3))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 14 & mmSL>115,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip <17 & is.na(pulse),4))%>%
  mutate(pulse=replace(pulse,cohort==2014 & is.na(pulse) & mmSL<124,4))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip ==18 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 18 & mmSL>140,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2014 & mmSL>149,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip ==13 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 13 & pulse == 3 & mmSL>100,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 14 & mmSL>100,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip ==15 & mmSL>100,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 15 & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip ==16,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 16 & mmSL>108,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 16 & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 17 & mmSL<112,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip ==17 & pulse==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 17 & pulse == 2 & mmSL<128,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 18 &mmSL>130,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 20 & mmSL<124,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 20 & pulse == 2 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & mmSL<126,4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & mmSL>199,1))%>%
  mutate(pulse=replace(pulse,cohort==2015 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & pulse==1 & mmSL<200,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 10 & pulse==4 & mmSL>45,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 10 & pulse == 2 & mmSL<70,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & mmSL>110,1))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse == 1 & mmSL<124,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse == 3 & mmSL>100,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 15,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 15 &mmSL>105,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 15 & mmSL<90,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 16 & pulse == 2 & mmSL<115,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 16 & mmSL>140,1))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 17 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 17 & pulse ==3 & mmSL>118,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 17 & pulse == 1 & mmSL<155,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 18 & mmSL<112,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 18 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 18 & pulse == 1 & mmSL<167,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 18 & pulse ==2 & mmSL<134,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 19 & mmSL<120,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 19 & pulse ==2 & mmSL<137,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 19 & pulse == 1 & mmSL<176,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 20 & pulse==3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 20 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip >= 21 & mmSL<125,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip >= 21 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 9 & pulse ==2 & mmSL<91,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & pulse == 1 & mmSL<118,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 9 & pulse == 4 & mmSL>62,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 13 & pulse == 3 & mmSL>110,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 13 & mmSL>135,1))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 14 & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 14 & pulse == 4 & mmSL>86,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 15 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 15 & pulse == 2 & mmSL<128,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 16,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 16 & mmSL<100,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 16 & mmSL>150,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 17 & pulse==4 & mmSL>108,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 18 & mmSL>150,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 18 & pulse ==4 & mmSL>125,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 20 & mmSL>136,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 21 & mmSL>175,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 21 & mmSL<150,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 21 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip >= 22, 4))


final%>%
  filter(cohort==2002)%>%
  ggplot(aes(y=mmSL,x=trip,colour=factor(pulse)))+geom_jitter(size=1)
 pulse.length%>%filter(cohort==2017)%>%distinct(trip)
#final$date<-ymd(paste(final$year,final$month,final$day,sep="-"))

final_range<-final%>%
  group_by(year,trip,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))%>%
  filter(!is.na(pulse))
# as complete as it can be
write.csv(final_range,"./data/data-working/pulse_range_age1_final.csv",row.names = FALSE)
