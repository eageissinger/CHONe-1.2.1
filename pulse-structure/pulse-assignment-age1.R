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
  mutate(pulse=replace(pulse,cohort==1998 & trip == 10 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 18 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 18 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 20 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 14 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 19 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 19 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 20 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 21 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 4,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse == 5,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 10 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 11 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 11 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 13 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 14 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 17 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 17 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 20 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 20 & pulse >= 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 9 & pulse == 4,5))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 9 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 9 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & pulse == 1,NA))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & pulse == 2,3))%>% 
  mutate(pulse=replace(pulse,cohort==2004 & trip == 18 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 20 & pulse == 1,NA))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 20 & pulse == 2,1))%>%
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
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & pulse ==5,NA))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 18 & pulse == 5,NA))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 18 & pulse == 3,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 18 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 18 & pulse == 4,2))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 9 & pulse == 3,6))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip == 9 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 10 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 13 & pulse == 2,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 13 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 10 & pulse == 1,2))%>% 
  mutate(pulse=replace(pulse,cohort==2010 & trip == 12 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 12 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 15 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 15 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 15 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 12 & pulse == 1,4))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 20 & pulse ==3,4))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 9 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 9 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 20 & pulse == 3,NA))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 21 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 21 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 21 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 10 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 10 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 14 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 14 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 20 & pulse ==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 22 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 17 & pulse == 1,NA))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 17 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 17 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 12 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 12 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 12 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 14 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 14 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 15 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 16 & pulse == 3,NA))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 16 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 16 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 18 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 19 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 20 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 20 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & pulse == 4,NA))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 10 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 13 & pulse ==3,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse==3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 16 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 16 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 17 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 19 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 20 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 9 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 13 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 13 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 14 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 15 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 15 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 17 & pulse == 1,NA))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 17 & pulse == 2,1))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 17 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 18 & pulse == 3,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 19 & pulse == 1,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 20 & pulse == 3,4))
#temporary...
#write.csv(mydata,"./data/data-working/pulse_range_age1_final.csv",row.names = FALSE)


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
  mutate(pulse=replace(pulse,cohort==1995,NA))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip<=16 & mmSL>124,1))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip==17 & mmSL>168,1))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip ==18 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip >= 19 & mmSL>201,1))%>%
  mutate(pulse=replace(pulse,cohort==1998 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==1998 & pulse ==4,1))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip ==17 & mmSL>150,1))%>%
  mutate(pulse=replace(pulse,cohort==1998 & trip == 17 & mmSL<150,2))%>%
  
  mutate(pulse=replace(pulse,cohort==1999 & trip == 13 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip ==13 & pulse == 1 & mmSL<130,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 14 & mmSL>110,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 15,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 16 & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 17 & pulse == 2,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 17 & pulse==1,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 18 & pulse == 2 & mmSL<149,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 19 & mmSL>159,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 19 & mmSL<160,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 19 & mmSL<120,NA))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 20 & pulse ==2,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 20 & mmSL>160,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 20 & mmSL<124,NA))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 21 & mmSL>174,2))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 21 & mmSL<175,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 21 & mmSL<134,NA))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip == 22 & mmSL>135,3))%>%
  mutate(pulse=replace(pulse,cohort==1999 & trip<17 & is.na(pulse),3))%>%
  
  
  mutate(pulse=replace(pulse,cohort==2000 & trip == 14 & pulse ==2 & mmSL<93,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==15,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==16 & mmSL>118,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 17 & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==18& mmSL>138,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 19 & mmSL<149,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 19 & pulse ==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip==20& mmSL<160,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 20 & pulse==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 21 & mmSL<168,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip ==21 & pulse==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 21 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & mmSL<180,3))%>%
  mutate(pulse=replace(pulse,cohort==2000 & trip == 22 & pulse ==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip ==11 & pulse ==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 11 & pulse==4 & mmSL>53,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 13 & pulse == 2 & mmSL<95,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip ==14 & mmSL<75,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 14 & pulse == 2 & mmSL<99,3))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 15 & mmSL<85,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 16 & mmSL<90,4))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip == 15 & mmSL>105,2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & trip >19,2))%>%
  mutate(pulse=replace(pulse,cohort==2001 & is.na(pulse),3))%>%
  
  mutate(pulse=replace(pulse,cohort==2002 & trip == 13 & mmSL<97,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip ==13 & mmSL>118,1))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip ==14 & pulse == 1 & mmSL<126,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 14 & mmSL<103,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 15 & mmSL>135,1))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 15 & mmSL<108,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip ==15 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 16 & pulse==1 & mmSL<145,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 16 & mmSL<115,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 18 & mmSL>160,1))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 19 & mmSL>165,1))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 20 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 20 & pulse == 3 & mmSL>130,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 22 & mmSL<140,3))%>%
  mutate(pulse=replace(pulse,cohort==2002 & trip == 22 & mmSL>140,2))%>%
  mutate(pulse=replace(pulse,cohort==2002 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip ==13 & mmSL>123,1))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 13 & mmSL<70,5))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 14,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 14 & mmSL<75,5))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 17 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 18 & mmSL>149,1))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip ==18 & mmSL>174,NA))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 18 & mmSL<124,4))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 19 & mmSL<160,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 20 & mmSL<161,3))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 19 & is.na(pulse) & mmSL<175,1))%>%
  mutate(pulse=replace(pulse,cohort==2004 & trip == 21 & mmSL<176,1))%>%
  mutate(pulse=replace(pulse,cohort==2005,3))%>%
  mutate(pulse=replace(pulse,cohort==2006 & trip==12 & mmSL>90,3))%>%
  mutate(pulse=replace(pulse,cohort==2006 & trip ==14,4))%>%
  mutate(pulse=replace(pulse,cohort==2006 & trip == 15 &mmSL<125,4))%>%
  mutate(pulse=replace(pulse,cohort==2006 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip ==10 & mmSL>101,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip ==13 & pulse ==1 & mmSL<112,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & pulse==1 & mmSL<126,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 17 & mmSL>125,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip >= 19 & mmSL>138,1))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 12 & pulse ==3 & mmSL>76,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 13 & pulse == 3 &mmSL>81,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 15 & mmSL>90,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 16 & pulse == 3 & mmSL>100,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip == 17 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & trip >=19 & is.na(pulse) & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2007 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2008 & is.na(pulse),6))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip==13 & mmSL>85,4))%>%
  mutate(pulse=replace(pulse,cohort==2008 & trip==16,4))%>% # Bob needs to check this year
  mutate(pulse=replace(pulse,cohort==2008 & trip ==17,4))%>%
  mutate(pulse=replace(pulse,cohort==2008 & mmSL>155,4))%>% # 2008 was difficult to assign
  mutate(pulse=replace(pulse,cohort==2009 & trip == 13 & pulse == 2 & mmSL<95,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip ==14 & mmSL<100,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 15,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 15 & mmSL>100,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 16,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 16 & mmSL>110,3))%>%
  mutate(pulse=replace(pulse,cohort==2009 & trip == 18 & mmSL<100,4))%>%
  mutate(pulse=replace(pulse,cohort==2009 & is.na(pulse),2))%>% # 2009 needs a double chekc
  mutate(pulse=replace(pulse,cohort==2010 & trip == 13 & pulse ==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 15 & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 14 & mmSL>130,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip ==20 & mmSL>180,2))%>%
  mutate(pulse=replace(pulse,cohort==2010 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 16,3))%>%
  mutate(pulse=replace(pulse,cohort==2010 & trip == 17,3))%>% # 2010 might be off
  mutate(pulse=replace(pulse,cohort==2011 & trip == 12 & mmSL>86,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 13 & mmSL>105,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 18 & mmSL>148,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 19 & mmSL>151,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 19 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 21 & mmSL>150,3))%>%
  mutate(pulse=replace(pulse,cohort==2011 & trip == 21 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,cohort==2011 & is.na(pulse),4))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip==9 & mmSL>100,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 16 & mmSL>125,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 16 & mmSL<126,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 17 & mmSL>135,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 18 & mmSL>140,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 19 & pulse==1 & mmSL<150,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 20 & pulse == 1 &mmSL<151,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 21 & mmSL>160,1))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 17 & pulse==3 & mmSL>115,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 18 & mmSL<126,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 19 & mmSL<135,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 20 & pulse == 3 & mmSL>136,2))%>%
  mutate(pulse=replace(pulse,cohort==2012 & trip == 22 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2012 & is.na(pulse),2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 10 & is.na(pulse),1))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 13 & pulse==1 & mmSL<150,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 16 & mmSL>165,1))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 17 & mmSL>170,1))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip ==18 & pulse==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip==19 & pulse==1 & mmSL<175,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 20 & pulse==1 & mmSL<175,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip ==21 & mmSL>174,1))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 22 & mmSL>175,1))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 13 & pulse==2 & mmSL<100,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 15 & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 16 & pulse == 4 & mmSL>118,2))%>%
  
  mutate(pulse=replace(pulse,cohort==2013 & trip == 17 & pulse ==3 & mmSL> 130,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip ==17 & pulse == 1 & mmSL<164,2))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 17 & pulse==2 & mmSL<131,3))%>%
  
  mutate(pulse=replace(pulse,cohort==2013 & trip == 18 & mmSL<140,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 19 & mmSL<151,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 21 & mmSL<159,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 22 & mmSL<159,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 13 & mmSL<69,4))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 15 & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 16 & pulse == 4 & mmSL>90,3))%>%
  mutate(pulse=replace(pulse,cohort==2013 & trip == 17 & mmSL<90,4))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 10 & pulse ==1 & mmSL<88,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 10 & mmSL<58,4))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip ==13 & mmSL>105,1))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 14 & mmSL>110,1))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 17 & mmSL>132,1))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 18 & mmSL>140,1))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip >= 19 & mmSL>149,1))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 13 & is.na(pulse) & mmSL>90,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 14 & is.na(pulse) & mmSL>95,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 16 & mmSL>105,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 17 & pulse==3 & mmSL>116,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 18 & is.na(pulse) & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 19 & is.na(pulse) & mmSL>125,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 20 & mmSL>130,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & trip == 22 & is.na(pulse) & mmSL>135,2))%>%
  mutate(pulse=replace(pulse,cohort==2014 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2014 & is.na(pulse),4))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 12 & pulse ==1 & mmSL<100,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 13 & pulse == 1 & mmSL<120,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 15 & pulse == 3 & mmSL>110,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 17 & pulse == 2 & mmSL<128,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 18 & pulse == 2 & mmSL<140,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 19 & pulse == 2 & mmSL<130,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & pulse == 2 & mmSL<120,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & is.na(pulse) & mmSL>110,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 12 & mmSL>175,NA))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 22 & mmSL>200,1))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 22 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 16 & mmSL>115,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 17 & mmSL>128,2))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 17 & mmSL>150,1))%>%
  mutate(pulse=replace(pulse,cohort==2015 & pulse == 4,3))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 20 & mmSL>176,1))%>%
  mutate(pulse=replace(pulse,cohort==2015 & trip == 21 & mmSL>185,1))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip ==13 & mmSL>111,1))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 14 & pulse == 1 & mmSL<124,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 16 & mmSL>140,1))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 16 & pulse == 4 & mmSL>100,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip ==17 &  mmSL>135,1))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 17 & pulse == 3 & mmSL>100,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 17 & pulse == 3,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 18 & pulse == 1 & mmSL<145,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 18 & mmSL<100,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 19 & mmSL>145,1))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 19 & mmSL<108,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 20 & mmSL<109,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 15 & mmSL<90,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip == 15 & mmSL>94,2))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip >= 21 & mmSL<112,4))%>%
  mutate(pulse=replace(pulse,cohort==2016 & trip >=21 & is.na(pulse),2))%>%
  # must fix 2017
  mutate(pulse=replace(pulse,cohort==2017 & trip == 9 & pulse==1 & mmSL<118,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 9 & pulse ==4 & mmSL>60,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 9 & pulse ==2 & mmSL<92,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 13 & pulse==1 & mmSL<140,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 13 & pulse == 2 & mmSL<120,3))%>%
  
  mutate(pulse=replace(pulse,cohort==2017 & trip == 14 & pulse == 4 & mmSL>87,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 14 & pulse == 2 & mmSL<125,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 15 & pulse == 1 & mmSL<175,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 15 & pulse == 2 & mmSL<126,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 16,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 16 & mmSL<100,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 16 & mmSL>150,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 17 & pulse ==4 & mmSL>108,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 17 & pulse == 2 & mmSL<149,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 17 & mmSL>170,1))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 18 & mmSL<115,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 18 & pulse==2 & mmSL<150,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 18 & pulse == 1 & mmSL<175,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 19 & mmSL<125,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 19 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 20 & pulse ==1,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 20 & mmSL<127,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 20 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 20 & mmSL>160,2))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 21 & mmSL<130,4))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 21 & pulse==2,3))%>%
  mutate(pulse=replace(pulse,cohort==2017 & trip == 22,4))


# --- check results ----
# ---- All years as function -----
cohort.graph<-function(length_pulse,na.rm=FALSE, ...){
  length_pulse$date<-ymd(paste(length_pulse$year,length_pulse$month,length_pulse$day))
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
cohort.graph(final)




final%>%
  filter(cohort==2013)%>%
  ggplot(aes(y=mmSL,x=trip,colour=factor(pulse)))+geom_point(size=1)
pulse.length%>%filter(cohort==1999)%>%distinct(trip, month, day)
#final$date<-ymd(paste(final$year,final$month,final$day,sep="-"))

final_range<-final%>%
  group_by(year,trip,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))%>%
  filter(!is.na(pulse))
# as complete as it can be
write.csv(final_range,"./data/data-working/pulse_range_age1_final.csv",row.names = FALSE)
write.csv(final,"./data/data-working/length_pulse_age1.csv",row.names = FALSE)
