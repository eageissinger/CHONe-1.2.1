# ---- Calculate catch/haul -----

setwd("C:/Users/Emilie/Dropbox/Thesis/Research/CHONe-1.2.1/")

# ---- load data ----
range_final<-read.csv("./data/data-working/pulse_range_mayjuly.csv")
length<-read.csv("./data/data-working/newman-length.csv")
count<-read.csv("./data/data-working/newman-catch.csv")
trips<-read.csv("./data/data-working/trip-dates-newman.csv")
hauls<-read.csv("./data/data-working/hauls.csv")

# ---- load packages -----
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(arsenal)

# ---- check data ----
summary(range_final)
str(range_final)
dim(range_final)
head(range_final)
names(range_final)

summary(length)
str(length)
dim(length)
head(length)
names(length)

summary(count)
str(count)
dim(count)
head(count)
names(count)

summary(trips)
str(trips)
dim(trips)
head(trips)
names(trips)

summary(hauls)
str(hauls)
dim(hauls)
head(hauls)
names(hauls)
hauls<-hauls%>%
  rename(year=Year,julian.date=J..Start.Date,num_hauls=X..of.Hauls)

# ---- format dates ----
range_final$date<-ymd(paste(range_final$year,range_final$month,range_final$day,sep="-"))
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
count$date<-ymd(paste(count$year,count$month,count$day,sep="-"))

trips$year<-as.integer(str_sub(trips$Date,start=1,end=4))
trips$month<-as.integer(str_sub(trips$Date,start=5,end=6))
trips$day<-as.integer(str_sub(trips$Date,start=7, end=8))
trips$date<-ymd(paste(trips$year,trips$month,trips$day,sep="-"))
trips$julian.date<-yday(trips$date)
trips<-trips%>%
  select(-Date)%>%
  rename(trip=Trip)


count<-left_join(count,trips)
hauls<-left_join(hauls,trips)

# add trips to length
length<-length%>%
  select(-trip)
length2<-left_join(length,trips)

# ---- Combine pulse and length data ----
age0length<-length2%>%
  filter(age==0)
names(age0length)

pulse_assign<-data.frame(trip=rep(range_final$trip,range_final$max-range_final$min+1),
                         date=rep(range_final$date,range_final$max-range_final$min+1),
                         cohort=rep(range_final$cohort,range_final$max-range_final$min+1),
                         pulse=rep(range_final$pulse,range_final$max-range_final$min+1),
                         mmSL=unlist(mapply(seq,range_final$min,range_final$max)))
length3<-length2%>%
  select(-pulse)
age1length<-left_join(length3,pulse_assign)

length_pulse<-bind_rows(age0length,age1length)
str(length_pulse)

# ---- Calculate extrapolated catch/haul -----
# create total catch column
totalcatch<-count%>%
  group_by(year,trip,age)%>%
  summarise(total_catch=sum(count))
measured<-length_pulse%>%
  group_by(year,trip,age)%>%
  summarise(total_measured=n())%>%
  mutate(total_measured=replace(total_measured,NA,0))

totalmeasured<-length_pulse%>%
  group_by(year,trip,age,pulse)%>%
  summarise(measured=n())
totalmeasured2<-totalmeasured%>%
  spread(pulse,measured,fill = 0)%>%
  rename(p1='1',p2='2',p2.3='2/3',p3='3',p4='4',p5='5',p6='6',unknown='<NA>')
names(totalmeasured2)
glimpse(totalmeasured2)
catch_haul<-full_join(totalcatch,measured)
catch_haul<-full_join(catch_haul,totalmeasured2)
catch_haul<-left_join(catch_haul,hauls)
View(catch_haul)
summary(catch_haul)

extrap<-catch_haul%>%
  mutate(weight=total_catch/total_measured)%>%
  mutate(extrap_1=(p1*weight)/num_hauls,extrap_2=(p2*weight)/num_hauls,
         extrap_2.3=(p2.3*weight)/num_hauls,
         extrap_3=(p3*weight)/num_hauls,extrap_4=(p4*weight)/num_hauls,
         extrap_5=(p5*weight)/num_hauls,extrap_6=(p6*weight)/num_hauls,
         extrap_unknown=(unknown*weight)/num_hauls)%>%
  mutate(total=(total_catch*weight)/num_hauls)%>%
  ungroup()
View(extrap)
test<-extrap%>%
  filter(age==0 & year==2000)
View(test)


# ---- check if my dataframe matches old one ----
original<-read.csv("./data/data-working/catch-pulse-newman.csv")
names(original)
names(extrap)
testextrap<-extrap%>%
  filter(age==0)%>%
  select(-trip,-age,-total_catch,-total_measured,-unknown,-month,-day,
         -date,-weight,-extrap_6,-p6,-extrap_unknown,-total)%>%
  rename(count_1=p1,count_2=p2,count_3=p3,count_4=p4,count_5=p5,
         j_start_date=julian.date)
compare(original,testextrap)
testextrap%>%
  filter(is.na(num_hauls))
# data sets have 23 additional observations in new calculation, 
# but everything else matches

write.csv(extrap,"./data/data-working/catch_haul.csv",row.names = FALSE)
