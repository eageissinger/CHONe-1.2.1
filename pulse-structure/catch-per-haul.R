# ---- Calculate catch/haul -----

setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

# ---- load data ----
length<-read.csv("./data/data-working/newman-length-all.csv")
count<-read.csv("./data/data-working/newman-catch.csv")
hauls<-read.csv("./data/data-working/hauls.csv")




# ---- load packages -----
library(tidyverse)
library(lubridate)
#library(arsenal)

# ---- check data ----
summary(length)
str(length)
dim(length)
head(length)
names(length)
length<-length%>%
  filter(species=="AC")

summary(count)
str(count)
dim(count)
head(count)
names(count)

summary(hauls) # missing 2017 and 2018
str(hauls)
dim(hauls)
head(hauls)
names(hauls)
hauls<-hauls%>%
  rename(year=Year,julian.date=J..Start.Date,num_hauls=X..of.Hauls)

# ---- format dates ----
length$date<-ymd(paste(length$year,length$month,length$day,sep="-"))
count$date<-ymd(paste(count$year,count$month,count$day,sep="-"))

# create trips df that can be joined to hauls
trips<-count%>%
  select(year,julian.date,trip)
hauls<-left_join(hauls,trips)

# ---- Calculate extrapolated catch/haul -----
# create total catch column
totalcatch<-count%>%
  #filter(age==0)%>%
  group_by(year,trip,age)%>%
  summarise(total_catch=sum(count))
measured<-length%>%
  #filter(age==0)%>%
  group_by(year,trip,age)%>%
  summarise(total_measured=n())%>%
  mutate(total_measured=replace(total_measured,NA,0))

totalmeasured<-length%>%
  #filter(age==0)%>%
  group_by(year,trip,age,pulse)%>%
  summarise(measured=n())
totalmeasured2<-totalmeasured%>%
  spread(pulse,measured,fill = 0)%>%
  rename(count_1='1',count_2='2',count_3='3',count_4='4',count_5='5',
         count_6='6',count_unknown='<NA>')
names(totalmeasured2)
glimpse(totalmeasured2)
catch_haul<-full_join(totalcatch,measured)
catch_haul<-full_join(catch_haul,totalmeasured2)
catch_haul<-left_join(catch_haul,hauls)
head(catch_haul, n=20)
summary(catch_haul)

extrap<-catch_haul%>%
  mutate(weight=total_catch/total_measured)%>%
  mutate(extrap_1=(count_1*weight)/num_hauls,extrap_2=(count_2*weight)/num_hauls,
         extrap_3=(count_3*weight)/num_hauls,extrap_4=(count_4*weight)/num_hauls,
         extrap_5=(count_5*weight)/num_hauls,extrap_6=(count_6*weight)/num_hauls,
         extrap_unknown=(count_unknown*weight)/num_hauls)%>%
  mutate(total=(total_catch*weight)/num_hauls)%>%
  ungroup()
head(extrap, n = 20)
test<-extrap%>%
  filter(year==2000)
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

# --- export data ----
write.csv(extrap,"./data/data-working/catch_haul.csv",row.names = FALSE)
