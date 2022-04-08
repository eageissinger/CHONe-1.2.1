# ---- Calculate catch/haul -----

setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/")

# ---- load data ----
length<-read.csv("./data/data-working/newman-length.csv")
catch<-read.csv("./data/data-working/newman-catch.csv")%>%
  filter(Include.Exclude==1)%>%
  filter(Species=="AC" & Age==0 | Age==1)%>%
  dplyr::select(-Include.Exclude)
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
  filter(Species=="AC")%>%
  filter(!is.na(Year))


summary(catch)
str(catch)
dim(catch)
head(catch)
names(catch)

summary(hauls) 
str(hauls)
dim(hauls)
head(hauls)
names(hauls)
hauls<-hauls%>%
  rename(Julian.Date=J..Start.Date,num_hauls=X..of.Hauls)

# ---- format dates ----
length$Date<-ymd(paste(length$Year,length$Month,length$Day,sep="-"))

count2<-catch%>%
 mutate(Month=as.integer(str_sub(Date,start=5,end=6)),
         Day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(Date=ymd(paste(Year,Month,Day,sep="-")))

# create trips df that can be joined to hauls
trips<-count2%>%
  dplyr::select(Year,Julian.Date,Trip)
hauls<-left_join(hauls,trips)

# ---- Calculate extrapolated catch/haul -----
# create total catch column
totalcatch<-count2%>%
  filter(Age<=1)%>%
  group_by(Year,Trip,Age)%>%
  summarise(total_catch=sum(Count))%>%
  ungroup()
# create measured (total)
measured<-length%>%
  filter(Age<=1)%>%
  group_by(Year,Trip,Age)%>%
  summarise(total_measured=n())%>%
  mutate(total_measured=replace(total_measured,NA,0))%>%
  ungroup()

# total measured by Pulse
totalmeasured<-length%>%
  filter(Age<=1)%>%
  group_by(Year,Trip,Age,Pulse)%>%
  summarise(measured=n())%>%
  ungroup()
# spread into wide format, with NAs as 0's
totalmeasured2<-totalmeasured%>%
  spread(Pulse,measured,fill = 0)%>%
  rename(count_1='1',count_2='2',count_3='3',count_4='4',count_5='5',
         count_6='6',count_unknown='<NA>')%>%
  ungroup()
names(totalmeasured2)
glimpse(totalmeasured2)

# combind into single files
catch_haul<-full_join(totalcatch,measured,by=c("Year","Trip","Age"))
catch_haul<-full_join(catch_haul,totalmeasured2)
catch_haul<-left_join(catch_haul,hauls)
head(catch_haul, n=20)
summary(catch_haul)

# calculate extrapalated catch, based on haul number
extrap<-catch_haul%>%
  mutate(weight=total_catch/total_measured)%>%
  mutate(extrap_1=(count_1*weight)/num_hauls,extrap_2=(count_2*weight)/num_hauls,
         extrap_3=(count_3*weight)/num_hauls,extrap_4=(count_4*weight)/num_hauls,
         extrap_5=(count_5*weight)/num_hauls,extrap_6=(count_6*weight)/num_hauls,
         extrap_unknown=(count_unknown*weight)/num_hauls)%>%
  mutate(total=(total_catch*weight)/num_hauls)%>%
  ungroup()%>%
  distinct()
head(extrap, n = 20)

# compare to excel version
test<-extrap%>%
  filter(Year==2000)
View(test)


# ---- check if my dataframe matches old one (excel version) ----
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
write.csv(extrap,"./data/output/catch_haul.csv",row.names = FALSE)
