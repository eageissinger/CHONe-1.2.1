### Mark-recapture ###

# ---- set working directory ----
setwd("C:/Users/USER/Documents/Research/CHONe-1.2.1/")

# ---- load required packages ----
library(tidyverse)
library(lubridate)
library(RMark)

# ---- load data ----
data<-read.csv("./data/data-working/mark-history-field-adj.csv")
subsample<-read.csv("./data/data-working/subsample_wk1-2-field.csv")
pulse0<-read.csv("./data/data-working/age-0-pulse-range.csv")
pulse1<-read.csv("./data/data-working/age1_dummypulse.csv")
trips<-read.csv("./data/data-working/trip-dates-newman.csv")

# ---- check data ----
names(data)
str(data)
summary(data)
head(data)

names(subsample)
str(subsample)
summary(subsample)
head(subsample)

str(trips)
# format date
data$date<-ymd(paste(data$year,data$month,data$day,sep="-"))
subsample$date<-ymd(paste(subsample$Year,subsample$Month,subsample$Day,sep="-"))

trips$year<-as.numeric(str_sub(trips$Date,start=1,end=4))
trips$month<-as.numeric(str_sub(trips$Date,start=5,end=6))
trips$day<-as.numeric(str_sub(trips$Date,start=7, end=8))
trips$date<-ymd(paste(trips$year,trips$month,trips$day,sep="-"))

trips<-trips%>%
  select(-Date)%>%
  rename(trip=Trip)

data<-left_join(data,trips)
                
# summary table for age1 pulse
# summary table for age1 pulse
pulse1<-pulse1%>%
  group_by(year,cohort,trip,month,pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL))%>%
  mutate(age=1)%>%
  filter(year==2017 & month == 5)
names(pulse0)
pulse0<-pulse0%>%rename(year=Year,cohort=Cohort,month=Month,trip=Trip..,
                        age=Age,pulse=Pulse,min=Min.size,max=Max.size,
                        notes=Notes)%>%
  filter(year==2016 & month ==10)
names(pulse1)

pulse.range<-bind_rows(pulse0,pulse1)%>%
  select(-notes)%>%
  mutate(max=replace(max,is.na(max),31))

size<-data.frame(month=rep(pulse.range$month,pulse.range$max-pulse.range$min+1),
                 cohort=rep(pulse.range$cohort,pulse.range$max-pulse.range$min+1),
                 year=rep(pulse.range$year,pulse.range$max-pulse.range$min+1),
                 trip=rep(pulse.range$trip,pulse.range$max-pulse.range$min+1),
                 age=rep(pulse.range$age,pulse.range$max-pulse.range$min+1),
                 pulse=rep(pulse.range$pulse, pulse.range$max-pulse.range$min+1),
                 sl=unlist(mapply(seq,pulse.range$min,pulse.range$max)))

# ----- pulse assign ----
test<-distinct(data) #no repeats
data$site<-str_sub(data$animal_id,start = 1,end = 2)
data$id<-as.numeric(str_sub(data$animal_id,start=4))
head(data)

# assign pulse to all fish with AD =0
df1<-data%>%
  filter(id<1)%>%
  select(-id)
duplicated(df1)

df2<-data%>%
  filter(AD==1 & day == 19)%>%
  select(-id)
duplicated(df2)

df3<-data%>%
  filter(AD==1 & day == 28)%>%
  select(-id)
duplicated(df3)

df4<-data%>%
  filter(AD==1 & month ==5)%>%
  select(-id)
duplicated(df4)

data2<-bind_rows(df1,df2,df3,df4) # individual fish
pulse.size<-left_join(data2,size)
# need to combine individual fish pulse assignments to full data
# without losing anything. 
# Assign pulse based on animal_id
data3<-data%>%
  select(-id)
pulse.size<-pulse.size%>%
  select(animal_id,pulse)
head(pulse.size)

final<-left_join(data3,pulse.size)

final%>%
  filter(month!=5)%>%
  ggplot(aes(y=sl,x=date,colour=factor(pulse)))+
  geom_jitter()
# fill in missing pulses for week between trip 19 and trip 20
test<-final[1:305,]

test$id<-as.numeric(str_sub(test$animal_id,start=4))

test1<-test%>%
  filter(id<1)
test2<-test%>%
  filter(AD==1)
test3<-bind_rows(test1,test2)

ggplot(test3,aes(x=date,y=sl,colour=factor(pulse)))+
  geom_point()

test4<-final%>%
  mutate(pulse=replace(pulse,date=="2016-10-28" & is.na(pulse),3))%>%
  mutate(pulse=replace(pulse,date=="2016-10-19" & is.na(pulse) & sl<50,3))%>%
  mutate(pulse=replace(pulse,date=="2016-10-19" & is.na(pulse) & sl<75,2))%>%
  mutate(pulse=replace(pulse,date=="2016-10-19" & is.na(pulse) & sl>75,1))

test4%>%
  filter(month!=5)%>%
  ggplot(aes(x=date,y=sl,colour=factor(pulse)))+
  geom_jitter()

final<-test4
# ---- preliminary analysis: size distribution ----

# select individual fish, size, and last date captured
# week 1
wk1<-subsample%>%
  select(SL,date)%>%
  filter(date=="2016-10-13")%>%
  data.frame()
wk2<-subsample%>%
  select(SL,date)%>%
  filter(date=="2016-10-19")%>%
  data.frame()
wk3<-data%>%
  select(animal_id,sl,date)%>%
  filter(date=="2016-10-28")%>%
  data.frame()
mayNB<-data%>%
  select(animal_id,sl,date,site,age)%>%
  filter(age==1 & date == "2017-05-24" & site == "NB" & sl >= 55)%>%
  data.frame()
mayMS<-data%>%
  select(animal_id,sl,date,site,age)%>%
  filter(age==1 & date=="2017-05-24" & site == "MS"& sl >= 55)%>%
  data.frame()
mayCC<-data%>%
  select(animal_id,sl,date,site,age)%>%
  filter(age==1 & date=="2017-05-24" & site == "CC" & sl >= 55)%>%
  data.frame()
names(wk1)<-c("sl","date")
names(wk2)<-c("sl","date")



oct<-bind_rows(wk1,wk2,select(wk3,sl,date))
may<-bind_rows(mayNB,mayMS,mayCC)

# ---- Size frequency plots ----

ggplot(wk1,aes(x=sl))+
  geom_histogram(binwidth = 5,colour='black',fill='white',size=1)+
  xlim(25,150)+
  ggtitle("October 14, 2016")+
  theme_gray()+
  xlab("Standard length (mm)")+ylab("Count")+
  theme(axis.title=element_text(size=20,face='bold'),
        axis.text=element_text(size=16,face='bold'))+
  theme(plot.title=element_text(hjust=.5,size=24,face='bold'))

ggplot(wk2,aes(x=sl))+
  geom_histogram(binwidth = 5,colour='black',fill='white',size=1)+
  xlim(25,153)+
  ggtitle("October 19, 2016")+
  theme_grey()+
  xlab("Standard length (mm)")+ylab("Count")+
  theme(axis.title=element_text(size=20,face='bold'),
        axis.text=element_text(size=16,face='bold'))+
  theme(plot.title=element_text(hjust=.5,size=24,face='bold'))

ggplot(wk3,aes(x=sl))+
  geom_histogram(binwidth = 5,colour='black',fill='white',size=1)+
  xlim(25,150)+
  ggtitle("October 28, 2016")+
  theme_grey()+
  xlab("Standard length (mm)")+ylab("Count")+
  theme(axis.title=element_text(size=20,face='bold'),
        axis.text=element_text(size=16,face='bold'))+
  theme(plot.title=element_text(hjust=.5,size=24,face='bold'))

ggplot(oct,aes(x=sl))+
  geom_histogram(binwidth = 5, colour= 'black',fill='white',size=1)+
  xlim(25,150)+
  ggtitle("October 2016")+
  theme_grey()+
  xlab("Standard length (mm)")+ylab("Count")+
  theme(axis.title=element_text(size=20,face='bold'),
        axis.text=element_text(size=16,face='bold'))+
  theme(plot.title=element_text(hjust=.5,size=24,face='bold'))+
  theme(axis.title.x=element_blank())+
  ylim(0,50)

ggplot(mayNB,aes(x=sl))+
  geom_histogram(binwidth = 5,colour='black',fill='white',size=1)+
  xlim(25,153)+
  ggtitle("Newbridge Cove")+
  theme_grey()+
  xlab("Standard length (mm)")+ylab("Count")+
  theme(axis.title=element_text(size=20,face='bold'),
        axis.text=element_text(size=16,face='bold'))+
  theme(plot.title=element_text(hjust=.5,size=24,face='bold'))+
  ylim(0,35)+
  theme(axis.title=element_blank())

ggplot(mayMS,aes(x=sl))+
  geom_histogram(binwidth = 5,colour='black',fill='white',size=1)+
  xlim(25,150)+
  ggtitle("Mistaken Cove")+
  theme_grey()+
  xlab("Standard length (mm)")+ylab("Count")+
  theme(axis.title=element_text(size=20,face='bold'),
        axis.text=element_text(size=16,face='bold'))+
  theme(plot.title=element_text(hjust=.5,size=24,face='bold'))+
  ylim(0,35)+
  theme(axis.title=element_blank())
       
       
ggplot(mayCC,aes(x=sl))+
  geom_histogram(binwidth = 5,colour='black',fill='white',size=1)+
  xlim(25,150)+
  ggtitle("Canning's Cove")+
  theme_grey()+
  xlab("Standard length (mm)")+ylab("Count")+
  theme(axis.title=element_text(size=20,face='bold'),
        axis.text=element_text(size=16,face='bold'))+
  theme(plot.title=element_text(hjust=.5,size=24,face='bold'))+
  ylim(0,35)+
  theme(axis.title.x=element_blank())

ggplot(may,aes(x=sl))+
  geom_histogram(binwidth = 5, colour='black',fill='white',size=1)+
  xlim(25,153)+
  ggtitle("24 May 2017")+
  theme_grey()+
  xlab("Standard length (mm)")+ylab("Count")+
  theme(axis.title=element_text(size=20,face='bold'),
        axis.text=element_text(size=16,face='bold'))+
  theme(plot.title=element_text(hjust=.5,size=24,face='bold'))+
  ylim(0,50)

 # ---- mark analysis ----

# select dat that is within popultion
# sl >55 mm and age <2

mrkdata<-final%>%
  filter(age<2)%>%
  select(date,year,month,animal_id,sl,mark,AD,pulse)%>%
  data.frame()

# Determine time-steps
collection.dates<-distinct(mrkdata,date)
year<-2016
month<-12
day<-31
reference<-as.data.frame(cbind(year,month,day))
reference$date<-ymd(paste(reference$year,reference$month,reference$day,sep="-"))
reference<-select(reference,date)
collection.dates<-rbind(collection.dates,reference)
collection.dates$julian<-yday(collection.dates$date)
collection.dates<-mutate(collection.dates,mark.period=julian-287)
collection.dates$mark.period[collection.dates$mark.period==-143]<-(144+79)
collection.dates<-collection.dates[1:4,]


# ---- format for RMark ----

# exclude fish that were culled mid study
unique(mrkdata$date)
mrkdata1<-mrkdata%>%
  select(-year,-month)%>%
  filter(date=="2016-10-14" & AD == 0 | 
           date == "2016-10-19" & AD ==0 |
           date == "2017-05-24")

# melt then cast, equivalent to gather then spread

mrkdata2<-spread(mrkdata1,date,mark,fill=0)

mrkdata3<-unite(mrkdata2,ch,c("2016-10-14","2016-10-19","2017-05-24"),
           sep="",remove=TRUE)
View(mrkdata3)
unique(mrkdata3$ch)
mrkdata3%>%filter(ch=="000")
mrkdata3<-mrkdata3%>%
  filter(ch!="000")
str(mrkdata3)

cod<-mrkdata3

View(cod)

# ---- CJS Test Run ----
# Newbridge only
# time and dependent - no length yet
nb<-cod[str_detect(cod$animal_id,"NB"),]
nb$sl<-as.numeric(nb$sl)
nb.all<-nb%>%
  select(ch)%>%
  data.frame()
duplicated(nb.all)
nb.all<-distinct(nb.all)

nb.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb.all,time.intervals = c(5,217))
  nb.ddl<-make.design.data(nb.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  Phi.time<-list(formula=~time)
  # define models for p
  p.dot<-list(formula=~1)
  p.time<-list(formula=~time)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=nb.processed,ddl=nb.ddl))
}
nb.results<-nb.model()
nb.results

summary(nb.results[[4]])
PIMS(nb.results[[1]],"Phi")
PIMS(nb.results[[1]],"p")
nb.results$model.table
nb.results[[4]]$output

# ---- October only ----
# ---- Add individual covariates -----
# exclude fish that were culled mid study
unique(mrkdata$date)
mrkdata$id<-as.numeric(str_sub(mrkdata$animal_id,start=4))
mrkoct<-mrkdata%>%
  filter(id<2)%>%
  filter(date=="2016-10-14" & AD == 0 | 
           date == "2016-10-19" & AD ==0 |
           date == "2016-10-28")


# melt then cast, equivalent to gather then spread

mrkoct2<-spread(mrkoct,date,mark,fill=0)
# adjust data frame to account for fish taken out of the population
# -1 means taken out of population

mrkoct3<-unite(mrkoct2,ch,c("2016-10-14","2016-10-19","2016-10-28"),
                sep="",remove=TRUE)
# ---- CJS October Run ----
# Newbridge and October only
# time and dependent - no length yet
oct<-mrkoct3[str_detect(mrkoct3$animal_id,"NB"),]
oct$sl<-as.numeric(oct$sl)
oct.all<-oct%>%
  select(ch)%>%
  data.frame()
unique(oct.all$ch)
oct.all<-oct.all%>%
  filter(ch!="000")
duplicated(oct.all)
oct.all<-distinct(oct.all)
oct.model<-function()
{
  # process data for CJS model and make default design data
  oct.processed<-process.data(oct.all,time.intervals = c(5,9))
  oct.ddl<-make.design.data(oct.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  Phi.time<-list(formula=~time)
  # define models for p
  p.dot<-list(formula=~1)
  p.time<-list(formula=~time)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=oct.processed,ddl=oct.ddl))
}
oct.results<-oct.model()
oct.results

summary(oct.results[[4]])
PIMS(nb.results[[1]],"Phi")
PIMS(nb.results[[1]],"p")

oct.results[[4]]$output

# ---- survival by pulse ----
# Newbridge only
# time and dependent - no length yet
nb.pulse<-nb%>%
  select(ch,pulse)%>%
  data.frame()
unique(nb.pulse$pulse)
nb.pulse$pulse<-as.factor(nb.pulse$pulse)
str(nb.pulse)
duplicated(nb.pulse)
nb.pulse<-distinct(nb.pulse)

pulse.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb.pulse,time.intervals = c(5,217),
                             groups="pulse")
  nb.ddl<-make.design.data(nb.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  Phi.time<-list(formula=~time)
  Phi.pulse<-list(formula=~pulse)
  Phi.timepluspulse<-list(formula=~time+pulse)

  # define models for p
  p.dot<-list(formula=~1)
  p.time<-list(formula=~time)
  p.pulse<-list(formula=~pulse)
  p.timepluspulse<-list(formula=~time+pulse)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=nb.processed,ddl=nb.ddl))
}
pulse.results<-pulse.model()
pulse.results

summary(pulse.results[[5]])
pulse.results[[5]]$output
