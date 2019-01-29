### Mark-recapture ###

# ---- set working directory ----
setwd("C:/Users/USER/Documents/Research/CHONe-1.2.1/")

# ---- load required packages ----
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(RMark)

# ---- load data ----
data<-read.csv("./data/data-working/mark-history-field-adj.csv")
subsample<-read.csv("./data/data-working/subsample_wk1-2-field.csv")

# ---- check data ----
names(data)
str(data)
summary(data)
head(data)

names(subsample)
str(subsample)
summary(subsample)
head(subsample)

# format date
data$date<-ymd(paste(data$year,data$month,data$day,sep="-"))
subsample$date<-ymd(paste(subsample$Year,subsample$Month,subsample$Day,sep="-"))

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

mrkdata<-data%>%
  filter(age<2 & sl>=55)%>%
  select(date,animal_id,sl,mark,AD)%>%
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
# melt then cast, equivalent to gather then spread

mrkdata2<-spread(mrkdata,date,mark,fill=0)
# adjust data frame to account for fish taken out of the population
# -1 means taken out of population


mrkdata3<-unite(mrkdata2,ch,c("2016-10-14","2016-10-19","2016-10-28","2017-05-24"),
           sep="",remove=TRUE)
View(mrkdata3)
unique(mrkdata3$ch)
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


nb.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb.all,time.intervals = c(5,9,208))
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

summary(nb.results[[3]])
PIMS(nb.results[[3]],"Phi")
PIMS(nb.results[[3]],"p")

# ---- Add individual covariates -----
nb.sl<-select(nb,ch,sl)

nb.covariate.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb.sl,time.intervals = c(5,9,208))
  nb.ddl<-make.design.data(nb.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  Phi.time<-list(formula=~time)
  # add sl for Phi
  Phi.sl<-list(formula=~sl)
  Phi.sl.plus.time<-list(formula=~sl+time)
  Phi.slxtime<-list(formula=~sl*time)
  # define models for p
  p.dot<-list(formula=~1)
  p.time<-list(formula=~time)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=nb.processed,ddl=nb.ddl))
}
nb.covariate.results<-nb.covariate.model()
nb.covariate.results

nb.covariate.results[[5]]$design.matrix

PIMS(nb.covariate.results[[5]],"Phi")
PIMS(nb.covariate.results[[5]],"p")


