### Mark-recapture ###

# ---- set working directory ----
setwd("C:/Users/eageissinger/Documents/Emilie-Lab-comp/")

# ---- load required packages ----
library(tidyverse)
library(lubridate)
library(RMark)
library(mixdist)

source("./code/pulse_range_fct.R")

# ---- load data ----
data<-read.csv("./data/data-working/CMR-field-adj.csv")
subsample<-read.csv("./data/data-working/subsample_wk1-2-field.csv")
pulse0<-read.csv("./data/data-working/CMR-0pulses.csv")
pulse1<-read.csv("./data/data-working/pulse_range_age1_final.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")

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

trips$date<-ymd(paste(trips$year,trips$month,trips$day,sep="-"))

data<-left_join(data,trips)
                

# Pulse assignments

# set up age 1 pulses
glimpse(pulse1)
pulse1<-pulse1%>%
  mutate(pulse=replace(pulse,pulse==3,2),
         pulse=replace(pulse,pulse==5,3))
pulse.assign1<-data.frame(trip=rep(pulse1$trip,pulse1$max-pulse1$min+1),
                         year=rep(pulse1$year,pulse1$max-pulse1$min+1),
                         pulse=rep(pulse1$pulse,pulse1$max-pulse1$min+1),
                         mmSL=unlist(mapply(seq,pulse1$min,pulse1$max)))

glimpse(pulse.assign1)
pulse.assign1<-pulse.assign1%>%
  mutate(age=1)
glimpse(pulse0)
pulse0<-pulse0%>%
  mutate(year=cohort, age=0)%>%
  select(-cohort)

pulses<-bind_rows(pulse0,pulse.assign1)%>%
  rename(sl=mmSL)

# ----- pulse assign ----
data$site<-str_sub(data$animal_id,start = 1,end = 2)
data$id<-as.numeric(str_sub(data$animal_id,start=4))
head(data)

# assign pulse to all individual fish
df1<-data%>%
  filter(id<1)
duplicated(df1)

df2<-data%>%
  filter(status==1 & day == 19)
duplicated(df2)

df3<-data%>%
  filter(status==1 & day == 28)
duplicated(df3)

df4<-data%>%
  filter(status==1 & month ==5)
duplicated(df4)

data2<-bind_rows(df1,df2,df3,df4) # individual fish
pulse.size<-left_join(data2,pulses)

# fill in missing pulses
pulse.size%>%
  filter(month==10)%>%
  ggplot(aes(x=date,y=sl,colour=factor(pulse)))+geom_point()
ggplot(pulse.size,aes(x=date,y=sl,colour=factor(pulse)))+geom_point()


oct<-pulse.size%>%filter(id<2 & day == 19)%>%
  filter(!is.na(sl))

qplot(sl,data=oct,binwidth=5)
SL<-select(oct,sl)
summarise(SL,min(sl),max(sl))
group.oct<-mixgroup(SL,breaks= c(0,seq(50,90,5),93),
                    xname=NULL,k=NULL,usecondit=FALSE)
plot(group.oct)

# ---- set initial parameters ----
par<-mixparam(c(60,80),c(5),pi=NULL)
plot(group.oct,par,"gamma")

# fit mixture
fit1<-mix(group.oct,par, dist="gamma",mixconstr(consigma = "CCV"),
          emsteps = 15, usecondit = FALSE)

summary(fit1)
plot(fit1)
plot(fit1,root=T)

head(oct)
notrip<-bind_cols(fit1$parameters, fit1$se)%>%
  mutate(trip=1,year=2016,month=10,day=19,cohort=2016)
notrip<-mutate(notrip,dummy_pulse=rev(seq(1:nrow(notrip))))

pulse.range<-pulse_range(notrip)

# use min and max for each pulse to then create a dataframe with all length possibilities per pulse
pulse.assign<-data.frame(trip=rep(pulse.range$trip,pulse.range$max-pulse.range$min+1),
                         cohort=rep(pulse.range$cohort,pulse.range$max-pulse.range$min+1),
                         pulse=rep(pulse.range$dummy_pulse,pulse.range$max-pulse.range$min+1),
                         mmSL=unlist(mapply(seq,pulse.range$min,pulse.range$max)))
View(pulse.assign)

# ---- pick up here ----
# add new pulse assignments to missing data
# assign 1 to NA trip
pulse.assign<-pulse.assign%>%
  mutate(year=cohort)%>%
  select(-cohort)%>%
  rename(sl=mmSL)%>%
  mutate(age=0)
pulses2<-bind_rows(pulses,pulse.assign)

data2<-data2%>%
  mutate(trip=replace(trip,is.na(trip),1))

pulse.size2<-left_join(data2,pulses2)


# need to combine individual fish pulse assignments to full data
# without losing anything. 
# Assign pulse based on animal_id
data3<-data%>%
  select(-id)
pulse.size3<-pulse.size2%>%
  select(animal_id,pulse)
head(pulse.size3)

final<-left_join(data3,pulse.size3)

final%>%
  filter(month!=5)%>%
  ggplot(aes(y=sl,x=date,colour=factor(pulse)))+
  geom_jitter()

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
  select(date,year,month,animal_id,sl,mark,status,pulse)%>%
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
collection.dates<-collection.dates%>%
  mutate(mark.period=replace(mark.period,mark.period==-143,223))
collection.dates<-collection.dates[1:4,]
collection.dates

# ---- format for RMark ----

# exclude fish that were culled mid study
unique(mrkdata$date)
mrkdata1.1<-mrkdata%>%
  filter(date=="2016-10-14" & status == 0)
mrkdata1.2<-mrkdata%>%
  filter(date=="2016-10-19" & status == 0)
mrkdata1.3<-mrkdata%>%
  filter(date=="2017-05-24")

mrkdata1<-bind_rows(mrkdata1.1,mrkdata1.2,mrkdata1.3)

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

summary(nb.results[[1]])
PIMS(nb.results[[1]],"Phi")
PIMS(nb.results[[1]],"p")
nb.results$model.table
nb.results[[1]]$output

nb2.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb.all,time.intervals = c(5,21))
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
nb2.results<-nb2.model()
nb2.results

summary(nb2.results[[1]])


# From RPubs Olivier Gimenez
phitable=get.real(nb.results[[1]],"Phi",se=TRUE)
phitable[c("estimate","se","lcl","ucl")][1,]

ptable=get.real(nb.results[[1]],"p",se=TRUE)
ptable[c("estimate","se","lcl","ucl")][1:3,]

# calculate the nb of recaptures individuals/occasion
obs=gregexpr("1",nb.all$ch)
n_obs=summary(as.factor(unlist(obs)))
estim_abundance=n_obs[-1]/ptable$estimate[1:3]
estim_abundance

nb_bootstrap=10
nb_time=3
target=data.frame(nb.all,stringAsFactors=F)
popsize=matrix(NA,nb_bootstrap,nb_time-1)
set.seed(5)
pseudo=target

nb2.proc=process.data(pseudo,model="CJS")
nb2.ddl=make.design.data(nb2.proc)

phi.dot=list(formula=~1)
p.dot=list(formula=~1)

for(k in 1:nb_bootstrap) {
  pseudo$ch=sample(target$ch, replace=T)
  res=mark(nb2.proc,nb2.ddl,model.parameters = list(Phi=phi.dot,p=p.dot),delete=TRUE,output=FALSE)
  ptable=get.real(res,"p",se=TRUE)
  allobs=gregexpr("1",pseudo$ch)
  n=summary(as.factor(unlist(allobs)))
  popsize[k,]<-n[-1]/ptable$estimate[1:(nb_time-1)]
}
ci_nb=apply(popsize,2,quantile,probs=c(2.5/100,97.5/100),na.rm=T)
ci_nb

period<-c(1:3)
estim_abundance
estimate<-c(155.0000,281.0001,155.0000)
ci_nb
lcl<-c(143.225,263.9001)
ucl<-c()
abund<-as.data.frame(cbind(period,estim_abundance))
ggplot(data=abund,aes(x=period,y=estim_abundance))+geom_line()+
  ylim(c(0,300))+
  geom_ribbon()

# ---- October only ----
# ---- Add individual covariates -----
# exclude fish that were culled mid study
unique(mrkdata$date)
mrkdata$id<-as.numeric(str_sub(mrkdata$animal_id,start=4))
mrkoct<-mrkdata%>%
  filter(id<2)%>%
  filter(date=="2016-10-14" & status == 0 | 
           date == "2016-10-19" & status ==0 |
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
oct<-mrkoct3
duplicated(oct)
oct$sl<-as.numeric(oct$sl)
oct.all<-oct%>%
  select(ch)%>%
  data.frame()

oct.all<-oct.all%>%
  filter(ch!="000")

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

summary(oct.results[[1]])
PIMS(nb.results[[1]],"Phi")
PIMS(nb.results[[1]],"p")

oct.results[[1]]$output

# ---- survival by pulse ----
# Newbridge only
# time and dependent - no length yet
nb.pulse<-nb%>%
  select(ch,pulse)%>%
  data.frame()
unique(nb.pulse$pulse)
nb.pulse$pulse<-as.factor(nb.pulse$pulse)
str(nb.pulse)


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

summary(pulse.results[[1]])
pulse.results[[5]]$output


# --- JS ----

pop.model<-function()
{
  # process data for CJS model and make default design data
  pop.processed<-process.data(nb.all,model="POPAN",time.intervals = c(5,217))
  pop.ddl<-make.design.data(pop.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  Phi.time<-list(formula=~time)
  # define models for p
  p.dot<-list(formula=~1)
  p.time<-list(formula=~time)
  # probability of entry
  pent.dot<-list(formula=~1)
  pent.time<-list(formula=~time)
  # create model list
  cml<-create.model.list(model="POPAN")
  # run and return models
  return(mark.wrapper(cml,data=pop.processed,ddl=pop.ddl))
}
pop.results<-pop.model()
pop.results

summary(pop.results[[2]])
summary(pop.results[[6]])
pop.results[[2]]$output
pop.results[[2]]
# October JS
oct.pop.model<-function()
{
  # process data for CJS model and make default design data
  pop.processed<-process.data(oct.all,model="POPAN",time.intervals = c(5,9))
  pop.ddl<-make.design.data(pop.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  Phi.time<-list(formula=~time)
  # define models for p
  p.dot<-list(formula=~1)
  p.time<-list(formula=~time)
  # probability of entry
  pent.dot<-list(formula=~1)
  pent.time<-list(formula=~time)
  # create model list
  cml<-create.model.list(model="POPAN")
  # run and return models
  return(mark.wrapper(cml,data=pop.processed,ddl=pop.ddl))
}
oct.pop.results<-oct.pop.model()
oct.pop.results

summary(oct.pop.results[[2]])
oct.pop.results[[2]]$output
oct.pop.results[[2]]
# add pulse
pop.pulse.model<-function()
{
  # process data for CJS model and make default design data
  pop.processed<-process.data(nb.pulse,model="POPAN",time.intervals = c(5,217),groups="pulse")
  pop.ddl<-make.design.data(pop.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  Phi.time<-list(formula=~time)
  Phi.pulse<-list(forumula=~pulse)
  # define models for p
  p.dot<-list(formula=~1)
  p.time<-list(formula=~time)
  p.pulse<-list(formula=~pulse)
  # probability of entry
  pent.dot<-list(formula=~1)
  pent.time<-list(formula=~time)
  pent.pulse<-list(formula=~pulse)
  # create model list
  cml<-create.model.list(model="POPAN")
  # run and return models
  return(mark.wrapper(cml,data=pop.processed,ddl=pop.ddl))
}
pop.pulse.results<-pop.pulse.model()
pop.pulse.results
summary(pop.pulse.results[[3]])
