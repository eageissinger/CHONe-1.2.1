### Mark-recapture ###

# ---- set working directory ----
# setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/overwinter-CMR/")

# ---- load required packages ----

library(lubridate)
library(mixdist)
#library(marked)
library(tidyverse)
library(RMark)

source("./pulse-structure/pulse_range_fct.R")

# ---- load data ----
data<-read.csv("./data/data-working/CMR/CMR-field-MAY-captures.csv")
allCMR<-read.csv("./data/data-working/CMR/CMR-field-adj.csv")
CMRpulse<-read.csv("./data/data-working/CMR/CMR-pulses.csv")
pulse0<-read.csv("./data/data-working/pulse_range_age0_final2019-12-06.csv")
pulse1<-read.csv("./data/data-working/pulse_range_age1_final2019-12-6.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")

# ---- check data ----
names(data)
str(data)
summary(data)
head(data)

#names(subsample)
#str(subsample)
#summary(subsample)
#head(subsample)

str(trips)
# format date
data$date<-ymd(paste(data$year,data$month,data$day,sep="-"))
#subsample$date<-ymd(paste(subsample$Year,subsample$Month,subsample$Day,sep="-"))

trips$date<-ymd(paste(trips$year,trips$month,trips$day,sep="-"))
#fallcatch$date<-ymd(paste(fallcatch$year,fallcatch$month,fallcatch$day,sep="-"))
#fallcatch<-fallcatch%>%filter(day==19 & status==1)

data<-left_join(data,trips)
                

# Pulse assignments
glimpse(CMRpulse)
summary(CMRpulse)
str(CMRpulse)

pulse.assign<-data.frame(trip=rep(CMRpulse$trip,CMRpulse$max-CMRpulse$min+1),
                          year=rep(CMRpulse$year,CMRpulse$max-CMRpulse$min+1),
                          pulse=rep(CMRpulse$pulse,CMRpulse$max-CMRpulse$min+1),
                          age=rep(CMRpulse$age,CMRpulse$max-CMRpulse$min+1),
                          mmSL=unlist(mapply(seq,CMRpulse$min,CMRpulse$max)))

# assign pulse to fall fish
pulse.size0<-allCMR%>%
  filter(site=="NB")%>%
  mutate(id=as.numeric(str_sub(animal_id,start=4)))%>%
  filter(id<2 & status==1 | id<1)%>%
  left_join(trips)%>%
  rename(mmSL=sl)%>%
  distinct()%>%
  filter(age==0)%>%
  left_join(pulse.assign)

# assign pulse to ALL fish
pulse.size1<-data%>%
  mutate(id=as.numeric(str_sub(animal_id,start=4)))%>%
  filter(id>2 & month == 5)%>%
  left_join(trips)%>%
  rename(mmSL=sl)%>%
  left_join(pulse.assign)

# assign pulses to 
# set up age 1 pulses
# glimpse(pulse1)
# summary(pulse1)
# str(pulse1)
# pulse1<-pulse1%>%
#   filter(!is.na(min) | !is.na(max))
# 
# 
# pulse.assign1<-data.frame(trip=rep(pulse1$trip,pulse1$max-pulse1$min+1),
#                          year=rep(pulse1$year,pulse1$max-pulse1$min+1),
#                          pulse=rep(pulse1$pulse,pulse1$max-pulse1$min+1),
#                          age=rep(pulse1$age,pulse1$max-pulse1$min+1),
#                          mmSL=unlist(mapply(seq,pulse1$min,pulse1$max)))
# 
# glimpse(pulse.assign1)
# glimpse(pulse0)
# pulse.assign0<-data.frame(trip=rep(pulse0$trip,pulse0$max-pulse0$min+1),
#                           year=rep(pulse0$year,pulse0$max-pulse0$min+1),
#                           pulse=rep(pulse0$pulse,pulse0$max-pulse0$min+1),
#                           age=rep(pulse0$age,pulse0$max-pulse0$min+1),
#                           mmSL=unlist(mapply(seq,pulse0$min,pulse0$max)))
# pulses<-bind_rows(pulse.assign0,pulse.assign1)%>%
#   rename(sl=mmSL)%>%
#   filter(year>=2016)


# create dataframe with all fish and status=1 or status 0 and id<1
# allCMR<-left_join(allCMR,trips)%>%
#   mutate(id=as.numeric(str_sub(animal_id,start=4)))
# pulse.size<-allCMR%>%
#   filter(!is.na(trip))%>%
#   filter(status==1 | id<=1)

cod.id<-bind_rows(pulse.size0,pulse.size1)%>%
  select(animal_id,pulse)

# assign pulses to fish during the no trip week
# notrip<-allCMR%>%
#   filter(is.na(trip))%>%
#   filter(status==1 | id<=1)

# pulse.size2<-left_join(notrip,CMRpulse)%>%
#   select(animal_id,pulse)

# pulse.size and pulses.notrip
# take out october no trip from pulse.size, then re-add pulses.notrip
# cod.id<-bind_rows(pulse.size1,pulse.size2)

# summary statistics on size breakdown for fall and spring

#fall
cod.id%>%
  separate(animal_id,into= c("Site", "ID"),sep="_")%>%
  mutate(ID=as.numeric(ID))%>%
  filter(Site=="NB")%>%
  filter(ID<2)%>%
  group_by(pulse)%>%
  summarise(n())

#spring
cod.id%>%
  separate(animal_id,into= c("Site", "ID"),sep="_")%>%
  mutate(ID=as.numeric(ID))%>%
  filter(Site=="NB")%>%
  filter(ID>=2)%>%
  group_by(pulse)%>%
  summarise(n())

# check if each fish has pulse assignment
allCMR%>%
  distinct(animal_id)%>%
  summarise(n())

final<-left_join(data,cod.id)


final%>%
  filter(month!=5 & age!=2)%>%
  ggplot(aes(y=sl,x=date,colour=factor(pulse)))+
  geom_jitter()

final%>%filter(month!=5 & age!=2)%>%
  filter(is.na(pulse) & !is.na(sl))


 # ---- mark analysis ----

# select dat that is within popultion
mrkdata<-final%>%
  filter(age<2)%>%
  select(date,year,month,animal_id,sl,mark,pulse)%>%
  data.frame()
mrkdata%>%
  filter(!is.na(sl))%>%
  filter(is.na(pulse))
# mrkdata<-mrkdata%>%
#   mutate(pulse=replace(pulse,is.na(pulse) & sl<=62,4))%>%
#   mutate(pulse=replace(pulse,is.na(pulse) & sl>135,1))
#<=62,4
#>135,1
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

mrkdata1<-mrkdata%>%
  select(-year,-month)

# melt then cast, equivalent to gather then spread

mrkdata2<-spread(mrkdata1,date,mark,fill=0)

mrkdata3<-unite(mrkdata2,ch,c("2016-10-14","2016-10-19","2017-05-24"),
           sep="",remove=TRUE)
View(mrkdata3)
unique(mrkdata3$ch)
str(mrkdata3)
duplicated(mrkdata3$animal_id)
cod<-mrkdata3

View(cod)

# 
# determine the proportion of fish caught (100 and 010) for each pulse for entire sound and apply the ratio to the unmeasured fish
# week 1
cod%>%filter(ch=="100")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(N=n())%>%
  mutate(ratio=N/52)

W1<-cod%>%filter(ch=="100")%>%
  filter(is.na(sl))%>% #88 unassigned fish
  select(-pulse)%>% # remove current pulse column
  add_column(pulse=c(rep(1, each=39),rep(2,each=46),rep(3,each=3))) # assign pulses to unmeasured fish based on ratio

# summary of week 1
W1%>%
  group_by(pulse)%>%
  summarise(n())
cod%>%filter(ch=="100")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl),mean(sl), sd(sl))

88*0.442 # pulse 1
88*0.519 # pulse 2
88*0.0385 # pulse 3
39+46+3

#week 2
cod%>%filter(ch=="010")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(N=n())%>%
  mutate(ratio=N/29)

W2<-cod%>%filter(ch=="010")%>%
  filter(is.na(sl))%>% #49 unassigned fish
  select(-pulse)%>% # remove current pulse column
  add_column(pulse=c(rep(1, each=24),rep(2,each=25))) # assign pulses to unmeasured fish based on ratio
#summary of week 2
W2%>%
  group_by(pulse)%>%
  summarise(n())
cod%>%filter(ch=="010")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl),mean(sl), sd(sl))
49*0.483 # pulse 1
49*0.517 # pulse 2
24+25

non.SL<-bind_rows(W1,W2)

#add full pulse structure to dataset
cod<-cod%>%
  filter(!is.na(pulse))%>%
  bind_rows(non.SL)

# size summary data from recaptures
head(cod)
cod%>%
  separate(animal_id,into= c("Site", "ID"),sep="_")%>%
  mutate(ID=as.numeric(ID))%>%
  filter(ch=="011" | ch == "101" | ch == "111")%>%
  group_by(Site,pulse)%>%
  summarise(n())
cod%>%
  separate(animal_id,into= c("Site", "ID"),sep="_")%>%
  mutate(ID=as.numeric(ID))%>%
  filter(ch == "111")


# ---- CJS Run ----
# Newbridge only
# time and dependent - no length yet
nb<-cod[str_detect(cod$animal_id,"NB"),]
nb$sl<-as.numeric(nb$sl)
nb.all<-nb%>%
  select(ch)%>%
  data.frame()
nb.processed<-process.data(nb.all,time.intervals = c(5,217))
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
#export.MARK(nb.processed,"overwinter",nb.results,replace=TRUE)
summary(nb.results[[2]])
nb.results[[2]]# deviance of 0, poor model fit, but c-hat =1 which is good model fit??
PIMS(nb.results[[1]],"Phi")
PIMS(nb.results[[1]],"p")
nb.results$model.table
nb.results[[2]]
# all four could biologically be a possibility
# may want to run bootstrap GOF analysis on each model to see
# what one to go with
#release.gof(nb.processed,invisible = TRUE,title = "Release-gof",view=TRUE)


# ---- CJS survival by pulse ----
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
nb.processed<-process.data(nb.pulse,time.intervals = c(5,217),
                           groups="pulse")
pulse.results
summary(pulse.results[[15]])
pulse.results[[15]]
c.hat<-pulse.results[[12]]$results$deviance/pulse.results[[12]]$results$deviance.df
c.hat
tail(nb.pulse)

PIMS(pulse.results[[15]],"p")

#release.gof(nb.processed,invisible = TRUE,title="release-gof",view=TRUE)
summary(pulse.results[[15]])
pulse.results[[15]]
pulse.results[[15]]$design.data
names(pulse.results)
round(pulse.results$Phi.timepluspulse.p.time$results$real[,1:4],3)

mymodel<-pulse.results[[15]]

model1<-mymodel$results$real[,1:4]
model1.est<-cbind(model1[rep(seq_len(nrow(model1))), ])
pulse.results[[15]]






# ---- pulses 1:3 ----
# Take out late pulses (the ones that were not marked)
nb13<-nb.pulse%>%
  filter(pulse!=4)%>%
  as.data.frame()

early.pulse.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb13,time.intervals = c(5,217),
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
  #p.pulse<-list(formula=~pulse) # biologically is moot, we know that we can capture all pulses in our net if they are present
  #p.timepluspulse<-list(formula=~time+pulse)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=nb.processed,ddl=nb.ddl))
}
early.pulse.results<-early.pulse.model()
early.pulse.results
early.pulse.results[[8]]


# rerun only model 8 Phi(~time + pulse) p(~time)
nb.processed<-process.data(nb13,time.intervals = c(5,217),
                           groups="pulse")
nb.ddl<-make.design.data(nb.processed)
Phi.timepluspulse<-list(formula=~time+pulse)
p.time<-list(formula=~time)

model8<-mark(nb.processed,nb.ddl,model.parameters=list(Phi=Phi.timepluspulse,p=p.time))
summary(model8)
results(model8)
export.MARK(nb.processed, "NB-results-final",model=early.pulse.results,replace=FALSE)


#adjust.chat(3.32,early.pulse.results)
#adjust.chat(0.80,early.pulse.results)
nb.processed<-process.data(nb13,time.intervals = c(5,217),
                           groups="pulse")
summary(early.pulse.results[[8]])
early.pulse.results[[8]]
c.hat<-early.pulse.results[[8]]$results$deviance/early.pulse.results[[8]]$results$deviance.df
c.hat
tail(nb.pulse)


names(early.pulse.results)
round(early.pulse.results$Phi.timepluspulse.p.time$results$real[,1:4],4)

write.csv(early.pulse.results[[8]]$results$real,"./data/output/CMR-pulse.csv",row.names = FALSE)

# ---- format for Mstrata ----

mstrata1<-mrkdata%>%
  select(-year,-month)%>%
  mutate(site=str_sub(animal_id,start=1,end=2))%>%
  mutate(state="A")%>%
  mutate(state=replace(state,site=="MI","B"))%>%
  mutate(state=replace(state,site=="CC","C"))%>%
  mutate(mark=replace(mark,date!="2017-05-24" & mark==1,"A"))%>%
  mutate(mark=replace(mark,date=="2017-05-24" & mark ==1 & state=="A","A"))%>%
  mutate(mark=replace(mark,date=="2017-05-24" & mark == 1 & state=="B","B"))%>%
  mutate(mark=replace(mark,date=="2017-05-24" & mark == 1 & state=="C","C"))
# melt then cast, equivalent to gather then spread

mstrata2<-spread(mstrata1,date,mark,fill=0)

mstrata3<-unite(mstrata2,ch,c("2016-10-14","2016-10-19","2017-05-24"),
                sep="",remove=TRUE)
View(mstrata3)
unique(mstrata3$ch)
str(mstrata3)
duplicated(mstrata3$animal_id)
mcod<-mstrata3%>%
  select(-animal_id,-state,-site)

View(mcod)

# ---- Multi-state ----
head(mcod)


data("mstrata")
head(mstrata)
head(mcod)

mstrata.processed<-process.data(mcod,model="Multistrata")
mstrata.ddl<-make.design.data(mstrata.processed)
summary(mstrata.ddl$Psi)
summary(mstrata.ddl$p)


run.mstrata=function()
{
  # Process data
  mstrata.processed=process.data(mcod,model="Multistrata",time.intervals = c(5,217),
                                 groups="pulse")
  # Create default design data
  mstrata.ddl=make.design.data(mstrata.processed) 
  mstrata.ddl$Psi$fix=NA
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$stratum=="B" & mstrata.ddl$Psi$tostratum=="A"]=0
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$stratum=="B" & mstrata.ddl$Psi$tostratum=="C"]=0
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$stratum=="C" & mstrata.ddl$Psi$tostratum=="A"]=0
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$stratum=="C" & mstrata.ddl$Psi$tostratum=="B"]=0
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$time==1 & mstrata.ddl$Psi$stratum=="A" & mstrata.ddl$Psi$tostratum=="B"]=0
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$time==1 & mstrata.ddl$Psi$stratum=="A" & mstrata.ddl$Psi$tostratum=="C"]=0
  mstrata.ddl$S$fix=NA
  mstrata.ddl$S$fix[mstrata$S$stratum=="B"]=0
  mstrata.ddl$S$fix[mstrata$S$stratum=="C"]=0
  mstrata.ddl$p$fix=NA
  mstrata.ddl$p$fix[mstrata.ddl$p$stratum=="B"]=0
  mstrata.ddl$p$fix[mstrata.ddl$p$stratum=="C"]=0
  # Create formula
  Psi.s=list(formula=~1+stratum:tostratum)
  p.time=list(formula=~time)
  S.time=list(formula=~time)
  p.dot=list(formula=~1)
  S.dot=list(formula=~1)
  Psi.pulse=list(formula=~pulse)
  #p.pulse=list(formula=~pulse)
  S.pulse=list(formula=~pulse)
  Psi.pulse.stratum=list(formula=~1+stratum:tostratum+pulse)
  #p.pulse.time=list(formula=~pulse+time)
  S.pulse.time=list(formula=~pulse+time)
  #add time after this run
  model.list=create.model.list("Multistrata")
  mstrata.results=mark.wrapper(model.list,data=mstrata.processed,ddl=mstrata.ddl)
  return(mstrata.results)
}
mstrata.results=run.mstrata()
mstrata.results
summary(mstrata.results[[15]])
mstrata.results[[15]]
mstrata.results[[15]]$design.matrix

# ---- take out pulse 4 for mulit-state ----
mcod13<-mcod%>%
  filter(pulse!=4)%>%
  select(-sl)%>%
  as.data.frame()
run.mstrata=function()
{
  # Process data
  mstrata.processed=process.data(mcod13,model="Multistrata",time.intervals = c(5,217),
                                 groups="pulse")
  # Create default design data
  mstrata.ddl=make.design.data(mstrata.processed) 
  mstrata.ddl$Psi$fix=NA
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$stratum=="B" & mstrata.ddl$Psi$tostratum=="A"]=0
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$stratum=="B" & mstrata.ddl$Psi$tostratum=="C"]=0
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$stratum=="C" & mstrata.ddl$Psi$tostratum=="A"]=0
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$stratum=="C" & mstrata.ddl$Psi$tostratum=="B"]=0
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$time==1 & mstrata.ddl$Psi$stratum=="A" & mstrata.ddl$Psi$tostratum=="B"]=0
  mstrata.ddl$Psi$fix[mstrata.ddl$Psi$time==1 & mstrata.ddl$Psi$stratum=="A" & mstrata.ddl$Psi$tostratum=="C"]=0
  mstrata.ddl$S$fix=NA
  mstrata.ddl$S$fix[mstrata$S$stratum=="B"]=0
  mstrata.ddl$S$fix[mstrata$S$stratum=="C"]=0
  mstrata.ddl$p$fix=NA
  mstrata.ddl$p$fix[mstrata.ddl$p$stratum=="B"]=0
  mstrata.ddl$p$fix[mstrata.ddl$p$stratum=="C"]=0
  # Create formula
  Psi.s=list(formula=~1+stratum:tostratum)
  p.time=list(formula=~time)
  S.time=list(formula=~time)
  p.dot=list(formula=~1)
  S.dot=list(formula=~1)
  Psi.pulse=list(formula=~pulse)
  #p.pulse=list(formula=~pulse)
  S.pulse=list(formula=~pulse)
  Psi.pulse.stratum=list(formula=~1+stratum:tostratum+pulse)
  #p.pulse.time=list(formula=~pulse+time)
  S.pulse.time=list(formula=~pulse+time)
  #add time after this run
  model.list=create.model.list("Multistrata")
  mstrata.results=mark.wrapper(model.list,data=mstrata.processed,ddl=mstrata.ddl)
  return(mstrata.results)
}
mstrata.results=run.mstrata()
mstrata.results
mstrata.results[[15]]
summary(mstrata.results[[15]])
#write.csv(mstrata.results[[33]]$results$real,"../data/output/multistrata.csv",row.names=FALSE)
mstrata.processed=process.data(mcod13,model="Multistrata",time.intervals = c(5,217),
                               groups="pulse")

#export.MARK(mstrata.processed,"multistate",model=mstrata.results,replace = TRUE)
#cleanup(ask=FALSE)

# ---- follow up -----
mcod%>%
  filter(ch == "0AB")
mcod%>%
  filter(ch == "0AA" | ch == "A0A" | ch == "AAA")
mcod%>%filter(pulse==3)
mcod%>%filter(pulse==4)
mcod%>%filter(pulse==2)
mcod%>%filter(pulse==1)

# format for recapture presence/absence

recap<-mrkdata%>%
  select(-year,-month,-day)%>%
  mutate(site=str_sub(animal_id,start=1,end=2))

mrkdata4<-spread(recap,date,mark,fill=0)
#write.csv(mrkdata4,"../data/data-working/CMR-wide.csv",row.names=FALSE)

