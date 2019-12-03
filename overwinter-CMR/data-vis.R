#---- CMR Visualization ----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/overwinter-CMR/")

# ---- load data ----
NB<-read.csv("../data/output/early-pulse-CJS.csv")
multi<-read.csv("../data/output/early-pulse-multi.csv")
octfish<-read.csv("../data/data-working/oct-cod-pulses.csv")
mayfish<-read.csv("../data/data-working/may-cod-pulses.csv")
# ---- load packages ----
library(tidyverse)
library(ggplot2)
library(lubridate)

# ----check data ----
str(multi)
head(multi)
str(NB)
head(NB)

head(octfish)
head(mayfish)
str(octfish)
str(mayfish)

# ----- format data -----
octfish$date<-ymd(paste(octfish$year,octfish$month,octfish$day,sep="-"))
mayfish$date<-ymd(paste(mayfish$year,mayfish$month,mayfish$day,sep="-"))

# ---- convert to instantaneous mort ----
# Zi=-ln(Phix)

NB%>%
  mutate(M=-log(estimate))%>%
  mutate(M=replace(M,parameter=="p",NA))->NB

multi%>%
  mutate(M=-log(estimate))%>%
  mutate(M=replace(Z, parameter =="p" | parameter=="Psi", NA))%>%
  mutate(Mse=-log(se),Mlcl=-log(lcl),Mucl=-log(ucl))->multi

NB%>%
  filter(parameter=="Phi")%>% # convert to instantaneous mort
  mutate(season="fall")%>%
  mutate(season=replace(season,time==6,"overwinter"))%>%
  ggplot()+
  geom_point(aes(x=season,y=estimate,colour=factor(group)),
             position=position_dodge(width=.2))+
  geom_errorbar(aes(x=season,ymin=lcl,ymax=ucl,colour=factor(group)), width=.5,
                position=position_dodge(width=.2))

multi%>%
  filter(parameter=="S")%>%
  mutate(season="fall")%>%
  mutate(season=replace(season,time==6,"overwinter"))%>%
  ggplot()+
  geom_point(aes(x=season,y=estimate,shape=factor(group)),
             position=position_dodge(width=.2))+
  geom_errorbar(aes(x=season,ymin=lcl,ymax=ucl,group=factor(group),linetype='dashed'),
                position=position_dodge(width=.2),
                width=.5)

multi%>%
  filter(parameter=="S")%>%
  mutate(season="fall")%>%
  mutate(season=replace(season,time==6,"overwinter"))%>%
  ggplot()+
  geom_point(aes(x=as.factor(group),y=M),
             position=position_dodge(width=.5),size=2)+
  geom_errorbar(aes(x=group,ymin=Mlcl,ymax=Mucl,group=factor(group)),
                                                             position=position_dodge(width=.5),
                                                             width=0)+
  facet_wrap(~season)+
  theme_bw()+
  xlab("Pulse")+
  ylab("M("~day^-1*")")

NB%>%
  filter(parameter=="Phi")%>%
  mutate(season="fall")%>%
  mutate(season=replace(season,time==6,"overwinter"))%>%
  ggplot()+
  geom_point(aes(x=season,y=Z,colour=factor(group)),
             position=position_dodge(width=.5))
# ---- Size Frequency -----

# October
octfish%>%
  filter(date=='2016-10-14' & id<1 & !is.na(sl))%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 3)


octfish%>%
  filter(date=='2016-10-19' & id <1 & !is.na(sl))%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 3)

octfish%>%
  group_by(date,pulse)%>%
  summarise(mean(sl),max(sl),min(sl))

mayfish%>%
  filter(id>=2 & site=="NB" & date == '2017-05-24' & age == 1)%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 3)
mayfish%>%
  filter(id>=2 & site=="NB" & date == '2017-05-24' & age == 1)%>%
  filter(is.na(pulse))

mayfish%>%
  filter(id>=2 & site == "MI" & date == '2017-05-24' & age == 1)%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 3)
mayfish%>%
  filter(id>=2 & site == "MI" & date == '2017-05-24' & age == 1)%>%
  filter(is.na(pulse))

mayfish%>%
  filter(id>=2 & site =="CC" & date == '2017-05-24' & age == 1)%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 3)
