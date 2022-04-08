# ---- CMR Condition analysis -----


# ---- packages-----
library(tidyverse)
library(car)

library(lubridate)
library(mixdist)
library(ggpubr)
#library(marked)


# data
data<-read.csv("./data/data-working/CMR/CMR-condition.csv")
CMRpulse<-read.csv("./data/data-working/CMR/CMR-pulses.csv")
tripdates<-read.csv("./data/data-working/newman-trips.csv")
recap<-read.csv("./data/data-working/CMR/CMR-wide.csv")

# check data
str(data)
names(data)
summary(data)

# format fish id to match animal_id
data<-data%>%
  filter(age==1)%>%
  mutate(animal_id=str_c(site,2,fish,sep="."))%>%
  mutate(animal_id=str_replace(animal_id,'.2.','_2.'))

#---- Add pulse ----
head(CMRpulse)


# pulses<-left_join(pulse1,tripdates)%>%
#   filter(year==2017 & month == 5)

pulse.assign<-data.frame(trip=rep(CMRpulse$trip,CMRpulse$max-CMRpulse$min+1),
                         year=rep(CMRpulse$year,CMRpulse$max-CMRpulse$min+1),
                         pulse=rep(CMRpulse$pulse,CMRpulse$max-CMRpulse$min+1),
                         age=rep(CMRpulse$age,CMRpulse$max-CMRpulse$min+1),
                         mmSL=unlist(mapply(seq,CMRpulse$min,CMRpulse$max)))

data1<-left_join(data,pulse.assign)%>%
  distinct()

# mark presence/absence
# ---- Condition Factor ----
data1%>%
  mutate(K=(body_weight_g/(mmSL*.1)^3)*1000)%>%
  mutate(HSI=((liver_weight_mg*.001)/body_weight_g)*1000)->data

ggplot(data)+geom_jitter(aes(x=site,y=K,colour=factor(pulse)))
ggplot(data)+geom_jitter(aes(x=site,y=HSI,colour=factor(pulse)))

# models

mK<-glm(K~site, family = Gamma(link="log"),data=data)
plot(mK)
Anova(mK,type="III")

mHSI<-glm(HSI~site, family=Gamma(link="log"),data=data)
plot(mHSI)
Anova(mHSI,type="III")

pulse.K<-glm(K~site*factor(pulse),family=Gamma(link="log"),data=data)
plot(pulse.K)
hist(resid(pulse.K))
Anova(pulse.K,type="III")
summary(pulse.K)

pulse.HSI<-glm(HSI~site+factor(pulse),family=Gamma(link="log"),data=data)
plot(pulse.HSI)
hist(resid(pulse.HSI))
Anova(pulse.HSI,type="III")
summary(pulse.HSI)

## INCORPORATE RECAPTURE VS NO RECAP
head(recap)
recap%>%
  filter(X2017.05.24==1)%>%
  dplyr::select(animal_id,sl,site,recap)->recapture

data2<-left_join(data,recapture)%>%
  filter(!is.na(recap))%>%
  mutate(pulse=replace(pulse,is.na(pulse) & sl<=61,4))%>%
  mutate(pulse=replace(pulse,is.na(pulse) & sl>135,1))
str(data2)


# full models
mK.full<-glm(K~site*factor(pulse)+recap,data=data2,
             family=Gamma(link="log"))
plot(mK.full)
hist(resid(mK.full))
Anova(mK.full)

mHSI.full<-glm(HSI~site+factor(pulse)+recap,data=data2,
               family=Gamma(link="log"))
plot(mHSI.full)
hist(resid(mHSI.full))
Anova(mHSI.full)

ggplot(data2,aes(x=factor(pulse),y=HSI))+geom_boxplot()

data2%>%
  group_by(pulse)%>%
  filter(!is.na(HSI))%>%
  summarise(mean(HSI))

ggplot(data2,aes(x=factor(pulse),y=HSI))+geom_boxplot()+
  geom_jitter(aes(x=factor(pulse),y=HSI,colour=site))

data2%>%
  group_by(site,pulse)%>%
  summarise(n())

# determine where the 62 mm fish fit
ggplot(data2,aes(x=site,y=sl,colour=factor(pulse)))+geom_point()



data3%>%
  group_by(pulse)%>%
  filter(!is.na(HSI))%>%
  summarise(mean(HSI))

data3%>%
  mutate(HSI.mean=1.46)%>%
  mutate(HSI.mean=replace(HSI.mean,pulse==2,1.42))%>%
  mutate(HSI.mean=replace(HSI.mean,pulse==3,NA))%>%
  mutate(HSI.mean=replace(HSI.mean,pulse==4,0.927))%>%
  mutate(HSI.res=HSI-HSI.mean)%>%
  ggplot()+
  geom_boxplot(aes(y=HSI.res,x=factor(pulse)),fill='grey64')+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x="Pulse",
       y="Hepatosomatic index residuals")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(axis.text = element_text(size=12))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  annotate(geom="segment",y=seq(0,3,.25),yend=seq(0,3,.25),x=-.01,xend=0.01)

# Evaluate recapture data
recap%>%
  filter(recap!=0)

marks<-data2%>%
  filter(recap!=0)%>%
  group_by(site,pulse)%>%
  summarise(mark=n())%>%
  ungroup()%>%
  add_row(site="MI",pulse=1,mark=0)%>%
  add_row(site="CC",pulse=1,mark=0)%>%
  add_row(site="CC",pulse=2,mark=0)%>%
  as.data.frame()%>%
  mutate(site2="Newbridge")%>%
  mutate(site2=replace(site2,site=="MI","Mistaken"))%>%
  mutate(site2=replace(site2,site=="CC","Canning's"))%>%
  mutate(site2=factor(site2))%>%
  mutate(site2=relevel(site2,"Mistaken"))%>%
  mutate(site2=relevel(site2,"Newbridge"))


clrs<- c("1" = '#1B9E77', "2" = '#D95F02', "3" = '#7570B3', "4" = '#E6AB02')

summary(marks)

marks2<-marks%>%
  add_row(site="NB",pulse=3,mark=0,site2="Newbridge")%>%
  add_row(site="MI",pulse=3,mark=0,site2="Mistaken")%>%
  add_row(site="CC",pulse=3,mark=0,site2="Canning's")%>%
  mutate(site=factor(site,levels=c("NB","MI","CC")))%>%
  mutate(site2=factor(site2,levels=c("Newbridge","Mistaken","Canning's")))

greys<-c("1"='grey10',"2"='grey50',"3"='grey90')


# Full condition follow up
head(data2)
data2%>%
  group_by(pulse,site)%>%
  summarise(min(K),max(K),mean(K),sd(K))
data2%>%summarise(mean(K),sd(K))

data4<-data2%>%
  add_row(pulse=3,site="NB")%>%
  add_row(pulse=3,site="MI")%>%
  add_row(pulse=3,site="CC")

greys2<-c("Newbridge"='grey10',"Mistaken"='grey50',"Canning's"='grey90')

Fig5a<-ggplot(data4)+
  geom_boxplot(aes(x=factor(pulse),y=K,fill=site),colour='black',outlier.shape = NA)+
  scale_fill_manual(values = c("grey10","grey50","grey90"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x="Pulse",
       y="Fulton's K")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(axis.text = element_text(size=12))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  annotate(geom="segment",y=seq(0,3,.25),yend=seq(0,3,.25),x=-.01,xend=0.01)+
  labs(fill="Site")+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  ylim(1,2.5)


Fig5b<-ggplot(data2)+
  geom_boxplot(aes(y=HSI,x=factor(pulse),fill=site),outlier.shape = NA)+
  scale_fill_manual(values = c("grey10","grey50","grey90"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x="Pulse",
       y="Hepatosomatic index")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(axis.text = element_text(size=12))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  annotate(geom="segment",y=seq(0,3,.25),yend=seq(0,3,.25),x=-.01,xend=0.01)+
  labs(fill="Site")+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))


Fig5<-ggarrange(Fig5a,Fig5b,legend = "bottom",common.legend = TRUE,labels = c("a","b"))

ggsave(file="./overwinter-CMR/output/Fig5.png",Fig5,width=170,height=85,units="mm")
ggsave(file="./overwinter-CMR/output/Fig5.jpg",Fig5,width=170,height=85,units="mm", dpi = 500)

# ---- K and HSI summaries ----
data2%>%
  group_by(site,pulse)%>%
  summarise(min(K),max(K),mean(K),sd(K))
data2%>%
  filter(!is.na(HSI))%>%
  group_by(pulse)%>%
  summarise(min(HSI),max(HSI),mean(HSI),sd(HSI))
