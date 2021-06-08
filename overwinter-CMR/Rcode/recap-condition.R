# ---- CMR Condition analysis -----

setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/overwinter-CMR/")

# ---- packages-----
library(tidyverse)
library(car)

library(lubridate)
library(mixdist)
#library(marked)


# data
data<-read.csv("../data/data-working/CMR-condition.csv")
CMRpulse<-read.csv("../data/data-working/CMR-pulses.csv")
tripdates<-read.csv("../data/data-working/newman-trips.csv")
recap<-read.csv("../data/data-working/CMR-wide.csv")

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

## NEED TO INCORPORATE RECAPTURE VS NO RECAP
head(recap)
recap%>%
  filter(X2017.05.24==1)%>%
  select(animal_id,sl,pulse,site,recap)->recapture

data2<-left_join(data,recap)%>%
  filter(!is.na(recap))%>%
  mutate(pulse=replace(pulse,is.na(pulse) & sl<=62,4))%>%
  mutate(pulse=replace(pulse,is.na(pulse) & sl>135,1))
str(data2)


# full models
mK.full<-glm(K~site+factor(pulse)+recap,data=data2,
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

data3<-data2%>%
  add_row(site="NB",age=1,pulse=3)

Fig7<-ggplot(data3)+geom_boxplot(aes(y=HSI,x=factor(pulse)),fill='grey64')+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x="Pulse",
       y="Hepatosomatic index")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(axis.text = element_text(size=12))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  annotate(geom="segment",y=seq(0,3,.25),yend=seq(0,3,.25),x=-.01,xend=0.01)+
  annotate("text",label="0",x=3,y=1.25,size=7)



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


Fig5<-ggplot(marks2)+
  geom_bar(aes(x=site2,y=mark,fill=factor(pulse)),stat="identity",position='dodge',colour='black')+
  #scale_y_continuous(expand=c(0,0),breaks = c(0,5,10,15,20,25))+
  geom_text(aes(x=0,y=28,label="stretch it"),vjust=-10,hjust=1)+
  #geom_hline(yintercept =0)+
  #scale_fill_manual(values=c("grey25","grey64","grey90"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  annotate(geom="segment",y=seq(0,27,1),yend=seq(0,27,1),x=-.01,xend=0.01)+
  labs(x="Recapture Site",
       y="Count",
       fill="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  theme(axis.text.x=element_text(angle = 40,hjust = 1))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  scale_fill_manual(values=clrs,
                    limits=c("1","2","3"),
                    labels=c("1", "2", "3"))+
  annotate("text",label="0",x=1.3, y=2, size=6)+
  annotate("text",label="0",x=1.7,y=2, size=6)+
  annotate("text", label="0",x=2.3, y =2, size=6)+
  annotate("text",label="0",x=2.7 ,y=2,size=6)+
  annotate("text",label="0",x=3,y=2,size=6)+
  annotate("text",label="0", x= 3.3, y=2, size=6)
Fig5


ggsave(file="./output/Fig5.png",plot=Fig5,width=160,height=84,units="mm")
ggsave(file="./output/Fig7.png",plot=Fig7,width=84,height=84,units="mm")


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

Fig6<-ggplot(data4)+
  geom_boxplot(aes(x=factor(pulse),y=K,fill=site),colour='black',outlier.shape = NA)+
  scale_fill_brewer(palette = "Dark2")+
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
  annotate("text",label="0",x=3,y=1.75,size=7)+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  ylim(1,2.5)
ggsave(file="./output/Fig6.png",plot=Fig6,width=160,height=120,units="mm")


# ---- K and HSI summaries ----
data2%>%
  group_by(site,pulse)%>%
  summarise(min(K),max(K),mean(K),sd(K))
data2%>%
  filter(!is.na(HSI))%>%
  group_by(pulse)%>%
  summarise(min(HSI),max(HSI),mean(HSI),sd(HSI))
