setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/overwinter-CMR/")


data<-read.csv("../SIMS/Cod Otolith Data June 2020.csv")
ID<-read.csv("../SIMS/SIMS-ID.csv")
pulse1<-read.csv("../data/data-working/pulse_range_age1_final2019-12-6.csv")
temp.field<-read.csv("../data/data-working/newman-temp-to-2017.csv")
temp.exp<-read.csv("../data/data-working/temperature-exp.csv")

head(data)
summary(data)
str(data)

head(ID)
summary(ID)
str(ID)

head(pulse1)
summary(pulse1)
str(pulse1)

#load packages
library(tidyverse)
library(lubridate)
library(car)
library(lme4)
library(ggpubr)
source("../pulse-structure/pulse_range_fct.R")

#---- Clean data----
data2<-data%>%
  filter(Sr.mmol!="#DIV/0!")%>%
  dplyr::select(File.name,Sr.mmol,Sr.ppm,Mg.mmol,Mg.ppm,Ba.mmol,Ba.ppm)%>%
  mutate(Sr.mmol=as.numeric(as.character(Sr.mmol)))%>%
  mutate(Sr.ppm=as.numeric(as.character(Sr.ppm)))%>%
  mutate(Mg.mmol=as.numeric(as.character(Mg.mmol)))%>%
  mutate(Mg.ppm=as.numeric(as.character(Mg.ppm)))%>%
  mutate(Ba.mmol=as.numeric(as.character(Ba.mmol)))%>%
  mutate(Ba.ppm=as.numeric(as.character(Ba.ppm)))
str(data2)

summary(data2)

ID2<-ID%>%
  rename(File.name=ï..File.name)%>% # fix heading name
  filter(!is.na(SL)) # get rid of empty rows


# set up age 1 pulses
glimpse(pulse1)
summary(pulse1)
str(pulse1)
pulse1<-pulse1%>%
  filter(!is.na(min) | !is.na(max))


pulse.assign1<-data.frame(trip=rep(pulse1$trip,pulse1$max-pulse1$min+1),
                          year=rep(pulse1$year,pulse1$max-pulse1$min+1),
                          pulse=rep(pulse1$pulse,pulse1$max-pulse1$min+1),
                          age=rep(pulse1$age,pulse1$max-pulse1$min+1),
                          mmSL=unlist(mapply(seq,pulse1$min,pulse1$max)))

CMRpulse<-pulse.assign1%>%
  filter(year==2017 & trip==10)%>%
  rename(SL=mmSL)
# combine fishID with data

data3<-left_join(data2,ID2)%>%
  filter(!is.na(SL))%>% # get rid of empty rows
  left_join(CMRpulse)%>% # add pulse structure
  mutate(site=str_sub(fish.ID,start=1,end=2)) #create site column
  
# ---- Visualize data ----
data3%>%
  filter(section=="cross")%>%
  ggplot(aes(x=point,y=Sr.mmol,colour=fish.ID,shape=factor(pulse)))+
  geom_point()

data3%>%
  filter(section=="cross")%>%
  ggplot(aes(x=point,y=Mg.mmol,colour=fish.ID,shape=factor(pulse)))+
  geom_point()

data3%>%
  filter(section=="cross")%>%
  ggplot(aes(x=point,y=Ba.mmol,colour=fish.ID,shape=factor(pulse)))+
  geom_point()


lab<-data3%>%
  filter(section=="cross")%>%
  mutate(site=replace(site,SL==103,"E14"))%>%
  mutate(site=replace(site,SL==102, "E12"))


# figures for MS

# legend
names<-as.factor(c("73","80","102","103"))
clrs<- c("73" = 'gray5',"80" = 'gray55', "102" = 'gray27',"103" = 'gray48')
shps <- c("73" = 21, "80" = 23, "102" = 22, "103" = 24)

p1Mg<-lab%>%
  filter(pulse==1)%>%
  ggplot()+
  geom_point(aes(x=point,y=Mg.mmol,fill=factor(SL),shape=factor(SL)),size=2)+
  scale_x_reverse()+
  ylim(0.05,0.50)+
  labs(x="Numerical sequence from the core to the edge",
       y="Mg (mmol)",
       fill="SL",
       shape="SL")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  #scale_colour_manual(values=c('gray6','gray45'))+
  scale_fill_manual(values= clrs,
                      limits = c("73", "80", "102", "103"),
                      labels = c("73", "80", "102", "103"))+
  scale_shape_manual(values = shps,
                     limits = c("73", "80", "102", "103"),
                     labels = c("73", "80", "102", "103"))+
  theme(axis.text.x = element_text(colour='white'))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text.y = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=12))

p2Mg<-lab%>%
  filter(pulse==2)%>%
  ggplot()+
  geom_point(aes(x=point,y=Mg.mmol,fill=factor(SL),shape=factor(SL)),size=2)+
  scale_x_reverse()+
  ylim(0.05,0.50)+
  labs(x="Numerical sequence from the core to the edge",
       y="Mg (mmol)",
       fill="SL",
       shape="SL")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_fill_manual(values= clrs,
                    limits = c("73", "80", "102", "103"),
                    labels = c("73", "80", "102", "103"))+
  scale_shape_manual(values = shps,
                     limits = c("73", "80", "102", "103"),
                     labels = c("73", "80", "102", "103"))+
  theme(axis.text.x = element_text(colour='white'))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.y = element_text(colour='white'))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=12))


p1Sr<-lab%>%filter(pulse==1)%>%
  ggplot()+
  geom_point(aes(x=point,y=Sr.mmol,fill=factor(SL),shape=factor(SL)),size=2)+
  scale_x_reverse()+
  ylim(1.2,3.75)+
  labs(x="Numerical sequence from the core to the edge",
       y="Sr (mmol)",
       fill="SL",
       shape="SL")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_fill_manual(values= clrs,
                    limits = c("73", "80", "102", "103"),
                    labels = c("73", "80", "102", "103"))+
  scale_shape_manual(values = shps,
                     limits = c("73", "80", "102", "103"),
                     labels = c("73", "80", "102", "103"))+
  theme(axis.text.x = element_text(colour='white'))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(colour='white'))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=12))


p2Sr<-lab%>%
  filter(pulse==2)%>%
  ggplot()+
  geom_point(aes(x=point,y=Sr.mmol,fill=factor(SL),shape=factor(SL)),size=2)+
  scale_x_reverse()+
  ylim(1.2,3.75)+
  labs(x="Numerical sequence from the core to the edge",
       y="Sr (mmol)",
       fill="SL",
       shape="SL")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_fill_manual(values= clrs,
                    limits = c("73", "80", "102", "103"),
                    labels = c("73", "80", "102", "103"))+
  scale_shape_manual(values = shps,
                     limits = c("73", "80", "102", "103"),
                     labels = c("73", "80", "102", "103"))+
  theme(axis.text.x = element_text(colour='white'))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(colour='white'))+
  theme(axis.title.y = element_text(colour='white'))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=12))



p1Ba<-lab%>%filter(pulse==1)%>%
  ggplot()+
  geom_point(aes(x=point,y=Ba.mmol,fill=factor(SL),shape=factor(SL)),size=2)+
  scale_x_reverse()+
  ylim(0,0.015)+
  labs(x="Numerical sequence from the core to the edge",
       y="Ba (mmol)",
       fill="SL",
       shape="SL")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_fill_manual(values= clrs,
                    limits = c("73", "80", "102", "103"),
                    labels = c("73", "80", "102", "103"))+
  scale_shape_manual(values = shps,
                     limits = c("73", "80", "102", "103"),
                     labels = c("73", "80", "102", "103"))+
  theme(axis.text.x = element_text(colour='white'))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(colour='white'))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=12))



p2Ba<-lab%>%
  filter(pulse==2)%>%
  ggplot()+
  geom_point(aes(x=point,y=Ba.mmol,fill=factor(SL),shape=factor(SL)),size=2)+
  scale_x_reverse()+
  ylim(0,0.015)+
  labs(x="Numerical sequence from the core to the edge",
       y="Ba (mmol)",
       fill="SL",
       shape="SL")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_fill_manual(values= clrs,
                    limits = c("73", "80", "102", "103"),
                    labels = c("73", "80", "102", "103"))+
  scale_shape_manual(values = shps,
                     limits = c("73", "80", "102", "103"),
                     labels = c("73", "80", "102", "103"))+
  theme(axis.text.x = element_text(colour='white'))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(colour='white'))+
  theme(axis.title.y = element_text(colour='white'))+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=12))


ggarrange(p1Sr,p2Sr,p1Ba,p2Ba,p1Mg,p2Mg,ncol = 2,nrow=3,
          align = "hv",labels="auto",common.legend = TRUE,legend = "top")

#lab models
lm1<-glm(Ba.mmol~point+fish.ID,data=lab,family=Gamma(link="log"))
plot(lm1)
summary(lm1)
Anova(lm1,type="III")

lm2<-lm(Mg.mmol~point+fish.ID,data=lab)
plot(lm2)
Anova(lm2,type="III")
summary(lm2)

lm3<-lm(Sr.mmol~point+fish.ID,data=lab)
plot(lm3)
Anova(lm3,type="III")


# Field fish

data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=section,y=Sr.mmol,colour=factor(pulse)))+
  geom_jitter(width=0.25)

data3%>%
  filter(section!="cross")%>%
  filter(Mg.mmol<4)%>%
  ggplot(aes(x=section,y=Mg.mmol,colour=factor(pulse)))+
  geom_jitter(width=0.25)

data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=section,y=Ba.mmol,colour=factor(pulse)))+
  geom_jitter(width=0.25)


# Group by site
data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=section,y=Sr.mmol,colour=site))+
  geom_jitter(width=0.25)

data3%>%
  filter(section!="cross")%>%
  filter(Mg.mmol<4)%>%
  ggplot(aes(x=section,y=Mg.mmol,colour=site))+
  geom_jitter(width=0.25)

data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=section,y=Ba.mmol,colour=site))+
  geom_jitter(width=0.25)

data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=site,y=Sr.mmol,colour=section))+
  geom_jitter(width=0.25)

data3%>%
  filter(section!="cross")%>%
  filter(Mg.mmol<4)%>%
  ggplot(aes(x=site,y=Mg.mmol,colour=section))+
  geom_jitter(width=0.25)

data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=site,y=Ba.mmol,colour=section))+
  geom_jitter(width=0.25)

#Newbridge
data3%>%
  filter(section!="cross" & site == "NB")%>%
  ggplot(aes(x=section,y=Sr.mmol,colour=factor(mark)))+
  geom_jitter(width=0.25)
data3%>%
  filter(section!="cross" & site == "NB")%>%
  filter(Mg.mmol<4)%>%
  ggplot(aes(x=section,y=Mg.mmol,colour=factor(mark)))+
  geom_jitter(width=0.25)
data3%>%
  filter(section!="cross" & site == "NB")%>%
  ggplot(aes(x=section,y=Ba.mmol,colour=factor(mark)))+
  geom_jitter(width=0.25)

#Mistaken
data3%>%
  filter(section!="cross" & site == "MI")%>%
  ggplot(aes(x=section,y=Sr.mmol,colour=factor(mark)))+
  geom_jitter(width=0.25)
data3%>%
  filter(section!="cross" & site == "MI")%>%
  filter(Mg.mmol<4)%>%
  ggplot(aes(x=section,y=Mg.mmol,colour=factor(mark)))+
  geom_jitter(width=0.25)
data3%>%
  filter(section!="cross" & site == "MI")%>%
  ggplot(aes(x=section,y=Ba.mmol,colour=factor(mark)))+
  geom_jitter(width=0.25)

# marked vs non marked fish

data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=section,y=Sr.mmol,colour=mark))+
  geom_jitter()

data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=section,y=Mg.mmol,colour=mark))+
  geom_jitter()

data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=section,y=Ba.mmol,colour=mark))+
  geom_jitter()

data3%>%
  filter(section!="cross")%>%
  filter(Mg.mmol<4)%>%
  filter(site=="NB")%>%
  ggplot()+
  geom_point(aes(x=fish.ID,y=Sr.mmol,colour=section))


# model time

field<-data3%>%
  filter(section!="cross")%>%
  filter(Mg.mmol<4) #get rid of outlier

m1.Sr<-lmer(Sr.mmol~factor(pulse)+section+site+factor(mark)+(1|fish.ID),data=field)
plot(m1.Sr)
qqnorm(resid(m1.Sr))
qqline(resid(m1.Sr))
hist(resid(m1.Sr))
summary(m1.Sr)
Anova(m1.Sr,type="III")

m1.Mg<-lmer(Mg.mmol~factor(pulse)+section+site+(1|fish.ID),data=field)
plot(m1.Mg)
qqnorm(resid(m1.Mg))
qqline(resid(m1.Mg))
hist(resid(m1.Mg))

Anova(m1.Mg,type="III")
summary(m1.Mg)
#try gamma

m2.Mg<-glmer(Mg.mmol~factor(pulse)+section+site+(1|fish.ID),data=field,
             family = Gamma)
plot(m2.Mg)
qqnorm(resid(m2.Mg))
qqline(resid(m2.Mg))
hist(resid(m2.Mg))

Anova(m2.Mg,type="III")
summary(m2.Mg)


m1.Ba<-lmer(Ba.mmol~factor(pulse)+section+site+factor(mark)+(1|fish.ID),data=field)
plot(m1.Ba)
qqnorm(resid(m1.Ba))
qqline(resid(m1.Ba))
hist(resid(m1.Ba))

#gamma distribution
m2.Ba<-glmer(Ba.mmol~factor(pulse)+section+site+(1|fish.ID),data=field,
             family = Gamma)
plot(m2.Ba)
qqnorm(resid(m2.Ba))
qqline(resid(m2.Ba))
hist(resid(m2.Ba))

Anova(m2.Ba, type="III")
summary(m2.Ba)


m3.Ba<-glmer(Ba.mmol~factor(pulse)+section+factor(site)+factor(mark)+(1|fish.ID),data=field,
             family = Gamma)
plot(m3.Ba)
qqnorm(resid(m3.Ba))
qqline(resid(m3.Ba))
hist(resid(m3.Ba))       

Anova(m3.Ba)
summary(m3.Ba)



# ----- summary talbes ----
data3%>%
  filter(section!="cross")%>%
  filter(Mg.mmol<4)%>% # take out outlier
  group_by(section)%>%
  summarise(MeanSr=mean(Sr.mmol),sdSr=sd(Sr.mmol),
            minSr=min(Sr.mmol),maxSr=max(Sr.mmol),
            MeanMg=mean(Mg.mmol),sdMg=sd(Mg.mmol),
            minMg=min(Mg.mmol),maxMg=max(Mg.mmol),
            MeanBa=mean(Ba.mmol),sdBa=sd(Ba.mmol),
            minBa=min(Ba.mmol), maxBa=max(Ba.mmol),
            n=n())->sum.table

#change in composition
fall.fish<-data3%>%
  filter(Mg.mmol<4)%>%
  filter(section=="fall")%>%
  dplyr::select(-section,-File.name)%>%
  rename(F_Sr.mmol=Sr.mmol,F_Sr.ppm=Sr.ppm,F_Mg.mmol=Mg.mmol,F_Mg.ppm=Mg.ppm,
         F_Ba.mmol=Ba.mmol,F_Ba.ppm=Ba.ppm)
winter.fish<-data3%>%
  filter(Mg.mmol<4)%>%
  filter(section=="winter")%>%
  dplyr::select(-section,-File.name)%>%
  rename(W_Sr.mmol=Sr.mmol,W_Sr.ppm=Sr.ppm,W_Mg.mmol=Mg.mmol,W_Mg.ppm=Mg.ppm,
         W_Ba.mmol=Ba.mmol,W_Ba.ppm=Ba.ppm)

delta.field<-left_join(fall.fish,winter.fish)%>%
  mutate(dSr=W_Sr.mmol-F_Sr.mmol,
         dMg=W_Mg.mmol-F_Mg.mmol,
         dBa=W_Ba.mmol-F_Ba.mmol)

ggplot(delta.field)+
  geom_jitter(aes(x=site,y=dSr,colour=factor(pulse),shape=factor(mark)),width=0.25)
ggplot(delta.field)+
  geom_jitter(aes(x=site,y=dMg,colour=factor(pulse),shape=factor(mark)),width=0.25)
ggplot(delta.field)+
  geom_jitter(aes(x=site,y=dBa,colour=factor(pulse),shape=factor(mark)),width=0.25)


# Figures
data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=section,y=Sr.mmol))+
  geom_boxplot(aes(colour=factor(pulse)))+
  geom_jitter(aes(colour=factor(pulse)),width=0.25)

data3%>%
  filter(section!="cross")%>%
  filter(Mg.mmol<4)%>%
  ggplot(aes(x=section,y=Mg.mmol))+
  geom_boxplot(aes(colour=factor(pulse)))+
  geom_jitter(aes(colour=factor(pulse)),width=0.25)

data3%>%
  filter(section!="cross")%>%
  ggplot(aes(x=section,y=Ba.mmol))+
  geom_boxplot(aes(colour=factor(pulse)))+
  geom_jitter(aes(colour=factor(pulse)),width=0.25)


# ---- temperature plots -----
#exp
temp.exp%>%
  rename(year=?..year)%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  mutate(julian_date=yday(date))%>%
  mutate(temperature=replace(temperature,month==2 & temperature==3.0,0.3))%>%
  filter(temperature!=4)%>%
  filter(temperature<4.5)%>%
  group_by(julian_date,date)%>%
  summarise(mean_temp=mean(temperature),
            sd_temp=sd(temperature))%>%
  ggplot()+
  geom_point(aes(x=date,y=mean_temp))

temp.field%>%
  mutate(date=ymd(paste(year,month,day,sep="-")))%>%
  filter(date>'2016-10-01')%>%
  filter(date<'2017-05-30')%>%
  ggplot()+
  geom_point(aes(x=date,y=daily_temp_C))
