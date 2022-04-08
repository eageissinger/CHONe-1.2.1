# Figures for pulse structure presentation

# load packages
library(tidyverse)
library(ggpubr)
library(lubridate)
# packages that should be loaded ahead of time
#tidyverse
#ggpubr


# load data
#length data
length<-read.csv("./data/data-working/newman-length.csv")%>%
  filter(Species=='AC')%>%
  mutate(date=ymd(paste(Year,Month,Day,sep="-")))

# trip dates
tripdates<-length%>%
  dplyr::select(Year,Trip,date)%>%
  group_by(Year,Trip)%>%
  summarise(date=min(date))%>%
  filter(Year==2018 | Year == 2019)
# mixture distribution data
mixture2018<-read.csv("./data/output/AC-mixture-dist-2018.csv")
mixture2019<-read.csv("./data/output/AC-mixture-dist-2019.csv")
#combine two years into single df
mixtures<-bind_rows(mixture2018,mixture2019)%>%
  rename(group=dummy_pulse,Year=year,Trip=trip,Age=age)%>%
  left_join(tripdates)
# pulse range data
pulses<-read.csv("./data/output/pulse_range_ALL.csv")%>%
  rename(Year=year,Trip=trip,Age=age,Pulse=pulse)%>%
  left_join(tripdates)



# ---- Figures for mixture distribution models -----
# histogram for presentation:

length%>%
  filter(Age == 0 & Trip == 16 & Year == 2018)%>%
  ggplot()+
  geom_density(aes(x=mmSL),fill='grey',colour='black')+
  ylab("Probability Density")+xlab("Standard length (mm)")+
  ggtitle("27-28 August")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=12),
        plot.title = element_text(size=16,hjust=1),
        axis.title = element_text(size=14),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        plot.margin = margin(t=.5,r=.5,b=.5,l=.5,unit='cm'))+
  theme(axis.title.x = element_text(color='white'))+ # remove x axis title without changing size of plot
  xlim(c(0,125))->a
  
length%>%
  filter(Age == 0 & Trip == 18 & Year == 2018)%>%
  ggplot()+
  geom_density(aes(x=mmSL),fill='grey',colour='black')+
  ylab("Probability Density")+xlab("Standard length (mm)")+
  ggtitle("24-25 September")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=12),
        plot.title = element_text(size=16,hjust=1),
        axis.title = element_text(size=14),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        plot.margin = margin(t=.5,r=.5,b=.5,l=.5,unit='cm'))+
  theme(axis.title.x = element_text(color='white'))+ # remove x axis title without changing size of plot
  xlim(c(0,125))->b

length%>%
  filter(Age == 0 & Trip == 20 & Year == 2018)%>%
  ggplot()+
  geom_density(aes(x=mmSL),fill='grey',colour='black')+
  ylab("Probability Density")+xlab("Standard length (mm)")+
  ggtitle("22-23 October")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=12),
        plot.title = element_text(size=16,hjust=1),
        axis.title = element_text(size=14),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        plot.margin = margin(t=.5,r=.5,b=.5,l=.5,unit='cm'))+
  xlim(c(0,125))->c

Fig2<-ggarrange(a,b,c,ncol=1)

ggsave(plot=Fig2,file="./pulse-structure/figures/Fig2.png","png",units="in",height=7,width=5)


# ----- initial figures: scatterplot with size distribution -----
Fig3a<-length%>%
  filter(Age==0 & Year == 2018)%>%
  ggplot()+
  geom_jitter(aes(x=date,y=mmSL),size=2,alpha=0.3,colour='grey')+
  theme_bw()+
  labs(x="Date",
       y="Standard length (mm)")+
  scale_x_date(date_breaks="2 weeks",
               date_labels = "%d-%b")+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        axis.text.x = element_text(angle=45,hjust=1),
        plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"))+
  ylim(c(25,130))


# --- mixture model output figures (age 0 only) ----
# mixtures%>%
#   filter(Year==2018 & Age == 0)%>%
#   ggplot()+
#   geom_point(aes(x=date,y=mu,shape=factor(group)),size=3)+
#   geom_errorbar(aes(x=date,ymin=mu-sigma,ymax=mu+sigma),width=1)+
#   theme_bw()+
#   labs(x="Date",
#        y="Standard length (mm)",
#        shape="Mixture group")+
#   scale_x_date(date_labels="%d-%b",
#                date_breaks="2 weeks")+
#   #theme(axis.text.x = element_text(angle=40,hjust=1))+
#   theme(axis.text = element_text(size=12))+
#   theme(axis.title = element_text(size=16))+
#   theme(legend.text = element_text(size=12))+
#   theme(legend.title = element_text(size=14))+
#   theme(axis.title.x = element_text(vjust=-2))+
#   theme(axis.title.y = element_text(vjust=4))+
#   theme(plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"))+
#   #scale_color_brewer(palette="Dark2")+
#   theme(legend.key.size = unit(1.5,"cm"))

# ----- mixture model output with data
Fig3b<-length%>%
  filter(Age==0 & Year == 2018)%>%
  mutate(Date2=date+3)%>%
  ggplot()+
  geom_jitter(aes(x=Date2,y=mmSL),size=2,alpha=0.3,colour='grey')+
  geom_point(data=filter(mixtures,Year==2018 & Age==0),
             aes(x=date,y=mu,shape=factor(group)),size=3)+
  geom_errorbar(data=filter(mixtures,Year==2018 & Age==0),
                aes(x=date,ymin=mu-sigma,ymax=mu+sigma),width=1)+
  theme_bw()+
  labs(x="Date",
       y="Standard length (mm)",
       shape= "Mixture Group")+
  scale_x_date(date_breaks="2 weeks",
               date_labels = "%d-%b")+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        axis.text.x = element_text(angle=45,hjust=1),
        plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"),
        legend.key.size = unit(1.5,"cm"),
        legend.position = 'bottom')+
  ylim(c(25,130))

Fig3<-ggarrange(Fig3a,Fig3b,common.legend = TRUE,labels = c('a','b'),ncol=1,legend = 'bottom')

ggsave(plot=Fig3,file="./pulse-structure/figures/Fig3.png","png",units="in",height=7,width=7)

# # ---- final assignment with mixture model for age 0 ------
# length%>%
#   filter(Age==0 & Year == 2018)%>%
#   mutate(Date2=date+3)%>%
#   ggplot()+
#   geom_jitter(aes(x=Date2,y=mmSL,colour=factor(Pulse)))+
#   geom_point(data=filter(mixtures,Year==2018 & Age==0),
#              aes(x=date,y=mu,shape=factor(group)))+
#   geom_errorbar(data=filter(mixtures,Year==2018 & Age==0),
#                 aes(x=date,ymin=mu-sigma,ymax=mu+sigma),width=0)

# final assignment without mixture model

Fig4<-length%>%
  filter(Age==0 & Year == 2018)%>%
  ggplot()+
  geom_jitter(aes(x=date,y=mmSL,colour=factor(Pulse)),size=2,alpha=0.3)+
  theme_bw()+
  labs(x="Date",
       y="Standard length (mm)",
       colour="Pulse")+
  scale_x_date(date_breaks="2 weeks",
               date_labels = "%d-%b")+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        axis.text.x = element_text(angle=45,hjust=1),
        plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"),
        legend.key.size = unit(1.5,"cm"),
        legend.position = 'bottom')+
  scale_colour_brewer(palette = "Dark2")

ggsave(plot=Fig4,file="./pulse-structure/figures/Fig4.png","png",units="in",height=5,width=7)

# ---- Age 0 to age 1 pulse structure -----
l18<-length%>%
  filter(Year==2018 & Age==0)%>%
  mutate(cohort=2018)
l19<-length%>%
  filter(Year==2019 & Age==1)%>%
  mutate(cohort=2018)

Fig5a<-ggplot()+
  geom_jitter(data=l18,aes(x=date,y=mmSL,colour=factor(Pulse)),alpha=0.3,size=2)+
  geom_jitter(data=l19,aes(x=date,y=mmSL),colour='grey',alpha=0.3,size=2)+
  theme_bw()+
  labs(x="Date",
       y="Standard length (mm)",
       colour="Pulse")+
  scale_x_date(date_breaks="4 weeks",
               date_labels = "%d-%b")+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        legend.position = 'none',
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"),
        axis.text.x = element_text(angle=45,hjust=1))+
  scale_colour_brewer(palette = "Dark2")+
  ylim(0,210)

sum18<-l18%>%
  group_by(Pulse,Trip,Year)%>%
  summarise(meanSL=mean(mmSL),sdSL=sd(mmSL))%>%
  left_join(tripdates)

Fig5b<-ggplot()+
  geom_jitter(data=l18,aes(x=date,y=mmSL,colour=factor(Pulse)),alpha=0.3,size=2)+
  geom_jitter(data=l19,aes(x=date,y=mmSL),colour='grey',alpha=0.3,size=2)+
  geom_errorbar(data=sum18,aes(x=date+5,ymin=meanSL-sdSL,ymax=meanSL+sdSL),width=0)+
  geom_point(data=sum18,aes(x=date+5,y=meanSL,fill=factor(Pulse)),size=2,shape=22)+
  geom_point(data=filter(mixtures,Age==1 & Year==2019),
             aes(x=date-5,y=mu,shape=factor(group)),size=2)+
  geom_errorbar(data=filter(mixtures,Age==1 & Year==2019),
                aes(x=date-5,ymin=mu-sigma,ymax=mu+sigma),width=2)+
  theme_bw()+
  labs(x="Date",
       y="Standard length (mm)",
       colour="Pulse",
       fill="Pulse",
       shape="Mixture group")+
  scale_x_date(date_breaks="4 weeks",
               date_labels = "%d-%b")+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=11),
        legend.title = element_text(size=12),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"),
        legend.key.size = unit(1,"cm"),
        axis.text.x = element_text(angle=45,hjust=1),
        legend.position='right')+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ylim(0,210)+
  annotate('text',label='Age 0',x=ymd('2018-08-27'),y=10,size=8)+
  annotate('text',label="Age 1", x=ymd('2019-06-03'),y=10,size=8)
Fig5b

Fig5<-ggarrange(Fig5a,Fig5b,nrow=2,labels=c('a','b'))

ggsave(plot=Fig5b,file="./pulse-structure/figures/Fig5.png","png",units="in",height=6,width=8.5)

Fig6<-ggplot()+
  geom_jitter(data=l18,aes(x=date,y=mmSL,colour=factor(Pulse)),alpha=0.3,size=2)+
  geom_jitter(data=l19,aes(x=date,y=mmSL,colour=factor(Pulse)),alpha=0.3,size=2)+
  theme_bw()+
  labs(x="Date",
       y="Standard length (mm)",
       colour="Pulse",
       fill="Pulse",
       shape="Mixture group")+
  scale_x_date(date_breaks="4 weeks",
               date_labels = "%d-%b")+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"),
        legend.key.size = unit(1.5,"cm"),
        axis.text.x = element_text(angle=45,hjust=1),
        legend.position = 'bottom')+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ylim(0,210)+
  annotate('text',label='Age 0',x=ymd('2018-08-27'),y=10,size=6)+
  annotate('text',label="Age 1", x=ymd('2019-06-03'),y=10,size=6)
ggsave(plot=Fig6,file="./pulse-structure/figures/Fig6.png","png",units="in",height=5,width=7)

#---- catch per haul ----
ch<-read.csv('./data/output/catch_haul.csv')
head(ch)
table1<-ch%>%
  filter(Year==2018 & Age==0)%>%
  dplyr::select(Year,Trip,extrap_1,extrap_2,extrap_3,extrap_4)%>%
  left_join(tripdates)%>%
  filter(!is.na(extrap_1))
table1
write.csv(table1,"./pulse-structure/figures/table1.csv",row.names=FALSE)
table2<-ch%>%
  filter(Year==2019 & Age==1)%>%
  dplyr::select(Year,Trip,extrap_1,extrap_2,extrap_3,extrap_4,extrap_5)%>%
  left_join(tripdates)
table2

# ----- Temperature -----
winter<-read.csv("./data/output/newman-winter-summary.csv")
winter%>%
  filter(cohort==2018)
t<-read.csv("./data/data-working/newman-temp-to-2019.csv")
summary(t)
t%>%
  filter(year==2019 & month==5)%>%
  summarise(mean(daily_temp_C))


# ---- Application: winter growth rates -----

#full list of dates and trips
tripdates<-length%>%
  dplyr::select(Year,Trip,date)%>%
  group_by(Year,Trip)%>%
  summarise(date=min(date))
p0<-length%>%
  filter(Age==0)%>%
  mutate(cohort=Year)
p1<-length%>%
  filter(Age==1)%>%
  mutate(cohort=Year-1)
p<-bind_rows(p0,p1)

p%>%
  group_by(Year,cohort,Age,Trip,Month,Pulse)%>%
  summarise(meanSL=mean(mmSL),sdSL=sd(mmSL),minSL=min(mmSL),maxSL=max(mmSL),N=n())%>%
  ungroup()%>%
  group_by(Year,cohort,Age,Pulse)%>%
  arrange(Trip)%>%
  filter(row_number()==1 | row_number()==n())%>%
  filter(N>1)%>%
  filter(Age==1 & Month ==5 | Age==0 & Month >10)%>%
  mutate(season='spring')%>%
  mutate(season=replace(season,Month!=5,'fall'))%>%
  filter(season=='fall')%>%
  mutate(fallSL=meanSL,
         fallsd=sdSL)%>%
  ungroup()%>%
  left_join(tripdates)%>%
  mutate(fall.day=yday(date))%>%
  dplyr::select(cohort,Pulse,fallSL,fallsd,fall.day)->f
p%>%
  group_by(Year,cohort,Age,Trip,Month,Pulse)%>%
  summarise(meanSL=mean(mmSL),sdSL=sd(mmSL),minSL=min(mmSL),maxSL=max(mmSL),N=n())%>%
  ungroup()%>%
  group_by(Year,cohort,Age,Pulse)%>%
  arrange(Trip)%>%
  filter(row_number()==1 | row_number()==n())%>%
  filter(N>1)%>%
  filter(Age==1 & Month ==5 | Age==0 & Month >10)%>%
  mutate(season='spring')%>%
  mutate(season=replace(season,Month!=5,'fall'))%>%
  filter(season=='spring')%>%
  mutate(springSL=meanSL,
        springsd=sdSL)%>%
  ungroup()%>%
  left_join(tripdates)%>%
  mutate(spring.day=yday(date))%>%
  dplyr::select(cohort,Pulse,springSL,springsd,spring.day)->s
head(f)
head(s)

winter.growth<-left_join(f,s)%>%
  filter(!is.na(springSL))%>%
  mutate(total.days=(365-fall.day)+spring.day)%>%
  mutate(sgr=((log(springSL)-log(fallSL))/total.days)*100)%>%
  filter(Pulse==2| Pulse==3)%>%
  left_join(winter)%>%
  mutate(Pulse=as.factor(Pulse))
head(winter.growth)

ggplot(winter.growth)+
  geom_point(aes(x=cohort,y=sgr,shape=factor(Pulse)),size=2)+
  theme_bw()+
  labs(x="Cohort",
       y=expression(atop("Specific growth rate", paste('(SGR'[ L],' ; % mm ·', day^-1*')'))),
       shape="Pulse")+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"),
        legend.key.size = unit(1.5,"cm"),
        axis.text.x = element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(17,15))+
  scale_x_continuous(breaks =seq(1998,2018,by=2))->Fig7

ggsave(plot=Fig7,file="./pulse-structure/figures/Fig7.png",'png',units = "in",height=5,width = 7.5)

ggplot(winter)+
  geom_point(aes(x=cohort,y=mean_temp))
ggplot(winter)+
  geom_point(aes(x=cohort,y=days_below_1))
library(lme4)
library(car)

wg.f<-winter.growth%>%mutate(cohort=factor(cohort))
m3<-lm(sgr~Pulse+cohort,data=wg.f)
plot(x=fitted(m3),y=resid(m3))
qqnorm(resid(m3))
qqline(resid(m3))
Anova(m3)
summary(m3)
plot(effects::allEffects(m3))
effects::allEffects(m3)

m1<-lmer(sgr~mean_temp+(1|cohort),data=wg.f)
plot(x=fitted(m1),y=resid(m1))
qqnorm(resid(m1))
qqline(resid(m1))
Anova(m1,type="III")
summary(m1)
m2<-lmer(sgr~days_below_1+(1|cohort),data=wg.f)
plot(x=fitted(m2),y=resid(m2))
qqnorm(resid(m2))
qqline(resid(m2))
Anova(m2,type="III")
summary(m2)
# annual change in growth
# no significant relationship between sgr and pulse, mean temp, or winter duration

# what does influence growth?

# ---- Application: winter mortality rate ----
data<-read.csv("./data/output/condition-field-formatted.csv")%>%
  filter(pulse<4)
  
#calculate overwinter mortality rate
  # 275 days
winter.mort.pulse<-data%>%
  mutate(mortality=(log(postCount/preCount)/-275)*100)%>%
  mutate(mortality=replace(mortality,preCount==0 |postCount==0,NA))%>%
  mutate(pulse=as.factor(pulse),
         cohort=as.factor(cohort))

mean(winter.mort.pulse$mortality,na.rm=TRUE)

winter.mort.pulse%>%
  mutate(cohort=as.character(cohort))%>%
  mutate(cohort=as.integer(cohort))%>%
  ggplot()+
  geom_point(aes(x=cohort,y=mortality,shape=factor(pulse)),size=2)+
  theme_bw()+
  labs(x="Cohort",
       y=expression(atop("% Mortality", paste('(% · ', day^-1*')'))),
       shape="Pulse")+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"),
        legend.key.size = unit(1.5,"cm"),
        axis.text.x = element_text(angle=45,hjust=1))+
  scale_x_continuous(breaks =seq(1998,2018,by=2))->Fig8

ggsave(plot=Fig8,file="./pulse-structure/figures/Fig8.png",'png',units = "in",height=5,width = 7.5)

m4<-lmer(mortality~pulse+(1|cohort),data=winter.mort.pulse)
plot(x=fitted(m4),y=resid(m4))
qqnorm(resid(m4))
qqline(resid(m4),col='red')
hist(resid(m4))
Anova(m4,type='III')
summary(m4)
plot(effects::allEffects(m4))

m5<-lm(mortality~pulse+cohort,data=winter.mort.pulse)
plot(x=fitted(m5),y=resid(m5))
qqnorm(resid(m5))
qqline(resid(m5),col='red')
hist(resid(m5))
Anova(m5,type='III')
summary(m5)
plot(effects::allEffects(m5))
effects::allEffects(m5)
# mortality changes by year and pulse
effects::Effect('pulse',m5)%>%as.data.frame()
effects::Effect('cohort',m5)%>%as.data.frame()
# check mort and temp

m6<-lmer(mortality~mean_temp+(1|cohort),data=winter.mort.pulse)
plot(x=fitted(m6),y=resid(m6))
qqnorm(resid(m6))
qqline(resid(m6),col='red')
hist(resid(m6))
Anova(m6,type="III")

m7<-lmer(mortality~days_below_1+(1|cohort),data=winter.mort.pulse)
plot(x=fitted(m7),y=resid(m7))
qqnorm(resid(m7))
qqline(resid(m7))
hist(resid(m7))
Anova(m7,type="III")
