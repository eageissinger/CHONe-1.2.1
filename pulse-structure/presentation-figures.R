# Figures for pulse structure presentation

# packages that should be loaded ahead of time
#tidyverse
#ggpubr

# ---- Figures for mixture distribution models -----
# histogram for presentation:

L1<-length%>%
  filter(year==2016 & age == 0 & trip == 22)%>%
  ggplot()+
  geom_histogram(aes(x=mmSL),binwidth=5,fill='grey',colour='black')+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(expand = c(0,0),
   limits=c(0,65))+
  #scale_x_continuous(limits=c(25,125))+
  theme(axis.text = element_text(size=16))+
  ggtitle("29 Nov 2016 (age 0 cod)")+
  theme(plot.title = element_text(size=24,hjust=0.5))+
  ylab("# age-0 Atlantic cod")+xlab("Standard length (mm)")+
  theme(axis.title = element_text(size=20))+
  theme(axis.title.x = element_text(vjust=-2))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin=margin(t=.5,r=.5,b=.5,l=.5,unit="cm"))

L2<-length%>%
  filter(year==2017 & age == 1 & trip == 10)%>%
  ggplot()+
  geom_histogram(aes(x=mmSL),binwidth=5,fill='grey',colour='black')+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(expand = c(0,0),
                     limits=c(0,65))+
  #scale_x_continuous(limits=c(25,125))+
  theme(axis.text = element_text(size=16))+
  ggtitle("23 May 2017 (age 1 cod)")+
  theme(plot.title = element_text(size=24,hjust=0.5))+
  ylab("# age-1 Atlantic cod")+xlab("Standard length (mm)")+
  theme(axis.title = element_text(size=20))+
  theme(axis.title.x = element_text(vjust=-2))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin=margin(t=.5,r=.5,b=.5,l=.5,unit="cm"))

Fig1<-ggarrange(L1,L2,ncol=2,nrow=1)

ggsave(filename="size-freq-0to1.png",plot=Fig1,device = "png",path="./pulse-structure/figures/",
       width=12,height=5,units="in")

ggsave(filename="Fig1a.png",plot=L1,device = "png",path="./pulse-structure/figures/",
       width=5.5,height=5.5,units="in")
ggsave(filename="Fig1b.png",plot=L2,device = "png",path="./pulse-structure/figures/",
       width=5.5,height=5.5,units="in")

F1<-length%>%
  filter(year==2016 & age ==0 & trip == 15)%>%
  ggplot()+
  geom_histogram(aes(x=mmSL),binwidth=5,fill='grey',colour='black')+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(expand = c(0,0),
                     limits=c(0,225))+
  scale_x_continuous(limits=c(25,125))+
  ylab("Count")+xlab("Standard Length (mm)")+
  theme(axis.title = element_blank())+
  theme(axis.text = element_text(size=16))+
  ggtitle("16 Aug 2016")+
  theme(plot.title = element_text(size=22,hjust=0.5))

F2<-length%>%
  filter(year==2016 & age == 0 & trip == 18)%>%
  ggplot()+
  geom_histogram(aes(x=mmSL),binwidth=5,fill='grey',colour='black')+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(expand = c(0,0),
                     limits=c(0,225))+
  scale_x_continuous(limits=c(25,125))+
  ylab("Count")+xlab("Standard Length (mm)")+
  theme(axis.title = element_blank())+
  theme(axis.text = element_text(size=16))+
  ggtitle("28 Sept 2016")+
  theme(plot.title = element_text(size=22,hjust=0.5))

F3<-length%>%
  filter(year==2016 & age == 0 & trip == 21)%>%
  ggplot()+
  geom_histogram(aes(x=mmSL),binwidth=5,fill='grey',colour='black')+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(expand = c(0,0),
                     limits=c(0,225))+
  scale_x_continuous(limits=c(25,125))+
  ylab("Count")+xlab("Standard Length (mm)")+
  theme(axis.title = element_blank())+
  theme(axis.text = element_text(size=16))+
  ggtitle("14 Nov 2016")+
  theme(plot.title = element_text(size=22,hjust=0.5))

Fig2<-ggarrange(F1,F2,F3,nrow=1,ncol=3)

ggsave(filename="Fig2.png",plot=Fig2,device = "png",path="./pulse-structure/figures/",
       width=12,height=5.5,units="in")

ggsave(filename = "Fig2a.png",plot=F2,device="png",path="./pulse-structure/figures/",
       width=3,height=3,units="in")
ggsave(filename = "Fig2b.png",plot=F3,device="png",path="./pulse-structure/figures/",
       width=3,height=3,units="in")

# mixture distribution examples

# September trip 18
age0.2016<-filter(length,year==2016 & age ==0 & trip==18)
# create dataframe with SL only
group2016<-select(age0.2016,mmSL)
# convert to frequency table
age0group<-mixgroup(group2016, breaks = c(0,seq(50,85,5),92),xname=NULL, k = NULL, usecondit = FALSE)
# set parameters
age0param<-mixparam(c(52,70),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(age0group) # place red triangles where the initial estimates were
plot(fit1)
plot(fit1,root=T)

# November trip 21
age0.2016<-filter(length,year==2016 & age == 0 & trip==21)
# create dataframe with SL only
group2016<-select(age0.2016,mmSL)

# convert to frequency table
age0group<-mixgroup(group2016, breaks = c(0,seq(40,115,5),118),xname=NULL, k = NULL, usecondit = FALSE)
# set parameters
age0param<-mixparam(c(40,60,88),c(5),pi=NULL)
plot(age0group,age0param,"gamma")

# fit mixture
fit1<-mix(age0group,age0param,dist="gamma",mixconstr(consigma="CCV"),
          emsteps=15, usecondit=FALSE, print.level=0)
summary(fit1)
plot(age0group) # place red triangles were the initial estimates were
plot(fit1)
plot(fit1,root=T)

#---- pulse assignment figures -----

Fig3<-new%>%
  filter(cohort==2016)%>%
  ggplot()+
  geom_errorbar(aes(x=date,ymin=min_size,ymax=max_size),width=0)+
  geom_point(aes(x=date,y=mean_size,shape=factor(pulse),colour=factor(pulse)),size=4)+
  theme_bw()+
  ylab("Standard length (mm)")+
  xlab("Date")+
  scale_x_date(date_labels="%d-%b",
               date_breaks="2 weeks")+
  ggtitle("2016 Age-0 Atlantic cod Mixture Distribution Output")+
  #theme(axis.text.x = element_text(angle=40,hjust=1))+
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=20))+
  theme(plot.title=element_text(size=24,hjust=0.5,vjust=4))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=18))+
  theme(axis.title.x = element_text(vjust=-2))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"))+
  scale_color_brewer(palette="Dark2")+
  labs(colour="Pulse",
       shape="Pulse")+
  theme(legend.key.size = unit(1.5,"cm"))

ggsave(filename="Fig3.png",plot=Fig3,device = "png",path="./pulse-structure/figures/",
       width=12,height=6,units="in")

Fig4<-ggplot()+
  geom_errorbar(data=filter(new,cohort==2016),aes(x=date,ymin=min_size,ymax=max_size),width=0)+
  geom_point(data=filter(new,cohort==2016), aes(x=date,y=mean_size,shape=pulse,colour=pulse),size=4)+
  geom_jitter(data=age0, aes(x=date2,y=mmSL),size=2,alpha=0.3,colour='lightskyblue3')+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="2 weeks",
               date_labels = "%d-%b")+
  ggtitle("2016 Age-0 Atlantic cod Mixture Distribution Output")+
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=20))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=18))+
  theme(plot.title = element_text(size=24,hjust=0.5,vjust=4))+
  theme(axis.title.x = element_text(vjust=-2))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"))+
  scale_color_brewer(palette="Dark2")+
  labs(shape="Pulse",
       colour="Pulse")+
  theme(legend.key.size = unit(1.5,"cm"))

ggsave(filename="Fig4.png",plot=Fig4,device = "png",path="./pulse-structure/figures/",
       width=12,height=6,units="in")

Fig5<-age0length_pulse%>%
  mutate(date2=date+3)%>%
  ggplot()+
  geom_jitter(aes(x=date2,y=mmSL,colour=factor(pulse)),alpha=0.3,size=2)+
  geom_errorbar(data=range2016,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  geom_point(data=range2016,aes(x=date,y=mean,shape=factor(pulse),colour=factor(pulse)),size=4)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="2 weeks",
               date_labels = "%d-%b")+
  ggtitle("2016 Age-0 Atlantic cod Preliminary Pulse ASsignment")+
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=20))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=18))+
  theme(plot.title = element_text(size=24,hjust=0.5,vjust=4))+
  theme(axis.title.x = element_text(vjust=-2))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"))+
  scale_color_brewer(palette="Dark2")+
  labs(shape="Pulse",
       colour="Pulse")+
  theme(legend.key.size = unit(1.5,"cm"))

ggsave(filename="Fig5.png",plot=Fig5,device = "png",path="./pulse-structure/figures/",
       width=12,height=6,units="in")

Fig6<-final%>%
  mutate(date2=date+3)%>%
  ggplot()+
  geom_jitter(aes(x=date2,y=mmSL,colour=as.character(pulse)),alpha=0.3,size=2)+
  geom_errorbar(data=pulse.range2016,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  geom_point(data=pulse.range2016,aes(x=date,y=mean,shape=factor(pulse),colour=factor(pulse)),size=4)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="2 weeks",
               date_labels = "%d-%b")+
  ggtitle("2016 Age-0 Atlantic cod Mixture Distribution Output")+
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=20))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=18))+
  theme(plot.title = element_text(size=24,hjust=0.5,vjust=4))+
  theme(axis.title.x = element_text(vjust=-2))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"))+
  scale_color_brewer(palette="Dark2")+
  labs(shape="Pulse",
       colour="Pulse")+
  theme(legend.key.size = unit(1.5,"cm"))+
  scale_shape_manual(values=c(16,17,15,18))

ggsave(filename="Fig6.png",plot=Fig6,device = "png",path="./pulse-structure/figures/",
       width=12,height=6,units="in")

length3<-length2%>%
  mutate(date2=date+5)

Fig7<-ggplot()+
  geom_jitter(data=filter(length3, age==0),aes(x=date2,y=mmSL,colour=factor(pulse)),alpha=0.3,size=2)+
  geom_jitter(data=filter(length3, age==1), aes(x=date2,y=mmSL),colour='grey60',alpha=0.3,size=2)+
  geom_errorbar(data=mix2,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  geom_point(data=mix2,aes(x=date,y=mean,shape=factor(pulse),colour=factor(pulse)),size=3)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels = "%b")+
  ggtitle("2016 Cohort: Age 0 to Age 1 Atlantic cod")+
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=20))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=18))+
  theme(plot.title = element_text(size=24,hjust=0.5,vjust=4))+
  theme(axis.title.x = element_text(vjust=-2))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"))+
  scale_color_brewer(palette="Dark2")+
  labs(shape="Pulse",
       colour="Pulse")+
  theme(legend.key.size = unit(1.5,"cm"))+
  scale_shape_manual(values=c(16,17,15,18,25))


ggsave(filename="Fig7.png",plot=Fig7,device = "png",path="./pulse-structure/figures/",
       width=12,height=6,units="in")

Fig8<-length2016%>%
  mutate(date2=date+5)%>%
  ggplot()+
  geom_jitter(aes(x=date2,y=mmSL,colour=factor(pulse)),alpha=0.3,size=2)+
  geom_errorbar(data=pulses,aes(x=date,ymin=minSL,ymax=maxSL),width=0)+
  geom_point(data=pulses,aes(x=date,y=mean,shape=factor(pulse),colour=factor(pulse)),size=3)+
  theme_bw()+
  xlab("Date")+ylab("Standard length (mm)")+
  scale_x_date(date_breaks="1 month",
               date_labels = "%b")+
  ggtitle("2016 Cohort: Age 0 to Age 1 Atlantic cod")+
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=20))+
  theme(legend.text = element_text(size=16))+
  theme(legend.title = element_text(size=18))+
  theme(plot.title = element_text(size=24,hjust=0.5,vjust=4))+
  theme(axis.title.x = element_text(vjust=-2))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin=margin(t=.75,r=.5,b=.5,l=.5,unit="cm"))+
  scale_color_brewer(palette="Dark2")+
  labs(shape="Pulse",
       colour="Pulse")+
  theme(legend.key.size = unit(1.5,"cm"))+
  scale_shape_manual(values=c(16,17,15,18,25))

ggsave(filename="Fig8.png",plot=Fig8,device = "png",path="./pulse-structure/figures/",
       width=12,height=6,units="in")
