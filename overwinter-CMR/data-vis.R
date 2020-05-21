#---- CMR Visualization ----
setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/overwinter-CMR/")

# ---- load data ----
NB<-read.csv("../data/output/CMR-pulse.csv")
multi<-read.csv("../data/output/multistrata.csv")
octfish<-read.csv("../data/data-working/oct-cod-pulses.csv")
mayfish<-read.csv("../data/data-working/may-cod-pulses.csv")
# ---- load packages ----
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)

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
  mutate(M=replace(M,parameter=="p",NA))%>%
  mutate(Mse=-log(se),Mlcl=-log(lcl),Mucl=-log(ucl))->NB

multi%>%
  mutate(M=-log(estimate))%>%
  mutate(M=replace(M, parameter =="p" | parameter=="Psi", NA))%>%
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
  geom_errorbar(aes(x=group,ymin=M-Mse,ymax=M+Mse,group=factor(group)),
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
  geom_point(aes(x=as.factor(group),y=M),
             position=position_dodge(width=.5),size=2)+
  geom_errorbar(aes(x=group,ymin=M-Mse,ymax=M+Mse,group=factor(group)),
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
  geom_point(aes(x=as.factor(group),y=M,shape=season),
             position=position_dodge(width=.5),size=2)+
  geom_errorbar(aes(x=as.factor(group),ymin=M-Mse,ymax=M+Mse,shape=season),
                position=position_dodge(width=.5),
                width=0)+
  theme_bw()+
  xlab("Pulse")+
  ylab("M("~day^-1*")")

# ---- Size Frequency -----

# October
octfish%>%
  filter(date=='2016-10-14' & id<1 & !is.na(sl))%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 1)
octfish%>%
  filter(date=='2016-10-14' & id<1 & !is.na(sl))%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl))

octfish%>%
  filter(date=='2016-10-19' & id <1 & !is.na(sl))%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 1)

octfish%>%
  filter(date=='2016-10-19' & id <1 & !is.na(sl))%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl))

octfish%>%
  group_by(date,pulse)%>%
  summarise(mean(sl),max(sl),min(sl))

mayfish<-mayfish%>%
  mutate(pulse=replace(pulse,is.na(pulse) & sl<=62,4))%>%
  mutate(pulse=replace(pulse,is.na(pulse) & sl>115,1))

mayfish%>%
  filter(id>=2 & site=="NB" & date == '2017-05-24' & age == 1)%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 3)
mayfish%>%
  filter(id>=2 & site=="NB" & date == '2017-05-24' & age == 1)%>%
  filter(is.na(pulse))
mayfish%>%
  filter(id>=2 & site=="NB" & date == '2017-05-24' & age == 1)%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl))
mayfish%>%
  filter(id>=2 & site == "MI" & date == '2017-05-24' & age == 1)%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 3)
mayfish%>%
  filter(id>=2 & site == "MI" & date == '2017-05-24' & age == 1)%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl))
mayfish%>%
  filter(id>=2 & site =="CC" & date == '2017-05-24' & age == 1)%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl))

mayfish%>%
  filter(id>=2 & site =="CC" & date == '2017-05-24' & age == 1)%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 3)

mayfish%>%
  filter(id>2 & date == '2017-05-24 & age == 1')%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth = 3)+
  facet_wrap(~site)

octfish%>%
  filter(id<1 & !is.na(sl))%>%
  ggplot()+geom_histogram(aes(x=sl,fill=factor(pulse)),binwidth=3)+
  facet_wrap(~date)

# ----- Manuscript Figures -----
# Fig 1
Fig1a<-octfish%>%
  filter(id<1 & !is.na(sl))%>%
  filter(date=="2016-10-14")%>%
  mutate(pulse=as.factor(pulse))%>%
  mutate(pulse=factor(pulse,levels=c(1,2,3)))%>%
  ggplot()+
  geom_histogram(aes(x=sl,fill=pulse),colour='black',binwidth = 3)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_fill_manual(values=c('grey25','grey64','grey85'),drop=FALSE)+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=40,y=7.5,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12,face='bold'),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  xlim(40,95)
  

Fig1b<-octfish%>%
  filter(id<1 & !is.na(sl))%>%
  filter(date=="2016-10-19")%>%
  mutate(pulse=as.factor(pulse))%>%
  ggplot()+
  geom_histogram(aes(x=sl,fill=pulse),colour='black',binwidth = 3)+
  scale_fill_manual(values=c('grey20','grey45','grey80','grey95'))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=40,y=7.5,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12,face='bold'),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  xlim(40,95)


Fig1<-annotate_figure(ggarrange(Fig1a+theme(axis.title.x=element_text(colour='white')),
          Fig1b+
            theme(axis.title.y = element_text(colour="white"))+
            theme(axis.title.x=element_text(colour='white')),
          labels=c("a","b"),ncol=2,nrow=1,
          common.legend = TRUE),
          bottom=text_grob("Standard Length (mm)",size=14,vjust=-1))

Fig2a<-mayfish%>%
  filter(id>2 & date == '2017-05-24' & age == 1)%>%
  filter(site=="NB")%>%
  mutate(pulse=as.factor(pulse))%>%
  ggplot()+
  geom_histogram(aes(x=sl,fill=pulse),binwidth = 3,colour='black')+
  scale_fill_manual(values=c('grey20','grey45','grey95'))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y=22,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12,face='bold'),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  xlim(30,155)+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))

Fig2b<-mayfish%>%
    filter(id>2 & date == '2017-05-24' & age == 1)%>%
    filter(site=="MI")%>%
    mutate(pulse=as.factor(pulse))%>%
    ggplot()+
    geom_histogram(aes(x=sl,fill=pulse),binwidth = 3,colour='black')+
    scale_fill_manual(values=c('grey20','grey45','grey95'))+
    theme_bw()+
    theme(panel.grid=element_blank())+
    scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y=22,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12,face='bold'),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  xlim(30,155)+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))

Fig2c<-mayfish%>%
  filter(id>2 & date == '2017-05-24' & age == 1)%>%
  filter(site=="CC")%>%
  mutate(pulse=as.factor(pulse))%>%
  ggplot()+
  geom_histogram(aes(x=sl,fill=pulse),binwidth = 3,colour='black')+
  scale_fill_manual(values=c('grey20','grey45','grey95'))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y=22,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12,face='bold'),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  xlim(30,155)+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))

Fig2<-annotate_figure(ggarrange(Fig2a+
                            theme(axis.title.x=element_text(colour='white')),
                                Fig2b+
                                  theme(axis.title.y = element_text(colour="white"))+
                                  theme(axis.title.x=element_text(colour='white')),
                          Fig2c+
                            theme(axis.title = element_text(colour="white")),
                                labels=c("a","b","c"),ncol=3,nrow=1,
                                common.legend = TRUE),
                      bottom=text_grob("Standard Length (mm)",size=14,vjust=-1))

ggsave(file="./output/Fig1.png",plot=Fig1,width=168,height=84,units="mm")
ggsave(file="./output/Fig2.png",plot=Fig2,width=168,height=84,units="mm")
