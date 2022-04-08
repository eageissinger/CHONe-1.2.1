#---- CMR Visualization ----

# ---- load data ----
NB<-read.csv("./data/output/CMR-pulse.csv")
multi<-read.csv("./data/output/multistrata.csv")
octfish<-read.csv("./data/data-working/oct-cod-pulses.csv")
mayfish<-read.csv("./data/data-working/may-cod-pulses.csv")
CMRpulse<-read.csv("./data/data-working/CMR/CMR-pulses.csv")
# ---- load packages ----
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(RColorBrewer)


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

# add pulse assignments to fall data
tripdates<-read.csv("./data/data-working/newman-trips.csv")
pulse.assign<-data.frame(trip=rep(CMRpulse$trip,CMRpulse$max-CMRpulse$min+1),
                         year=rep(CMRpulse$year,CMRpulse$max-CMRpulse$min+1),
                         pulse=rep(CMRpulse$pulse,CMRpulse$max-CMRpulse$min+1),
                         sl=unlist(mapply(seq,CMRpulse$min,CMRpulse$max)))
octfish2<-octfish%>%
  dplyr::select(-pulse)%>%
  left_join(tripdates)%>%
  left_join(pulse.assign)

mayfish2<-mayfish%>%
  dplyr::select(-pulse)%>%
  left_join(tripdates)%>%
  left_join(pulse.assign)
# Fig 2

Fig2a<-octfish2%>%
  filter(id<1 & !is.na(sl))%>%
  filter(date=="2016-10-14")%>%
  mutate(pulse=as.factor(pulse))%>%
  mutate(pulse=factor(pulse,levels=c(1,2,3)))%>%
  ggplot()+
  geom_histogram(aes(x=sl,fill=pulse),colour='black',binwidth = 3)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  #scale_fill_brewer(palette = "Dark2",drop=FALSE)+
  scale_fill_manual(values=c('grey10','grey50','grey90'),drop=FALSE)+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=40,y=7.5,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  xlim(40,95)+
  ggtitle("a. 14 October 2016")+
  theme(plot.title = element_text(size=14))

octfish%>%
  filter(id<1 & !is.na(sl))%>%
  filter(date=="2016-10-14")%>%
  mutate(pulse=as.factor(pulse))%>%
  mutate(pulse=factor(pulse,levels=c(1,2,3)))%>%
  ggplot()+
  geom_histogram_pattern(aes(x=sl,fill=pulse),colour='black',binwidth = 3)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  #scale_fill_brewer(palette = "Dark2",drop=FALSE)+
  scale_fill_manual(values=c('grey10','grey50','grey90'),drop=FALSE)+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=40,y=7.5,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  xlim(40,95)+
  guides(fill=guide_legend(reverse=TRUE))


Fig2b<-octfish2%>%
  filter(id<1 & !is.na(sl))%>%
  filter(date=="2016-10-19")%>%
  mutate(pulse=as.factor(pulse))%>%
  ggplot()+
  geom_histogram(aes(x=sl,fill=pulse),colour='black',binwidth = 3)+
  #scale_fill_brewer(palette = "Dark2",drop=FALSE)+
  scale_fill_manual(values=c('grey10','grey50','grey90'),drop=FALSE)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=40,y=7.5,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  xlim(40,95)+
  ggtitle("b. 19 October 2016")+
  theme(plot.title = element_text(size=14))


Fig2<-annotate_figure(ggarrange(Fig2a+theme(axis.title.x=element_text(colour='white')),
          Fig2b+
            theme(axis.title.y = element_text(colour="white"))+
            theme(axis.title.x=element_text(colour='white')),
          ncol=2,nrow=1,
          common.legend = TRUE,legend = 'bottom'),
          bottom=text_grob("Standard Length (mm)",size=14,vjust=-5))


brewer.pal(6,"Dark2")
clrs<- c("1" = '#1B9E77', "2" = '#D95F02', "3" = '#7570B3', "4" = '#E6AB02')
greys<-c("1"='grey10',"2"='grey50',"3"='grey80',"4"='white')

Fig3a<-mayfish2%>%
  filter(id>2 & date == '2017-05-24' & age == 1)%>%
  filter(site=="NB")%>%
  mutate(pulse=as.factor(pulse))%>%
  ggplot()+
  geom_histogram(aes(x=sl,fill=pulse),colour='black',binwidth = 3)+
  #scale_fill_brewer(palette = "Dark2",drop=FALSE)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y=22,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse",
       colour="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12,face='bold'),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  xlim(30,155)+
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25),"cm"))+
  scale_fill_manual(values= greys,
                    limits = c("1", "2", "3", "4"),
                    labels = c("1", "2", "3", "4"))+
  theme(axis.title.y = element_blank())+
  theme(axis.title.x=element_text(colour='white'))
  # scale_colour_manual(values= clrs,
  #                   limits = c("1", "2", "3", "4"),
  #                   labels = c("1", "2", "3", "4"))

Fig3b<-mayfish2%>%
    filter(id>2 & date == '2017-05-24' & age == 1)%>%
    filter(site=="MI")%>%
    mutate(pulse=as.factor(pulse))%>%
    ggplot()+
    geom_histogram(aes(x=sl,fill=pulse),colour='black',binwidth = 3)+
    #scale_fill_manual(values=c('grey20','grey45','grey95'))+
  #scale_fill_brewer(palette = "Dark2",drop=FALSE)+
  #scale_fill_manual(values=c('grey10','grey50','grey90','white'),drop=FALSE)+
    theme_bw()+
    theme(panel.grid=element_blank())+
    scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y=22,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse",
       colour="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12,face='bold'),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  xlim(30,155)+
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25),"cm"))+
  scale_fill_manual(values= greys,
                    limits = c("1", "2", "3", "4"),
                    labels = c("1", "2", "3", "4"))+
  theme(axis.title.y = element_blank())+
  theme(axis.title.x=element_text(colour='white'))
  # scale_colour_manual(values= clrs,
  #                   limits = c("1", "2", "3", "4"),
  #                   labels = c("1", "2", "3", "4"))

Fig3c<-mayfish2%>%
  filter(id>2 & date == '2017-05-24' & age == 1)%>%
  filter(site=="CC")%>%
  mutate(pulse=as.factor(pulse))%>%
  ggplot()+
  geom_histogram(aes(x=sl,fill=pulse),colour='black',binwidth = 3)+
  #scale_fill_manual(values=c('grey10','grey50','grey90','white'),drop=FALSE)+
  #scale_fill_manual(values=c('grey20','grey45','grey95'))+
  #scale_fill_brewer(palette = "Dark2",drop=FALSE)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y=22,label="stretch it"),vjust=-1)+
  labs(x="Standard Length (mm)",
       y="Count",
       fill="Pulse",
       colour="Pulse")+
  theme(axis.title.x= element_text(size=14,margin = margin(t=10)))+
  theme(axis.title.y= element_text(size=14,margin = margin(r=10)))+
  theme(legend.title = element_text(size=12,face='bold'),
        legend.key.size = unit(1.5,'lines'))+
  theme(axis.text = element_text(size=12))+
  xlim(30,155)+
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25),"cm"))+
  scale_fill_manual(values= greys,
                    limits = c("1", "2", "3", "4"),
                    labels = c("1", "2", "3", "4"))+
  theme(axis.title.y = element_blank())+
  theme(axis.title.x=element_text(colour='white'))
  # scale_colour_manual(values= clrs,
  #                   limits = c("1", "2", "3", "4"),
  #                   labels = c("1", "2", "3", "4"))

Fig3<-annotate_figure(ggarrange(Fig3a+
                              ggtitle("a. Newbridge")+theme(plot.title = element_text(size=14)),
                                Fig3b+
                              ggtitle("b. Mistaken")+theme(plot.title = element_text(size=14)),
                          Fig3c+
                            ggtitle("c. Canning's")+theme(plot.title = element_text(size=14)),
                                #labels=c("a","b","c"),
                          ncol=1,nrow=3,
                                common.legend = TRUE,legend = 'right'),
                      left = text_grob("Count", size=14,rot=90),
                      bottom=text_grob("Standard Length (mm)",size=14,vjust=-1))

ggsave(file="./overwinter-CMR/output/Fig2.png",plot=Fig2,width=168,height=84,units="mm")
ggsave(file="./overwinter-CMR/output/Fig3.png",plot=Fig3,width=100,height=168,units="mm")

ggsave(file="./overwinter-CMR/output/Fig2.jpg",plot=Fig2,width=170,height=85,units="mm",dpi=500)
ggsave(file="./overwinter-CMR/output/Fig3.jpg",plot=Fig3,width=85,height=170,units="mm", dpi = 500)
