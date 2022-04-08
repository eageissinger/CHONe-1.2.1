# Figure and data visualization for condition analysis
# run newman-condition-analysis before running this code

# ----- Figure 2 -----
# Condition
Fig2a<-K%>%
  filter(season=="fall")%>%
  add_row(season="fall",
          cohort=2004)%>%
  ggplot()+
  geom_boxplot(aes(y=fulton,x=factor(cohort)))+
  theme_bw()+
  ylab("Pre-winter K")+
  xlab("Cohort")+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=12,angle=40,hjust=1),
        axis.title.x = element_text(size=14,vjust=0),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14,vjust=2))+
  scale_y_continuous(limits=c(0.4,1.1),
                     breaks=seq(0.4,1.1,by=.1))+
  theme(axis.title.x = element_text(colour='white'))+
  ggtitle("Age-0")+
  theme(plot.title = element_text(size=14))

Fig2b<-K%>%
  filter(season=="spring")%>%
  add_row(season="spring",
          cohort=2003)%>%
  add_row(season="spring",
          cohort=2004)%>%
  add_row(season="spring",
          cohort=2006)%>%
  add_row(season="spring",
          cohort=2008)%>%
  ggplot()+
  geom_boxplot(aes(y=fulton,x=factor(cohort)))+
  theme_bw()+
  ylab("Post-winter K")+
  xlab("Cohort")+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=12,angle=40,hjust=1),
        axis.title.x = element_text(size=14,vjust=0),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14,vjust=2))+
  scale_y_continuous(limits=c(0.4,1.1), 
                     breaks=seq(0.4,1.1,by=.1))+
  ggtitle("Age-1")+
  theme(plot.title = element_text(size=14))


Fig2<-ggarrange(Fig2a,Fig2b,nrow=2,
                labels = c("a","b"))

ggsave("./figures/Figure2.png", Fig2, "png",units='in',width=6,height=5)

# ggplot(data=winter.mort.full)+
#   geom_hline(yintercept = 0.4260159,linetype='dashed')+
#   geom_smooth(aes(x=cohort,y=mortality))



# ---- Figure 4 -----
# full model summary

fig4a<-ggplot(as.data.frame(p1))+
  geom_jitter(data=full,aes(x=pulse,y=p),width=0.1,colour='grey40',size=1.5)+
  geom_point(aes(x=pulse,y=fit),size=3,shape=15)+
  geom_errorbar(aes(x=pulse,ymin=lower,ymax=upper),width=0.05)+
  theme_bw()+
  xlab("Pulse")+ylab("Survival probability")+
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(size=14,vjust=0),
        axis.title.y = element_text(size=14,vjust=2),
        axis.text = element_text(size=12))+
  scale_y_continuous(limits=c(0.0,1.0), 
                     breaks=seq(0.0,1.0,by=.2))

fig4b<-ggplot(as.data.frame(p2))+
  geom_line(aes(x=preK,y=fit),size=1)+
  geom_ribbon(aes(x=preK,ymin=lower,ymax=upper),alpha=0.3,fill='steelblue2')+
  geom_point(data=full,aes(x=preK,y=p),size=1.5,colour='grey40')+
  theme_bw()+
  xlab("Pre-winter K")+ylab("Survival probability")+
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(size=14,vjust=0),
        axis.title.y = element_text(size=14,vjust=2),
        axis.text = element_text(size=12))+
  scale_y_continuous(limits=c(0.0,1.0), 
                     breaks=seq(0.0,1.0,by=.2))+
  theme(axis.title.y = element_text(colour='white'))

fig4c<-ggplot(as.data.frame(p4))+
  geom_line(aes(x=days_below_1,y=fit),size=1)+
  geom_ribbon(aes(x=days_below_1,ymin=lower,ymax=upper),alpha=0.3,fill='steelblue2')+
  geom_point(data=full,aes(x=days_below_1,y=p),size=1.5,colour='grey40')+
  theme_bw()+
  xlab("Days below 1? C ")+ylab("Survival probability")+
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(size=14,vjust=0),
        axis.title.y = element_text(size=14,vjust=2),
        axis.text = element_text(size=12))+
  scale_y_continuous(limits=c(0.0,1.0), 
                     breaks=seq(0.0,1.0,by=.2))

fig4d<-ggplot(as.data.frame(p3))+
  geom_line(aes(x=postK,y=fit),size=1)+
  geom_ribbon(aes(x=postK,ymin=lower,ymax=upper),alpha=0.3,fill='steelblue2')+
  geom_point(data=full,aes(x=postK,y=p),size=1.5,colour='grey40')+
  theme_bw()+
  xlab("Post-winter K")+ylab("Survival probability")+
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(size=14,vjust=0),
        axis.title.y = element_text(size=14,vjust=2),
        axis.text = element_text(size=12))+
  scale_y_continuous(limits=c(0.0,1.0), 
                     breaks=seq(0.0,1.0,by=.2))+
  theme(axis.title.y = element_text(colour='white'))


Fig4<-ggarrange(fig4a, fig4b, fig4c, fig4d,
          nrow=2,ncol=2,
          labels=c('a','b','c','d'))

ggsave("./figures/Figure4.png",Fig4,"png",units="in",height=6,width=6)


# ---- Figure 5 ----
postKpred<-alldata%>%
  filter(!is.na(postK))%>%
  bind_cols(pred.m3)%>%
  rename(estimate='...14')%>%
  mutate(p=exp(estimate))

fig5<-ggplot(postKpred)+
  geom_smooth(aes(x=preK,y=postK),method="glm",colour='black',fill='steelblue2')+
  geom_point(aes(x=preK,y=p),size=1.5,colour='grey40')+
  theme_bw()+
  xlab("Pre-winter Fulton's K") + ylab("Post-winter Fulton's K")+
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(size=14,vjust=0),
        axis.title.y = element_text(size=14,vjust=2),
        axis.text = element_text(size=12))+
  scale_y_continuous(limits=c(0.55,.9),
                     breaks=seq(0.5,.9,by=.1))

ggsave("./figures/Figure5.png",fig5,"png")


# ---- Figure 6 ----

fig6<-ggplot(Ksettle)+
  geom_smooth(aes(x=settle.week,y=p,colour=pulse),method="glm")+
  geom_point(aes(x=settle.week,y=p),size=1.5,colour='grey40')+
  theme_bw()+
  labs(x="Settlement week",
       y="Post-winter Fulton's K estimate",
       colour="Pulse")+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(size=14,vjust=0),
        axis.title.y=element_text(size=14,vjust=2),
        axis.text=element_text(size=12),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))+
  scale_y_continuous(limits=c(0.5,1), 
                     breaks=seq(0.6,1,by=.2))+
  scale_x_continuous(limits=c(32,44),
                     breaks=seq(32,44,by=2))+
  scale_colour_brewer(palette = "Dark2")


ggsave("./figures/Figure6.png",fig6,"png")



# ----- Figure 7 -----
fig7<-ggplot(settleS)+
  geom_smooth(aes(x=settle.week,y=p,colour=pulse),method="glm")+
  geom_point(aes(x=settle.week,y=p),size=1.5,colour='grey40')+
  theme_bw()+
  labs(x="Settlement week",
       y="Survival probability estimate",
       colour="Pulse")+
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(size=14,vjust=0),
        axis.title.y=element_text(size=14,vjust=2),
        axis.text=element_text(size=12),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))+
  scale_y_continuous(limits=c(0.0,1.0),
                     breaks=seq(0.0,1.0,by=0.2))+
  scale_x_continuous(limits=c(25,45),
                     breaks=seq(26,44,by=2))+
  scale_colour_brewer(palette = "Dark2")


ggsave("./figures/Figure7.png",fig7,"png")


# ---- Figure Y ----
# settlement and pulse
p1<-alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  filter(pulse==1)%>%
  ggplot()+
  geom_smooth(aes(x=settle.week,y=postCount/preCount2),method='lm',colour='black',size=1.5)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Settlement Week")+
  ggtitle("Pulse 1")+
  theme(plot.title = element_text(size=16,hjust=.5))+
  theme(axis.title.x = element_text(colour='white'))
p2<-alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  filter(pulse==2)%>%
  ggplot()+
  geom_smooth(aes(x=settle.week,y=postCount/preCount2),method='lm',colour='black',size=1.5)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Settlement Week")+
  ggtitle("Pulse 2")+
  theme(plot.title = element_text(size=16,hjust=.5))+
  theme(axis.title.x = element_text(colour='white'))


p3<-alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  filter(pulse==3)%>%
  ggplot()+
  geom_smooth(aes(x=settle.week,y=postCount/preCount2),method='lm',colour='black',size=1.5)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Settlement Week")+
  ggtitle("Pulse 3")+
  theme(plot.title = element_text(size=16,hjust=.5))+
  theme(axis.title.x = element_text(colour="white"))


p4<-alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  filter(pulse==4)%>%
  ggplot()+
  geom_smooth(aes(x=settle.week,y=postCount/preCount2),method='lm',colour='black',size=1.5)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Settlement Week")+
  ggtitle("Pulse 4")+
  theme(plot.title = element_text(size=16,hjust=.5))+
  theme(axis.title.y = element_text(colour='white'))

ggarrange(p1,p2,p3,nrow=1)

# ----- data summary -----
# pulse and length summary
AC0<-SLfull%>%
  filter(Species=="AC" & Age==0 & Year>1998 & Year <2019 & Pulse <4)%>%
  mutate(cohort=Year)
AC1<-SLfull%>%
  filter(Species=="AC" & Age == 1 & Year >1999 & Year < 2020 & Pulse <4)%>%
  mutate(cohort=Year-1)
acSL<-bind_rows(AC0,AC1)

acSL%>%
  filter(Age==0 & Trip==19)%>%
  ggplot()+
  geom_histogram(aes(x=mmSL,fill=factor(Pulse)))+
  facet_wrap(~cohort)

acSL%>%
  filter(Age==0 & Month==10)%>%
  group_by(Pulse)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL),sd=sd(mmSL))

acSL%>%
  filter(Age == 0 & Month == 10 & Pulse ==1)%>%
  group_by(Pulse,cohort)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL),sd=sd(mmSL))%>%
  arrange(mean)
acSL%>%
  filter(Age == 0 & Month == 10 & Pulse ==2)%>%
  group_by(Pulse,cohort)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL),sd=sd(mmSL))%>%
  arrange(mean)
acSL%>%
  filter(Age == 0 & Month == 10 & Pulse ==3)%>%
  group_by(Pulse,cohort)%>%
  summarise(min=min(mmSL),max=max(mmSL),mean=mean(mmSL),sd=sd(mmSL))%>%
  arrange(mean)
# settlement time
head(settle)
settle%>%
  filter(Pulse==1)%>%
  mutate(date=ymd(paste(settle.year,settle.month,settle.day)))%>%
  arrange(settle.yday)
settle%>%
  filter(Pulse==2)%>%
  mutate(date=ymd(paste(settle.year,settle.month,settle.day)))%>%
  arrange(settle.yday)
settle%>%
  filter(Pulse==3)%>%
  mutate(date=ymd(paste(settle.year,settle.month,settle.day)))%>%
  arrange(settle.yday)
# condition summary
condition%>%
  filter(age==0 & month == 10 & pulse <4)%>%
  group_by(pulse)%>%
  summarise(min=min(fulton),max=max(fulton),mean=mean(fulton),sd=sd(fulton))
condition%>%
  filter(age==0 & month == 10 & pulse <4)%>%
  filter(cohort>1998 & cohort<2019)%>%
  group_by(cohort)%>%
  summarise(min=min(fulton),max=max(fulton),mean=mean(fulton),sd=sd(fulton))%>%
  arrange(mean)

condition%>%
  filter(age==1 & month == 5 & pulse <4)%>%
  group_by(pulse)%>%
  summarise(min=min(fulton),max=max(fulton),mean=mean(fulton),sd=sd(fulton))
condition%>%
  filter(age==1 & month == 5 & pulse <4)%>%
  filter(cohort>1998 & cohort<2019)%>%
  group_by(cohort)%>%
  summarise(min=min(fulton),max=max(fulton),mean=mean(fulton),sd=sd(fulton))%>%
  arrange(mean)
