# Survival and Pre- and post-winter condition
# Part 2: Analysis

# ---- working directory -----
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/condition-field/")

# ---- Packages -----
library(car)
library(lme4)
library(effects)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(MASS)
library(tidyverse)
library(ggpubr)
library(broom.mixed)
library(lubridate)

# ---- data ----
SLfull<-read.csv("../data/data-working/newman-length.csv")
condition<-read.csv("../data/output/condition-field-clean.csv")
data<-read.csv("../data/output/condition-field-formatted.csv")
settle<-read.csv("../data/output/settlement-day.csv")
temp<-read.csv("../data/data-working/newman-temp-to-2017.csv")
winter<-read.csv("../data/output/newman-winter-summary.csv")
#mort<-read.csv("../data/output/mortalities.csv") # need to redo mortality calculations

#check data
head(data)
str(data)
summary(data)

# ---- characterize pulse structure -----

# pick example year
SLfull%>%
  filter(Species=="AC" & Age == 0)%>%
  distinct(Pulse,Year)
# 2017 or 2018 or 2008?

SLfull%>%
  mutate(Month=as.integer(str_sub(Date,start=5,end=6)),
         Day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(Date=ymd(paste(Year,Month,Day,sep="-")))%>%
  filter(Species=="AC" & Age == 0 & Year == 2018)%>%
  ggplot()+
  geom_jitter(aes(y=mmSL,x=Date,colour=factor(Pulse)))


SLa<-SLfull%>%
  mutate(Month=as.integer(str_sub(Date,start=5,end=6)),
         Day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(Date=ymd(paste(Year,Month,Day,sep="-")))%>%
  filter(Species=="AC" & Age == 0 & Year == 2012)%>%
  ggplot()+
  geom_jitter(aes(y=mmSL,x=Date,colour=factor(Pulse)),size=2,alpha=0.75)+
  theme_bw()+
  labs(ylab="Standard Length (mm",
       xlab="Date",
       colour="Pulse")+
  scale_x_date(date_labels="%d-%b",
               date_breaks="2 weeks")+
  theme(axis.text.x = element_text(angle=40,hjust=1))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  scale_colour_brewer(palette="Dark2")+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=12))+
  ggtitle("2012 Cohort")+
  theme(plot.title = element_text(size=18,hjust=0.5))+
  ylim(c(25,120))+
  theme(axis.title.x = element_blank())
SLb<-SLfull%>%
  mutate(Month=as.integer(str_sub(Date,start=5,end=6)),
         Day=as.integer(str_sub(Date,start=7,end=8)))%>%
  mutate(Date=ymd(paste(Year,Month,Day,sep="-")))%>%
  filter(Species=="AC" & Age == 0 & Year == 2019 & !is.na(Pulse))%>%
  ggplot()+
  geom_jitter(aes(y=mmSL,x=Date,colour=factor(Pulse)),size=2,alpha=0.75)+
  theme_bw()+
  labs(ylab="Standard Length (mm",
       xlab="Date",
       colour="Pulse")+
  scale_x_date(date_labels="%d-%b",
               date_breaks="2 weeks")+
  theme(axis.text.x = element_text(angle=40,hjust=1))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  scale_colour_brewer(palette="Dark2")+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=12))+
  ggtitle("2019 Cohort")+
  theme(plot.title = element_text(size=18,hjust=0.5))+
  theme(axis.title.y = element_text(colour='white'))+
  ylim(25,120)+
  theme(axis.title.x = element_blank())

ggarrange(SLa,SLb,common.legend = TRUE,legend="bottom")

SLfull%>%filter(Year==2017 & Trip >16 & Trip <22)%>%
  distinct(Date)

# format data
# ---- Graphical model ----

alldata<-data%>%filter(pulse<4) # remove pulse 5 and 6 from working data
alldata$pulse<-as.factor(alldata$pulse) # convert pulse to factor
alldata$pulse<-droplevels(alldata$pulse) # remove 5 and 6 from factor levels

# ---- preliminary models -----

# Winter characterisation and analysis:
winter.m0<-lm(mean_temp~days_below_1+cohort, data=winter)
plot(winter.m0)
qqnorm(resid(winter.m0))
qqline(resid(winter.m0),col='red')
hist(resid(winter.m0))
summary(winter.m0)
Anova(winter.m0,type="III")

Fig4<-winter%>%
  filter(cohort>1998)%>%
  ggplot()+geom_line(aes(x=cohort,y=mean_temp),size=1)+
  geom_ribbon(aes(ymin=(mean_temp-sd_temp),ymax=(mean_temp+sd_temp),x=cohort),alpha=0.3)+
  geom_text(aes(x=cohort, y=mean_temp,label=as.character(days_below_1)),vjust=2.25,size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  scale_x_continuous(breaks=seq(1999,2016,by=1))+
  theme(axis.text.x = element_text(angle=40,hjust=1,size=10))+
  ylab("Winter Temperature (°C)")+xlab("Cohort")

ggsave(file="./figures/Fig4.png",plot=Fig4,width=168,height=84,units="mm")

#calculate overwinter mortality rate
# 275 days
winter.mort.pulse<-alldata%>%
  mutate(mortality=(log(postCount/preCount)/-275)*100)%>%
  mutate(mortality=replace(mortality,preCount==0 |postCount==0,NA))
winter.mort.full<-alldata%>%
  group_by(cohort,days_below_1,mean_temp)%>%
  summarise(preCount=sum(preCount),postCount=sum(postCount))%>%
  mutate(mortality=(log(postCount/preCount)/-275)*100)%>%
  mutate(mortality=replace(mortality,preCount==0 |postCount==0,NA))

mean(winter.mort.pulse$mortality,na.rm=TRUE)
winter.mort.pulse%>%
  filter(pulse!="4")%>%
  ggplot()+
  geom_hline(yintercept = 0.3134413,linetype='dashed')+
  geom_point(aes(x=cohort,y=mortality))+
  facet_wrap(~pulse)


mean(winter.mort.full$mortality,na.rm=TRUE)
ggplot(data=winter.mort.full)+
  geom_hline(yintercept = 0.4260159,linetype='dashed')+
  geom_point(aes(x=cohort,y=mortality))

#winter mortality and temp
winter.m1<-lm(mortality~days_below_1+cohort,data=winter.mort.full)
plot(winter.m1)
qqnorm(resid(winter.m1))
qqline(resid(winter.m1),col='red')
hist(resid(winter.m1))
summary(winter.m1)
Anova(winter.m1,type="III")

winter.m2<-lmer(mortality~days_below_1+pulse+(1|cohort),data=winter.mort.pulse)
plot(winter.m2)
qqnorm(resid(winter.m2))
qqline(resid(winter.m2),col='red')
hist(resid(winter.m2))
summary(winter.m2)
Anova(winter.m2,type="III")
plot(allEffects(winter.m2))
# indications of pulse-structure mortality, but weak signal
# add more variables (condition)

# Figure 2
ggplot(data=winter.mort.full)+
  geom_hline(yintercept = 0.4260159,linetype='dashed')+
  geom_point(aes(x=cohort,y=mortality),size=2)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab('Mortality Rate % ??? ' ~d^-1)+
  xlab('Cohort')+
  scale_x_continuous(breaks = seq(1998,2016,by=2))+
  ylim(-1,2)
  
winter.mort.pulse%>%
  
ggplot()+
  geom_hline(yintercept = 0.3134413,linetype='dashed')+
  geom_point(aes(x=cohort,y=mortality,colour=factor(pulse)),size=2)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab('Mortality Rate % ??? ' ~d^-1)+
  xlab('Cohort')+
  scale_x_continuous(breaks = seq(1998,2016,by=2))+
  #ylim(-1,2)+
  theme(legend.position = "top")

# ---- full model ----
# October and July model
summary(alldata)
glimpse(alldata)

# everything (all pulses, all variables)
m1<-glmer(cbind(postCount,preCount)~fallSL+preK+postK+scale(days_below_1)+(1|cohort),
          data=alldata,family=binomial)
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1),col='red')
hist(resid(m1))
summary(m1)
Anova(m1,type="III")
plot(allEffects(m1),residuals=TRUE)
allEffects(m1)
a<-plot(effect("fallSL",m1),
     main=list(label="",cex=1.5),
     ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5),
     xlab=list(label="Pulse",cex=1.5))
b<-plot(effect("preK",m1),
     xlab=list(label="Prewinter K"),
     main=list(label="",cex=1.5),
     ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5))
c<-plot(effect("postK",m1),
     xlab=list(label="Postwinter K"),
     main=list(label="",cex=1.5),
     ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5))
d<-plot(effect("days_below_1",m1),
     xlab=list(label="Days Below 1"),
     main=list(label="",cex=1.5),
     ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5))
ggarrange(a,b,c,d)

plot(predictorEffects(m1))
Anova(m1,type="III")

exp(4.3409)/2
exp(4.8825)/2
exp(0.0500)/2
exp(1.1920)/2
# ----- graphical model ------

ggplot(data=alldata,aes(x=pulse,y=postCount/preCount))+
  geom_boxplot(outlier.shape = NA)+ylim(0,1)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab('Abundance Ratio')

ggplot(winter.mort.pulse)+
  geom_boxplot(aes(x=pulse,y=mortality))



plot_model(
  m1, 
  type="std",
  title = "Survival",
  colors = "black", 
  show.values = TRUE,
  value.offset = .4,
  value.size = 4,
  dot.size = 3,
  line.size = 1.5,
  vline.color = "#a8ddb5"
)

# ---- Odds -----
model.df1 <- tidy(m1)%>%
  filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation
model.df1

e1<-model.df1 %>% 
  mutate(odds = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(m1)),  # Variance of each coefficient
         odds.se = sqrt(odds^2 * var.diag))%>%  # Odds-ratio adjusted
  mutate(p = odds/(1+odds))
e1

# summary: attempted predictions - doesn't seem to work
# must look at additional info for visualizing ANCOVA model..... Split up into four pulses maybe??

# ---- what is the evidence -----
null<-glmer(cbind(postCount,preCount)~1+
              (1|cohort),data=alldata,family=binomial)
deviance(null)
deviance(m1)
deviance(null)-deviance(m1)
exp(218.0794/2)
Anova(m1,type="III")
# strong evidence for the model

# --- model 2: pulse and preK days_below_1 ----
# everything (all pulses, all variables)
m2<-glmer(cbind(postCount,preCount)~fallSL+preK+scale(days_below_1)+(1|cohort),
          data=alldata,family=binomial)
plot(m2)
qqnorm(resid(m2))
qqline(resid(m2),col='red')
hist(resid(m2))
summary(m2)
Anova(m2,type="III")
plot(allEffects(m2),residuals=TRUE)
allEffects(m2)
a<-plot(effect("pulse",m2),
        main=list(label="",cex=1.5),
        ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5),
        xlab=list(label="Pulse",cex=1.5))
b<-plot(effect("preK",m2),
        xlab=list(label="Prewinter K"),
        main=list(label="",cex=1.5),
        ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5))
ggarrange(a,b)

plot(predictorEffects(m2))

exp(13.3618)/2
exp(6.5428)/2
exp(7.2952)/2
exp(4.9128)/2

### SL and preK only (winter no effect)

# --- model 3: SL and preK only ----
# everything (all pulses, all variables)
m3<-glmer(cbind(postCount,preCount)~fallSL*scale(preK)+(1|cohort),
          data=alldata,family=binomial)
plot(m3)
qqnorm(resid(m3))
qqline(resid(m3),col='red')
hist(resid(m3))
summary(m3)
Anova(m3,type="III")
plot(allEffects(m3),residuals=TRUE)

# no interactive effect

# --- model 4 SL and preK no interaction ----
m4<-glmer(cbind(postCount,preCount)~fallSL+preK+(1|cohort),
          data=alldata,family=binomial)
plot(m4)
qqnorm(resid(m4))
qqline(resid(m4),col='red')
hist(resid(m4))
summary(m4)
Anova(m4,type="III")
plot(allEffects(m4),residuals=TRUE)

# ---- winter ----
alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=days_below_1),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Days Below 1° C")+
  ylim(0,6)


mw<-glm(cbind(postCount,preCount)~days_below_1,data=distinct(cohort,days_below_1),family=binomial)
plot(mw)
hist(resid(mw))
Anova(mw,type="III")
summary(mw)

pred.w1<-predict(mw)
winter.pred<-cbind(filter(alldata, !is.na(days_below_1)),pred.w1)
winter.pred%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=days_below_1),size=3)+
  geom_smooth(aes(y=pred.w1,x=days_below_1),method="glm",colour='black',size=2)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Days Below 1° C")+
  annotate("text",x=120,y=16,label="y = -1.79 + 0.002x",size=5)

ggplot(alldata)+
  geom_point(aes(x=cohort,y=days_below_1),size=2)+
  geom_line(aes(x=cohort,y=days_below_1))+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Days Below 1° C")+xlab("Cohort")


# ---- postwinter K and survival ----
alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=postK),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Postwinter K")+
  ylim(c(0,6))

mK<-glmer(cbind(postCount,preCount)~postK+(1|cohort),data=alldata,family=binomial)
plot(mK)
hist(resid(mK))
Anova(mK,type="III")
summary(mK)

pred.K1<-predict(mK)
postK.pred<-cbind(filter(alldata, !is.na(postK)),pred.K1)
postK.pred%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=postK),size=3)+
  geom_smooth(aes(y=pred.K1,x=postK),method="glm",colour='black',size=2)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Postwinter K")+
  annotate("text",x=0.85,y=5,label="y = -4.14 + 2.94x",size=5)


ggplot(alldata)+
  geom_jitter(aes(x=cohort,y=postK,colour=factor(pulse)),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  scale_colour_brewer(palette="Dark2")+
  labs(y ="Postwinter K",
       x = "Cohort",
       colour="Pulse")+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  scale_colour_brewer(palette="Dark2")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=12))


# ---- prewinter and survival-----
head(settle)
head(condition)
settle<-settle%>%
  rename(cohort=Year,pulse=Pulse)
cond2<-left_join(condition,settle,by=c("cohort","pulse"))%>%
  left_join(temp)

# ----- condition -----
a<-condition%>%
  filter(fulton<1.5)%>%
  filter(month==10)%>%
  filter(pulse<5)%>%
  filter(cohort>2000)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton,fill=factor(pulse)))+
  theme_bw()+
  xlab("Cohort")+
  ylab("Fulton's K")+
  scale_fill_discrete(name="Pulse")+
  theme(axis.text.x = element_text(angle=45))
condition%>%
  filter(fulton<1.5)%>%
  filter(month==11)%>%
  filter(pulse<5)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton,fill=factor(pulse)))+
  theme(axis.text.x = element_text(angle=45))
b<-condition%>%
  filter(fulton<1.5)%>%
  filter(month==5)%>%
  filter(pulse<5)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton,fill=factor(pulse)))+
  theme_bw()+
  xlab("Cohort")+
  ylab("Fulton's K")+
  scale_fill_discrete(name="Pulse")+
  theme(axis.text.x = element_text(angle=45))
ggarrange(a,b,nrow = 2,ncol=1,common.legend = TRUE)


condition%>%
  filter(fulton<1.5)%>%
  filter(month==10)%>%
  filter(pulse<5)%>%
  filter(cohort>2000 & cohort<2017)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton),size=1)+
  xlab("Cohort")+
  ylab("Fulton's K")+
  theme(axis.text.x = element_text(angle=45))+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))

condition%>%
  filter(fulton<1.5)%>%
  filter(month==5)%>%
  filter(pulse<5)%>%
  filter(cohort>2000 & cohort<2017)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(cohort),y=fulton),size=1)+
  xlab("Cohort")+
  ylab("Fulton's K")+
  theme(axis.text.x = element_text(angle=45))+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))

mK2<-glmer(cbind(postCount,preCount)~preK+(1|cohort),data=alldata,family=binomial)
plot(mK2)
hist(resid(mK2))
Anova(mK2,type="III")
summary(mK2)

mK3<-glm(cbind(postCount,preCount)~preK+fallSL,data=alldata,family=binomial)
plot(mK3)
hist(resid(mK3))
Anova(mK3,type="III")


alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=preK),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Prewinter K")+
  ylim(0,6)

ggplot(alldata)+
  geom_jitter(aes(x=cohort,y=preK,colour=factor(pulse)),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  scale_colour_brewer(palette="Dark2")+
  labs(y="Prewinter K",
       x = "Cohort",
       colour="Pulse")+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  scale_colour_brewer(palette="Dark2")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=12))


# pre-winter size

mS<-glmer(cbind(postCount,preCount)~fallSL+(1|cohort),data=alldata,family=binomial)
plot(mS)
hist(resid(mS))
Anova(mS,type="III")
summary(mS)


alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=fallSL),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Fall SL (mm)")+
  ylim(0,6)

ggplot(alldata)+
  geom_jitter(aes(x=cohort,y=fallSL,colour=factor(pulse)),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  scale_colour_brewer(palette="Dark2")+
  labs(y="Fall SL (mm)",
       x = "Cohort",
       colour="Pulse")+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  scale_colour_brewer(palette="Dark2")+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=12))


# look at pre and post winter pulse during years where pulses are present before and after winter

# for preK and postK look at:
# 2001 pulse 2 and 3
# 2007 both pulses (pulse 2 and 3?)
# 2009 pulse 2 and 3
# 2011 pulse 2, 3, and 4
# 2012 pulse 2 and 3
# 2013 pulse 2 and 3
# 2015 pulse 2 and 3
ggplot(alldata)+
  geom_boxplot(aes(x=factor(cohort),y=preK,fill=pulse))

K1<-cond2%>%
  filter(pulse==2 | pulse ==3)%>%
  filter(cohort==2001 | cohort == 2007 | cohort == 2009 | cohort == 2011 | cohort == 2012 |
           cohort == 2013 | cohort == 2015)%>%
  filter(month==10)%>%
  mutate(season="fall")%>%
  dplyr::select(fulton,cohort,pulse,season)
K2<-cond2%>%
  filter(pulse==2 | pulse ==3)%>%
  filter(cohort==2001 | cohort == 2007 | cohort == 2009 | cohort == 2011 | cohort == 2012 |
           cohort == 2013 | cohort == 2015)%>%
  filter(month==5)%>%
  mutate(season="spring")%>%
  dplyr::select(fulton,cohort,pulse,season)

K<-bind_rows(K1,K2)%>%
  left_join(dplyr::select(alldata, cohort, days_below_1),by=c("cohort"))%>%
  distinct()%>%
  filter(fulton<1.6)%>%
  mutate(pulse=as.factor(pulse))



Km5<-lmer(fulton~season+factor(pulse)+scale(days_below_1)+(1|cohort),data=K)

plot(Km5)
qqnorm(resid(Km5))
qqline(resid(Km5),col='red')
hist(resid(Km5))
Anova(Km5,type="III")
summary(Km5)


ggplot(K)+
  geom_boxplot(aes(x=factor(cohort),y=fulton,fill=season))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Fulton's K")+
  xlab("Cohort")+
  ylim(0.25,1.2)

# prewinter K only
Km6<-lmer(fulton~pulse+(1|cohort),data=filter(K,season=="fall"))

plot(Km6)
qqnorm(resid(Km6))
qqline(resid(Km6),col='red')
hist(resid(Km6))
Anova(Km6,type="III")
summary(Km6)
plot(allEffects(Km6))



pred.km6.1<-predict(km6.1)
k.prediction<-cbind(filter(alldata,!is.na(preK)),pred.km6.1)
alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=preK),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Prewinter Fulton's K")

km6.1<-glm(cbind(postCount,preCount)~preK,family=binomial(link="logit"),data=alldata)
plot(km6.1)
hist(resid(km6.1))
Anova(km6.1,type="III")
summary(km6.1)

cond2%>%
  filter(fulton<1.6 & pulse<5 & month == 10)%>%
  ggplot()+
  geom_boxplot(aes(y=fulton,x=factor(pulse)),size=1)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Pre-winter K")+xlab("Pulse")


# no effect

# postwinter K
Km7<-lmer(fulton~scale(days_below_1)+(1|cohort),data=filter(K,season=="spring"))

plot(Km7)
qqnorm(resid(Km7))
qqline(resid(Km7),col='red')
hist(resid(Km7))
Anova(Km7,type="III")
summary(Km7)

Km7<-lm(fulton~days_below_1,data=filter(K,season=="spring"))

plot(Km7)
qqnorm(resid(Km7))
qqline(resid(Km7),col='red')
hist(resid(Km7))
Anova(Km7,type="III")
summary(Km7)

Km7

alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_point(aes(y=postCount/preCount2,x=postK),size=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Postwinter Fulton's K")

K3<-cond2%>%
  filter(month==5)%>%
  dplyr::select(fulton,cohort,pulse)%>%
  left_join(dplyr::select(alldata, cohort, days_below_1),by=c("cohort"))%>%
  distinct()%>%
  filter(fulton<1.6)%>%
  mutate(pulse=as.factor(pulse))%>%
  filter(!is.na(cohort))


km8<-glm(cbind(postCount,preCount)~postK,data=alldata,family=binomial(link = "logit"))
plot(km8)
hist(resid(km8))
Anova(km8,type="III")
summary(km8)


pred.k1<-predict(km8)
k.prediction<-cbind(filter(alldata,!is.na(postK),pred.k1))
ggplot(data=cbind(K3,pred.k1))+
  geom_jitter(aes(y=fulton,x=days_below_1),size=2)+
  geom_smooth(aes(y=pred.k1,x=days_below_1),method="lm",colour='black')+
  theme_bw(base_rect_size = 2)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Post-winter K")+xlab("Days Below 1° C")+
  annotate("text",x=115,y=1.2,label="y = 0.609 + 0.0009x",size=5)

cond2%>%
  filter(fulton<1.6 & pulse<5 & month == 5)%>%
  ggplot()+
  geom_boxplot(aes(y=fulton,x=factor(pulse)),size=2)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  #scale_x_continuous(breaks=seq(1999,2016,by=1))+
  theme(axis.text.x = element_text(angle=40,hjust=1,size=10))+
  ylab("Pre-winter K")+xlab("pulse")
# abundance ratio~pulse+K+winter with select years

data.pulse<-alldata%>%
  filter(cohort==2001 | cohort == 2007 | cohort == 2009 | cohort == 2011 | cohort == 2012 |
         cohort == 2013 | cohort == 2015)%>%
  filter(pulse==2 | pulse ==3)

km8<-glmer(cbind(postCount,preCount)~pulse+preK+postK+scale(days_below_1)+(1|cohort),
           data=data.pulse,family=binomial)
plot(km8)
qqnorm(resid(km8))
qqline(resid(km8),col='red')
hist(resid(km8))
Anova(km8,type="III")
summary(km8)
plot(allEffects(km8))

# ---- Pulse settlement ----

pre.m11<-lmer(log(weight)~log(mmSL)+(1|cohort),
              data=filter(cond2,month==10 & age == 0 &pulse<5 & fulton<1.5))
plot(pre.m11)
Anova(pre.m11)
summary(pre.m11)

pred1<-predict(pre.m11)


ggplot(data=cbind(filter(cond2,month==10 & age == 0 & pulse <5 & fulton <1.5),
                  pred1),
       aes(x=log(mmSL),y=log(weight),color=factor(pulse)))+geom_point(alpha=0.8,size=2)+
  geom_smooth(aes(y=pred1),method="lm",colour='black')+
  annotate("text",x=4.2,y=-2.8,label="y = -11.71 + 2.966x",size=5)+
  xlab("log(SL) (mm)")+
  ylab("log(weight) (g)")+
  scale_color_discrete(name="Pulse")+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))


# --- The Closer Look ----
# determine years to look at

#select years above the mean age-0 abundance level
# 1998, 1999, 2007, 2012, 2013, 2014, 2015, 2016, 2017

high.year<-alldata%>%
  filter(cohort== 1998 | cohort == 1999 | cohort ==2007 | cohort >= 2013 & cohort & cohort <=2017)

summary(high.year)
# pulse 4 less present in these high abundance years

m4<-glmer(cbind(postCount,preCount)~pulse+preK+scale(days_below_1)+(1|cohort),
          data=filter(high.year,pulse!="4"),family=binomial)
plot(m4)
qqnorm(resid(m4))
qqline(resid(m4),col='red')
hist(resid(m4))
Anova(m4,type="III")
summary(m4)
plot(allEffects(m4))


model.df4 <- tidy(m4)%>%
  filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation
model.df4

e4<-model.df4 %>% 
  mutate(odds = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(m4)),  # Variance of each coefficient
         odds.se = sqrt(odds^2 * var.diag))%>%  # Odds-ratio adjusted
  mutate(p = odds/(1+odds))
e4
# pulse the only factor when looking at high abundance years

# low abundance years
# 2000 to 2005, 2006 to 2011
low.year<-alldata%>%
  filter(cohort>=2000 & cohort<=2005 | cohort >= 2006 & cohort & cohort <=2011)

# parse out smaller components

ggplot(low.year)+
  geom_point(aes(y=postCount/preCount, x=preK))

ggplot(low.year)+
  geom_point(aes(y=postCount/preCount, x = postK))

ggplot(low.year)+
  geom_point(aes(y=postCount/preCount, x = days_below_1))

low.year%>%
  filter(pulse!="4")%>%
  ggplot()+
  geom_boxplot(aes(y=postCount/preCount, x = pulse))+ylim(0,1)

# pulse 4 less present in these high abundance years

m5<-glmer(cbind(postCount,preCount)~pulse+preK+scale(days_below_1)+(1|cohort),
          data=filter(low.year,pulse!="4"),family=binomial)
plot(m5)
qqnorm(resid(m5))
qqline(resid(m5),col='red')
hist(resid(m5))
Anova(m5,type="III")
summary(m5)
plot(allEffects(m5))

#none of our factors explain low abundance years... BUT pulse 2 and pulse 4 show high "survival"

# ---- pulse structure and winter survival ----

pm1<-glmer(cbind(postCount,preCount)~pulse+(1|cohort),data=alldata,family=binomial(link = "logit"))
plot(pm1)
hist(resid(pm1))
Anova(pm1,type="III")
summary(pm1)

ggplot(alldata)+
  geom_boxplot(aes(y=postCount,x=factor(pulse)),size=1,outlier.shape = NA)+
  theme_bw(base_rect_size = 1)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Post-winter Abundance (catch/haul)")+xlab("Pulse")+
  ylim(0,6)

alldata%>%
  mutate(preCount2=preCount)%>%
  mutate(preCount2=replace(preCount2, preCount2==0,1))%>%
  ggplot()+
  geom_boxplot(aes(y=postCount/preCount2,x=pulse),size=1,outlier.shape = NA)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Prewinter to Postwinter \nAbundance")+xlab("Pulse")+
  ylim(0,1)

# settlement timing
ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.yday,colour=pulse,size=postCount/preCount))

ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.week,colour=pulse,size=postCount))

ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.week,colour=pulse,size=preCount))

#post winter abundance and settlement by pulse
ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.week,size=postCount))+
  facet_wrap(~pulse,nrow=3)+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Settlement Week")+xlab("Cohort")+
  labs(size="Post-winter Abundance")+
  theme(legend.position = "top")+
  theme(legend.title = element_text(size=14))+
  theme(legend.text = element_text(size=12))
  







m6<-glmer(cbind(postCount,preCount)~scale(settle.week)*pulse+(1|cohort),data=alldata,family = binomial(link="logit"))
plot(m6)
qqnorm(resid(m6))
qqline(resid(m6),col='red')
hist(resid(m6))
Anova(m6,type="III")
summary(m6)
plot(allEffects(m6),main="Settlement week x Pulse Effect Plot",xlab="Settlement Week",
     ylab="Prewinter to Postwinter \nAbundance")

plot(allEffects(m6),
        main=list(label="Settlement Week * Pulse Effect Plot",cex=1.5),
        ylab=list(label="Prewinter to Postwinter \nAbundance",cex=1.5),
        xlab=list(label="Settlement Week",cex=1.5))

model.df6 <- tidy(m6)%>%
  filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation

e6<-model.df6 %>% 
  mutate(odds = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(m6)),  # Variance of each coefficient
         odds.se = sqrt(odds^2 * var.diag))%>%  # Odds-ratio adjusted
  mutate(p = odds/(1+odds))
e6


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

#---- mortality leading to winter----

# ----- abundance pre and post winter ----


alldata%>%
  group_by(cohort)%>%
  summarise(Count=sum(preCount))%>%
  ggplot()+
  #geom_line(aes(x=cohort,y=Count))+
  geom_point(aes(x=cohort,y=Count),size=2)+
  geom_hline(yintercept = 37.6,linetype='dashed')+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Catch per haul")+xlab("Cohort")+
  ggtitle("Age-0 Abundance")+
  theme(plot.title = element_text(size=16,hjust=.5))+
  ylim(0,140)

alldata%>%
  group_by(cohort)%>%
  summarise(Count=sum(postCount))%>%
  ggplot()+
  #geom_line(aes(x=cohort,y=Count))+
  geom_point(aes(x=cohort,y=Count),size=2)+
  geom_hline(yintercept = 9.78,linetype='dashed')+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Catch per haul")+xlab("Cohort")+
  ggtitle("Age-1 Abundance")+
  theme(plot.title = element_text(size=16,hjust=.5))

ggplot(alldata)+
  geom_point(aes(x=cohort,y=preCount),size=2)+
  geom_hline(yintercept = 11.27,linetype='dashed')+
  facet_grid(cols=vars(pulse))+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Catch per haul")+xlab("Cohort")+
  ggtitle("Age-0 Abundance by Pulse")+
  theme(plot.title = element_text(size=16,hjust=.5))+
  theme(axis.text.x = element_text(angle=40,hjust=1))+
  ylim(0,140)


ggplot(alldata)+
  geom_point(aes(x=cohort,y=postCount),size=2)+
  geom_hline(yintercept = 2.93,linetype='dashed')+
  facet_grid(cols=vars(pulse))+
  theme_bw(base_rect_size = 1)+
  theme(panel.grid=element_blank())+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y =element_text(margin=margin(r=10)))+
  theme(axis.title.x= element_text(margin=margin(t=10)))+
  ylab("Catch per haul")+xlab("Cohort")+
  ggtitle("Age-1 Abundance by Pulse")+
  theme(plot.title = element_text(size=16,hjust=.5))+
  theme(axis.text.x = element_text(angle=40,hjust=1))+
  ylim(0,140)

