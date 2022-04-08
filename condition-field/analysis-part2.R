# Condition summary
m0<-glm(fulton~factor(cohort),data=filter(K,season=="fall"),family=Gamma(link="log"))
plot(x=fitted(m0),y=resid(m0))
qqnorm(resid(m0))
qqline(resid(m0))
hist(resid(m0))
Anova(m0,type="III")
summary(m0)

  
m00<-glm(fulton~factor(cohort),data=filter(K,season=="spring"),family=Gamma(link="log"))
plot(x=fitted(m00),y=resid(m00))
qqnorm(resid(m00))
qqline(resid(m00))
hist(resid(m00))
Anova(m00,type="III")
summary(m00)

K%>%
  group_by(season)%>%
  summarise(mean(fulton),sd=sd(fulton),min(fulton),max(fulton))

library(DescTools)

m0<-glm(cbind(postCount,preCount)~pulse+preK+postK+days_below_1,
        data=filter(alldata,!is.na(postK)),family=binomial)
plot(x=fitted(m0),y=resid(m0))
qqnorm(resid(m0))
qqline(resid(m0))
hist(resid(m0))
Anova(m0,type="III")
summary(m0)




m1<-glm(cbind(postCount,preCount)~pulse+preK+poly(postK,2)+days_below_1,
          data=filter(alldata,!is.na(postK)),family=binomial)
plot(m1)
ggplot(m1)+geom_point(aes(x=fitted(m1),y=resid(m1)))
qqnorm(resid(m1))
qqline(resid(m1),col='red')
hist(resid(m1))
Anova(m1,type="III")
summary(m1)

# fit statistics
PseudoR2(m1) # Pseudo R2
(1-PseudoR2(m1))^(-21/2) # Liklihood Ratio
# RSME
data.predicted<-c(predict(m1,filter(alldata,!is.na(postK))))
data.predicted2<-as.data.frame(data.predicted)%>%
  mutate(data.predicted=exp(data.predicted)/(1+exp(data.predicted)))
RMSE(full$s,full$p)
plot(allEffects(m1))
allEffects(m1)%>%as.data.frame()

m1.pred<-predict(m1)

full<-alldata%>%filter(!is.na(postK))%>%
  dplyr::select(cohort,pulse,days_below_1,preCount,preK,postCount,postK)%>%
  bind_cols(m1.pred)%>%
  rename(prediction='...8')%>%
  mutate(odds=exp(prediction))%>%
  mutate(p=odds/(1+odds))%>%
  mutate(s=postCount/preCount)

ggplot(full)+
  geom_point(aes(x=pulse,y=s))+
  geom_boxplot(aes(x=pulse,y=p),colour='red')
ggplot(full)+
  geom_point(aes(x=preK,y=s))+
  geom_smooth(aes(x=preK,y=p),colour='red')
ggplot(full)+
  geom_point(aes(x=postK,y=s))+
  geom_point(aes(x=postK,y=p),colour='red')
ggplot(full)+
  geom_point(aes(x=days_below_1,y=s))+
  geom_point(aes(x=days_below_1,y=p),colour='red')

plot(allEffects(m1))



model.df1 <- tidy(m1)%>%
  filter(!is.na(std.error)) # Convert model to dataframe for easy manipulation
model.df1

e1<-model.df1 %>% 
  mutate(odds = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(m1)),  # Variance of each coefficient
         odds.se = sqrt(odds^2 * var.diag))%>%  # Odds-ratio adjusted
  mutate(p = odds/(1+odds))
e1%>%
  dplyr::select(term,estimate,std.error,odds)

# ----- Effect plots with estimates and/or real values -----

# output<-allEffects(m2)
# 
# pulse.output<-data.frame(pulse=output$pulse$x,
#                          estimate=exp(output$pulse$fit)/(1+exp(output$pulse$fit)),
#                          lcl=exp(output$pulse$lower)/(1+exp(output$pulse$lower)),
#                          ucl=exp(output$pulse$upper)/(1+exp(output$pulse$upper)),
#                          se=exp(output$pulse$se)/(1+exp(output$pulse$se)))
# 
# ggplot(pulse.output)+
#   geom_point(aes(x=pulse,y=estimate))+
#   geom_errorbar(aes(ymin=lcl,ymax=ucl,x=pulse))+
#   geom_jitter(data=full,aes(x=pulse,y=p,shape=pulse))+
#   theme_bw()
# 
# preK.output<-data.frame(preK=output$`poly(preK,2)`$x,
#                          estimate=exp(output$`poly(preK,2)`$fit)/(1+exp(output$`poly(preK,2)`$fit)),
#                          lcl=exp(output$`poly(preK,2)`$lower)/(1+exp(output$`poly(preK,2)`$lower)),
#                          ucl=exp(output$`poly(preK,2)`$upper)/(1+exp(output$`poly(preK,2)`$upper)))
# 
# ggplot(preK.output)+
#   geom_smooth(aes(x=preK,y=estimate),se=FALSE)
#   geom_point(aes(x=preK,y=estimate))
#   theme_bw()
# 
# ggplot(full)+
#   geom_smooth(aes(x=preK,y=p),formula=y~poly(x,2),method='glm')
# 
# ggplot(full)+
#   geom_smooth(aes(x=postK,y=p),method='glm',formula=y~poly(x,2))
#   
#   
# postK.output<-data.frame(postK=output$`poly(postK,2)`$x,
#                         estimate=exp(output$`poly(postK,2)`$fit)/(1+exp(output$`poly(postK,2)`$fit)),
#                         lcl=exp(output$`poly(postK,2)`$lower)/(1+exp(output$`poly(postK,2)`$lower)),
#                         ucl=exp(output$`poly(postK,2)`$upper)/(1+exp(output$`poly(postK,2)`$upper)))
# 
# ggplot(postK.output)+
#   geom_smooth(aes(x=postK,y=estimate),method='glm',formula = y~poly(x,2),se=FALSE)+
#   theme_bw()
# 
p1<-predictorEffect("pulse",m1)
plot(p1)
brief(p1$model.matrix)
p1a<-predictorEffect("pulse",m1,focal.levels = 3)
p1a
summary(p1a)
summary(p1)

as.data.frame(p1)
ggplot(as.data.frame(p1))+
  geom_point(aes(x=pulse,y=fit))+
  geom_errorbar(aes(x=pulse,ymin=lower,ymax=upper),width=.1)+
  theme_bw()+
  geom_jitter(data=full,aes(x=pulse,y=p),width=0.2)



p2<-predictorEffect("preK",m1)
as.data.frame(p2)

ggplot(as.data.frame(p2))+
  geom_line(aes(x=preK,y=fit),size=1)+
  geom_ribbon(aes(x=preK,ymin=lower,ymax=upper),alpha=0.2)+
  theme_bw()+
  geom_point(data=full,aes(x=preK,y=p))

p3<-predictorEffect("postK",m1)

ggplot(as.data.frame(p3))+
  geom_line(aes(x=postK,y=fit),size=1)+
  geom_ribbon(aes(x=postK,ymin=lower,ymax=upper),alpha=0.2)+
  theme_bw()+
  geom_point(data=full,aes(x=postK,y=p))

p4<-predictorEffect("days_below_1",m1)

ggplot(as.data.frame(p4))+
  geom_line(aes(x=days_below_1,y=fit),size=1)+
  geom_ribbon(aes(x=days_below_1,ymin=lower,ymax=upper),alpha=0.2)+
  theme_bw()+
  geom_point(data=full,aes(x=days_below_1,y=p))


# ---- follow up analyses -----
# Pulse (size) and PreK
ggplot(alldata)+
  geom_point(aes(x=preK,y=postK))
m3<-glmer(postK~preK+(1|cohort),data=alldata,family=Gamma(link = "log"))
plot(x=fitted(m3),y=resid(m3))
hist(resid(m3))
qqnorm(resid(m3))
qqline(resid(m3))
Anova(m3,type="III")
summary(m3)
PseudoR2(m3)

plot(allEffects(m3))
pred.m3<-predict(m3)

ggplot(alldata)+
  geom_point(aes(x=fallSL,y=preK))

m4<-glmer(preK~scale(fallSL)+(1|cohort),data=alldata,family=Gamma(link = "log"))
plot(y=resid(m4),x=fitted(m4))
qqnorm(resid(m4))
qqline(resid(m4))
hist(resid(m4))
Anova(m4,type="III")

m5<-glmer(preK~pulse+(1|cohort),data=alldata,family = Gamma(link = "log"))
plot(y=resid(m5),x=fitted(m5))
qqnorm(resid(m5))
qqline(resid(m5))
hist(resid(m5))
Anova(m5,type="III")

m6<-glmer(preK~scale(settle.week)+pulse+(1|cohort),data=alldata,family = Gamma(link = "log"))
plot(y=resid(m6),x=fitted(m6))
qqnorm(resid(m6))
qqline(resid(m6))
hist(resid(m6))
Anova(m6,type="III")

# pulse and postK
ggplot(alldata)+
  geom_boxplot(aes(x=pulse,y=postK))
ggplot(alldata)+
  geom_point(aes(x=fallSL,y=postK))

m7<-glmer(postK~pulse+(1|cohort),data=alldata,family=Gamma(link="log"))
plot(y=resid(m7),x=fitted(m7))
qqnorm(resid(m7))
qqline(resid(m7))
hist(resid(m7))
Anova(m7,type="III")
summary(m7)

m8<-glm(postK~fallSL,data=alldata,family=Gamma(link="log"))
plot(x=fitted(m8),y=resid(m8))
qqnorm(resid(m8))
qqline(resid(m8))
hist(resid(m8))
Anova(m8,type="III")

# postwinter K and winter duration

m9<-glm(postK~days_below_1,data=alldata,family=Gamma(link="log"))
plot(x=fitted(m9),y=resid(m9))
qqnorm(resid(m9))
qqline(resid(m9))
hist(resid(m9))
Anova(m9,type='III')
summary(m9)

# --- settlement timing -----
# look at how settlement time changes for each pulse over time in relation to size
ggplot(alldata)+
  geom_point(aes(x=settle.week,y=fallSL,size=postCount/preCount,colour=pulse))
#post winter abundance and settlement by pulse
ggplot(alldata)+
  geom_point(aes(x=cohort,y=settle.week,size=postCount))+
  facet_wrap(~pulse)+
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

m10<-lm(fallSL~settle.week+pulse,data=alldata)
plot(x=fitted(m10),y=resid(m10))
qqnorm(resid(m10))
qqline(resid(m10))
hist(resid(m10))
Anova(m10,type='III')

m11<-glmer(preK~scale(settle.week)+(1|cohort),data=alldata,family=Gamma(link="log"))
plot(x=fitted(m11),y=resid(m11))
qqnorm(resid(m11))
qqline(resid(m11))
hist(resid(m11))
Anova(m11,type='III')

m12<-glmer(postK~scale(settle.week)+pulse+(1|cohort),data=alldata,family=Gamma(link = "log"))
plot(x=fitted(m12),y=resid(m12))
qqnorm(resid(m12))
qqline(resid(m12))
hist(resid(m12))
Anova(m12,type='III')
summary(m12)
plot(allEffects(m12))

m12.pred<-predict(m12)

Ksettle<-alldata%>%
  filter(!is.na(postK))%>%
  bind_cols(m12.pred)%>%
  rename(estimate='...14')%>%
  mutate(p=exp(estimate))

post<-alldata%>%
  filter(!is.na(postK))%>%
  bind_cols(m12.pred)%>%
  rename(prediction='...14')%>%
  mutate(p=exp(prediction))

k1<-predictorEffect("settle.week",m12)

ggplot(as.data.frame(k1))+
  geom_line(aes(x=settle.week,y=fit,colour=pulse),size=1)+
  geom_ribbon(aes(x=settle.week,ymin=lower,ymax=upper),alpha=0.2)+
  theme_bw()+
  geom_point(data=post,aes(x=settle.week,y=p))

k2<-predictorEffect("pulse",m12)

ggplot(as.data.frame(k2))+
  geom_point(aes(x=pulse,y=fit),size=2)+
  geom_errorbar(aes(x=pulse,ymin=lower,ymax=upper),width=0.2)+
  theme_bw()+
  geom_jitter(data=post,aes(x=pulse,y=p),width = .2)

# does year influence prewinter condition?
m13<-glm(preK~cohort+pulse,data=alldata,family=Gamma(link="log"))
plot(x=fitted(m13),y=resid(m13))
qqnorm(resid(m13))
qqline(resid(m13))
hist(resid(m13))
Anova(m13, type="III")

# settlement time and winter survival
m14<-glm(cbind(postCount,preCount)~settle.week+pulse,data=alldata,family=binomial)
plot(x=fitted(m14),y=resid(m14))
qqnorm(resid(m14))
qqline(resid(m14))
hist(resid(m14))
Anova(m14)
summary(m14)
plot(allEffects(m14))
allEffects(m14)%>%as.data.frame()

pred.m14<-predict(m14)

settleS<-alldata%>%
  bind_cols(pred.m14)%>%
  rename(estimate='...14')%>%
  mutate(p=exp(estimate)/(1+exp(estimate)))
