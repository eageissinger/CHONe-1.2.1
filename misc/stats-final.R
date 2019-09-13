setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

data<-read.csv("./data/data-working/age-1-mixture-dist.csv")

library(betareg)
library(tidyverse)
library(lmtest)
library(lubridate)
library(gridExtra)
library(grid)
library(glmmTMB)

summary(data)
names(data)
str(data)
min(data$pi)
max(data$pi)

# subset data to use only published data (1998-2004)
bd<-data%>%
  filter(year<2006)%>%
  filter(pi>0)%>%
  filter(pi<1.0)%>%
  slice(-37,-39,-36,-78,-41,-19,-42,-70,-74,-38)
summary(bd)
min(bd$pi)
max(bd$pi)
m1<-lm(pi~year*month*factor(dummy_pulse),data=bd)
plot(m1)
hist(resid(m1))
qqnorm(resid(m1))


m3<-betareg(pi~year*month*factor(dummy_pulse),data=bd)
plot(m3)
summary(m3)

exp(logLik(m1))
exp(logLik(m2))
exp(logLik(m3))


m1<-lm(pi~year+month+factor(dummy_pulse),data=data)
plot(m1)
hist(resid(m1))
qqnorm(resid(m1))
summary(m1)

par(mfrow=c(1,1))

m1<-glm(pi~year+month+factor(dummy_pulse),data=bd,family = gaussian(link = "identity"))
plot(m1)
hist(resid(m1))
qqnorm(resid(m1))
plot(m1,which=4)
summary(m1)
anova(m1)
Anova(m1,type="III")

m1.intercept<-glm(pi~1,data=bd,family=gaussian(link="identity"))
m1.yr<-glm(pi~1+year,data=bd,family=gaussian(link="identity"))
m1.month<-glm(pi~1+year+month,data=bd,family=gaussian(link="identity"))
m1.mu<-glm(pi~1+year+month+factor(dummy_pulse),data=bd,family=gaussian(link="identity"))

lrtest(m1.intercept,m1.yr,m1.month,m1.mu)

par(mfrow=c(2,2))

plot(x=fitted(m1),y=resid(m1),main=NULL,xlab="Fitted Values",ylab="Residuals")
hist(resid(m1),main=NULL,xlab="Residuals")
qqnorm(resid(m1),main=NULL)
qqline(resid(m1),col='red')
plot(m1,which=4,main=NULL)

# deviance residuals for beta
res.dev<-residuals(m1,type = "deviance")
sum(res.dev^2)
# sum(res.dev^2)/resdidual df
sum(res.dev^2)/84
summary(m1)

# Export, save plot with width 829, height 590

m2<-betareg(pi~year+month+factor(dummy_pulse),data=bd,link="logit")
plot(m2)
summary(m2)

exp(logLik(m1))
exp(logLik(m2))

plot(m2,which=1,type="pearson")

plot(m2,which=1:4,type="pearson")
plot(m2,which=5,type="deviance")

plot(fitted(m2),resid(m2))
coef(m2,model="precision")
par(mfrow=c(1,1), bg = rgb(232,245,252, maxColorValue = 255))
plot(m2,which=1,type="pearson")
plot(m2,which=4,type="pearson")
plot(m2,which=5,type="deviance")
plot(m2,which=2,type="pearson")


# Analysis of Deviance table
m2.intercept<-betareg(pi~1,data=bd,link="logit")
m2.yr<-betareg(pi~1+year,data=bd,link="logit")
m2.month<-betareg(pi~1+year+month,data=bd,link="logit")
m2.mu<-betareg(pi~1+year+month+factor(dummy_pulse),data=bd,link="logit")

m2ANODEV<-lrtest(m2.intercept,m2.yr,m2.month,m2.mu)
m2ANODEV
summary(m2)
# deviance residuals for beta
res.dev<-residuals(m2,type = "deviance")
sum(res.dev^2)
# sum(res.dev^2)/resdidual df
sum(res.dev^2)/77
summary(m1)
5.6749/77



# Mixed effect
library(car)
library(glmmTMB)
model.test<-glmmTMB(pi~month+factor(dummy_pulse)+(1|year), data = bd, family = beta_family(link = "logit"))
model.test
summary(model.test)

Anova.glmmTMB(model.test)

