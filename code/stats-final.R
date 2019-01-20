setwd("C:/Users/Emilie/Dropbox/Thesis/Research/CHONe-1.2.1/")

data<-read.csv("./data/data-working/age-1-mixture-dist.csv")

library(betareg)
library(dplyr)
library(lmtest)
library(stringr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)

summary(data)
names(data)
str(data)

data<-data%>%
  filter(mu<500)
m1<-lm(pi~year*month*factor(dummy_pulse),data=data)
plot(m1)
hist(resid(m1))
qqnorm(resid(m1))


m2<-glm(pi~year*month*factor(dummy_pulse),data=data,family=Gamma(link = "log"))
plot(m2)
hist(resid(m2))
qqnorm(resid(m2))

m3<-betareg(pi~year*month*factor(dummy_pulse),data=data)
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

m1<-glm(pi~year+month+factor(dummy_pulse),data=data,family = gaussian(link = "identity"))
plot(m1)
hist(resid(m1))
qqnorm(resid(m1))
plot(m1,which=4)
summary(m1)
anova(m1)

m1.intercept<-glm(pi~1,data=data,family=gaussian(link="identity"))
m1.yr<-glm(pi~1+year,data=data,family=gaussian(link="identity"))
m1.month<-glm(pi~1+year+month,data=data,family=gaussian(link="identity"))
m1.mu<-glm(pi~1+year+month+factor(dummy_pulse),data=data,family=gaussian(link="identity"))

lrtest(m1.intercept,m1.yr,m1.month,m1.mu)

par(mfrow=c(2,2))

plot(x=fitted(m1),y=resid(m1),main=NULL,xlab="Fitted Values",ylab="Residuals")
hist(resid(m1),main=NULL,xlab="Residuals")
qqnorm(resid(m1),main=NULL)
qqline(resid(m1),col='red')
plot(m1,which=4,main=NULL)

# Export, save plot with width 829, height 590


m2<-betareg(pi~year+month+factor(dummy_pulse),data=data,link="logit")
plot(m2)
summary(m2)

exp(logLik(m1))
exp(logLik(m2))

plot(m2,which=1,type="pearson")

plot(m2,which=1:4,type="pearson")
plot(m2,which=5,type="deviance")

plot(fitted(m2),resid(m2))
coef(m2,model="precision")

plot(m2,which=1,type="pearson")
plot(m2,which=4,type="pearson")
plot(m2,which=5,type="deviance")
plot(m2,which=2,type="pearson")


# Analysis of Deviance table
m2.intercept<-betareg(pi~1,data=data,link="logit")
m2.yr<-betareg(pi~1+year,data=data,link="logit")
m2.month<-betareg(pi~1+year+month,data=data,link="logit")
m2.mu<-betareg(pi~1+year+month+factor(dummy_pulse),data=data,link="logit")

m2ANODEV<-lrtest(m2.intercept,m2.yr,m2.month,m2.mu)
m2ANODEV

# deviance residuals for beta
res.dev<-residuals(m2,type = "deviance")
sum(res.dev^2)
# sum(res.dev^2)/resdidual df
sum(res.dev^2)/254
summary(m1)
