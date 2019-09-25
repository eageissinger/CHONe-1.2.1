# -----Newman Sound Site Analysis ------
#Purpose: to determine differences in age0 and age1 cod abundance based on 
#location wtihin Newman Sound

# ----Set working directory -----
setwd("C:/Users/user/Dropbox/Thesis/Empirical")

#-----load packages -----
library(MASS)
library(car)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gdata)
library(mfx)
library(Hmisc)

# ----- Source Custom Functions ------
source("HighStatLib.R")
source("MoreFunctions.R")

#------import dataset -----
mydata<-read.csv("newman_catch_working.csv",header=TRUE,sep=",")

#-----check data structure -----
summary(mydata)
head(mydata)
tail(mydata)
str(mydata)
names(mydata)
dim(mydata)
View(mydata)


#check that levels are correct
levels(mydata$site)

# ---- format date ------
# Get help from Danielle at RBar
#
#look up grep to determine patterns and then assign to categories
#
#
# ----- Add Eelgrass data to dataset -----

eg<-read.csv("eelgrass.csv",header=TRUE,sep=",",na.strings=c(""))

summary(eg)
str(eg)
dim(eg)
head(eg)
names(eg)
View(eg)

unique(eg$eelgrass)

#----Merge eelgrass and mydata-----
newdata<- merge(mydata,eg,by=c("site","year"))

summary(newdata)
str(newdata)
dim(newdata)
head(newdata)
names(newdata)
View(newdata)

# ----Age 0 Analysis -----

#----Summarise data ----


October<-newdata%>%
  filter(age==0)%>%
  filter(julian_date>=280)%>%
  filter(julian_date<=295)%>%
  select(year,date,julian_date,site,age,count,eelgrass)%>%
  as.data.frame()
View(October)


summary(October)
str(October)

# ---- Basic data visualization ------
#October age 0 cod count by site

baseline<-ggplot(October,aes(x=site,y=count))+
  geom_point()+
  theme_classic()+
  ylab("Abundance")+xlab("Site")
baseline

# -----Data analysis -----

# ---- Linear Model -----

m0<-lm(count~site,data=October)
m0
summary(m0)

# --- Linear model: Diagnostics ----
#Residuals vs fitted values
diag.m0<-data.frame(residuals=resid(m0),fitted=fitted(m0))

#Visualize fitted values vs residuals
ggplot(diag.m0)+
  geom_point(aes(x=fitted,y=residuals),size=3)+
  geom_hline(yintercept=0,linetype="dashed",col="blue")+
  theme_classic()+ylab("Residuals")+xlab("Fitted Values")

#not randomly distributed around zero

#check dispersion
dispersion(m0)

#really high, not good

plot(m0)


#1. want to see randomness
#2. Want to see points fall on the line with minimal "tails"
#3. want to see randomness
#4. Want to see randomness


#move on to next model

# ---- Poisson GLM -----
m1<-glm(count~site,data=October,family=poisson)
m1
summary(m1)

# ----- Poisson GLM: Diagnostics -----
#Residuals vs fitted values
diag.m1<-data.frame(residuals=resid(m1),fitted=fitted(m1))

#Visualize fitted values vs residuals
ggplot(diag.m1)+
  geom_point(aes(x=fitted,y=residuals),size=3)+
  geom_hline(yintercept=0,linetype="dashed",col="blue")+
  theme_classic()+ylab("Residuals")+xlab("Fitted Values")


#Check disperion
dispersion(m1,modeltype="poisson")

plot(m1)

#still no good

# ---- Negative Binomial GLM-----
m2<-glm.nb(count~site,data=October)
m2
summary(m2)

# ---- NB GLM: Diagnostics -----
#Residuals vs fitted values
diag.m2<-data.frame(residuals=resid(m2),fitted=fitted(m2))

#Visualize fitted values vs residuals
ggplot(diag.m2)+
  geom_point(aes(x=fitted,y=residuals),size=3)+
  geom_hline(yintercept=0,linetype="dashed",col="blue")+
  theme_classic()+ylab("Residuals")+xlab("Fitted Values")

#Looking much better

#Check dispersion
dispersion(m2)

#close to 1!!!!

plot(m2)


#probably as good as it will get. There are some outliers
#improve model by adding eelgrass as a factor



#-----NB GLM: Factor eelgrass

m3<-glm.nb(count~factor(site)*factor(eelgrass),data=October)
m3
summary(m3)

# ---- NB GLM: Diagnostics -----
#Residuals vs fitted values
diag.m3<-data.frame(residuals=resid(m3),fitted=fitted(m3))

#Visualize fitted values vs residuals
ggplot(diag.m3)+
  geom_point(aes(x=fitted,y=residuals),size=3)+
  geom_hline(yintercept=0,linetype="dashed",col="blue")+
  theme_classic()+ylab("Residuals")+xlab("Fitted Values")

#Looking much better

#Check dispersion
dispersion(m3)

#close to 1!!!!

plot(m3)

# ---- Incidence rate ratios for NB ----- 
negbinirr(m3,data=October,robust=FALSE,clustervar1=NULL,clustervar2=NULL,start=NULL,control=glm.control())

# ---- Marginal effects for NB -----
negbinmfx(m3,data=October,robust=FALSE,clustervar1=NULL,clustervar2=NULL,start=NULL,control=glm.control())
baseline

# ----- Age 1 analysis ------
#July week 1, 190-205...
july<-newdata%>%
  filter(age==1)%>%
  filter(julian_date>=190)%>%
  filter(julian_date<=200)%>%
  select(year,date,julian_date,site,age,count,eelgrass)%>%
  as.data.frame()
View(july)

# ----Age 1 Baseline -----
# ---- Basic data visualization ------
#july age 1 cod count by site

baseline1<-ggplot(july,aes(x=site,y=count))+
  geom_point()+
  theme_classic()+
  ylab("Abundance")+xlab("Site")
baseline1

# ---- Poisson GLM Age 1 -----
m4.1<-glm(count~site,data=july,family=poisson)
m4.1
summary(m4.1)

diag.m4.1<-data.frame(residuals=resid(m4.1),fitted=fitted(m4.1))


#Visualize fitted values vs residuals
ggplot(diag.m4.1)+
  geom_point(aes(x=fitted,y=residuals),size=3)+
  geom_hline(yintercept=0,linetype="dashed",col="blue")+
  theme_classic()+ylab("Residuals")+xlab("Fitted Values")

plot(m4.1)
dispersion(m4.1)


# ----- NB GLM age 1 -----

m4<-glm.nb(count~site*eelgrass,data=july)
m4
summary(m4)

# ----- NB GLM Diagnostics -----

diag.m4<-data.frame(residuals=resid(m4),fitted=fitted(m4))


#Visualize fitted values vs residuals
ggplot(diag.m4)+
  geom_point(aes(x=fitted,y=residuals),size=3)+
  geom_hline(yintercept=0,linetype="dashed",col="blue")+
  theme_classic()+ylab("Residuals")+xlab("Fitted Values")

plot(m4)
dispersion(m4)

# ---- NB without interaction -----
m5<-glm.nb(count~site+eelgrass,data=july)
m5
summary(m5)

#eelgrass not signifcant - take out of model

# ----- Diagnostics -----
diag.m5<-data.frame(residuals=resid(m5),fitted=fitted(m5))

#Visualize fitted values vs residuals
ggplot(diag.m5)+
  geom_point(aes(x=fitted,y=residuals),size=3)+
  geom_hline(yintercept=0,linetype="dashed",col="blue")+
  theme_classic()+ylab("Residuals")+xlab("Fitted Values")

#better without interaction
dispersion(m5)
plot(m5)


# ----- NG GLM no factor -----
m6<-glm.nb(count~site,data=july)
m6
summary(m6)

# ----- Diagnostics -----
diag.m6<-data.frame(residuals=resid(m6),fitted=fitted(m6))

#Visualize fitted values vs residuals
ggplot(diag.m6)+
  geom_point(aes(x=fitted,y=residuals),size=3)+
  geom_hline(yintercept=0,linetype="dashed",col="blue")+
  theme_classic()+ylab("Residuals")+xlab("Fitted Values")

#better without interaction
dispersion(m6)
plot(m6)

# ----- Incidence Rate Ratio age 1 -----
negbinirr(m6,data=july,robust=FALSE,clustervar1=NULL,clustervar2=NULL,start=NULL,control=glm.control())

# ---- Marginal effects for NB age 1 -----
negbinmfx(m6,data=july,robust=FALSE,clustervar1=NULL,clustervar2=NULL,start=NULL,control=glm.control())
