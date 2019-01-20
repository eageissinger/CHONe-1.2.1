# ---- Species Presence Absence in Newmand Sound, NL -----

# ---- set working directory -----
setwd("C:/Users/eageissinger/Documents/CHONe-1.2.1/")

# --- load pacakges ----
library(lubridate)
library(car)
library(ggplot2)
library(MASS)
library(dplyr)
# --- Load data -----
species<-read.csv("./data/data-working/NewmanSpeciesData.csv",header=TRUE,sep=",")

# -----Check dtaa structure -----
summary(species)
str(species)
dim(species)
head(species)
names(species)
View(species)

names(species)<-c("year","date","julian_date","site","species","age","count")

#-----Merge eelgrass data with species data -----
eg<-read.csv("./data/data-working/eelgrass.csv",header=TRUE,sep=",",na.strings=c(""))

summary(eg)
str(eg)
dim(eg)
head(eg)
names(eg)
View(eg)

unique(eg$eelgrass)

#----Merge eelgrass and mydata-----
all<-full_join(species,eg)

summary(all)
str(all)
dim(all)
head(all)
names(all)
View(all)

# ----Presence Absense formatting ------

# Create eelgrass dataframe 
eel<-all%>%
  filter(eelgrass=="Yes")

#add presence absence column

eel$presence[eel$count>0]<-1
eel$presence[eel$count==0]<-0

#create rock dataframe
noneel<-all%>%
  filter(eelgrass=="No")

#add presence absence column
rock$presence[rock$count>0]<-1
rock$presence[rock$count==0]<-0

# group by year
yeareel<-eel%>%
  group_by(year,species)%>%
  summarise(totalcount=sum(count))
yeareel$presence<-NA
yeareel$presence[yeareel$totalcount>0]<-1
yeareel$presence[yeareel$totalcount==0]<-0

# group by site
eel.site<-eel%>%
  group_by(site,species)%>%
  summarise(totalcount=sum(count))
eel.site$presence<-NA
eel.site$presence[eel.site$totalcount>0]<-1
eel.site$presence[eel.site$totalcount==0]<-0

eel.site2<-eel.site%>%
  filter(presence!=0)%>%
  group_by(site)%>%
  summarise(n=n())

# number of species in eelgrass vs non eelgrass
num.site<-all%>%
  group_by(species,site,eelgrass)%>%
  summarise(totalcount=sum(count))
num.site$presence<-NA
num.site$presence[num.site$totalcount>0]<-1
num.site$presence[num.site$totalcount==0]<-0

num.site<-num.site%>%
  filter(presence!=0)%>%
  group_by(site,eelgrass)%>%
  summarise(n=n())%>%
  as.data.frame()

m1<-lm(n~eelgrass,data=num.site)
plot(m1)
summary(m1)
anova(m1)

#Compile data into overall sum across years
totaleel<-eel%>%
  group_by(species)%>%
  summarise(totalcount=sum(count))%>%
  ungroup()%>%
  as.data.frame()
#add presence absnece column
totaleel$presence[totaleel$totalcount>0]<-1
totaleel$presence[totaleel$totalcount==0]<-0

#copile rocky substrate data
totalrock<-rock%>%
  group_by(species)%>%
  summarise(totalcount=sum(count))%>%
  ungroup()%>%
  as.data.frame()

#add presence absnece column
totalrock$presence[totalrock$totalcount>0]<-1
totalrock$presence[totalrock$totalcount==0]<-0

#Save files
write.csv(totaleel,file="./data/data-working/eelgrassPA.csv",row.names=FALSE)
write.csv(totalrock,file="rockPA.csv",row.names=FALSE)

# ----- additional comparisons ----
# NB
all%>%
  filter(site=="NB")%>%
  filter(species=="AC")%>%
  mutate(eelgrass=ifelse(eelgrass=="No",0,1))%>% #convert to binomial response
  ggplot()+geom_point(aes(x=year,y=count,colour=factor(eelgrass)))+
  ggtitle("Atlantic cod count at Newbridge: Before and After Eelgrass")

all%>%
  filter(site=="NB")%>%
  ggplot()+geom_point(aes(x=year,y=count,colour=eelgrass))+
  ggtitle("All Species count at Newbridge: Before and After Eelgrass")
#m1<-lm(count~factor(eelgrass)*year,data=nb)
#summary(m1)
#plot(m1)
#anova(m1)

ms<-all%>%
  filter(site=="MS")%>%
  filter(species=="AC")%>%
  ggplot()+geom_point(aes(x=year,y=count,colour=eelgrass))+
  ggtitle("Atlantic cod count at Mount Stamford: Before and After Eelgrass")
#m2<-lm(count~factor(eelgrass)*year,data=ms)
#plot(m2)
#summary(m2)
#Anova(m2,type = "II")

all%>%
  filter(site=="MS")%>%
  ggplot()+geom_point(aes(x=year,y=count,colour=eelgrass))+
  ggtitle("All species count at Mount Stamford: Before and After Eelgrass")

# 1995-2003
early<-all%>%
  filter(species=="AC")%>%
  filter(year<2004)%>%
  filter(site=="NB" | site=="MS" | site =="WR"| site=="HC")%>%
  mutate(eelgrass=ifelse(eelgrass=="No",0,1)) #convert to binomial response

all%>%
  filter(species=="AC")%>%
  ggplot()+geom_jitter(aes(x=year,y=count,colour=eelgrass,shape=site))

ggplot(data=early)+geom_jitter(aes(x=year,y=count,colour=factor(eelgrass)))

early%>%
  filter(eelgrass==1)%>%ggplot()+geom_jitter(aes(x=year,y=count))+
  facet_grid(site~.)

all%>%
  filter(year<2004)%>%
  ggplot()+geom_jitter(aes(x=year,y=count))+
  facet_grid(eelgrass~.)

all%>%
  filter(year<2004)%>%
  filter(species=="AC")%>%
  ggplot()+geom_jitter(aes(x=year,y=count))+
  facet_grid(eelgrass~.)
