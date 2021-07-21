library(tidyverse)
library(car)

# Mortality for age 0 cod with revised pulse structure
test<-data.frame(y=c(12.7,1.5,2.55556,2.454545,0.833333),
                 x=c(245,259,273,288,302))

m1<-lm(y~x,data = test)
summary(m1)
Anova(m1)

test<-data.frame(y=c(2.541602,0.405465,0.93827,0.897942,-0.18232),
                 x=c(245,259,273,288,302))

m1<-lm(y~x,data = test)
summary(m1)
Anova(m1)

# save value to dataframe:
m1$coefficients[2]

setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/")

data<-read.csv("./data/data-working/newman-length.csv")

data%>%
  filter(Species=="AC" & Age ==0 & Year == 2020)%>%
ggplot()+
  geom_jitter(aes(x=Trip,y=mmSL,colour=factor(Pulse)))
data%>%
  filter(Species=="AC" & Age ==0 & Year == 2020)%>%
  group_by(Pulse,Trip)%>%
  summarise(n())

catch<-read.csv("./data/output/catch_haul.csv")%>%
  select(Year,Trip,Age,Julian.Date,extrap_1,extrap_2,extrap_3,extrap_4,extrap_5,extrap_6,extrap_unknown)%>%
  rename(pulse.1=extrap_1,pulse.2=extrap_2,pulse.3=extrap_3,pulse.4=extrap_4,pulse.5=extrap_5,pulse.6=extrap_6,pulse.unknown=extrap_unknown)

head(catch)


# create data frame with the correct trip ranges for each fully settled pulse
time.range<-data.frame(Year=c(1996,1996,1996,1998,1998,1998,1999,1999,1999,1999,2000,2000,2000,
                              2001,2001,2001,2001),
                       Pulse=c(1,2,3,1,2,3,1,2,3,4,1,2,3,
                               1,2,3,4),
                       Julian.Start=c(253,267,NA,216,245,319,250,272,311,NA,256,271,298,
                                      232,258,289,NA),
                       Julian.End=c(310,310,NA,286,286,333,298,325,325,NA,298,298,331,
                                    ))

#2000
catch%>%
  filter(Year==2020 & Age==0)
p1.2000<-catch%>%
  filter(Year==2020 & Age==0)%>%
  select(Year,Trip,Julian.Date,pulse.1)%>%
  filter(Julian.Date>=245 & Julian.Date<=317)
mp1.2000<-lm(pulse.1~Julian.Date,data = p1.2000)
summary(mp1.2000)
mp1.2000$coefficients[2]
