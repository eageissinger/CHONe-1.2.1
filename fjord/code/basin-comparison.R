# cod distribution in fjord ecosystem

# ---- set working directory ----
setwd("C:/Users/Emilie/Dropbox/FHL/course-project/")

# ---- load packages ----
library(ggplot2)
library(dplyr)
library(lubridate)

# ---- load data ----
catch<-read.csv("./data/newman-catch-full.csv")

# ---- check data -----
names(catch)
summary(catch)
glimpse(catch)
dim(catch)
head(catch)
tail(catch)

# ---- format dates -----
catch$date<-ymd(paste(catch$year,catch$month,catch$day,sep="-"))

# ---- assign inner and outer sound -----
unique(catch$site)

# inner:
# NB, BC, WR, DS, CC, MI
# HC, MC, MS, SB, LSB

fjordy<-catch

fjordy$basin[fjordy$site=='NB' | fjordy$site=='BC' | fjordy$site=='WR' |
               fjordy$site=='DS' | fjordy$site=='CC' | fjordy$site== 'MI' | fjordy$site=='BB']='inner'
fjordy$basin[fjordy$site=='HC' | fjordy$site == 'MC' | fjordy$site == 'MS' |
               fjordy$site=='SB' | fjordy$site=='LSB']='mid'
fjordy[is.na(fjordy$basin),]

# get rid of LCE, SD1, SD2
fjordy<-fjordy%>%
  filter(site !='LCE')%>%
  filter(site != 'SD1')%>%
  filter(site != 'SD2')%>%
  filter(!is.na(year))%>%
  data.frame()
unique(fjordy$site)

# data is set, next step anlaysis

