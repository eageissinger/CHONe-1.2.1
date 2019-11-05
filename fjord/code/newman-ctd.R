# ----- Newman Sound CTD ------
# purpose: 1) explore CTD data for Fjord project

# ----- set working directory -----
setwd("C:/Users/user/Documents/FHL/course-project/")

# ----- load data -----
ctd<-read.csv("./data/newman_ctd_all.csv")

# ----- load packages -----
library(dplyr)
library(lubridate)
library(maptools)
library(raster)
library(rgdal)
library(maps)
library(mapdata)
library(ggmap)
library(lattice)
library(ggplot2)
library(ggsn)
library(gridExtra)
library(grid)
library(lattice)

# ----- explore data ----
summary(ctd)
glimpse(ctd)
dim(ctd)
head(ctd)
tail(ctd)
names(ctd)

# ---- format dates (and time) ----
ctd$date<-ymd(paste(ctd$year,ctd$month,ctd$day,sep="-"))
# not going to deal with time right now....


#Create Newfoundland map
nl<-readOGR(dsn="shapefiles",layer="NFLD")
nl@data$id=rownames(nl@data)
nl.points=fortify(nl,region="id")
nl.df=join(nl.points,nl@data,by="id")

newmanmap<-ggplot(nl.df, aes(long,lat))+
  geom_polygon()+
  geom_path(color="white")+
  coord_equal()+
  theme_classic()+
  xlab("Longitude")+
  ylab("Lattitude")+
  ggtitle("Newman Sound")+
  ylim(48.4,48.8)+
  xlim(-54,-53.5)
newmanmap
# ---- plot stations -----
stations<-distinct(ctd,lat_dec,long_dec)

min(stations$lat_dec)
max(stations$lat_dec)
min(stations$long_dec)
max(stations$long_dec)

#Google map
newmanmap+
  geom_point(data=stations,aes(x=long_dec,y=lat_dec),colour='red',size=.5)

# get rid of outlier casts
# cast 23 and 20
ctd<-ctd%>%filter(cast!=23 & cast!=20)
stations<-distinct(ctd,lat_dec,long_dec)

newmanmap+
  geom_point(data=stations,aes(x=long_dec,y=lat_dec),colour='red',size=.5)

# ---- assign location -----
ctd%>%
  filter(long_dec<(-53.91))%>%
  distinct(lat_dec,long_dec)# inner basin

ctd%>%
  filter(long_dec>(-53.91) & long_dec < (-53.875))%>%
  distinct(lat_dec,long_dec) # middle basin

ctd%>%
  filter(long_dec>(-53.875) & long_dec<(-53.8))%>%
  distinct(lat_dec,long_dec) # slope

ctd%>%
  filter(long_dec>(-53.79999) & long_dec< (-53.65))%>%
  distinct(lat_dec,long_dec) # outer basin

ctd%>%
  filter(long_dec>(-53.65))%>%
  distinct(lat_dec,long_dec) # shelf

ctd$location[ctd$long_dec<(-53.91)]='inner_basin'
ctd$location[ctd$long_dec>(-53.91) & ctd$long_dec< (-53.875)]='middle_basin'
ctd$location[ctd$long_dec>(-53.875) & ctd$long_dec<(-53.8)]='slope'
ctd$location[ctd$long_dec>=(-53.8) & ctd$long_dec< (-53.65)]='outer_basin'
ctd$location[ctd$long_dec>(-53.65)]='shelf'

unique(ctd$location)


ctd<-ctd%>%
  filter(pres>0)
# ---- format data for ODV ----
# Column names:
# CRUISE, Station ID, STATION, LAT, LONG, YEAR,DATE (%m/%d/%Y),
# MONTH, TIME (h:m), SALINITY, TEMP_C, DEPTH_m, DENSITY, CONDUCTIVITY,
# FLORESCENCE

names(ctd)

ctd2<-ctd%>%
  rename(STATION=cast)%>%
  rename(LAT=lat_dec)%>%
  rename(LONG=long_dec)%>%
  rename(YEAR=year)%>%
  rename(MONTH=month)%>%
  rename(SALINITY=sal)%>%
  rename(TEMP_C=temp)%>%
  rename(DEPTH_m=pres)%>%
  rename(DENSITY=sigt)%>%
  rename(CONDUCTIVITY=cond)%>%
  rename(FLUORESCENCE=flor)%>%
  mutate(CRUISE="DFO")%>%
  mutate(Station_ID = c(1:52246))

ctd2$TIME<-paste(ctd2$hour,":",ctd2$minute)
ctd2$TIME<-gsub(" ", "", ctd2$TIME) # remove white space

ctd2$DATE<-paste(ctd2$MONTH,"/",ctd2$day,"/",ctd2$YEAR)
ctd2$DATE<-gsub(" ","",ctd2$DATE) # remove white space

names(ctd2)

ctd2<-select(ctd2, -scan,-day,-hour,-minute,-date)

ctd3<-ctd2[c('CRUISE','Station_ID','STATION','LAT','LONG','YEAR','DATE',
       'MONTH','TIME','SALINITY','TEMP_C','DEPTH_m','DENSITY',
       'CONDUCTIVITY','FLUORESCENCE')]

write.csv(ctd3,"./data/newman_odv.csv",row.names = FALSE)


# ---- plot salinity by year and season ----

summary(ctd)
glimpse(ctd)
ctd$cast<-as.character(ctd$cast)

# ---- 1998 -----

ctd%>%
  filter(year==1998)%>%
  distinct(date) # november casts

ctd%>%
  filter(year==1998)%>%
  distinct(cast)

# plot 1998 locations

newmanmap+
  geom_point(data=filter(ctd,year==1998),aes(x=long_dec,y=lat_dec,color=cast))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==1998)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==1998)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

# ---- 1999 -----

ctd%>%
  filter(year==1999)%>%
  distinct(date) # june and November/December 

ctd%>%
  filter(year==1999)%>%
  distinct(cast) 

# plot 1999 locations
newmanmap+
  geom_point(data=filter(ctd,year==1999),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

# look at winter only (summer is only in outer basin)

ctd%>%
  filter(year==1999 & month >10)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==1999 & month >10)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==1999 & month > 10)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

# ---- 2000 -----

ctd%>%
  filter(year==2000)%>%
  distinct(date) # june casts

ctd%>%
  filter(year==2000)%>%
  distinct(cast)

# skip 2000, not enough data

# ---- 2001 -----

ctd%>%
  filter(year==2001)%>%
  distinct(date) # november casts

ctd%>%
  filter(year==2001)%>%
  distinct(cast)

# plot 2001 locations

newmanmap+
  geom_point(data=filter(ctd,year==2001),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==2001 & month ==11)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2001 & month ==11)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2001 & month ==6)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

# ---- 2002 -----

ctd%>%
  filter(year==2002)%>%
  distinct(date) # one june, one december

ctd%>%
  filter(year==2002)%>%
  distinct(cast)

# plot 2002 locations

newmanmap+
  geom_point(data=filter(ctd,year==2002),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==2002 & month==12)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2002 & month ==12)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

# ---- 2003 -----

ctd%>%
  filter(year==2003)%>%
  distinct(date) # winter

ctd%>%
  filter(year==2003)%>%
  distinct(cast)

# plot 2003 locations
newmanmap+
  geom_point(data=filter(ctd,year==2003),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==2003 & month >6)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2003 & month >6)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2003 & month >6)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

# ---- 2004 -----

ctd%>%
  filter(year==2004)%>%
  distinct(date) # spring/summer and november

ctd%>%
  filter(year==2004)%>%
  distinct(cast)

# plot 2004 locations

newmanmap+
  geom_point(data=filter(ctd,year==2004),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==2004 & month ==5)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2004 & month ==5)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2004 & month ==5)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2004 & month ==7)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2004 & month ==7)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2004 & month ==7)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2004 & month ==11)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2004 & month ==11)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2004 & month ==11)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()


# ---- 2005 -----

ctd%>%
  filter(year==2005)%>%
  distinct(date) # november

ctd%>%
  filter(year==2005)%>%
  distinct(cast)

# plot 2002 locations

newmanmap+
  geom_point(data=filter(ctd,year==2005),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==2005)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2005)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2005)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

# ---- 2006 -----

ctd%>%
  filter(year==2006)%>%
  distinct(date) # may

ctd%>%
  filter(year==2006)%>%
  distinct(cast)

# plot 2006 locations

newmanmap+
  geom_point(data=filter(ctd,year==2006),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==2006)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2006)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2006)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

# ---- 2007 -----

ctd%>%
  filter(year==2007)%>%
  distinct(date) # may and november

ctd%>%
  filter(year==2007)%>%
  distinct(cast)

# plot 2007 locations

newmanmap+
  geom_point(data=filter(ctd,year==2007),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==2007 & month ==5)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2007 & month ==5)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2007 & month ==5)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2007 & month ==11)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2007 & month ==11)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2007 & month ==11)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

# ---- 2008 -----

ctd%>%
  filter(year==2008)%>%
  distinct(date) # november

ctd%>%
  filter(year==2008)%>%
  distinct(cast)

# plot 2008 locations

ggmap(newman)+
  geom_point(data=filter(ctd,year==2008),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==2008)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2008)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2008)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

# ---- 2009 -----

ctd%>%
  filter(year==2009)%>%
  distinct(date) # november

ctd%>%
  filter(year==2009)%>%
  distinct(cast)

# plot 2009 locations

ggmap(newman)+
  geom_point(data=filter(ctd,year==2009),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==2009)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2009)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2009)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

# ---- 2010 -----

ctd%>%
  filter(year==2010)%>%
  distinct(date) # november

# no 2010 data

# ---- 2011 -----

ctd%>%
  filter(year==2011)%>%
  distinct(date) # october

ctd%>%
  filter(year==2011)%>%
  distinct(cast)

# plot 2011 location

ggmap(newman)+
  geom_point(data=filter(ctd,year==2011),aes(x=long_dec,y=lat_dec,color=cast,shape=as.character(month)))+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

ctd%>%
  filter(year==2011)%>%
  ggplot(aes(x=long_dec,y=pres,color=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2011)%>%
  ggplot(aes(x=long_dec,y=pres,color=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()

ctd%>%
  filter(year==2011)%>%
  ggplot(aes(x=long_dec,y=pres,color=flor))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()



# ----- salinity plots -----
ctd%>%
  filter(location=="inner_basin")%>%
  ggplot(aes(x=sal,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("salinity")+ylab("depth (m)")+
  ggtitle("Inner Basin")+
  theme_bw()

ctd%>%
  filter(location=="middle_basin")%>%
  ggplot(aes(x=sal,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("salinity")+ylab("depth (m)")+
  ggtitle("Middle Basin")+
  theme_bw()

ctd%>%
  filter(location=="slope")%>%
  ggplot(aes(x=sal,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("salinity")+ylab("depth (m)")+
  ggtitle("Slope")+
  theme_bw()

ctd%>%
  filter(location=="outer_basin")%>%
  ggplot(aes(x=sal,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("salinity")+ylab("depth (m)")+
  ggtitle("Outer Basin")+
  theme_bw()

ctd%>%
  filter(location=="shelf")%>%
  ggplot(aes(x=sal,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("salinity")+ylab("depth (m)")+
  ggtitle("Shelf")+
  theme_bw()

# ----- Temperature plots -----
ctd%>%
  filter(location=="inner_basin")%>%
  ggplot(aes(x=temp,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("Temperature")+ylab("depth (m)")+
  ggtitle("Inner Basin")+
  theme_bw()

ctd%>%
  filter(location=="middle_basin")%>%
  ggplot(aes(x=temp,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("Temperature")+ylab("depth (m)")+
  ggtitle("Middle Basin")+
  theme_bw()

ctd%>%
  filter(location=="slope")%>%
  ggplot(aes(x=temp,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("Temperature")+ylab("depth (m)")+
  ggtitle("Slope")+
  theme_bw()

ctd%>%
  filter(location=="outer_basin")%>%
  ggplot(aes(x=temp,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("Temperature")+ylab("depth (m)")+
  ggtitle("Outer Basin")+
  theme_bw()

ctd%>%
  filter(location=="shelf")%>%
  ggplot(aes(x=temp,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("temperature")+ylab("depth (m)")+
  ggtitle("Shelf")+
  theme_bw()

# ---- Florescense -----
ctd%>%
  filter(location=="inner_basin")%>%
  ggplot(aes(x=flor,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("Fluorescence")+ylab("depth (m)")+
  ggtitle("Inner Basin")+
  theme_bw()

ctd%>%
  filter(location=="middle_basin")%>%
  ggplot(aes(x=flor,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("Fluorescence")+ylab("depth (m)")+
  ggtitle("Middle Basin")+
  theme_bw()

ctd%>%
  filter(location=="slope")%>%
  ggplot(aes(x=flor,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("Fluorescence")+ylab("depth (m)")+
  ggtitle("Slope")+
  theme_bw()

ctd%>%
  filter(location=="outer_basin")%>%
  ggplot(aes(x=flor,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("Fluorescence")+ylab("depth (m)")+
  ggtitle("Outer Basin")+
  theme_bw()

ctd%>%
  filter(location=="shelf")%>%
  ggplot(aes(x=flor,y=pres,colour=as.character(month)))+geom_point()+
  scale_y_reverse()+
  xlab("Fluorescence")+ylab("depth (m)")+
  ggtitle("Shelf")+
  theme_bw()

flordata<-ctd%>%
  filter(month==5 | month ==11)%>%
  group_by(location, year, month)%>%
  summarise(max(flor))

# 2005 November bloom
# 2006 largest May bloom




# ----- Averaged plots -----

shelf_sal_may<-ctd%>%
  filter(location=='shelf' & month == 5)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("shelf")+
  xlim(320,0)+ylim(29,34)

shelf_temp_may<-ctd%>%
  filter(location=='shelf' & month ==5)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,6)

shelf_flor_may<-ctd%>%
  filter(location=='shelf' & month == 5)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

ob_sal_may<-ctd%>%
  filter(location=='outer_basin' & month == 5)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("outer basin")+
  xlim(320,0)+ylim(29,34)
ob_temp_may<-ctd%>%
  filter(location=='outer_basin' & month ==5)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,6)
ob_flor_may<-ctd%>%
  filter(location=='outer_basin' & month == 5)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

mb_sal_may<-ctd%>%
  filter(location=='slope' & month==5)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("Slope")+
  xlim(320,0)+ylim(29,34)
mb_temp_may<-ctd%>%
  filter(location=='slope' & month ==5)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,6)
mb_flor_may<-ctd%>%
  filter(location=='slope' & month == 5)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

sill_sal_may<-ctd%>%
  filter(location=='middle_basin' & month == 5)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("Middle Basin")+
  xlim(320,0)+ylim(29,34)
sill_temp_may<-ctd%>%
  filter(location=='middle_basin' & month ==5)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,6)
sill_for_may<-ctd%>%
  filter(location=='middle_basin' & month ==5)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

ib_sal_may<-ctd%>%
  filter(location=='inner_basin' & month ==5)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("inner basin")+
  xlim(320,0)+ylim(29,34)
ib_temp_may<-ctd%>%
  filter(location=='inner_basin' & month ==5)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,6)
ib_flor_may<-ctd%>%
  filter(location=='inner_basin' & month == 5)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

shelf_sal_july<-ctd%>%
  filter(location=='shelf' & month == 7)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("shelf")+
  xlim(320,0)+ylim(29,34)
shelf_temp_july<-ctd%>%
  filter(location=='shelf' & month == 7)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,12.5)
shelf_flor_july<-ctd%>%
  filter(location=='shelf' & month == 7)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

ob_sal_july<-ctd%>%
  filter(location=='outer_basin' & month == 7)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("outer basin")+
  xlim(320,0)+ylim(29,34)
ob_temp_july<-ctd%>%
  filter(location=='outer_basin' & month ==7)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,12.5)
ob_flor_july<-ctd%>%
  filter(location=='outer_basin' & month == 7)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

mb_sal_july<-ctd%>%
  filter(location=='slope' & month==7)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("slope")+
  xlim(320,0)+ylim(29,34)
mb_temp_july<-ctd%>%
  filter(location=='slope' & month ==7)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,12.5)
mb_flor_july<-ctd%>%
  filter(location=='slope' & month == 7)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

sill_sal_july<-ctd%>%
  filter(location=='middle_basin' & month == 7)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("middle basin")+
  xlim(320,0)+ylim(29,34)
sill_temp_july<-ctd%>%
  filter(location=='middle_basin' & month ==7)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,12.5)
sill_for_july<-ctd%>%
  filter(location=='middle_basin' & month ==7)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

ib_sal_july<-ctd%>%
  filter(location=='inner_basin' & month ==7)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("inner basin")+
  xlim(320,0)+ylim(29,34)
ib_temp_july<-ctd%>%
  filter(location=='inner_basin' & month ==7)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,12.5)
ib_flor_july<-ctd%>%
  filter(location=='inner_basin' & month == 7)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

shelf_sal_nov<-ctd%>%
  filter(location=='shelf' & month == 11)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("shelf")+
  xlim(320,0)+ylim(29,34)
shelf_temp_nov<-ctd%>%
  filter(location=='shelf' & month == 11)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,12.5)
shelf_flor_nov<-ctd%>%
  filter(location=='shelf' & month == 11)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

ob_sal_nov<-ctd%>%
  filter(location=='outer_basin' & month == 11)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("outer basin")+
  xlim(320,0)+ylim(29,34)
ob_temp_nov<-ctd%>%
  filter(location=='outer_basin' & month ==11)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,12.5)
ob_flor_nov<-ctd%>%
  filter(location=='outer_basin' & month == 11)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

mb_sal_nov<-ctd%>%
  filter(location=='slope' & month==11)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("slope")+
  xlim(320,0)+ylim(29,34)
mb_temp_nov<-ctd%>%
  filter(location=='slope' & month ==11)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,12.5)
mb_flor_nov<-ctd%>%
  filter(location=='slope' & month == 11)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

sill_sal_nov<-ctd%>%
  filter(location=='middle_basin' & month == 11)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("middle basin")+
  xlim(320,0)+ylim(29,34)
sill_temp_nov<-ctd%>%
  filter(location=='middle_basin' & month ==11)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,12.5)
sill_for_nov<-ctd%>%
  filter(location=='middle_basin' & month ==11)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

ib_sal_nov<-ctd%>%
  filter(location=='inner_basin' & month ==11)%>%
  ggplot(aes(x=pres,y=sal))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  ggtitle("inner basin")+
  xlim(320,0)+ylim(29,34)
ib_temp_nov<-ctd%>%
  filter(location=='inner_basin' & month ==11)%>%
  ggplot(aes(x=pres,y=temp))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(-1.75,12.5)
ib_flor_nov<-ctd%>%
  filter(location=='inner_basin' & month == 11)%>%
  ggplot(aes(x=pres,y=flor))+geom_smooth()+
  scale_x_reverse()+coord_flip()+
  xlim(320,0)+ylim(0,3)

may<-grid.arrange(shelf_sal_may,ob_sal_may,mb_sal_may,sill_sal_may,ib_sal_may,
                  shelf_temp_may,ob_temp_may,mb_temp_may,sill_temp_may,ib_temp_may,
                  shelf_flor_may,ob_flor_may,mb_flor_may,sill_for_may,ib_flor_may,
                  ncol=5,top="May")

july<-grid.arrange(shelf_sal_july,ob_sal_july,mb_sal_july,sill_sal_july,ib_sal_july,
                   shelf_temp_july,ob_temp_july,mb_temp_july,sill_temp_july,ib_temp_july,
                   shelf_flor_july,ob_flor_july,mb_flor_july,sill_for_july,ib_flor_july,
                   ncol=5,top="July")

november<-grid.arrange(shelf_sal_nov,ob_sal_nov,mb_sal_nov,sill_sal_nov,ib_sal_nov,
                   shelf_temp_nov,ob_temp_nov,mb_temp_nov,sill_temp_nov,ib_temp_nov,
                   shelf_flor_nov,ob_flor_nov,mb_flor_nov,sill_for_nov,ib_flor_nov,
                   ncol=5,top="November")

ctd%>%
  filter(month==5)%>%
  summarise(max(pres),min(pres))


# ---- temporal cycle plots -----

# ---- fluorescence -----
ctd%>%
  filter(location=='shelf' & pres<=60.0 & !is.na(flor))%>%
  ggplot(aes(x=date,y=pres,colour=flor))+geom_point(size=6)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_classic()+ggtitle("Shelf")+
  scale_x_date(date_breaks="2 month",date_labels="%b-%y")+
  theme(axis.text.x=element_text(angle=40,vjust=1.3,hjust=1.3))+
  ylab("Depth (m)")+xlab("Date")+
  theme(plot.title = element_text(size=16,face='bold',hjust=.5))+
  theme(axis.title = element_text(size=14,face='bold'))+
  theme(axis.text = element_text(size=12))
ctd%>%
  filter(location=='outer_basin' & pres<=60.0 & !is.na(flor))%>%
  ggplot(aes(x=date,y=pres,colour=flor))+geom_point(size=6)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_classic()+ggtitle("Outer Basin")+
  scale_x_date(date_breaks="6 month",date_labels="%b-%y")+
  theme(axis.text.x=element_text(angle=40,vjust=1.3,hjust=1.3))+
  ylab("Depth (m)")+xlab("Date")+
  theme(plot.title = element_text(size=16,face='bold',hjust=.5))+
  theme(axis.title = element_text(size=14,face='bold'))+
  theme(axis.text = element_text(size=12))

ctd%>%
  filter(location=='slope' & pres<=60.0 & !is.na(flor))%>%
  ggplot(aes(x=date,y=pres,colour=flor))+geom_point(size=6)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_classic()+ggtitle("Slope")+
  scale_x_date(date_breaks="4 month",date_labels="%b-%y")+
  theme(axis.text.x=element_text(angle=40,vjust=1.3,hjust=1.3))+
  ylab("Depth (m)")+xlab("Date")+
  theme(plot.title = element_text(size=16,face='bold',hjust=.5))+
  theme(axis.title = element_text(size=14,face='bold'))+
  theme(axis.text = element_text(size=12))

ctd%>%
  filter(location=='middle_basin' & pres<=60.0 & !is.na(flor))%>%
  ggplot(aes(x=date,y=pres,colour=flor))+geom_point(size=6)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_classic()+ggtitle("Middle Basin")+
  scale_x_date(date_breaks="4 month",date_labels="%b-%y")+
  theme(axis.text.x=element_text(angle=40,vjust=1.3,hjust=1.3))+
  ylab("Depth (m)")+xlab("Date")+
  theme(plot.title = element_text(size=16,face='bold',hjust=.5))+
  theme(axis.title = element_text(size=14,face='bold'))+
  theme(axis.text = element_text(size=12))

ctd%>%
  filter(location=='inner_basin' & pres<=60.0 & !is.na(flor))%>%
  ggplot(aes(x=date,y=pres,colour=flor))+geom_point(size=6)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_classic()+ggtitle("Inner Basin")+
  scale_x_date(date_breaks="6 month",date_labels="%b-%y")+
  theme(axis.text.x=element_text(angle=40,vjust=1.3,hjust=1.3))+
  ylab("Depth (m)")+xlab("Date")+
  theme(plot.title = element_text(size=16,face='bold',hjust=.5))+
  theme(axis.title = element_text(size=14,face='bold'))+
  theme(axis.text = element_text(size=12))


# ---- salinity ----

ctd%>%
  filter(location=='shelf')%>%
  ggplot(aes(x=date,y=pres,colour=sal))+geom_point(size=4)+
  scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()+ggtitle("shelf")+
  scale_y_reverse()

ctd%>%
  filter(location=='outer_basin')%>%
  ggplot(aes(x=date,y=pres,colour=sal))+geom_point(size=4)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()+ggtitle("outer basin")

ctd%>%
  filter(location=='slope')%>%
  ggplot(aes(x=date,y=pres,colour=sal))+geom_point(size=4)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()+ggtitle("slope")
ctd%>%
  filter(location=='middle_basin')%>%
  ggplot(aes(x=date,y=pres,colour=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()+ggtitle("middle basin")
ctd%>%
  filter(location=='inner_basin')%>%
  ggplot(aes(x=date,y=pres,colour=sal))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()+ggtitle("inner basin")

# ---- temperature ------
ctd%>%
  filter(location=='shelf')%>%
  ggplot(aes(x=date,y=pres,colour=temp))+geom_point(size=4)+
  scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()+ggtitle("shelf")+
  scale_y_reverse()

ctd%>%
  filter(location=='outer_basin')%>%
  ggplot(aes(x=date,y=pres,colour=temp))+geom_point(size=4)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()+ggtitle("outer basin")

ctd%>%
  filter(location=='slope')%>%
  ggplot(aes(x=date,y=pres,colour=temp))+geom_point(size=4)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()+ggtitle("slope")
ctd%>%
  filter(location=='middle_basin')%>%
  ggplot(aes(x=date,y=pres,colour=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()+ggtitle("middle basin")
ctd%>%
  filter(location=='inner_basin')%>%
  ggplot(aes(x=date,y=pres,colour=temp))+geom_point(size=5)+
  scale_y_reverse()+scale_colour_gradientn(colours=rainbow(3))+
  theme_bw()+ggtitle("inner basin")

# ----- TS plots -----
# TS plot with chl as colour
# May by location and by year
# select desired stations for each location

ctd%>%
  filter(month==5 & !is.na(flor))%>%
  distinct(cast, location,year)

