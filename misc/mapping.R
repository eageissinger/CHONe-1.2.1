# ---- mapping in R ------

#set working directory
setwd("C:/Users/Emilie/Dropbox/Thesis/Research/CHONe-1.2.1/")


#load packages
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maptools)
library(raster)
library(maps)
library(mapproj)
library(mapdata)
library(rgeos)
library(sp)
library(rgdal)
library(ggmap)
library(marmap)
library(lattice)
library(sf)
library(robis)
library(gridExtra)
library(devtools)
library(ggsn)
library(broom)
library(tmap)



#load data
can<-readOGR(dsn="shapefiles",layer="canada")
can@data$id=rownames(can@data)
can.points=fortify(can,region="id")
can.df=join(can.points,can@data,by="id")

#transform to an sf object
proj_area<-"+proj=laea +lat_0=49.12500949815647 +lon_0=-56.25"

#basic plot
plot(can)

canmap<-ggplot(can.df)+
    aes(long,lat,group=group)+
    geom_polygon()+
    geom_path(color="white")+
    coord_cartesian()+
    theme_classic()+
    north(can.df,symbol=12)+
    scalebar(can.df,dist=2000,dd2km=TRUE,model='WGS84')+
    xlab("Longitude")+
    ylab("Lattitude")
canmap+
    theme(axis.title.y=element_text(angle=90,size=14))+
    theme(axis.title.x=element_text(size=14))+
    theme(plot.title=element_text(size=24,face='bold',hjust=0.5))



#Create Newfoundland map
nl<-readOGR(dsn="shapefiles",layer="NFLD")
nl@data$id=rownames(nl@data)
nl.points=fortify(nl,region="id")
nl.df=join(nl.points,nl@data,by="id")

newmanmap<-ggplot(nl.df)+
    aes(long,lat,group=group)+
    geom_polygon()+
    geom_path(color="white")+
    coord_equal()+
    theme_classic()+
    xlab("Longitude")+
    ylab("Lattitude")+
    ggtitle("Newman Sound")+
  ylim(48.3,49)+
  xlim(-54.1,-53.5)
newmanmap+
  theme(axis.title.y=element_text(angle=90,size=14))+
  theme(axis.title.x=element_text(size=14))+
  theme(plot.title=element_text(size=24,face='bold',hjust=0.5))+
  north(nl.df,symbol=12)+
  scalebar(nl.df,dist=5,dd2km=TRUE,model='WGS84')

nlmap
nlmap<-ggplot(nl.df)+
  aes(long,lat,group=group)+
  geom_polygon()+
  geom_path(color="white")+
  coord_equal()+
  theme_classic()+
  north(nl.df,symbol=12)+
  scalebar(nl.df,dist=100,dd2km=TRUE,model='WGS84')+
  xlab("Longitude")+
  ylab("Lattitude")+
  ggtitle("Newfoundland")
nlmap+
    theme(axis.title.y=element_text(angle=90,size=14))+
    theme(axis.title.x=element_text(size=14))+
    theme(plot.title=element_text(size=24,face='bold',hjust=0.5))
    