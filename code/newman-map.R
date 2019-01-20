# ---- set working directory ----
setwd("C:/Users/Emilie/Dropbox/Thesis/Research")

# ---- load packages ----
library(maptools)
library(rgdal)
library(raster)
library(maps)
library(mapdata)
library(ggmap)
library(marmap)
library(lattice)
library(ggplot2)
library(ggsn)
library(sp)

# ---- Getting a basemap ----
data(wrld_simpl)
plot(wrld_simpl)
plot(wrld_simpl,col='olivedrab3',bg='lightblue')


# ---- exporting and importing ----
writeOGR(wrld_simpl,dsn=getwd(),layer="world_test",
         driver="ESRI Shapefile",overwrite_layer = TRUE)
world_shp<-readOGR(dsn=getwd(),layer="world_test")
plot(world_shp)

# ---- making nicer maps
# using raster
Canada<-getData('GADM',country="CAN",level=1)
plot(Canada)

Canada

NL<-Canada[Canada$NAME_1=="Newfoundland and Labrador",]
plot(NL,col="blue")

Lat.lim=c(46,52)
Long.lim=c(-60,-52.5)
map("worldHires",xlim=Long.lim,ylim=Lat.lim,col="grey",
    fill=TRUE, resolution=0);map.axes()
map.scale(ratio=FALSE)

# ---- Google map ----
google<-get_map(location=c(-53.9,48.56),zoom=12,maptype = 'satellite')
ggmap(google)
ggmap(google)+geom_point(aes(x=-53.92724,y=48.58407),colour='yellow',shape=15,size=3.5)+
  geom_point(aes(x=-53.921207,y=48.567545),colour='yellow',shape=16,size=3.5)+
  geom_point(aes(x=-53.917300,y=48.588810),colour='yellow',shape=17,size=3.5)+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title=element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))
# 48.58407, -53.92724


xcoord<-c(-53.92724,-53.921207,-53.917300)
ycoord<-c(48.58407,48.567545,48.588810)

newman<-get_map(location=c(-53.92,48.575),zoom=14,maptype="satellite")
ggmap(newman)+scalebar(data=newman,dist=5,dd2km=TRUE,model='WGS84')
mymap<-ggmap(newman)+geom_point(aes(x=-53.92724,y=48.58407),colour='yellow',size=4,shape=15)+
  geom_point(aes(x=-53.921207,y=48.567545),colour='yellow',size=4,shape=16)+
  geom_point(aes(x=-53.917300,y=48.588810),colour='yellow',size=4,shape=17)+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))


north(mymap) +
  scalebar(mymap, dist = 5, dd2km = TRUE, model = 'WGS84')
# Map of canada using googlemap

canada<-get_map(location=c(-107.3688,57.709),zoom=3,maptype="satellite")
ggmap(canada)+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))
nl<-get_map(location=c(-56.304,48.5808),zoom=6,maptype="satellite")
ggmap(nl)+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text=element_text(size=14))


nl<-get_map(location=c(-56.304,48.5808),zoom=4,maptype="satellite")
ggmap(nl)+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text=element_text(size=14))


# ---- fjord map -----

canada<-get_map(location=c(-107.3688,57.709),zoom=3,maptype="satellite")
ggmap(canada)+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

Lat.lim=c(46,52)
Long.lim=c(-60.2,-51)
map("worldHires",xlim=Long.lim,ylim=Lat.lim,col='grey',fill=TRUE,resolution=0)
map.axes()
map.scale(ratio=FALSE)

nl<-get_map(location=c(-53.9,48.7),zoom=9,maptype="satellite")
ggmap(nl)+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text=element_text(size=14))

fjord<-get_map(location=c(-53.6,48.6),zoom=10,maptype = 'satellite')
ggmap(fjord)+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=16,face='bold'))+
  theme(axis.text = element_text(size=14))



newman<-get_map(location=c(-53.8,48.575),zoom=11,maptype="satellite")
ggmap(newman)+
  xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(size=18,face='bold'))+
  theme(axis.text = element_text(size=14))

# ---- project study site -----
Lat.lim<-c(48.5327,48.6916)
Long.lim<-c(-53.9724,-53.5784)

data("wrld_simpl")
map(wrld_simpl,xlim=Long.lim,ylim=Lat.lim,col='grey',fill=TRUE,resolution=0)
map.axes()
map.scale(ratio=FALSE)