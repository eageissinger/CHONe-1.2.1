# ----- Newman Sound Site Map ------

# ----- load packages -----
library(raster)
library(ggmap)
library(ggplot2)
library(ggsn)
library(tidyverse)
library(cowplot)

mapcanada<- raster::getData("GADM", country = "canada", level = 1) ##gets Canada map available from raster package
mapnl<- mapcanada[5,] ##extracts NL from larger Canada map
mapnl_df<- fortify(mapnl) ##fortify to convert from SpatialPolygonsDataFrame into a regular df



locations<-read.csv("C:/Users/emili/Documents/Research/CHONe-1.2.1/data/data-working/Newman Sound Seining Locations 2013.csv")
head(locations)

loc<-locations%>%
  dplyr::select(Easting,Northing)

library(proj4)

pjstring<- "+proj=utm +zone=22 +north +ellps=WGS84 +units=m +no_defs"



pj<-project(loc,pjstring,inverse=TRUE)

latlon<-data.frame(lat=pj$y,lon=pj$x)

latlon

Lat.lim=c(48.53,48.61)
Long.lim=c(-53.98,-53.84)

NLmap<-ggplot()+ #removed data=NLdf,aes(x=long,y=lat,group=group and used geom_map instead
  geom_map(data = mapnl_df, map = mapnl_df, aes(group = group, map_id = id),fill='grey') + 
  theme(legend.position = "none",title=element_blank())+ 
  coord_quickmap(xlim=Long.lim,ylim=Lat.lim)+
  geom_point(data=latlon,aes(x=lon,y=lat),size=2.5,colour='black')+
  theme_bw()+
  theme(panel.grid.major = element_line(colour='white'),panel.grid.minor = element_blank())+
  theme(panel.background=element_rect(fill='white',
                                      colour='white'))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y = element_text(size=14,vjust=3))+
  theme(axis.title.x = element_text(size=14,vjust=-1))+
  labs(x="Longitude",
       y="Latitude",
       shape="Location")+
  ggsn::scalebar(transform = TRUE, model = "WGS84", dist_unit = "km", dist=1, #removed data = NLdf, and added model = "WGS84" to match the map ellipsoid model (projection) of the original mapnl SpatialPolygonsDataFrame
                 x.min=-53.88,x.max=-53.84,y.min=48.535,y.max=48.61, #removed anchor = c(x=-53.8399,y=48.5301), as I wanted to the x.min, x.max, y.min, y.max arguments to specify scalebar position
                 st.dist = 0.04, st.size=4, height=0.05)+ ##adjusted the scalebar height and added st.dist to adjust the text's distance from the scalebar
  theme(plot.background = element_rect(fill="white",colour="white"))+
  theme(legend.background = element_rect(fill='white',colour='black'))

ggsave(NLmap,file="C:/Users/emili/Documents/Research/CHONe-1.2.1/condition-field/figures/map.png",
       width=168,height=168,units="mm")








# Canada map.
mapcanada_df<-fortify(mapcanada)
head(mapcanada_df)
plot(mapcanada)
ggplot()+
  geom_map(data=mapcanada_df, map=mapcanada_df, aes(group=group, map_id=id))


# North america
US<-raster::getData("GADM", country = "united states", level = 1) ##gets Canada map available from raster package
plot(US)
mapUS_df<-fortify(US)
n.america<-bind_rows(mapcanada_df,mapUS_df)

long.lim2=c(-83.74,-35.00)
lat.lim2=c(24.5,55)

Map1<-ggplot()+
  geom_map(data=n.america,map=n.america,aes(group=group,map_id=id))+
  geom_point(aes(x=-53.89,y=48.47),shape=0,colour='seagreen3',size=3,stroke=2)+
  theme(legend.position = "none",title=element_blank())+ #removed axis.text=element_blank() so that Lat/Long labels showed up
  coord_quickmap(xlim=long.lim2,ylim=lat.lim2)+
  theme_bw()+
  theme(panel.grid.major = element_line(colour='lightgrey'),panel.grid.minor = element_blank())+
  theme(panel.background=element_rect(fill='lightgrey',
                                       colour='lightgrey'))+
  theme(panel.border=element_blank())+
  ggsn::north(symbol=10,x.min= -83.74,x.max = -35,y.min = 24.5, y.max = 55)
  

# theme(axis.text = element_text(size=12))+
  # theme(axis.title.y = element_text(size=14,vjust=3))+
  # theme(axis.title.x = element_text(size=14,vjust=-1))+
  # labs(x="Longitude",
  #      y="Latitude",
  #      shape="Location")#+
  # ggsn::scalebar(transform = TRUE, model = "WGS84", dist_unit = "km", dist=1, #removed data = NLdf, and added model = "WGS84" to match the map ellipsoid model (projection) of the original mapnl SpatialPolygonsDataFrame
  #                x.min=-53.88,x.max=-53.84,y.min=48.535,y.max=48.61, #removed anchor = c(x=-53.8399,y=48.5301), as I wanted to the x.min, x.max, y.min, y.max arguments to specify scalebar position
  #                st.dist = 0.04, st.size=4, height=0.05) ##adjusted the scalebar height and added st.dist to adjust the text's distance from the scalebar
ggsave(Map1,file="C:/Users/emili/Documents/Research/CHONe-1.2.1/overwinter-CMR/output/map2.png",
       width=500,height=230,units="mm")

# embed NL map in eastern seaboard map
Map1+
  annotation_custom(grob = NLmap, xmin = -80.49, xmax = -50.1,
                    ymin = 24.05, ymax = 58.34) 

gg_inset_map1 = ggdraw() +
  draw_plot(Map1) +
  draw_plot(NLmap, x = 0.05, y = 0.65, width = 0.3, height = 0.3)


# ---- presentation map ----


Lat.lim=c(48.56,48.59)
Long.lim=c(-53.94,-53.895)

ggplot()+ #removed data=NLdf,aes(x=long,y=lat,group=group and used geom_map instead
  geom_map(data = mapnl_df, map = mapnl_df, aes(group = group, map_id = id),fill='forestgreen') + #instead of geom_polygon and geom_path I usually use geom_map just out of personal preference I suppose
  theme(legend.position = "none",title=element_blank())+ #removed axis.text=element_blank() so that Lat/Long labels showed up
  coord_quickmap(xlim=Long.lim,ylim=Lat.lim)+
  geom_point(data=locations,aes(x=long,y=lat,group=group,shape=site),size=4,colour='seagreen3')+
  theme_bw()+
  theme(panel.grid.major = element_line(colour='royalblue4'),panel.grid.minor = element_blank())+
  theme(panel.background=element_rect(fill='royalblue4',
                                      colour='royalblue4'))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y = element_text(size=14,vjust=3))+
  theme(axis.title.x = element_text(size=14,vjust=-1))+
  labs(x="Longitude",
       y="Latitude",
       shape="Location")+
  ggsn::scalebar(transform = TRUE, model = "WGS84", dist_unit = "km", dist=1, #removed data = NLdf, and added model = "WGS84" to match the map ellipsoid model (projection) of the original mapnl SpatialPolygonsDataFrame
                 x.min=-53.88,x.max=-53.84,y.min=48.535,y.max=48.61, #removed anchor = c(x=-53.8399,y=48.5301), as I wanted to the x.min, x.max, y.min, y.max arguments to specify scalebar position
                 st.dist = 0.04, st.size=4, height=0.05)+ ##adjusted the scalebar height and added st.dist to adjust the text's distance from the scalebar
  theme(plot.background = element_rect(fill="white",colour="white"))+
  theme(legend.background = element_rect(fill='white',colour='black'))


Lat.lim=c(48.565,48.59)
Long.lim=c(-53.94,-53.895)

ggplot()+ #removed data=NLdf,aes(x=long,y=lat,group=group and used geom_map instead
  geom_map(data = mapnl_df, map = mapnl_df, aes(group = group, map_id = id),fill='forestgreen') + #instead of geom_polygon and geom_path I usually use geom_map just out of personal preference I suppose
  theme(legend.position = "none",title=element_blank())+ #removed axis.text=element_blank() so that Lat/Long labels showed up
  coord_quickmap(xlim=Long.lim,ylim=Lat.lim)+
  geom_point(data=locations,aes(x=long,y=lat,group=group,shape=site),size=4,colour='seagreen3')+
  theme_bw()+
  theme(panel.grid.major = element_line(colour='royalblue4'),panel.grid.minor = element_blank())+
  theme(panel.background=element_rect(fill='royalblue4',
                                      colour='royalblue4'))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.y = element_text(size=14,vjust=3))+
  theme(axis.title.x = element_text(size=14,vjust=-1))+
  labs(x="Longitude",
       y="Latitude",
       shape="Location")+
  ggsn::scalebar(transform = TRUE, model = "WGS84", dist_unit = "km", dist=1, #removed data = NLdf, and added model = "WGS84" to match the map ellipsoid model (projection) of the original mapnl SpatialPolygonsDataFrame
                 x.min=-53.88,x.max=-53.84,y.min=48.535,y.max=48.61, #removed anchor = c(x=-53.8399,y=48.5301), as I wanted to the x.min, x.max, y.min, y.max arguments to specify scalebar position
                 st.dist = 0.04, st.size=4, height=0.05)+ ##adjusted the scalebar height and added st.dist to adjust the text's distance from the scalebar
  theme(plot.background = element_rect(fill="white",colour="white"))+
  theme(legend.background = element_rect(fill='white',colour='black'))
