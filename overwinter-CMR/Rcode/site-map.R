# ----- Newman Sound Site Map ------

# ----- load packages -----
library(raster)
library(ggmap)
library(ggplot2)
library(ggsn)

mapcanada<- raster::getData("GADM", country = "canada", level = 1) ##gets Canada map available from raster package
mapnl<- mapcanada[5,] ##extracts NL from larger Canada map
mapnl_df<- fortify(mapnl) ##fortify to convert from SpatialPolygonsDataFrame into a regular df

#lines 67-69  above are more or less the same as or in place of your lines 22-35

locations<-data.frame(site=c("NB","CC","MI"),
                      lat=c(48.584106,48.571073,48.588562),
                      long=c(-53.927348,-53.919795,-53.917563),
                      group=c(9.1,9.1,9.1))
locations$site<-factor(locations$site, levels=c("NB","MI","CC"))

Lat.lim=c(48.53,48.61)
Long.lim=c(-53.98,-53.84)

ggplot()+ #removed data=NLdf,aes(x=long,y=lat,group=group and used geom_map instead
  geom_map(data = mapnl_df, map = mapnl_df, aes(group = group, map_id = id), fill = "lightgrey", color = "black") + #instead of geom_polygon and geom_path I usually use geom_map just out of personal preference I suppose
  #geom_polygon(fill='grey')+
  #geom_path(colour='black')+ 
  theme(legend.position = "none",title=element_blank())+ #removed axis.text=element_blank() so that Lat/Long labels showed up
  coord_fixed(xlim=Long.lim,ylim=Lat.lim)+
  geom_point(data=locations,aes(x=long,y=lat,group=group,shape=site),size=4)+
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
                 st.dist = 0.04, st.size=4, height=0.05) ##adjusted the scalebar height and added st.dist to adjust the text's distance from the scalebar
# add text near scalebar for 0 km, 1 km, 2 km

long.lim2=c()
lat.lim2=c()
# Canada map.
mapcanada_df<-fortify(mapcanada)
head(mapcanada_df)
plot(mapcanada)
ggplot()+
  geom_map(data=mapcanada_df, map=mapcanada_df, aes(group=group, map_id=id))
