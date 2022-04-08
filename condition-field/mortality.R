setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

library(tidyverse)


catch_haul<-read.csv("./data/output/catch_haul.csv")
trips<-read.csv("./data/data-working/newman-trips.csv")

head(catch_haul)
catch_haul<-catch_haul%>%distinct()

#calculate mortality rates
mort<-catch_haul%>%
  filter(age==0)%>%
  group_by(year)%>%
  mutate(N.N0_1=extrap_1/lag(extrap_1),
         N.N0_2=extrap_2/lag(extrap_2),
         N.N0_3=extrap_3/lag(extrap_3),
         N.N0_4=extrap_4/lag(extrap_4),
         N.N0_5=extrap_5/lag(extrap_5),
         N.N0_6=extrap_6/lag(extrap_6))%>%
  mutate(t=julian.date-lag(julian.date))%>%
  mutate(Z_1=log(N.N0_1)/(-1*t),
         Z_2=log(N.N0_2)/(-1*t),
         Z_3=log(N.N0_3)/(-1*t),
         Z_4=log(N.N0_4)/(-1*t),
         Z_5=log(N.N0_5)/(-1*t),
         Z_6=log(N.N0_6)/(-1*t))%>%
  dplyr::select(year,trip,age,total_catch,num_hauls,total,t,Z_1,Z_2,Z_3,Z_4,Z_5,Z_6)%>%
  gather("pulse","Z",-year,-trip,-age,-total_catch,-num_hauls,-total,-t)%>%
  mutate(pulse=as.integer(str_sub(pulse,start=3,end=3)))%>%
  ungroup()%>%
  left_join(trips)%>%
  filter(!is.na(Z))%>%
  filter(is.finite(Z))
str(mort)

write.csv(mort,"./data/output/mortalities.csv",row.names=FALSE)
