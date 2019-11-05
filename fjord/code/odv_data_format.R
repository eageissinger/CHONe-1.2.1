# ---- Newman Sound CTD data organization-----

# ---- set working directory -----
setwd("C:/Users/Emilie/Dropbox/FHL/course-project/Newman_sound_ctd_data/ctd-csv/")

# ---- load packages -----
library(dplyr)
library(stringr)

# ---- load data -----
d1<-read.csv("CTD-03284002-1998.csv")
d2<-read.csv("CTD-03284003-1998.csv")
d3<-read.csv("CTD-03284004-1998.csv")
d4<-read.csv("CTD-03284005-1998.csv")
d5<-read.csv("CTD-03284008-1998.csv")
d6<-read.csv("CTD-03284009-1998.csv")
d7<-read.csv("CTD-03284010-1998.csv")
d8<-read.csv("CTD-03284011-1998.csv")
d9<-read.csv("CTD-03288003-1999.csv")
d10<-read.csv("CTD-03288004-1999.csv")
d11<-read.csv("CTD-03288007-1999.csv")
d12<-read.csv("CTD-03288008-1999.csv")
d13<-read.csv("CTD-03288009-1999.csv")
d14<-read.csv("CTD-03296001-1999.csv")
d15<-read.csv("CTD-03296002-1999.csv")
d16<-read.csv("CTD-03296003-1999.csv")
d17<-read.csv("CTD-03296029-1999.csv")
d18<-read.csv("CTD-03329003-2000.csv")
d19<-read.csv("CTD-03335030-2001.csv")
d20<-read.csv("CTD-03335031-2001.csv")
d21<-read.csv("CTD-03388009-2001.csv")
d22<-read.csv("CTD-03388010-2001.csv")
d23<-read.csv("CTD-03388012-2001.csv")
d24<-read.csv("CTD-03389006-2001.csv")
d25<-read.csv("CTD-03389007-2001.csv")
d26<-read.csv("CTD-03389008-2001.csv")
d27<-read.csv("CTD-03436012-2002.csv")
d28<-read.csv("CTD-03446027-2002.csv")
d29<-read.csv("CTD-03446028-2002.csv")
d30<-read.csv("CTD-03446029-2002.csv")
d31<-read.csv("CTD-03506026-2003.csv")
d32<-read.csv("CTD-03506027-2003.csv")
d33<-read.csv("CTD-03506028-2003.csv")
d34<-read.csv("CTD-03561050-2004.csv")
d35<-read.csv("CTD-03561051-2004.csv")
d36<-read.csv("CTD-03567049-2004.csv")
d37<-read.csv("CTD-03567050-2004.csv")
d38<-read.csv("CTD-03567057-2004.csv")
d39<-read.csv("CTD-03567058-2004.csv")
d40<-read.csv("CTD-03567059-2004.csv")
d41<-read.csv("CTD-03567060-2004.csv")
d42<-read.csv("CTD-035679029-2004.csv")
d43<-read.csv("CTD-035679030-2004.csv")
d44<-read.csv("CTD-035679031-2004.csv")
d45<-read.csv("CTD-03605006-2011.csv")
d46<-read.csv("CTD-03651030-2005.csv")
d47<-read.csv("CTD-03651031-2005.csv")
d48<-read.csv("CTD-03651032-2005.csv")
d49<-read.csv("CTD-03713028-2006.csv")
d50<-read.csv("CTD-03713029-2006.csv")
d51<-read.csv("CTD-03713033-2006.csv")
d52<-read.csv("CTD-03713034-2006.csv")
d53<-read.csv("CTD-03713035-2006.csv")
d54<-read.csv("CTD-03713036-2006.csv")
d55<-read.csv("CTD-03781028-2007.csv")
d56<-read.csv("CTD-03781029-2007.csv")
d57<-read.csv("CTD-03781030-2007.csv")
d58<-read.csv("CTD-03781031-2007.csv")
d59<-read.csv("CTD-03781032-2007.csv")
d60<-read.csv("CTD-03781033-2007.csv")
d61<-read.csv("CTD-03796003-2007.csv")
d62<-read.csv("CTD-03796004-2007.csv")
d63<-read.csv("CTD-03862010-2008.csv")
d64<-read.csv("CTD-03862011-2008.csv")
d65<-read.csv("CTD-03883014-2009.csv")
d66<-read.csv("CTD-03883015-2009.csv")
d67<-read.csv("CTD-039416010-2003.csv")

# add cast label (d1 = cast #1, etc)
d1<-mutate(d1,cast=1)
d2<-mutate(d2,cast=2)
d3<-mutate(d3,cast=3)
d4<-mutate(d4,cast=4)
d5<-mutate(d5,cast=5)
d6<-mutate(d6,cast=6)
d7<-mutate(d7,cast=7)
d8<-mutate(d8,cast=8)
d9<-mutate(d9,cast=9)
d10<-mutate(d10,cast=10)
d11<-mutate(d11,cast=11)
d12<-mutate(d12,cast=12)
d13<-mutate(d13,cast=13)
d14<-mutate(d14,cast=14)
d15<-mutate(d15,cast=15)
d16<-mutate(d16,cast=16)
d17<-mutate(d17,cast=17)
d18<-mutate(d18,cast=18)
d19<-mutate(d19,cast=19)
d20<-mutate(d20,cast=20)
d21<-mutate(d21,cast=21)
d22<-mutate(d22,cast=22)
d23<-mutate(d23,cast=23)
d24<-mutate(d24,cast=24)
d25<-mutate(d25,cast=25)
d26<-mutate(d26,cast=26)
d27<-mutate(d27,cast=27)
d28<-mutate(d28,cast=28)
d29<-mutate(d29,cast=29)
d30<-mutate(d30,cast=30)
d31<-mutate(d31,cast=31)
d32<-mutate(d32,cast=32)
d33<-mutate(d33,cast=33)
d34<-mutate(d34,cast=34)
d35<-mutate(d35,cast=35)
d36<-mutate(d36,cast=36)
d37<-mutate(d37,cast=37)
d38<-mutate(d38,cast=38)
d39<-mutate(d39,cast=39)
d40<-mutate(d40,cast=40)
d41<-mutate(d41,cast=41)
d42<-mutate(d42,cast=42)
d43<-mutate(d43,cast=43)
d44<-mutate(d44,cast=44)
d45<-mutate(d45,cast=45)
d46<-mutate(d46,cast=46)
d47<-mutate(d47,cast=47)
d48<-mutate(d48,cast=48)
d49<-mutate(d49,cast=49)
d50<-mutate(d50,cast=50)
d51<-mutate(d51,cast=51)
d52<-mutate(d52,cast=52)
d53<-mutate(d53,cast=53)
d54<-mutate(d54,cast=54)
d55<-mutate(d55,cast=55)
d56<-mutate(d56,cast=56)
d57<-mutate(d57,cast=57)
d58<-mutate(d58,cast=58)
d59<-mutate(d59,cast=59)
d60<-mutate(d60,cast=60)
d61<-mutate(d61,cast=61)
d62<-mutate(d62,cast=62)
d63<-mutate(d63,cast=63)
d64<-mutate(d64,cast=64)
d65<-mutate(d65,cast=65)
d66<-mutate(d66,cast=66)
d67<-mutate(d67,cast=67)

# ---- combine data sets ----

dfw<-union_all(d1,d2)
dfw<-union_all(dfw,d3)
dfw<-union_all(dfw,d4)
dfw<-union_all(dfw,d5)
dfw<-union_all(dfw,d6)
dfw<-union_all(dfw,d7)
dfw<-union_all(dfw,d8)
dfw<-union_all(dfw,d9)
dfw<-union_all(dfw,d10)
dfw<-union_all(dfw,d11)
dfw<-union_all(dfw,d12)
dfw<-union_all(dfw,d13)
dfw<-union_all(dfw,d14)
dfw<-union_all(dfw,d15)
dfw<-union_all(dfw,d16)
dfw<-union_all(dfw,d17)
dfw<-union_all(dfw,d18)
dfw<-union_all(dfw,d19)
dfw<-union_all(dfw,d20)
dfw<-union_all(dfw,d21)
dfw<-union_all(dfw,d22)
dfw<-union_all(dfw,d23)
dfw<-union_all(dfw,d24)
dfw<-union_all(dfw,d25)
dfw<-union_all(dfw,d26)
dfw<-union_all(dfw,d27)
dfw<-union_all(dfw,d28)
dfw<-union_all(dfw,d29)
dfw<-union_all(dfw,d30)
dfw<-union_all(dfw,d31)
dfw<-union_all(dfw,d32)
dfw<-union_all(dfw,d33)
dfw<-union_all(dfw,d34)
dfw<-union_all(dfw,d35)
dfw<-union_all(dfw,d36)
dfw<-union_all(dfw,d37)
dfw<-union_all(dfw,d38)
dfw<-union_all(dfw,d39)
dfw<-union_all(dfw,d40)
dfw<-union_all(dfw,d41)
dfw<-union_all(dfw,d42)
dfw<-union_all(dfw,d43)
dfw<-union_all(dfw,d44)
dfw<-union_all(dfw,d45)
dfw<-union_all(dfw,d46)
dfw<-union_all(dfw,d47)
dfw<-union_all(dfw,d48)
dfw<-union_all(dfw,d49)
dfw<-union_all(dfw,d50)
dfw<-union_all(dfw,d51)
dfw<-union_all(dfw,d52)
dfw<-union_all(dfw,d53)
dfw<-union_all(dfw,d54)
dfw<-union_all(dfw,d55)
dfw<-union_all(dfw,d56)
dfw<-union_all(dfw,d57)
dfw<-union_all(dfw,d58)
dfw<-union_all(dfw,d59)
dfw<-union_all(dfw,d60)
dfw<-union_all(dfw,d61)
dfw<-union_all(dfw,d62)
dfw<-union_all(dfw,d63)
dfw<-union_all(dfw,d64)
dfw<-union_all(dfw,d65)
dfw<-union_all(dfw,d66)
dfw<-union_all(dfw,d67)

# ---- check data -----
ctd<-dfw
summary(ctd)
glimpse(ctd)

# ---- fix lat and long ----
ctd$lat_deg<-str_sub(ctd$lat,start=1,end=2)
ctd$lat_min<-str_sub(ctd$lat,start=4,end=8)
ctd$long_deg<-str_sub(ctd$long,start=1,end=4)
ctd$long_min<-str_sub(ctd$long,start=6,end=10)

ctd$lat_deg<-as.numeric(ctd$lat_deg)
ctd$lat_min<-as.numeric(ctd$lat_min)
ctd$long_deg<-as.numeric(ctd$long_deg)
ctd$long_min<-as.numeric(ctd$long_min)

glimpse(ctd)

# convert lat and long to degree decimal
ctd<-ctd%>%
  mutate(lat_dec=lat_deg+(lat_min/60),
         long_dec=long_deg-(long_min/60))%>%
  select(scan,pres,temp,cond,sal,sigt,year,month,day,hour,minute,cast,flor,lat_dec,long_dec)

write.csv(ctd,"newman_ctd_all.csv",row.names = FALSE)

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
  rename(FLORESCENCE=flor)%>%
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
             'CONDUCTIVITY','FLORESCENCE')]

write.csv(ctd3,"./data/newman_odv.csv",row.names = FALSE)