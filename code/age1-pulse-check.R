# Assign remaining pulses

# set working directory
setwd("C:/Users/eageissinger/Documents/Emilie-Lab-comp/")

# ---- load packages ----
library(tidyverse)
library(lubridate)

# --- load data ----
range_final<-read.csv("./data/data-working/pulse_range_age1_final.csv")
catch_haul<-read.csv("./data/data-working/catch_haul.csv")
length<-read.csv("./data/data-working/newman-length.csv")

# update pulse assignments for age 1
str(range_final)


pulse_assign1<-data.frame(trip=rep(range_final$trip,range_final$max-range_final$min+1),
                          year=rep(range_final$year,range_final$max-range_final$min+1),
                          cohort=rep(range_final$cohort,range_final$max-range_final$min+1),
                          pulse=rep(range_final$pulse,range_final$max-range_final$min+1),
                          mmSL=unlist(mapply(seq,range_final$min,range_final$max)))

# Add to age 1 length data
glimpse(length)
glimpse(pulse_assign1)
length1<-length%>%
  filter(age==1)%>%
  select(-pulse)

length_pulse<-left_join(length1,pulse_assign1)
View(length_pulse)
length_pulse$date<-ymd(paste(length_pulse$year,length_pulse$month,length_pulse$day,sep="-"))
length_pulse<-length_pulse%>%
  select(-cohort)%>%
  mutate(cohort=year-1)
# ----- Abundance data ----
dim(catch_haul)
names(catch_haul)
str(catch_haul)
summary(catch_haul)
head(catch_haul)

catch<-catch_haul%>%
  select(year,month,trip,age,total_catch,total_measured,extrap_1,extrap_2,
         extrap_3,extrap_4,extrap_5,extrap_6,extrap_unknown,total)%>%
  gather(key="extrap",value = "catch_haul",c(7:13),na.rm = FALSE)%>%
  mutate(total_measured=replace(total_measured,total_catch==0,0))%>%
  mutate(catch_haul=replace(catch_haul,total_catch==0,0))%>%
  mutate(pulse=str_sub(extrap,start=8,end = 8))%>%
  mutate(pulse=replace(pulse,pulse=="u",NA))%>%
  select(-extrap)%>%
  rename(count=catch_haul)%>%
  group_by(year,month,age,trip,pulse)%>%
  summarise(count=sum(count))%>%
  filter(!is.na(month))%>%
  filter(!is.na(pulse))
View(catch)
catch0<-catch%>%
  filter(age==0)%>%
  filter(month == 10 | month ==11)%>%
  mutate(cohort=year)
catch1<-catch%>%
  filter(age==1)%>%
  filter(month == 5 | month ==7)%>%
  mutate(cohort=year-1)
abundance<-bind_rows(catch0,catch1)


# Data visualization

length_pulse%>%
  filter(cohort==1999)%>%
  ggplot(aes(x=date,y=mmSL,colour=factor(pulse)))+geom_point()+
  theme_bw()+
  ylim(c(20,225))

length_pulse<-length_pulse%>%
  mutate(pulse=replace(pulse,is.na(pulse),0))%>%
  arrange(desc(cohort))
# ---- All years as function -----
cohort.graph<-function(length_pulse,na.rm=FALSE, ...){
  
  cohort_list<-rev(unique(length_pulse$cohort))
  pdf()
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(length_pulse,length_pulse$cohort==cohort_list[i]),
                 aes(x=date,y=mmSL,group=cohort,colour=factor(pulse)))+
      geom_jitter()+
      theme_bw()+
      ylim(c(25,222))+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
    print(plot)
  }
}
cohort.graph(length_pulse)

scale_fill_manual(values=c('grey0','grey25','grey39','grey64'))