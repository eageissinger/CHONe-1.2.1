# Assign remaining pulses

# set working directory
setwd("C:/Users/geissingere/Documents/CHONe-1.2.1-office/")

# ---- load packages ----
library(tidyverse)
library(lubridate)

# --- load data ----
range0<-read.csv("./data/data-working/pulse_range_age0_round1.csv")
#catch_haul<-read.csv("./data/data-working/catch_haul.csv")
length<-read.csv("./data/data-working/newman-length.csv")

# update pulse assignments for age 1
str(range0)


pulse_assign0<-data.frame(trip=rep(range0$trip,range0$max-range0$min+1),
                          year=rep(range0$year,range0$max-range0$min+1),
                          cohort=rep(range0$cohort,range0$max-range0$min+1),
                          pulse=rep(range0$pulse,range0$max-range0$min+1),
                          mmSL=unlist(mapply(seq,range0$min,range0$max)))

# Add to age 1 length data
glimpse(length)
glimpse(pulse_assign1)
length0<-length%>%
  filter(age==0)%>%
  select(-pulse)

length_pulse<-left_join(length0,pulse_assign0)
View(length_pulse)
length_pulse$date<-ymd(paste(length_pulse$year,length_pulse$month,length_pulse$day,sep="-"))
length_pulse<-length_pulse%>%
  select(-cohort)%>%
  mutate(cohort=year)


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
  pdf("coloured-pulse-plot.pdf")
  for (i in seq_along(cohort_list)) {
    plot<-ggplot(subset(length_pulse,length_pulse$cohort==cohort_list[i]),
                 aes(x=date,y=mmSL,group=cohort,colour=factor(pulse)))+
      geom_jitter(size=.25)+
      theme_bw()+
      ylim(c(25,222))+
      ggtitle(paste(cohort_list[i], "Cohort"))+
      xlab("Date")+ylab("Standard length (mm)")+
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
    print(plot)
  }
  #dev.off()
}
cohort.graph(length_pulse)
