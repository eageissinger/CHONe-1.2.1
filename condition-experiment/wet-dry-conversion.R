setwd("C:/Users/user/Documents/Research/CHONe-1.2.1/")

# load data

conv<-read.csv("./data/data-working/wet-dry-conversion.csv")

#load packages
library(tidyverse)

conv%>%
  mutate(ratio=dry_full2/live_wet_weight)->conv

conv%>%
  mutate(ratio=live_wet_weight/dry_full2)%>%
  summarise(mean(ratio))

conv%>%
  summarise(mean(ratio),sd(ratio),se=sd(ratio)/sqrt(n()))

ggplot(conv,aes(x=mmSL,y=ratio))+geom_point()
lm(ratio~mmSL,data=conv)


diet<-read.csv("./data/data-working/diet_feeding-exp.csv")
leftover<-read.csv("./data/data-working/diet_leftover-exp.csv")
weight<-read.csv("./data/data-working/length-weight-exp.csv")
head(diet)
names(diet)
head(leftover)
head(weight)
str(weight)

weight<-weight%>%
  group_by(tank,ï..year,month,day)%>%
  summarise(weight=sum(weight_g))
head(weight)

head(diet)
weight_food<-left_join(diet,weight)
View(weight_food)
str(weight_food)

#convert to new treatment
diet%>%
  mutate(ration=as.numeric(str_sub(trt,start=1,end=3)))%>%
  mutate(tank_weight=feeding..g./(ration/100))%>%
  mutate(dry_weight=0.19*tank_weight)%>%
  mutate(dry_food=feeding..g.*0.93)%>%
  mutate(new_ration=(dry_food/dry_weight)*100)%>%
  filter(!is.na(new_ration))%>%
  group_by(trt)%>%
  summarise(ration=mean(new_ration))


weight_food%>%
  mutate(dry_weight=0.19*weight)%>%
  mutate(dry_food=feeding..g.*0.93)%>%
  filter(feeding..g.>0)%>%
  mutate(new_ration=(dry_food/dry_weight)*100)->weight2
%>%
  group_by(as.character(trt))%>%
  summarise(mean(new_ration))
