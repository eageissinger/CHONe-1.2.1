# growth rate plot for cod assessment meeting 2021

setwd("C:/Users/geissingere/Documents/pulse-structure/")

growth<-read.csv("./data/growth.csv")

library(tidyverse)

p1<-growth%>%
  filter(Pulse==1)%>%
  filter(!is.na(growth))%>%
  mutate(mean=mean(growth))

ggplot(p1)+
  geom_point(aes(x=Year,y=growth),size=2)+
  geom_hline(aes(yintercept=mean),linetype='dashed',size=0.7)+
  theme_classic()+
  ylab('Instantaneous Growth Rate · '~d^-1)+
  theme(text = element_text(size=12))+
  scale_x_continuous(breaks=seq(1996,2020,by=2))

ggplot(p1)+
  geom_point(aes(x=Year,y=growth),size=2)+
  geom_hline(aes(yintercept=mean),linetype='dashed',size=0.7)+
  theme_classic()+
  ylab('Instantaneous Growth Rate · '~d^-1)+
  theme(text = element_text(size=12))+
  scale_x_continuous(breaks=seq(1996,2020,by=2))+
  geom_line(aes(x=Year,y=growth),size=1)

write.csv(p1,"./output/growth-p1.csv",row.names=FALSE)
