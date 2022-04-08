# Compile all years of pulse structure data

# set working directory
setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/pulse-structure/")


pulse0<-read.csv("../data/output/pulse_range_0_final.csv")%>% # pulse range for age 0
  select(-date)%>% #remove date column
  rename(minSL=min, maxSL=max) # rename columns to match 2019 and 2020 data
pulse1<-read.csv("../data/output/pulse_range_age1_final.csv")%>% # pulse range for age 1
  select(-cohort)%>% # remove cohort column
  rename(minSL=min, maxSL=max)%>% # rename columns to match 2019 and 2020 data
  mutate(age=1) # add age to dataframe

summary(pulse0) # to 2018
summary(pulse1) # to 2018

pulse19<-read.csv("../data/output/pulse_range2019.csv")%>%
  select(-Date,-meanSL)%>%
  rename(year=Year,age=Age,trip=Trip,pulse=Pulse)
pulse20<- read.csv("../data/output/pulse_range2020.csv")%>%
  select(-Date,-meanSL)%>%
  rename(year=Year,age=Age,trip=Trip,pulse=Pulse)

head(pulse19)
head(pulse20)
summary(pulse19)
summary(pulse20)


#combine all pulse ranges
#check headings
head(pulse0)
head(pulse1)
head(pulse19)
head(pulse20)


pulses<-bind_rows(pulse0,pulse1,pulse19,pulse20)

#save file
write.csv(pulses,"../data/output/pulse_range_ALL.csv",row.names = FALSE)
