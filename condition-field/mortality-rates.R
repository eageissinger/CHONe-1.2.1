library(tidyverse)
library(broom)
library(car)

# Mortality for age 0 cod with revised pulse structure

setwd("C:/Users/emili/Documents/Research/CHONe-1.2.1/condition-field/") #set working directory

trips<-read.csv("../data/data-working/newman-length.csv")%>%# read in length data to extract trip dates
  select(Year, Julian.Date,Trip)%>% # select relevant columns
  distinct()%>% # remove duplictes
  group_by(Year,Trip)%>% 
  slice(which.min(Julian.Date))%>% # select the first day of each trip
  filter(!is.na(Trip)) # remove NAs

  
catch<-read.csv("../data/output/catch_haul.csv")%>% #catch per haul data
  select(Year,Trip,Age,Julian.Date,extrap_1,extrap_2,extrap_3,extrap_4,extrap_5,extrap_6,extrap_unknown)%>% # select extrapolated catch/haul
  rename(pulse.1=extrap_1,pulse.2=extrap_2,pulse.3=extrap_3,pulse.4=extrap_4,pulse.5=extrap_5,pulse.6=extrap_6,pulse.unknown=extrap_unknown) # rename columns

head(catch)


# create data frame with the correct trip ranges for each fully settled pulse
time.range<-data.frame(Year=c(1996,1996,1996,
                              1998,1998,1998,
                              1999,1999,1999,1999,
                              2000,2000,2000,
                              2001,2001,2001,2001,
                              2002,2002,2002,
                              2003,2003,2003,
                              2004,2004,2004,2004,
                              2005,2005,2005,
                              2006,2006,2006,2006,
                              2007,2007,2007,
                              2008,2008,2008,2008,2008,2008,
                              2009,2009,2009,2009,
                              2010,2010,2010,2010,
                              2011,2011,2011,2011,
                              2012,2012,2012,
                              2013,2013,2013,
                              2014,2014,2014,
                              2015,2015,2015,
                              2016,2016,2016,2016,
                              2017,2017,2017,2017,
                              2018,2018,2018,2018,
                              2019,2019,2019,2019,
                              2020,2020,2020),
                       Pulse=c(1,2,3, #1996
                               1,2,3, #1998
                               1,2,3,4, #1999
                               1,2,3, #2000
                               1,2,3,4, #2001
                               1,2,3, #2002
                               1,2,3, #2003
                               1,2,3,4, #2004
                               1,2,3, #2005
                               1,2,3,4, #2006
                               1,2,3, #2007
                               1,2,3,4,5,6, #2008
                               1,2,3,4, #2009
                               1,2,3,4, #2010
                               1,2,3,4, #2011
                               1,2,3, #2012
                               1,2,3, #2013
                               1,2,3, #2014
                               1,2,3, #2015
                               1,2,3,4, #2016
                               1,2,3,4, #2017
                               1,2,3,4, #2018
                               1,2,3,4, #2019
                               1,2,3), #2020
                       Julian.Start=c(226,267,NA, #1996
                                      216,245,319, #1998
                                      237,272,298,NA, #1999
                                      256,256,298, #2000
                                      232,258,289,NA, #2001
                                      263,NA,NA, #2002
                                      239,282,NA, #2003
                                      230,272,287,NA, #2004
                                      220,277,NA, #2005
                                      220,248,NA,NA, #2006
                                      239,267,NA, #2007
                                      211,241,273,273,NA,NA, #2008
                                      216,246,278,NA, #2009
                                      221,263,292,NA, #2010
                                      241,270,285,NA, #2011
                                      213,289,NA, #2012
                                      218,231,288, #2013
                                      266,266,294, #2014
                                      237,285,299, #2015
                                      244,272,NA,NA, #2016
                                      233,290,290,304, #2017
                                      225,282,295,NA, #2018
                                      225,288,301,316, #2019
                                      245,288,NA), #2020
                       Julian.End=c(295,295,NA, #1996
                                    286,286,333, #1998
                                    298,325,325,NA, #1999
                                    298,298,331, #2000
                                    289,289,319,NA, #2001
                                    324,NA,NA, #2002
                                    297,297,NA, #2003
                                    299,299,328,NA, #2004
                                    277,291,NA, #2005
                                    276,296,NA,NA, #2006
                                    282,296,NA, #2007
                                    273,288,301,301,NA,NA, #2008
                                    278,278,320,NA, #2009
                                    263,292,306,NA, #2010
                                    285,326,326,NA, #2011
                                    303,331,NA, #2012
                                    288,288,308, #2013
                                    294,308,322, #2014
                                    285,313,327, #2015
                                    287,301,NA,NA, #2016
                                    290,319,319,319, #2017
                                    309,323,323,NA, #2018
                                    301,330,330,343, #2019
                                    302,317,NA)) #2020
# clean data frame
date.range<-time.range%>%
  filter(!is.na(Julian.Start) & !is.na(Julian.End))

# create dataframe with list of potential dates
full.date.range<-data.frame(Year=rep(date.range$Year,date.range$Julian.End-date.range$Julian.Start+1),
           Pulse=rep(date.range$Pulse,date.range$Julian.End-date.range$Julian.Start+1),
           Julian.Date=unlist(mapply(seq,date.range$Julian.Start,date.range$Julian.End)))

# add trip numbers to the date range
full.date.range<-left_join(full.date.range, trips)%>%
  filter(!is.na(Trip)) # remove NA entries

# convert catch to long format
head(catch)
catch.long<-catch%>%
  gather("Pulse","Catch",-Year,-Trip,-Age,-Julian.Date)%>%
  mutate(Pulse=str_sub(Pulse,start=7,end=7))%>%
  mutate(Pulse=replace(Pulse,Pulse=="u",NA))%>%
  arrange(Year,Trip,Age,Pulse)%>%
  filter(!is.na(Julian.Date) | !is.na(Catch))%>%
  mutate(Pulse=as.integer(Pulse))%>%
  select(-Julian.Date)

# create dataframe with periods of interest
data2<-left_join(catch.long,full.date.range)%>%
  arrange(Year,Pulse,Trip,Age)%>%
  filter(!is.na(Julian.Date))
  
# Linear regression for all years and pulses
data3<-data2%>%
  select(Year,Pulse,Catch,Julian.Date)%>%
  group_by(Year,Pulse)%>%
  nest()

# Define function you want to apply to each nested data set
mymodel <- function(x) {lm(Catch ~ Julian.Date, data = x)}

# Use {purrr} to map function to each nested data set and add it to newdata
map(data3$data, mymodel)
mort.rates <- data3 %>% mutate(results = map(data, mymodel),
                            tidied=map(results,tidy))%>%
  unnest(tidied)%>%
  select(Year,Pulse,term,estimate,std.error)%>%
  filter(term=="Julian.Date")%>%
  rename(M=estimate)%>%
  select(-term)

head(mort.rates)

# Review rates

ggplot(filter(mort.rates,Pulse!=4))+
  geom_point(aes(x=Year,y=M))+
  #geom_line(aes(x=Year,y=M))+
  geom_errorbar(aes(x=Year,ymin=M-std.error,ymax=M+std.error),width=0)+
  facet_wrap(~Pulse)

write.csv(mort.rates,"../data/output/age-0-mortality-R.csv",row.names=FALSE)
