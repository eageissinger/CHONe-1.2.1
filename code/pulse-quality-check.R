test1<-mixtures%>%
  group_by(year,month,day,trip,dummy_pulse,cohort)%>%
  summarise(min=(mu-sigma),max=mu+sigma,sd=sigma)%>%
  mutate(min_round=floor(min),max_round=ceiling(max))%>%# add rounded min and max to create an integer column. Min is rounded down, max is rounded up
  select(-min,-max)%>%
  rename(min=min_round,max=max_round)%>%
  ungroup()
assign.test1<-data.frame(trip=rep(test1$trip,test1$max-test1$min+1),
                         year=rep(test1$year,test1$max-test1$min+1),
                         month=rep(test1$month,test1$max-test1$min+1),
                         day=rep(test1$day,test1$max-test1$min+1),
                         cohort=rep(test1$cohort,test1$max-test1$min+1),
                         pulse=rep(test1$dummy_pulse,test1$max-test1$min+1),
                         mmSL=unlist(mapply(seq,test1$min,test1$max)))
assign.test1<-select(assign.test1,-year,-month,-day)
df1<-length%>%filter(age==1 & year ==2008)%>%select(-pulse)%>%
  filter(trip==10 | trip == 12 | trip ==13 | trip ==16 | trip ==18)
unique(mixtures$trip)
# assign pulses to subsetted age1 length data
length.test1<-left_join(df1,assign.test1,by=c("mmSL","trip"))
View(length.test1)
# total percent not accounted for
length.test1$trip<-as.factor(length.test1$trip)
t1<-length.test1%>%
  filter(!is.na(pulse))%>%
  group_by(trip)%>%
  summarise(assigned=n())
t2<-length.test1%>%
  filter(is.na(pulse))%>%
  group_by(trip)%>%
  summarise(na=n())
check1<-left_join(t1,t2)%>%
  mutate(freq=na/assigned)


test1<-mixtures%>%
  group_by(year,month,day,trip,dummy_pulse,cohort)%>%
  summarise(min=(mu-sigma),max=mu+sigma,sd=sigma)%>%
  mutate(min_round=floor(min),max_round=ceiling(max))%>%# add rounded min and max to create an integer column. Min is rounded down, max is rounded up
  ungroup()%>%
  group_by(year,month,day)%>%
  mutate(diff=(lag(min_round)-max_round)/2)%>% # split difference between the pulse groups
  mutate(plusmax=floor(diff))%>% # round to prevent overlap
  mutate(minusmin=floor(lead(diff)))%>% # round to prevent overlap
  replace_na(list(plusmax=0,minusmin=0))%>% # change NAs to 0
  mutate(min_final=min_round-minusmin,max_final=max_round+plusmax)%>% # add/subtract appropriate terms
  mutate(lagmin=lag(min_final))%>% # create a lag column of min values to compare to max
  mutate(max1=replace(max_final,lagmin==max_final,1))%>% # any point where lagmin and lagmax match, put in a 1
  mutate(max1=replace(max1,max1==max_final,0))%>% # 0 for all others
  mutate(max_final2=max_final-max1)%>% # subtract 1 from max final (gets rid of any remaining overlap)
  select(-diff,-plusmax,-minusmin,-max_final,-lagmin,-max1)%>% #get rid of useless columns
  mutate(adjustmax=ceiling(2*sd))%>% # adjust maximum value to be 2 SDs for pulse 1
  mutate(adjustmax=replace(adjustmax,dummy_pulse!=1,0))%>% # applies the above function to only pulse 1
  mutate(adjustmin=ceiling(2*sd))%>% # adjust maximum value to be 2 SDs for last pulse
  mutate(adjustmin=replace(adjustmin,dummy_pulse!=max(dummy_pulse),0))%>% # applies above function to last pulse only
  mutate(max_final3=adjustmax+max_final2,
         min_final3=min_final-adjustmin)%>% # create final max and min columns
  select(year,month,day,trip,dummy_pulse,year,cohort,min_final3,max_final3)%>% # select final columns
  rename(min=min_final3,max=max_final3) # rename to be min and max
# use min and max for each pulse to then create a dataframe with all length possibilities per pulse
assign.test1<-data.frame(trip=rep(test1$trip,test1$max-test1$min+1),
                         year=rep(test1$year,test1$max-test1$min+1),
                         month=rep(test1$month,test1$max-test1$min+1),
                         day=rep(test1$day,test1$max-test1$min+1),
                         cohort=rep(test1$cohort,test1$max-test1$min+1),
                         pulse=rep(test1$dummy_pulse,test1$max-test1$min+1),
                         mmSL=unlist(mapply(seq,test1$min,test1$max)))
assign.test1<-select(assign.test1,-year,-month,-day)
df1<-length%>%filter(age==1 & year ==2008)%>%select(-pulse)%>%
  filter(trip==10 | trip == 12 | trip ==13 | trip ==16 | trip ==18)
unique(mixtures$trip)
# assign pulses to subsetted age1 length data
length.test1<-left_join(df1,assign.test1)
View(length.test1)
# total percent not accounted for
length.test1$trip<-as.factor(length.test1$trip)
t3<-length.test1%>%
  filter(!is.na(pulse))%>%
  group_by(trip)%>%
  summarise(assigned=n())
t4<-length.test1%>%
  filter(is.na(pulse))%>%
  group_by(trip)%>%
  summarise(na=n())
check2<-left_join(t3,t4)%>%
  mutate(freq=na/assigned)

check1
check2

