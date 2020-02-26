pulse_range<-function(mixtures) {
  ranges<-mixtures%>%
    group_by(year,trip,age,dummy_pulse)%>%
    summarise(min=(mu-sigma),max=mu+sigma,sd=sigma)%>%
    mutate(min_round=floor(min),max_round=ceiling(max))%>%# add rounded min and max to create an integer column. Min is rounded down, max is rounded up
    ungroup()%>%
    group_by(year,trip,age)%>%
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
    select(age,year,trip,dummy_pulse,min_final3,max_final3)%>% # select final columns
    rename(min=min_final3,max=max_final3)%>%
    ungroup()
  return(ranges)
}
