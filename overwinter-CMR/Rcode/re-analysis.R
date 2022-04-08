# copmarison analysis to test the fall only data to full data (to verify fall estimates)

# ---- format for RMark ----

# ---- Fall 1 ----
mrkdata1<-mrkdata%>%
  select(-year,-month)%>%
  filter(date!="2016-10-19")

# melt then cast, equivalent to gather then spread

mrkdata2<-spread(mrkdata1,date,mark,fill=0)

mrkdata3<-unite(mrkdata2,ch,c("2016-10-14","2017-05-24"),
                sep="",remove=TRUE)
head(mrkdata3)
unique(mrkdata3$ch)
str(mrkdata3)
duplicated(mrkdata3$animal_id)
cod<-mrkdata3

head(cod)

# 
# determine the proportion of fish caught (10) for each pulse for entire sound and apply the ratio to the unmeasured fish
# week 1
cod%>%filter(ch=="10")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(N=n())%>%
  mutate(ratio=N/52)

W1<-cod%>%filter(ch=="10")%>%
  filter(is.na(sl))%>% #88 unassigned fish
  select(-pulse)%>% # remove current pulse column
  add_column(pulse=c(rep(1, each=39),rep(2,each=46),rep(3,each=3))) # assign pulses to unmeasured fish based on ratio

# summary of week 1
W1%>%
  group_by(pulse)%>%
  summarise(n())
cod%>%filter(ch=="10")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl),mean(sl), sd(sl))

88*0.442 # pulse 1
88*0.519 # pulse 2
88*0.0385 # pulse 3
39+46+3



non.SL<-bind_rows(W1)

#add full pulse structure to dataset
cod<-cod%>%
  filter(!is.na(pulse))%>%
  bind_rows(non.SL)

# ---- CJS Run ----
# Newbridge only
# ---- pulses 1:3 ----
# Take out late pulses (the ones that were not marked)
nb<-cod[str_detect(cod$animal_id,"NB"),]
nb.pulse<-nb%>%
  select(ch,pulse)%>%
  data.frame()
unique(nb.pulse$pulse)
nb.pulse$pulse<-as.factor(nb.pulse$pulse)
str(nb.pulse)
nb13<-nb.pulse%>%
  filter(pulse!=4)%>%
  as.data.frame()

early.pulse.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb13,time.intervals = c(223),
                             groups="pulse")
  nb.ddl<-make.design.data(nb.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  #Phi.time<-list(formula=~time)
  Phi.pulse<-list(formula=~pulse)
  #Phi.timepluspulse<-list(formula=~time+pulse)
  
  # define models for p
  p.dot<-list(formula=~1)
  #p.time<-list(formula=~time)
  #p.pulse<-list(formula=~pulse) # biologically is moot, we know that we can capture all pulses in our net if they are present
  #p.timepluspulse<-list(formula=~time+pulse)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=nb.processed,ddl=nb.ddl))
}
early.pulse.results<-early.pulse.model()
early.pulse.results
early.pulse.results[[2]]
# ---- Fall 2 -----
# ---- format for RMark ----
mrkdata%>%distinct(date)
mrkdata1<-mrkdata%>%
  select(-year,-month)%>%
  filter(date!="2016-10-14")

# melt then cast, equivalent to gather then spread

mrkdata2<-spread(mrkdata1,date,mark,fill=0)

mrkdata3<-unite(mrkdata2,ch,c("2016-10-19","2017-05-24"),
                sep="",remove=TRUE)
head(mrkdata3)
unique(mrkdata3$ch)
str(mrkdata3)
duplicated(mrkdata3$animal_id)
cod<-mrkdata3

head(cod)

# 
# determine the proportion of fish caught (10) for each pulse for entire sound and apply the ratio to the unmeasured fish
#week 2
cod%>%filter(ch=="10")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(N=n())%>%
  mutate(ratio=N/29)

W2<-cod%>%filter(ch=="10")%>%
  filter(is.na(sl))%>% #49 unassigned fish
  select(-pulse)%>% # remove current pulse column
  add_column(pulse=c(rep(1, each=24),rep(2,each=25))) # assign pulses to unmeasured fish based on ratio
#summary of week 2
W2%>%
  group_by(pulse)%>%
  summarise(n())
cod%>%filter(ch=="10")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl),mean(sl), sd(sl))
49*0.483 # pulse 1
49*0.517 # pulse 2
24+25

non.SL<-bind_rows(W2)


#add full pulse structure to dataset
cod<-cod%>%
  filter(!is.na(pulse))%>%
  bind_rows(non.SL)

# ---- CJS Run ----
# Newbridge only
# ---- pulses 1:3 ----
# Take out late pulses (the ones that were not marked)
nb<-cod[str_detect(cod$animal_id,"NB"),]
nb.pulse<-nb%>%
  select(ch,pulse)%>%
  data.frame()
unique(nb.pulse$pulse)
nb.pulse$pulse<-as.factor(nb.pulse$pulse)
str(nb.pulse)
nb13<-nb.pulse%>%
  filter(pulse!=4)%>%
  as.data.frame()

early.pulse.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb13,time.intervals = c(217),
                             groups="pulse")
  nb.ddl<-make.design.data(nb.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  #Phi.time<-list(formula=~time)
  Phi.pulse<-list(formula=~pulse)
  #Phi.timepluspulse<-list(formula=~time+pulse)
  
  # define models for p
  p.dot<-list(formula=~1)
  #p.time<-list(formula=~time)
  #p.pulse<-list(formula=~pulse) # biologically is moot, we know that we can capture all pulses in our net if they are present
  #p.timepluspulse<-list(formula=~time+pulse)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=nb.processed,ddl=nb.ddl))
}
early.pulse.results<-early.pulse.model()
early.pulse.results
early.pulse.results[[2]]


# --- single fall event -----
mrkdata%>%distinct(date)
mrkdata1<-mrkdata%>%
  select(-year,-month)%>%
  mutate(date=replace(date,date=="2016-10-14","2016-10-19"))

# melt then cast, equivalent to gather then spread

mrkdata2<-spread(mrkdata1,date,mark,fill=0)

mrkdata3<-unite(mrkdata2,ch,c("2016-10-19","2017-05-24"),
                sep="",remove=TRUE)
head(mrkdata3)
unique(mrkdata3$ch)
str(mrkdata3)
duplicated(mrkdata3$animal_id)
cod<-mrkdata3

head(cod)

# 
# determine the proportion of fish caught (10) for each pulse for entire sound and apply the ratio to the unmeasured fish
# week 1
cod%>%filter(ch=="10")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(N=n())%>%
  mutate(ratio=N/52)

W1<-cod%>%filter(ch=="10")%>%
  filter(is.na(sl))%>% #88 unassigned fish
  select(-pulse)%>% # remove current pulse column
  add_column(pulse=c(rep(1, each=39),rep(2,each=46),rep(3,each=3))) # assign pulses to unmeasured fish based on ratio

# summary of week 1
W1%>%
  group_by(pulse)%>%
  summarise(n())
cod%>%filter(ch=="10")%>%
  filter(!is.na(sl))%>%
  group_by(pulse)%>%
  summarise(min(sl),max(sl),mean(sl), sd(sl))

88*0.442 # pulse 1
88*0.519 # pulse 2
88*0.0385 # pulse 3
39+46+3



non.SL<-bind_rows(W1)

#add full pulse structure to dataset
cod<-cod%>%
  filter(!is.na(pulse))%>%
  bind_rows(non.SL)

# ---- CJS Run ----
# Newbridge only
# ---- pulses 1:3 ----
# Take out late pulses (the ones that were not marked)
nb<-cod[str_detect(cod$animal_id,"NB"),]
nb.pulse<-nb%>%
  select(ch,pulse)%>%
  data.frame()
unique(nb.pulse$pulse)
nb.pulse$pulse<-as.factor(nb.pulse$pulse)
str(nb.pulse)
nb13<-nb.pulse%>%
  filter(pulse!=4)%>%
  as.data.frame()

early.pulse.model<-function()
{
  # process data for CJS model and make default design data
  nb.processed<-process.data(nb13,time.intervals = c(223),
                             groups="pulse")
  nb.ddl<-make.design.data(nb.processed)
  # define models for Phi
  Phi.dot<-list(formula=~1)
  #Phi.time<-list(formula=~time)
  Phi.pulse<-list(formula=~pulse)
  #Phi.timepluspulse<-list(formula=~time+pulse)
  
  # define models for p
  p.dot<-list(formula=~1)
  #p.time<-list(formula=~time)
  #p.pulse<-list(formula=~pulse) # biologically is moot, we know that we can capture all pulses in our net if they are present
  #p.timepluspulse<-list(formula=~time+pulse)
  # create model list
  cml<-create.model.list("CJS")
  # run and return models
  return(mark.wrapper(cml,data=nb.processed,ddl=nb.ddl))
}
early.pulse.results<-early.pulse.model()
early.pulse.results
