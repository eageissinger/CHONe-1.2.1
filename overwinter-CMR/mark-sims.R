library(marked)
library(RMark)

#
# simulate data with simHMM in marked package
#
# create 3 marking periods
df=data.frame(ch=c("100","010","001"),freq=rep(500,3),stringsAsFactors=FALSE)
# note: marked uses 4 values and throws out the last release which doesn't contain any information for cjs
dp=marked::process.data(df,model="HMMcjs",time.intervals = c(5,217))
ddl=marked::make.design.data(dp)
# fix p to 0 for 2003-2009,2011
#ddl$p$fix=ifelse(ddl$p$time%in%(c(2003:2009,2011)),0,NA)
# simulate data
data=simHMM(dp,ddl,model.parameters = list(Phi=list(formula=~1),p=list(formula=~time)),
            initial = list(Phi=c(4.637),p=c(-4.923,30.922)))
#
# fit model in RMark
#
# RMark does not accept "," in capture history which marked uses as defaults so remove those
markdata=data
markdata$ch=apply(marked::splitCH(markdata$ch),1,paste,collapse="")
# process data and create design data with RMark functions
dp=RMark::process.data(markdata,time.intervals = c(5,217))
ddl=RMark::make.design.data(dp)
# fix p=0 for the years with no sampling
#ddl$p$fix=ifelse(ddl$p$time%in%(c(2003:2009,2011)),0,NA)
# fit model and show coefficients
model=RMark::mark(dp,ddl)
coef(model)
summary(model)

# fit model with marked
dp=marked::process.data(data,time.intervals = c(5,217))
ddl=marked::make.design.data(dp)
# fix p=0 for the years with no sampling
#ddl$p$fix=ifelse(ddl$p$time%in%(c(2003:2009,2011)),0,NA)
model=crm(dp,ddl)
coef(model)

#----calculate Phi and p from probability -----
logit<-function(p) {
  log(p/(1-p))
}
logit(0.09333333)

#---- Hypothesis 1 ----
# simulate data with simHMM in marked package
#
# create 3 marking periods
df=data.frame(ch=c("100","010","110","101","011","001","111"),freq=rep(1000,7),stringsAsFactors=FALSE)
# note: marked uses 4 values and throws out the last release which doesn't contain any information for cjs
dp=marked::process.data(df,model="hmmCJS",time.intervals = c(5,217))
ddl=marked::make.design.data(dp)
# fix p to 0 for 2003-2009,2011
#ddl$p$fix=ifelse(ddl$p$time%in%(c(2003:2009,2011)),0,NA)
# simulate data
# select Phi and p
logit(0.99)
logit(0.007221) # something is off with p.
logit(0.99999)

data=simHMM(dp,ddl,model.parameters = list(Phi=list(formula=~1),p=list(formula=~time)),
            initial = list(Phi=c(4.59512),p=c(-4.923515)))
#
# fit model in RMark
#
# RMark does not accept "," in capture history which marked uses as defaults so remove those
markdata=data
markdata$ch=apply(marked::splitCH(markdata$ch),1,paste,collapse="")
# process data and create design data with RMark functions
dp=RMark::process.data(markdata,time.intervals = c(5,217))
ddl=RMark::make.design.data(dp)
# fix p=0 for the years with no sampling
#ddl$p$fix=ifelse(ddl$p$time%in%(c(2003:2009,2011)),0,NA)
# fit model and show coefficients
model=RMark::mark(dp,ddl)
coef(model)
summary(model)


# ---- simulation with pulse structure -----

# simulate data with simHMM in marked package
#
# create 3 marking periods
df=data.frame(ch=c("100","100","100","100","010","010","010","010","001","001","001","001",
                   "110","110","110","110","101","101","101","101","011","011","011","011",
                   "111","111","111","111"), 
              pulse=factor(c("1","2","3","4")),
              freq=c(2000,1000,500,100),stringsAsFactors=FALSE)
# note: marked uses 4 values and throws out the last release which doesn't contain any information for cjs
dp=marked::process.data(df,model="hmmCJS",time.intervals = c(5,217),groups="pulse")
ddl=marked::make.design.data(dp)
# fix p to 0 for 2003-2009,2011
#ddl$p$fix=ifelse(ddl$p$time%in%(c(2003:2009,2011)),0,NA)
# simulate data
# select Phi and p
logit(0.99)
logit(0.007221) # something is off with p.
logit(0.99999)

data=simHMM(dp,ddl,model.parameters = list(Phi=list(formula=~time),p=list(formula=~pulse)),
            initial = list(Phi=c(0.8737695,20.6672290),p=c(-1.8035378,2.1367641,-36.1800360,-0.0999900)))
#
# fit model in RMark
#
# RMark does not accept "," in capture history which marked uses as defaults so remove those
markdata=data
markdata$ch=apply(marked::splitCH(markdata$ch),1,paste,collapse="")
# process data and create design data with RMark functions
dp=RMark::process.data(markdata,model="CJS",time.intervals = c(5,217),groups="pulse")
ddl=RMark::make.design.data(dp)
# fix p=0 for the years with no sampling
#ddl$p$fix=ifelse(ddl$p$time%in%(c(2003:2009,2011)),0,NA)
# fit model and show coefficients
model=RMark::mark(dp,ddl)
coef(model)
summary(model)

# ---- Hypotheses simms -----

# simulate data with simHMM in marked package
#
# create 3 marking periods
df=data.frame(ch=c("100","100","100","100","010","010","010","010","001","001","001","001",
                   "110","110","110","110","101","101","101","101","011","011","011","011",
                   "111","111","111","111"), 
              pulse=factor(c("1","2","3","4")),
              freq=c(2000,1000,500,100),stringsAsFactors=FALSE)
# note: marked uses 4 values and throws out the last release which doesn't contain any information for cjs
dp=marked::process.data(df,model="hmmCJS",time.intervals = c(5,217),groups="pulse")
ddl=marked::make.design.data(dp)
# fix p to 0 for 2003-2009,2011
#ddl$p$fix=ifelse(ddl$p$time%in%(c(2003:2009,2011)),0,NA)
# simulate data
# select Phi and p
logit(0.531441)
logit(6.258*10^(-11)) # something is off with p.
logit(1e-06)
logit(1e-223)

data=simHMM(dp,ddl,model.parameters = list(Phi=list(formula=~time),p=list(formula=~time)),
            initial = list(Phi=c(0.1259302,-23.49458),p=c(-13.81551,-513.4765)))
#
# fit model in RMark
#
# RMark does not accept "," in capture history which marked uses as defaults so remove those
markdata=data
markdata$ch=apply(marked::splitCH(markdata$ch),1,paste,collapse="")
# process data and create design data with RMark functions
dp=RMark::process.data(markdata,model="CJS",time.intervals = c(5,217),groups="pulse")
ddl=RMark::make.design.data(dp)
# fix p=0 for the years with no sampling
#ddl$p$fix=ifelse(ddl$p$time%in%(c(2003:2009,2011)),0,NA)
# fit model and show coefficients
model=RMark::mark(dp,ddl)
coef(model)
summary(model)
