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
df=data.frame(ch=c("100","010","001"),freq=rep(1000,3),stringsAsFactors=FALSE)
# note: marked uses 4 values and throws out the last release which doesn't contain any information for cjs
dp=marked::process.data(df,model="HMMcjs",time.intervals = c(5,217))
ddl=marked::make.design.data(dp)
# fix p to 0 for 2003-2009,2011
#ddl$p$fix=ifelse(ddl$p$time%in%(c(2003:2009,2011)),0,NA)
# simulate data
# select Phi and p
logit(0.1)
logit(0.95) # something is off with p.

data=simHMM(dp,ddl,model.parameters = list(Phi=list(formula=~1),p=list(formula=~1)),
            initial = list(Phi=c(-2.197225),p=c(2.944439)))
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




