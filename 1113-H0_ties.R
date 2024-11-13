#############
### ties:
###  efron: default, more accurate when dealing with tied death times, and is as efficient computationally
###  breslow: breslow's likelihood
###  exact: discrete likelihood
#############

library(KMsurv)
data(kidney)

kidney
?kidney

summary(survfit(Surv(time,delta)~1,data=kidney))

r.efron<-coxph(Surv(time,delta)~type,data=kidney)
summary(r.efron)
r.efron$loglik

r.breslow<-coxph(Surv(time,delta)~type,data=kidney,ties="breslow")
summary(r.breslow)
r.breslow$loglik

r.exact<-coxph(Surv(time,delta)~type,data=kidney,ties="exact")
summary(r.exact)
r.exact$loglik

system.time(coxph(Surv(time,delta)~type,data=kidney))
system.time(coxph(Surv(time,delta)~type,data=kidney,ties="breslow"))
system.time(coxph(Surv(time,delta)~type,data=kidney,ties="exact"))

####################
### Baseline hazard: basehaz (reference group: mean)
###   produce cumulative hazard function, use the Breslow estimator
#####################

r<-coxph(Surv(time,delta)~type,data=kidney)
basehaz(r)


######################
### predict
###  type="lp": linear term
###  type = "expected": expected number of events given the covariates and follow-up time
###  type = "survival": exp(-expected)
######################

length(predict(r))
dim(kidney)

#H0 <- basehaz(r) 
#LP <- predict(r, type="lp") 
#H0[, 1]*exp(LP[kidney$delta==1])

head(kidney)

head(predict(r, type="survival") )
head(H0 <- basehaz(r) )
head(LP <- predict(r, type="lp") )

H0.data<-H0[match(kidney$time,H0[,2]),1]
head(exp(-H0.data*exp(LP)))

head(exp(-predict(r, type="expected")))

##############
## basehaz:
##  centered = FALSE
#############

head(kidney)

head(H0_2 <- basehaz(r,centered=FALSE) )
head(predict(r, type="survival") )
exp(-H0_2[2,1]*exp(r$coefficients*kidney[1,3]))

head(predict(r, type="survival") )


