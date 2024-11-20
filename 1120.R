library(survival)

fit <- coxph(Surv(futime, fustat) ~ rx + age + ecog.ps + resid.ds, data=ovarian) 
summary(fit)

## AIC Backward variable selection
drop1(fit)
step(fit)

## local test: wald test

coef(fit) #b
fit$var   #Var_hat(b)

# H0: beta_1=beta_2=beta_3=beta_4=0
t(as.matrix(coef(fit)))%*%solve(fit$var)%*%(as.matrix(coef(fit)))
fit$wald.test

# H0: beta_1 =0 
coef(fit)[1]
stat.w <- coef(fit)[1]^2/fit$var[1,1]
pchisq(stat.w,df=1,lower.tail=FALSE)

sqrt(stat.w)

summary(fit)

# H0: beta_1=beta_2=0
coe_2<-as.matrix(coef(fit)[1:2])
var_2<-fit$var[1:2,1:2]
stat.w2 <- t(coe_2)%*%solve(var_2)%*%(coe_2)
pchisq(stat.w2,df=2,lower.tail=FALSE)

## local test: likelihood ratio test, anova.coxph()

# H0: beta_1=beta_2=beta_3=beta_4=0
fit0 <- coxph(Surv(futime, fustat) ~ 1, data=ovarian)
anova(fit0,fit)
fit

# H0: beta_1 = beta_2 = 0 
fit1 <- coxph(Surv(futime, fustat) ~ ecog.ps+resid.ds, data=ovarian)
anova(fit1,fit)

# H0: beta_1 = beta_3 = beta_4 = 0
#  Wald test
coe_3<-as.matrix(coef(fit)[-2])
var_3<-fit$var[-2,-2]
stat.w3 <- t(coe_3)%*%solve(var_3)%*%(coe_3)
pchisq(stat.w3,df=3,lower.tail=FALSE)

#  LRT
fit2<-coxph(Surv(futime, fustat) ~ age, data=ovarian)
anova(fit2,fit)


## martingale residuals
fit$residuals
?coxph

## mar = delta - cox-snell res
res.cs<- fit$residuals - ovarian$fustat
