library(KMsurv)
library(survival)

data(channing)
?channing

attach(channing)

head(channing)
surv.object<-Surv(ageentry,age,death)
sort(age-ageentry)
surv.object[time==0]

r.left<-survfit(Surv(ageentry,age,death)~1,data=channing)
summary(r.left)

min(ageentry)/12

plot(r.left,,xscale=12)

r.left.68<-survfit(Surv(ageentry,age,death)~1,data=channing[ageentry>=(68*12),])
summary(r.left.68)
plot(r.left.68,,xscale=12)
abline(v=68*12)

r.left.80<-survfit(Surv(ageentry,age,death)~1,data=channing[ageentry>=(80*12),])
summary(r.left.80)
plot(r.left.80,,xscale=12)
abline(v=80*12)