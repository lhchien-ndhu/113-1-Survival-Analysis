# exponential scaling gives a straight line curve for the log(survival)
#  under the parametric exponential model
library(rpart)
require(survival)
pfit <- rpart(Surv(pgtime, pgstat) ~ age + eet + g2 + grade +gleason + ploidy, data = stagec)
print(pfit)
printcp(pfit)

pfit2 <- prune(pfit, cp = 0.016)
par(mar = rep(0.2, 4))
plot(pfit2, uniform = TRUE, branch = 0.4, compress = TRUE)
text(pfit2, use.n = TRUE)

library(rpart.plot)
rpart.plot(pfit2,  nn = TRUE)

temp <- snip.rpart(pfit2, 6)
rpart.plot(temp,nn=T)
km <- survfit(Surv(pgtime, pgstat) ~ temp$where, stagec)
plot(km, lty = 1:4, mark.time = FALSE,
     xlab = "Years", ylab = "Progression")
legend(10, 0.3, paste('node', c(4,5,6,7)), lty = 1:4)
