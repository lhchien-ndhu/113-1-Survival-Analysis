## https://www.randomforestsrc.org/articles/survival.html

# 安裝套件
install.packages("randomForestSRC")

# 加載套件
library(randomForestSRC)

# 加載示例資料
data(peakVO2, package = "randomForestSRC")
dta <- peakVO2



# 構建生存隨機森林模型
obj <- rfsrc(Surv(ttodead,died)~., dta,
             ntree = 1000, nodesize = 5, nsplit = 50, importance = TRUE)

# 查看模型結果
print(obj)

get.cindex(obj$yvar[,1], obj$yvar[,2], obj$predicted.oob)

# tree size
plot(obj$err.rate) 

# prediction
newdata <- data.frame(lapply(1:ncol(obj$xvar),function(i){median(obj$xvar[,i])}))
colnames(newdata) <- obj$xvar.names
newdata1 <- newdata2 <- newdata
newdata1[,which(obj$xvar.names == "peak.vo2")] <- quantile(obj$xvar$peak.vo2, 0.25)
newdata2[,which(obj$xvar.names == "peak.vo2")] <- quantile(obj$xvar$peak.vo2, 0.75)
newdata <- rbind(newdata1,newdata2)
y.pred <- predict(obj,newdata = rbind(newdata,obj$xvar)[1:2,])

#pdf("survival.pdf", width = 10, height = 8)
par(cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0, mar = c(6.0,6,1,1), mgp = c(4, 1, 0))
plot(round(y.pred$time.interest,2),y.pred$survival[1,], type="l", xlab="Time (Year)",   
     ylab="Survival", col=1, lty=1, lwd=2)
lines(round(y.pred$time.interest,2), y.pred$survival[2,], col=2, lty=2, lwd=2)
legend("topright", legend=c("Peak VO2=12.8","Peak VO2=19.3"), col=c(1:2), lty=1:2, cex=2, lwd=2)
dev.off() 


# variable importance
jk.obj <- subsample(obj) #jackknife
#pdf("VIMPsur.pdf", width = 15, height = 20)
par(oma = c(0.5, 10, 0.5, 0.5))
par(cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0, mar = c(6.0,17,1,1), mgp = c(4, 1, 0))
plot(jk.obj, xlab = "Variable Importance (x 100)", cex = 1.2)
#dev.off()
