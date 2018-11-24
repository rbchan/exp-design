## ----swissData,size='small'----------------------------------------------
swissData <- read.csv("swissData.csv")
head(swissData, n=11)

## ----four-lms------------------------------------------------------------
fm1 <- lm(sppRichness ~ forest, data=swissData)
fm2 <- lm(sppRichness ~ elevation, data=swissData)
fm3 <- lm(sppRichness ~ forest + elevation +
          water, data=swissData)
fm4 <- lm(sppRichness ~ forest + elevation +
          I(elevation^2) + water, data=swissData)

## ----fm4,size='scriptsize'-----------------------------------------------
summary(fm4)

## ----anova-fm4,size='scriptsize'-----------------------------------------
summary.aov(fm4)

## ----n,size='footnotesize'-----------------------------------------------
n <- nrow(swissData)

## ----rss,size='footnotesize'---------------------------------------------
logL <- c(logLik(fm1), logLik(fm2), logLik(fm3), logLik(fm4))

## ----K,size='footnotesize'-----------------------------------------------
K <- c(3, 3, 5, 6)

## ----aic,size='footnotesize'---------------------------------------------
AIC <- -2*logL + 2*K

## ----delta,size='footnotesize'-------------------------------------------
delta <- AIC - min(AIC)

## ----w,size='footnotesize'-----------------------------------------------
w <- exp(-0.5*delta)/sum(exp(-0.5*delta))

## ----ms,size='footnotesize'----------------------------------------------
ms <- data.frame(logL, K, AIC, delta, w)
rownames(ms) <- c("fm1", "fm2", "fm3", "fm4")
round(ms, digits=2)

## ----ms-order,size='footnotesize'----------------------------------------
ms <- ms[order(ms$AIC),]
round(ms, digits=2)

## ----AIC-fn--------------------------------------------------------------
AIC(fm1, fm2, fm3, fm4)

## ----predData1,size='scriptsize'-----------------------------------------
predData1 <- data.frame(elevation=1000, forest=25, water="No")

## ----E1,size='scriptsize'------------------------------------------------
E1 <- predict(fm1, newdata=predData1, type="response")
as.numeric(E1) # remove names (optional)

## ----E2,size='scriptsize'------------------------------------------------
E2 <- predict(fm2, newdata=predData1, type="response")
as.numeric(E2)

## ----E3,size='scriptsize'------------------------------------------------
E3 <- predict(fm3, newdata=predData1, type="response")
as.numeric(E3)

## ----E4,size='scriptsize'------------------------------------------------
E4 <- predict(fm4, newdata=predData1, type="response")
as.numeric(E4)

## ----ma------------------------------------------------------------------
E1*w[1] + E2*w[2] + E3*w[3] + E4*w[4]

## ----Emat----------------------------------------------------------------
predData2 <- data.frame(forest=seq(0, 100, length=50),
                        elevation=1000, water="No")
E1 <- predict(fm1, newdata=predData2)
E2 <- predict(fm2, newdata=predData2)
E3 <- predict(fm3, newdata=predData2)
E4 <- predict(fm4, newdata=predData2)
Emat <- cbind(E1, E2, E3, E4)

## ----Evec----------------------------------------------------------------
Evec <- Emat %*% w

## ----reglines,fig.show='hide',fig.width=8,fig.height=6,size='tiny'-------
plot(sppRichness~forest, data=swissData, xlab="Forest cover", ylab="Species richness", cex.lab=1.5)
lines(E1 ~ forest, predData2, col="lightgreen", lwd=4)
lines(E2 ~ forest, predData2, col="orange", lwd=3)
lines(E3 ~ forest, predData2, col="purple", lwd=2)
lines(E4 ~ forest, predData2, col="red", lwd=1)
lines(Evec ~ forest, predData2, col=rgb(0,0,1,0.2), lwd=10)
legend(60, 30, c("Model 1","Model 2","Model 3","Model 4","Model averaged"), lty=1, cex=1.2,
       lwd=c(4,3,2,1,10), col=c("lightgreen", "orange", "purple", "red", rgb(0,0,1,0.2)))

