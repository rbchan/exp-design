## ----plantData,size='scriptsize'-----------------------------------------
plantData <- read.csv("plantData.csv")
plantData$plant <- factor(plantData$plant)
plantData$week <- factor(plantData$week)
str(plantData)

## ----headPlantData,size='scriptsize'-------------------------------------
head(plantData, n=8)

## ----plotDataLines,echo=FALSE,fig.width=7,fig.height=5.5-----------------
matplot(matrix(plantData$leaves, 5, 10), type="l",
        col=rep(c(1,4), each=5), lwd=2,
        lty=rep(c(1,2), each=5),
        xlab="Week", ylab="Leaves", cex.lab=1.4)
legend(1,14,c("High fertilizer", "Low fertilizer"),
       lty=c(2,1), col=c(4,1), lwd=2)

## ----aov1,size='scriptsize'----------------------------------------------
aov1 <- aov(leaves ~ fertilizer*week + Error(plant),
            data=plantData)

## ----aov1summary,size='scriptsize'---------------------------------------
summary(aov1)

## ----plantData2,size='footnotesize'--------------------------------------
plantData2 <- reshape(plantData, idvar="plant",
                      timevar="week", v.names="leaves",
                      direction="wide")

## ----plantData2print,size='scriptsize'-----------------------------------
plantData2

## ----manova1,size='scriptsize'-------------------------------------------
manova1 <- manova(cbind(leaves.1, leaves.2, leaves.3,
                        leaves.4, leaves.5) ~ fertilizer,
                  data=plantData2)

## ----manova1anova,size='scriptsize'--------------------------------------
anova(manova1, X=~1, test="Spherical")

## ----mauchly,size='scriptsize'-------------------------------------------
mauchly.test(manova1, X=~1)

## ----compare1,size='tiny'------------------------------------------------
summary(aov1)

## ----compare2,size='tiny'------------------------------------------------
anova(manova1, X=~1, test="Spherical")

## ----wilks,size='footnotesize'-------------------------------------------
anova(manova1, X=~1, test="Wilks")

## ----pillai,size='footnotesize'------------------------------------------
anova(manova1, X=~1, test="Pillai")

## ----manova2,size='footnotesize'-----------------------------------------
manova2 <- manova(
    cbind(leaves.2-leaves.1, leaves.3-leaves.2,
          leaves.4-leaves.3, leaves.5-leaves.4) ~
    fertilizer, data=plantData2)

## ----profile,size='scriptsize'-------------------------------------------
summary.aov(manova2)

## ----plotit,size='footnotesize'------------------------------------------
leavesMat <- plantData2[,3:7]
growthMat <- leavesMat[,2:5] - leavesMat[,1:4]
colnames(growthMat) <- paste("interval", 1:4, sep=".")
(lowFertilizer <- colMeans(growthMat[1:5,]))
(highFertilizer <- colMeans(growthMat[6:10,]))

## ----SEs,size='footnotesize'---------------------------------------------
SE <- sqrt(diag(stats:::vcov.mlm(manova2)))
SE <- SE[names(SE)==":(Intercept)"] # Only use "intercept" SEs
unname(SE) ## Ignore the names

## ----gr,fig.show='hide',fig.width=7,fig.height=5.5,size='small'----------
plot(1:4-0.05, lowFertilizer, type="b", xlim=c(0.9, 4.1),
     ylim=c(-1, 4), xaxp=c(1,4,3), cex.lab=1.5,
     xlab="Time interval", ylab="Growth rate (leaves/week)")
abline(h=0, lty=3)
arrows(1:4-.05, lowFertilizer-SE, 1:4-.05, lowFertilizer+SE,
       angle=90, code=3, length=0.05)
lines(1:4+0.05, highFertilizer, type="b", pch=17, col=4)
arrows(1:4+0.05, highFertilizer-SE, 1:4+0.05,
       highFertilizer+SE, angle=90, code=3, length=0.05)
legend(1, 4, c("Low fertilizer", "High fertilizer"),
       col=c("black", "blue"), pch=c(1,17))

