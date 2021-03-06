## ----dietData,size='small'-----------------------------------------------
dietData <- read.csv("dietData.csv")
levels(dietData$diet)


## ----reorder,size='small'------------------------------------------------
levels(dietData$diet) <- list(Control="Control", Low="Low",
                              Med="Med", High="High")
levels(dietData$diet)


## ----plot-wt,echo=TRUE,fig.show="hide"-----------------------------------
plot(weight ~ age, dietData)


## ----bp,echo=TRUE,fig.show="hide"----------------------------------------
boxplot(weight ~ diet, dietData, ylab="Weight")


## ----contr,echo=FALSE,size='scriptsize'----------------------------------
options(contrasts=c("contr.treatment", "contr.poly"))

## ----fm1, size='scriptsize'----------------------------------------------
fm1 <- lm(weight ~ age, dietData)
summary(fm1)


## ----pred1,size='footnotesize'-------------------------------------------
age <- dietData$age
predData1 <- data.frame(age=seq(min(age), max(age), length=50)) 
pred1 <- predict(fm1, newdata=predData1, se.fit=TRUE,
              interval="confidence")
predictions1 <- data.frame(pred1$fit, predData1)


## ----plot-fm1,fig.show='hide',size='scriptsize'--------------------------
plot(weight ~ age, data=dietData)                      # raw data
lines(fit ~ age, data=predictions1, lwd=2)             # fitted line
lines(lwr ~ age, data=predictions1, lwd=2, col="gray") # lower CI
lines(upr ~ age, data=predictions1, lwd=2, col="gray") # upper CI


## ----contr2,size='small'-------------------------------------------------
options(contrasts=c("contr.sum", "contr.poly"))
fm2 <- lm(weight ~ diet, dietData)
summary.aov(fm2)


## ----summary-aov,size='small'--------------------------------------------
summary(aov(weight ~ diet, dietData))






## ----summary-fm2,size='tiny'---------------------------------------------
summary(fm2)


## ----pred2---------------------------------------------------------------
predData2 <- data.frame(diet=levels(dietData$diet))
pred2 <- predict(fm2, newdata=predData2,
                 se.fit=TRUE, interval="confidence")


## ----predDat2------------------------------------------------------------
predictions2 <- data.frame(pred2$fit, SE=pred2$se, predData2)
predictions2

## ----barplot1,fig.show="hide",echo=FALSE---------------------------------
bp <- barplot(predictions2$fit, xlab="Diet", ylab="Weight",
              ylim=c(20, 30), xpd=FALSE,
              names.arg=levels(dietData$diet))
box()
arrows(bp, predictions2$fit, bp,
       predictions2$fit+predictions2$SE, code=3, angle=90)


## ----contr3--------------------------------------------------------------
options(contrasts=c("contr.sum", "contr.poly"))


## ----center-age----------------------------------------------------------
dietData$ageCentered <- dietData$age - mean(dietData$age)


## ----fm3,size='tiny'-----------------------------------------------------
fm3 <- lm(weight ~ ageCentered + diet, dietData)


## ----summary-fm3,size='tiny'---------------------------------------------
summary(fm3)


## ----summary-aov3--------------------------------------------------------
summary.aov(fm3)


## ----predict-age,size='small'--------------------------------------------
ageC <- dietData$ageCentered
predData3 <- data.frame(
    diet=rep(c("Control", "Low", "Med", "High"), each=20),
    ageCentered=rep(seq(min(ageC), max(ageC),
                          length=20),
                      times=4))
pred3 <- predict(fm3, newdata=predData3, se.fit=TRUE,
                 interval="confidence")
predictions3 <- data.frame(pred3$fit, predData3)


## ----scatplot0,fig.show='hide',echo=FALSE,size='footnotesize'------------
colrs <- c("black", "royalblue", "orange", "darkcyan")
plot(weight ~ ageCentered, dietData, cex=1.2,
     pch=rep(15:18, each=15), 
     col=rep(colrs, each=15))

## ----scatplot1,fig.show='hide',size='footnotesize'-----------------------
colrs <- c("black", "royalblue", "orange", "darkcyan")
plot(weight ~ ageCentered, dietData, cex=1.2,
     pch=rep(15:18, each=15),
     col=rep(colrs, each=15))
lines(fit ~ ageCentered, predictions3, subset=diet=="Control",
      col=colrs[1], lwd=2)
lines(fit ~ ageCentered, predictions3, subset=diet=="Low", lty=1,
      col=colrs[2], lwd=2)
lines(fit ~ ageCentered, predictions3, subset=diet=="Med", lty=1,
      col=colrs[3], lwd=2)
lines(fit ~ ageCentered, predictions3, subset=diet=="High", lty=1,
      col=colrs[4], lwd=2)
legend(4, 23, c("High", "Med", "Low", "Control"), pch=18:15,
       title="Diet", lwd=2, col=rev(colrs))


## ----multcomp,eval=TRUE--------------------------------------------------
## install.packages("multcomp")
library(multcomp)
summary(glht(fm3, linfct=mcp(diet="Tukey")))

