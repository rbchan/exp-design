## ----sawData,size='scriptsize'-------------------------------------------
sawData <- read.csv("sawData.csv")
sawData

## ----coef,size='scriptsize'----------------------------------------------
ADvBC <- c(1/2, -1/2, -1/2, 1/2)
AvD <- c(1, 0, 0, -1)
BvC <- c(0, 1, -1, 0)

## ----orth1,size='scriptsize'---------------------------------------------
sum(ADvBC)
sum(AvD)
sum(BvC)

## ----orth2,size='scriptsize'---------------------------------------------
sum(ADvBC * AvD)
sum(ADvBC * BvC)
sum(AvD * BvC)

## ----conmat--------------------------------------------------------------
contrast.mat <- cbind(ADvBC, AvD, BvC)
contrast.mat

## ------------------------------------------------------------------------
aov.out <- aov(y ~ Brand, data=sawData,
               contrasts=list(Brand=contrast.mat))

## ------------------------------------------------------------------------
summary(aov.out, split = list(Brand =
                      list("ADvBC"=1, "AvD"=2, "BvC"=3)))

## ----gmeans1,size='scriptsize'-------------------------------------------
(group.means <- tapply(sawData$y, sawData$Brand, mean))

## ----gmeans2,size='scriptsize'-------------------------------------------
group.means <- unname(group.means) # Drop names (optional)
group.means[1] - group.means[4]

## ----gdiff1,size='scriptsize'--------------------------------------------
group.means[2] - group.means[3]

## ----gdiff2,size='scriptsize'--------------------------------------------
mean(group.means[c(1,4)]) - mean(group.means[2:3])

## ----se-con1,size='footnotesize'-----------------------------------------
se.contrast(aov.out, list(sawData$Brand=="A",
                          sawData$Brand=="D"))

## ----se-con2,size='footnotesize'-----------------------------------------
se.contrast(aov.out, list(sawData$Brand=="B",
                           sawData$Brand=="C"))

## ----se-con3,size='footnotesize'-----------------------------------------
se.contrast(aov.out, list(sawData$Brand=="A" |
                          sawData$Brand=="D",
                          sawData$Brand=="B" |
                          sawData$Brand=="C"))

## ----effSE---------------------------------------------------------------
effects.SE <- model.tables(aov.out, type="effects",
                           se=TRUE)
effects.SE

## ----alphas,size='footnotesize'------------------------------------------
# str(effects.SE)
alpha.i <- as.numeric(effects.SE$tables$Brand)
SE <- as.numeric(effects.SE$se)

## ----CIs,size='footnotesize'---------------------------------------------
tc <- qt(0.975, 4*(5-1))
lowerCI <- alpha.i - tc * SE
upperCI <- alpha.i + tc * SE

## ----CIr,size='footnotesize'---------------------------------------------
CI <- data.frame(effect.size=alpha.i, SE,
                 lowerCI, upperCI)
round(CI, 2)

## ----ciplot,fig.show='hide',fig.width=8,fig.height=6,size='tiny'---------
plot(1:4, CI$effect.size, xlim=c(0.5, 4.5), ylim=c(-18, 20), xaxt="n",
     xlab="Brand", ylab="Difference from grand mean", pch=16, cex.lab=1.5)
axis(1, at=1:4, labels=c("A", "B", "C", "D"))
abline(h=0, lty=3)
arrows(1:4, CI$lowerCI, 1:4, CI$upperCI, code=3, angle=90, length=0.05)

## ------------------------------------------------------------------------
power.t.test(n=NULL, delta=3, sd=2, sig.level=0.05,
             power=0.8)

## ------------------------------------------------------------------------
power.anova.test(groups=4, n=5, between.var=360.0,
                 within.var=101.2, power=NULL)

