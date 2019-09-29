## ----gypsyData,size='footnotesize'---------------------------------------
gypsyData <- read.csv("gypsyData.csv")
gypsyData$region <- factor(gypsyData$region) # Convert to factor
gypsyData


## ----gmean,size='footnotesize'-------------------------------------------
caterpillars <- gypsyData$caterpillars
(grand.mean <- mean(caterpillars))


## ----tmeans,size='footnotesize'------------------------------------------
pesticide <- gypsyData$pesticide
(treatment.means <- tapply(caterpillars, pesticide, mean))


## ----bmeans,size='footnotesize'------------------------------------------
region <- gypsyData$region
(block.means <- tapply(caterpillars, region, mean))


## ----SSt-----------------------------------------------------------------
b <- 4
b <- nlevels(region)
SS.treat <- b*sum((treatment.means - grand.mean)^2)
SS.treat


## ----SSb-----------------------------------------------------------------
a <- nlevels(pesticide)
SS.block <- a*sum((block.means - grand.mean)^2)
SS.block


## ----SSw,size='small'----------------------------------------------------
treatment.means.long <- rep(treatment.means, each=b)
block.means.long <- rep(block.means, times=a)
SS.within <- sum((caterpillars - treatment.means.long -
                  block.means.long + grand.mean)^2)
SS.within


## ----ANOVAtable,size='small'---------------------------------------------
df.treat <- a-1
df.block <- b-1
df.within <- df.treat*df.block
ANOVAtable <- data.frame(
    df = c(df.treat, df.block, df.within),
    SS = c(SS.treat, SS.block, SS.within))
rownames(ANOVAtable) <- c("Treatment", "Block", "Within")
ANOVAtable


## ----MSE-----------------------------------------------------------------
MSE <- ANOVAtable$SS / ANOVAtable$df
ANOVAtable$MSE <- MSE


## ----F1------------------------------------------------------------------
F <- c(MSE[1]/MSE[3], MSE[2]/MSE[3], NA)
ANOVAtable$F <- F


## ----P-------------------------------------------------------------------
P <- c(1 - pf(F[1], 2, 6), 1 - pf(F[2], 3, 6), NA)
ANOVAtable$P <- P
round(ANOVAtable, 3)


## ----Fdist,echo=FALSE,fig.show=TRUE,include=FALSE,fig.width=12,fig.height=6----
curve(df(x, df1=2, df2=6), 0, 10, xlab="F", ylab="")
xx <- seq(F[1], 20, length=50)
den <- df(xx, 2, 6)
polygon(c(xx, rev(xx)), c(den, rep(0, length(xx))), col="red")
#abline(h=0, col=gray(0.5))
crit <- qf(0.95, 2, 6)
text(crit, 0.4, "critical value")
arrows(crit, 0.38, crit, 0.05, length=0.1)
text(F[1], 0.3, "F-value")
arrows(F[1], 0.28, F[1], 0.05, length=0.1)


## ----Cval,size='scriptsize'----------------------------------------------
qf(0.95, df1=2, df2=6) # 95% of the distribution is before this value of F


## ----Pval,size='scriptsize'----------------------------------------------
1-pf(F[1], df1=2, df2=6) # Proportion of the distribution beyond this F value


## ----aov1----------------------------------------------------------------
aov1 <- aov(caterpillars ~ pesticide + region, gypsyData)
summary(aov1)


## ----atable--------------------------------------------------------------
round(ANOVAtable, 3)


## ----aov2----------------------------------------------------------------
aov2 <- aov(caterpillars ~ pesticide, gypsyData)
summary(aov2)


## ----aov3----------------------------------------------------------------
aov3 <- aov(caterpillars ~ pesticide + Error(region), gypsyData)
summary(aov3)

