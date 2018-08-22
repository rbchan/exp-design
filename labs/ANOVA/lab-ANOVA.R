## ----kick-data-----------------------------------------------------------
kick.angle.brandA <- c(42,17,24,39,43)
kick.angle.brandB <- c(28,50,44,32,61)
kick.angle.brandC <- c(57,45,48,41,54)
kick.angle.brandD <- c(29,40,22,34,30)

## ----sawData-------------------------------------------------------------
n <- length(kick.angle.brandA)
a <- 4
sawData <- data.frame(
    Kick.angle=c(kick.angle.brandA, kick.angle.brandB,
        kick.angle.brandC, kick.angle.brandD),
    Brand=rep(c("A","B","C","D"), each=n))

## ----boxplot1,fig.show='hide',fig.width=8,fig.height=6,size='scriptsize'----
boxplot(Kick.angle ~ Brand, data=sawData, xlab="Brand",
        ylab="Kickback angle", cex.lab=1.3, col="darkcyan")

## ----aov-out1------------------------------------------------------------
aov.out1 <- aov(Kick.angle ~ Brand, data=sawData)

## ----aov-out1-summary, size='footnotesize'-------------------------------
summary(aov.out1)

## ----model-tables--------------------------------------------------------
model.tables(aov.out1, type="means", se=TRUE)

## ----model-tables-effects------------------------------------------------
model.tables(aov.out1, type="effects", se=TRUE)

## ----ybar, size='footnotesize'-------------------------------------------
ybar. <- mean(sawData$Kick.angle)
ybar.

## ----ybar-i, size='footnotesize'-----------------------------------------
ybar.i <- c(mean(kick.angle.brandA), mean(kick.angle.brandB),
            mean(kick.angle.brandC), mean(kick.angle.brandD))
ybar.i

## ----ybar-i-tapply, size='footnotesize'----------------------------------
ybar.i <- tapply(sawData$Kick.angle, sawData$Brand, mean)
ybar.i

## ----SSa-----------------------------------------------------------------
SSa <- n*sum((ybar.i - ybar.)^2)
SSa

## ----y-ij----------------------------------------------------------------
y.ij <- sawData$Kick.angle
SSw <- sum((y.ij - rep(ybar.i, each=n))^2)
SSw

## ----MSa, size='footnotesize'--------------------------------------------
df1 <- a-1
MSa <- SSa / df1
MSa

## ----MSw, size='footnotesize'--------------------------------------------
df2 <- a*(n-1)
MSw <- SSw / df2
MSw

## ----F-stat, size='footnotesize'-----------------------------------------
F.stat <- MSa / MSw
F.stat

## ----F-crit--------------------------------------------------------------
F.crit <- qf(0.95, df1, df2)
F.crit

## ----p-val---------------------------------------------------------------
p.value <- 1 - pf(F.stat, df1, df2)
p.value

## ----warbler-weight,echo=FALSE-------------------------------------------
set.seed(342)
n <- 20
pop1 <- rnorm(n, 8, sd=1.9)
pop2 <- rnorm(n, 9, sd=1.9)
pop3 <- rnorm(n, 9, sd=1.9)
pop4 <- rnorm(n, 7, sd=1.9)
warblerWeight <- data.frame(weight=c(pop1, pop2, pop3, pop4),
                            food=c(rep("Low", each=n),
                                   rep("Med", each=n),
                                   rep("High", each=n),
                                   rep("Control", each=n)))
write.csv(warblerWeight, "warblerWeight.csv")

## ----barplot2,fig.show='hide',fig.width=8,fig.height=6,size='scriptsize'----
xx <- barplot(ybar.i, ylim=c(0, 60), xlab="Brand", cex.lab=1.5,
              ylab="Chainsaw kickback angle")
mean.SE <- 6.364 # from model.tables(). See slide 8.
arrows(xx, ybar.i, xx, ybar.i+mean.SE, angle=90, length=0.05)

## ----tukey,size='small'--------------------------------------------------
TukeyHSD(aov.out1)

## ----tukey-plot,fig.show='hide',size='footnotesize'----------------------
plot(TukeyHSD(aov.out1))

