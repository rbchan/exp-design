## ----build-fun, include=FALSE, cache=TRUE--------------------------------
## A function to compile and open the pdf
## Usage:
## rnw2pdf("lab-t-tests") # Don't include the file extension
source("../rnw2pdf.R")

## ----knitr-theme, include=FALSE------------------------------------------
##knit_theme$set("navajo-night")
knit_theme$set("edit-kwrite")

## ----pop,echo=FALSE,include=TRUE,fig.width=9,fig.height=3,cache=TRUE-----
op <- par(mai=c(0.6,.1,0.4,0.1))
curve(dnorm(x, mean=3, sd=1), 0, 10, xlab="Tree density", ylab="", xaxt="n", yaxt="n", frame=FALSE)
curve(dnorm(x, mean=6, sd=1), 0, 10, add=TRUE, col=4)
segments(3, 0, 3, dnorm(3, 3, 1), lty=2)
segments(6, 0, 6, dnorm(6, 6, 1), col=4, lty=2)
text(3, dnorm(3,3,1), expression(mu[1]), pos=1, cex=2)
text(6, dnorm(6,6,1), expression(mu[2]), pos=1, cex=2, col=4)

## ----tdist,echo=FALSE,include=FALSE,fig.height=6,fig.width=8-------------
op <- par(mai=c(0.8, 0.2, 0.2, 0.2))
curve(dt(x, df=18), -4, 4, xlab="t-value", ylab="", yaxt="n",
      ylim=c(0,0.5),
      frame=FALSE , main="t distribution with df=18", cex.main=1.5)
xs1 <- seq(qt(.025, df=18), -4, by=-0.1)
ys1 <- dt(xs1, df=19)
xs2 <- seq(qt(.975, df=18), 4, by=0.1)
ys2 <- dt(xs2, df=19)
polygon(c(xs1, rev(xs1)), c(rep(0, length(xs1)), rev(ys1)), col=gray(0.7))
polygon(c(xs2, rev(xs2)), c(rep(0, length(xs2)), rev(ys2)), col=gray(0.7))
#text(xs1[1], dt(0,18)/1.5, "t=-2.10\np=0.05", pos=3)
#text(xs2[1], dt(0,18)/1.5, "t=2.10\np=0.05", pos=3)
text(xs1[1], dt(0,18)/1.5, "critical value\nt=-2.10", pos=3)
text(xs2[1], dt(0,18)/1.5, "critical value\nt=2.10", pos=3)
arrows(xs1[1], dt(0,18)/1.5, xs1[1], ys1[1], length=0.1)
arrows(xs2[1], dt(0,18)/1.5, xs2[1], ys2[1], length=0.1)
#text(-3, dt(0,18), "t=-3\np=0.0038", pos=3)
#text(3, dt(0,18), "t=3\np=0.0038", pos=3)
#text(-3, dt(0,18), "t=-3", pos=3)
#text(3, dt(0,18), "t=3", pos=3)
#arrows(-3, dt(0,18), -3, dt(-3,18), length=0.1)
#arrows(3, dt(0,18), 3, dt(3,18), length=0.1)
par(op)

## ----echo=FALSE----------------------------------------------------------
yL <- c(16,24,18,17,29,31,14,16,22,15)
yH <- c(2, 11, 6, 8, 0, 3,19, 1, 6,5)
treedata <- data.frame(treeDensity=c(yL, yH),
                       Elevation=c(rep('Low', length(yL)),
                           rep('High', length(yH))))
write.csv(treedata, "treedata.csv", row.names=FALSE)

## ----boxplot,include=FALSE,fig.width=6,fig.height=6----------------------
boxplot(yL, yH, xlab="Elevation", names=c("Low", "High"),
        ylab="Tree density", col=c("lightblue", "purple"))

## ----histogram,echo=FALSE,include=FALSE,fig.width=12,fig.height=6--------
par(mfrow=c(1,2))
hist(yL, xlab="Tree density (low elevation)", col="lightblue",
     xlim=c(0, 40))
hist(yH, xlab="Tree density (high elevation)", col="purple",
     xlim=c(0, 40))

## ----eval=FALSE----------------------------------------------------------
## hist(yL, xlab="Tree density (low elevation)", col="lightblue",
##      xlim=c(0, 40))
## hist(yH, xlab="Tree density (high elevation)", col="purple",
##      xlim=c(0, 40))

## ----popsamp,echo=FALSE,include=TRUE,fig.height=7,fig.width=7------------
op <- par(mfrow=c(2,2), mai=c(0.7,0.7,0.7,0.6))
curve(dnorm(x, mean=19, sd=sqrt(25)), 0, 50, xlim=c(0,50),
      xlab="Tree height (m) below 2000 m",
      ylab="Probability density",
      main="Normal distribution with \nmean=19 and var=25")
curve(dnorm(x, mean=7, sd=sqrt(25)), 0, 50, xlim=c(0,50),
      xlab="Tree height (m) above 1000 m",
      ylab="Probability density",
      main="Normal distribution with\n mean=7 and var=25")
set.seed(350)
#n <- 15
#y1 <- rnorm(n, mean=5, sd=sqrt(4)) # Sample 1
#y2 <- rnorm(n, mean=7, sd=sqrt(4)) # Sample 2
hist(yL, xlim=c(0, 50), ylim=c(0,0.2), breaks=seq(0,50,2), freq=FALSE,
     main="Histogram of sample",
      xlab="Tree density below 2000 m",
      ylab="Probability density")
lines(density(yL))
hist(yH, xlim=c(0, 50), ylim=c(0,0.2), breaks=seq(0,50,2), freq=FALSE,
     main="Histogram of sample",
      xlab="Tree density above 2000 m",
      ylab="Probability density")
lines(density(yH))
par(op)

## ----hand1, size='scriptsize'--------------------------------------------
mean.L <- mean(yL)
mean.H <- mean(yH)
s2.L <- var(yL)
s2.H <- var(yH)
n.L <- length(yL) # length returns the number of elements in a vector
n.H <- length(yH)
s2.p <- ((n.L-1)*s2.L + (n.H-1)*s2.H)/(n.L+n.H-2)
SE <- sqrt(s2.p/n.L + s2.p/n.H)
t.stat <- (mean.L - mean.H) / SE
t.stat

## ----hand2, size='tiny'--------------------------------------------------
alpha <- 0.05
## NOTE: qt returns critical values. No need to use "t tables"
critical.vals <- qt(c(alpha/2, 1-alpha/2), df=n.L+n.H-2)
critical.vals

## ----t-test1, size='normalsize'------------------------------------------
t.test(yH, yL, var.equal=TRUE,
       paired=FALSE, alternative="two.sided")

## ------------------------------------------------------------------------
t.test(treeDensity ~ Elevation, data=treedata, var.equal=TRUE,
       paired=FALSE, alternative="two.sided")

## ------------------------------------------------------------------------
var.ratio <- var(yL)/var(yH) # F-statistic
var.ratio

## ------------------------------------------------------------------------
alpha <- 0.05
critical.vals <- qf(c(alpha/2, 1-alpha/2), df1=n.L-1, df2=n.H-1)
critical.vals

## ----vartest, size='small'-----------------------------------------------
var.test(yL, yH)

## ----F,echo=FALSE,eval=FALSE---------------------------------------------
## curve(df(x, n.L, n.H), 0, 5, xlab="F value", yaxt="n", frame=FALSE,
##       ylab="")
## xs1 <- seq(qf(.025, df1=10, df2=10), -0, by=-0.1)
## ys1 <- df(xs1, df1=10, df2=10)
## xs2 <- seq(qf(.975, df1=10, df2=10), 6, by=0.1)
## ys2 <- df(xs2, df1=10, df2=10)
## polygon(c(xs1, rev(xs1)), c(rep(0, length(xs1)), rev(ys1)), col=gray(0.7))
## polygon(c(xs2, rev(xs2)), c(rep(0, length(xs2)), rev(ys2)), col=gray(0.7))
## text(xs1[1], .7, "F=0.022\np=0.05", pos=3)
## text(xs2[1], .7, "F=3.72\np=0.05", pos=3)
## arrows(xs1[1], .7, xs1[1], ys1[1], length=0.1)
## arrows(xs2[1], .7, xs2[1], ys2[1], length=0.1)

## ----catdat--------------------------------------------------------------
location <- 1:12
untreated <- c(23,18,29,22,33,20,17,25,27,30,25,27)
treated <- c(19,18,24,23,31,22,16,23,24,26,24,28)

## ------------------------------------------------------------------------
diff <- untreated-treated
diff
mean(diff) ## Estimate of the mean of the differences

## ----box2,include=FALSE,fig.width=6,fig.height=6-------------------------
boxplot(diff, col="lightgreen",
        ylab="Differences in caterpillars (untreated-treated)")
abline(h=0, col="grey")

## ----diff,include=FALSE,fig.width=8,fig.height=6-------------------------
diff <- untreated-treated
diff
hist(diff, main="", xlab="Difference in caterpillars", breaks=5)
abline(v=0, lty=2)

