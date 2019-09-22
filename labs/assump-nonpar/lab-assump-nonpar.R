## ----infectionRates,size='tiny'------------------------------------------
infectionRates <- read.csv("infectionRates.csv")
str(infectionRates)
summary(infectionRates)


## ----anova1,size='footnotesize'------------------------------------------
anova1 <- aov(percentInfected ~ landscape,
              data=infectionRates)
summary(anova1)


## ----boxfor1,fig.show='hide',size='footnotesize'-------------------------
boxplot(percentInfected~landscape, infectionRates,
        col="lightgreen", cex.lab=1.5, cex.axis=1.3,
        ylab="Percent forest cover")


## ----histresid0,fig.show='hide'------------------------------------------
resids <- resid(anova1)
hist(resids, col="turquoise", breaks=10, xlab="Residuals")


## ----shapiro-------------------------------------------------------------
shapiro.test(resids)


## ----log,size='scriptsize',fig.show='hide'-------------------------------
boxplot(log(percentInfected)~landscape, infectionRates, col="green",
      cex.lab=1.5, cex.axis=1.3, ylab="log(percent forest cover)")


## ----sqrt,size='scriptsize',fig.show='hide'------------------------------
boxplot(sqrt(percentInfected)~landscape, infectionRates, col="yellow",
        cex.lab=1.5, cex.axis=1.3, ylab="sqrt(percent forest cover)")


## ----asin,size='scriptsize',fig.show='hide'------------------------------
boxplot(asin(sqrt(percentInfected))~landscape, infectionRates, col="orange",
        cex.lab=1.5, cex.axis=1.3, ylab="asin(sqrt(percent forest cover))")


## ----recip,size='scriptsize',fig.show='hide'-----------------------------
boxplot(1/percentInfected~landscape, infectionRates, col="pink",
        cex.lab=1.5, cex.axis=1.3, ylab="1/percent forest cover")


## ----anova2,size='footnotesize'------------------------------------------
anova2 <- aov(log(percentInfected)~landscape,
              data=infectionRates)
summary(anova2)


## ----shapiro2,size='footnotesize'----------------------------------------
shapiro.test(resid(anova2))

