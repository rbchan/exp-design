## ----infectData,echo=FALSE-----------------------------------------------
set.seed(3550)
M <- 100
p1 <- 0.5
p2 <- 0.3
p3 <- 0.2
n <- 30
y <- c(rbinom(n, M, p1), rbinom(n, M, p2),
       rbinom(n, M, p3)) / M
infectionRates <- data.frame(percentInfected=round(sin(y)^2,2),
                             landscape=rep(c("Park", "Suburban", "Urban"), each=n))
write.csv(infectionRates, "infectionRates.csv", row.names=FALSE)

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

## ----bartlett,size='footnotesize'----------------------------------------
bartlett.test(percentInfected~landscape, data=infectionRates)

## ----histresid0,fig.show='hide'------------------------------------------
resids <- resid(anova1)
hist(resids, col="turquoise", breaks=10, xlab="Residuals")

## ----shapiro-------------------------------------------------------------
shapiro.test(resids)

## ----anova2,size='footnotesize'------------------------------------------
anova2 <- aov(log(percentInfected)~landscape,
              data=infectionRates)
summary(anova2)

## ----shapiro2,size='footnotesize'----------------------------------------
shapiro.test(resid(anova2))

