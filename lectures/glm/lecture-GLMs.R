## ----logit-p,size='tiny'-------------------------------------------------
beta0 <- 5
beta1 <- -0.08
elevation <- 100
(logit.p <- beta0 + beta1*elevation)


## ----inv-logit,size='tiny'-----------------------------------------------
p <- exp(logit.p)/(1+exp(logit.p))
p


## ----plogis,size='tiny'--------------------------------------------------
plogis(logit.p)


## ----logit,size='tiny'---------------------------------------------------
log(p/(1-p))
qlogis(p)


## ----nologit,fig.show='hide',fig.width=6,fig.height=4,size='scriptsize'----
plot(function(x) 5 + -0.08*x, from=0, to=100,
     xlab="Elevation", ylab="logit(prob of occurrence)")


## ----logit2,fig.show='hide',fig.width=6,fig.height=4,size='scriptsize'----
plot(function(x) plogis(5 + -0.08*x), from=0, to=100,
     xlab="Elevation", ylab="Probability of occurrence")


## ----simFrogs,echo=FALSE,results='hide'----------------------------------
set.seed(43340)
n <- 30
elev <- round(runif(n, 0, 500))
habitat <- gl(3, 10, labels=c("Oak", "Maple", "Pine"))
beta0 <- -1
beta1 <- 0.01
mu <- plogis(beta0 + beta1*elev)
summary(mu)
y <- rbinom(n, 1, mu)
frogData <- data.frame(presence=y,
                       abundance=rpois(n, exp(beta0 + beta1*elev)),
                       elevation=elev, habitat)
glm1 <- glm(presence ~ elev+habitat, family=binomial(link="logit"), data=frogData)
summary(glm1)
anova(glm1)


## ----binom1,echo=FALSE,fig.width=7,fig.height=6,out.width="0.9\\textwidth"----
plot(0:5, dbinom(0:5, 5, 0.5), type="h",
     xlab="Number of 'successes'", ylab="Probability",
     lend="butt", lwd=5, col="blue", ylim=c(0,0.6),
     main="Binomial(N=5, p=0.5)")


## ----binom2,echo=FALSE,fig.width=7,fig.height=6,out.width="0.9\\textwidth"----
plot(0:5, dbinom(0:5, 5, 0.9), type="h",
     xlab="Number of 'successes'", ylab="Probability",
     lend="butt", lwd=5, col="blue", ylim=c(0,0.6),
     main="Binomial(N=5, p=0.9)")


## ----frogData,size='tiny'------------------------------------------------
head(frogData, n=25)


## ----raw-elev,fig.show='hide',size='tiny'--------------------------------
plot(presence ~ elevation, frogData,
     xlab="Elevation", ylab="Frog Occurrence")


## ----raw-habitat,fig.show='hide',size='tiny'-----------------------------
group.prop <- tapply(frogData$presence, frogData$habitat, mean)
barplot(group.prop, ylab="Proportion of sites with frogs")


## ----fm1,size='tiny'-----------------------------------------------------
fm1 <- glm(presence ~ habitat + elevation,
           family=binomial(link="logit"), data=frogData)


## ----summary-fm1,size='tiny'---------------------------------------------
summary(fm1)


## ----newdat,sizse='footnotesize'-----------------------------------------
newdat <- data.frame(elevation=seq(12, 489, length=50),
                     habitat="Oak")
head(newdat)


## ----pred-link,size='footnotesize'---------------------------------------
pred.link <- predict(fm1, newdata=newdat, se.fit=TRUE, type="link")
newdat$mu <- plogis(pred.link$fit)
newdat$lower <- plogis(pred.link$fit - 1.96*pred.link$se.fit)
newdat$upper <- plogis(pred.link$fit + 1.96*pred.link$se.fit)


## ----pred1,fig.show='hide',fig.width=6,fig.height=5,echo=FALSE,size='tiny'----
plot(mu ~ elevation, newdat, type="l", ylim=c(0,1),
     xlab="Elevation", ylab="Probability of occurrence")
points(presence ~ elevation, frogData)
lines(lower ~ elevation, newdat, lty=2)
lines(upper ~ elevation, newdat, lty=2)


## ----pois1,fig.show='hide',echo=FALSE------------------------------------
x <- 0:25
plot(x, dpois(x, lambda=1), type="h", lwd=5, col="blue", lend="butt",
     xlab="Response variable", ylab="Probability",
     main=expression(paste("Poisson(", lambda, "= 1)", sep="")), cex.lab=1.5 )

## ----pois2,fig.show='hide',echo=FALSE------------------------------------
plot(x, dpois(x, lambda=5), type="h", lwd=5, col="blue", lend="butt",
     xlab="Response variable", ylab="Probability",
     main=expression(paste("Poisson(", lambda, "= 5)", sep="")), cex.lab=1.5 )

## ----pois3,fig.show='hide',echo=FALSE------------------------------------
plot(x, dpois(x, lambda=10), type="h", lwd=5, col="blue", lend="butt",
     xlab="Response variable", ylab="Probability",
     main=expression(paste("Poisson(", lambda, "= 10)", sep="")), cex.lab=1.5 )


## ----nolog,fig.show='hide',fig.width=7,fig.height=5,size='footnotesize'----
plot(function(x) 5 + -0.08*x, from=0, to=100,
     xlab="Elevation", ylab="log(Expected abundance)")


## ----log,fig.show='hide',fig.width=7,fig.height=5,size='footnotesize'----
plot(function(x) exp(5 + -0.08*x), from=0, to=100,
     xlab="Elevation", ylab="Expected abundance")


## ----fm2,size='tiny'-----------------------------------------------------
fm2 <- glm(abundance ~ habitat + elevation,
           family=poisson(link="log"), data=frogData)


## ----summary-fm2,size='tiny'---------------------------------------------
summary(fm2)


## ----newdat-pois,size='footnotesize'-------------------------------------
newdat <- data.frame(elevation=seq(12, 489, length=50),
                     habitat="Oak")
head(newdat)


## ----pred-link-pois,size='scriptsize'------------------------------------
pred.link <- predict(fm2, newdata=newdat, se.fit=TRUE, type="link")
newdat$mu <- exp(pred.link$fit)
newdat$lower <- exp(pred.link$fit - 1.96*pred.link$se.fit)
newdat$upper <- exp(pred.link$fit + 1.96*pred.link$se.fit)


## ----pred2,fig.show=FALSE,fig.width=7,fig.height=5,echo=FALSE,size='tiny'----
plot(mu ~ elevation, newdat, type="l", ylim=c(0,100),
     xlab="Elevation", ylab="Abundance")
points(abundance ~ elevation, frogData)
lines(lower ~ elevation, newdat, lty=2)
lines(upper ~ elevation, newdat, lty=2)


## ----dev,size='footnotesize'---------------------------------------------
N <- nrow(frogData)                   # sample size
K <- length(coef(fm2))                # number of parameters
df.resid <- N-K                       # degrees-of-freedom
Dev <- deviance(fm2)                  # residual deviance
p.value <- 1-pchisq(Dev, df=df.resid) # p-value
p.value                               # fail to reject H0


## ----chisq,fig.show='hide',fig.width=7,fig.height=5,size='tiny'----------
curve(dchisq(x, df=df.resid), from=0, to=50, xlab="Deviance", ylab="Density")
abline(v=Dev, lwd=3, col="red")


## ----nb1,fig.show='hide',echo=FALSE--------------------------------------
x <- 0:25
plot(x, dnbinom(x, mu=2, size=10), type="h", lwd=5, col="blue", lend="butt",
     xlab="Response variable", ylab="Probability",
     main=expression(paste("NegBin(", lambda, "= 2, ", alpha, "= 10", ")", sep="")), cex.lab=1.5 )

## ----nb2,fig.show='hide',echo=FALSE--------------------------------------
plot(x, dnbinom(x, mu=2, size=5), type="h", lwd=5, col="blue", lend="butt",
     xlab="Response variable", ylab="Probability",
     main=expression(paste("NegBin(", lambda, "= 2, ", alpha, "= 5", ")", sep="")), cex.lab=1.5 )

## ----nb3,fig.show='hide',echo=FALSE--------------------------------------
plot(x, dnbinom(x, mu=2, size=.1), type="h", lwd=5, col="blue", lend="butt",
     xlab="Response variable", ylab="Probability",
     main=expression(paste("NegBin(", lambda, "= 2, ", alpha, "= 0.1", ")", sep="")), cex.lab=1.5 )

