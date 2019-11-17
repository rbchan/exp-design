## ----frogData,size='tiny'------------------------------------------------
frogData[1:25,] # First 25 rows


## ----fm1,size='tiny'-----------------------------------------------------
fm1 <- glm(presence ~ habitat + elevation,
           family=binomial(link="logit"), data=frogData)


## ----summary-fm1,size='tiny'---------------------------------------------
summary(fm1)


## ----newdat,size='scriptsize'--------------------------------------------
predData.elev <- data.frame(elevation=seq(12, 489, length=50),
                       habitat="Oak")
head(predData.elev)


## ----pred-link,size='scriptsize'-----------------------------------------
pred.link <- predict(fm1, newdata=predData.elev, se.fit=TRUE, type="link")
predData.elev$p <- plogis(pred.link$fit)
predData.elev$lower <- plogis(pred.link$fit - 1.96*pred.link$se.fit)
predData.elev$upper <- plogis(pred.link$fit + 1.96*pred.link$se.fit)


## ----pred1,fig.show='hide',fig.width=6,fig.height=5,size='tiny'----------
plot(p ~ elevation, data=predData.elev, type="l", ylim=c(0,1),
     xlab="Elevation", ylab="Probability of occurrence")
points(presence ~ elevation, frogData)
lines(lower ~ elevation, data=predData.elev, lty=2)
lines(upper ~ elevation, data=predData.elev, lty=2)


## ----habitat,fig.show='hide',size='tiny',fig.width=6,fig.height=6--------
predData.hab <- data.frame(habitat=c("Oak", "Maple", "Pine"), elevation=250)
pred <- predict(fm1, newdata=predData.hab, se.fit=TRUE, type="link")
bp <- barplot(plogis(pred$fit), ylab="Probability of occurrence", cex.lab=1.5,
              names=c("Oak", "Maple", "Pine"), col="lightblue", ylim=c(0, 1.1))
arrows(bp, plogis(pred$fit), bp, plogis(pred$fit + pred$se.fit),
       angle=90, code=3, length=0.1)


## ----fm2,size='tiny'-----------------------------------------------------
fm2 <- glm(abundance ~ habitat + elevation,
           family=poisson(link="log"), data=frogData)


## ----summary-fm2,size='tiny'---------------------------------------------
summary(fm2)


## ----pred-link2,size='scriptsize'----------------------------------------
pred.link <- predict(fm2, newdata=predData.elev, se.fit=TRUE, type="link")
predData.elev$lambda <- exp(pred.link$fit) # exp is the inverse-link function
predData.elev$lower <- exp(pred.link$fit - 1.96*pred.link$se.fit)
predData.elev$upper <- exp(pred.link$fit + 1.96*pred.link$se.fit)


## ----pred2,fig.show='hide',fig.width=6,fig.height=5,size='tiny'----------
plot(lambda ~ elevation, predData.elev, type="l", ylim=c(0,60),
     xlab="Elevation", ylab="Expected abundance")
points(abundance ~ elevation, frogData)
lines(lower ~ elevation, predData.elev, lty=2)
lines(upper ~ elevation, predData.elev, lty=2)

