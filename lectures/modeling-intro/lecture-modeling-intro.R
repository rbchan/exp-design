## ----linmod1,include=FALSE,fig.show="hide"-------------------------------
par(mai=c(0.8,0.8,0.1,0.1))
plot(function(x) 20 + 0.5*x, 0, 10, ylab="y")


## ----linmod2,include=FALSE,fig.show='hide'-------------------------------
par(mai=c(0.8,0.8,0.1,0.1))
plot(function(x) 20 + 0.5*x - 0.3*x^2 , 0, 10, ylab="y")


## ----linmod,include=FALSE------------------------------------------------
set.seed(3400)  
x <- runif(100, 0, 50)  
y <- rnorm(100, 10 + 1*x, 5)
plot(x, y)
abline(lm(y~x))


## ----linmod-out,size='tiny'----------------------------------------------
lm(y~x)


## ----contr-trt,size='small'----------------------------------------------
options(contrasts=c("contr.treatment","contr.poly"))


## ----contr-sum,size='small',eval=FALSE-----------------------------------
## options(contrasts=c("contr.sum","contr.poly"))


## ----cruz,echo=FALSE,results='hide'--------------------------------------
library(unmarked)
head(cruz)

## ----cruz2,echo=FALSE,results='hide'-------------------------------------
cruz2 <- cruz
set.seed(43893)
cruz2$habitat <- factor(sample(c("Pine","Oak","Bare"),
                               size=nrow(cruz2), replace=TRUE,
                               prob=c(0.3, 0.6, 0.1)))
cruz2$seeds <- factor(sample(c("Low","Med","High"),
                               size=nrow(cruz2), replace=TRUE,
                               prob=c(0.3, 0.6, 0.1)))
plots <- sample(1:nrow(cruz2), size=100)
jayData <- cruz2[plots,]
jayX <- model.matrix(~elevation+I(elevation^2)+chaparral+habitat, jayData)
jayBeta <- c(30, 0.01, -0.000001, 1, 2.5, 1.5)
summary(jaymu <- jayX %*% jayBeta)
sigmaSq <- 5
set.seed(4530)
summary(jayData$jays <- round(rnorm(nrow(jayData), jaymu, sqrt(sigmaSq))))
# hist(jayData$jays)
summary(lm(jays ~ elevation + I(elevation^2) + chaparral + habitat + forest + seeds,
           jayData))


## ----cruz2-head----------------------------------------------------------
head(cruz2)


## ----elev,fig.show='hide',fig.width=6,fig.height=4,echo=FALSE------------
library(lattice)
levelplot(elevation ~ x + y, data=cruz2, aspect="iso",
          scales=list(draw=FALSE),
##          col.regions=rev(topo.colors(101)),
          at=seq(10, 2300, length=100),
          xlab="Easting", ylab="Northing", main="Elevation")


## ----forest,fig.show='hide',fig.width=6,fig.height=4,echo=FALSE----------
levelplot(forest ~ x + y, data=cruz, aspect="iso",
          xlab="",ylab="",main="Forest Cover",
          col.regions=rev(terrain.colors(100)),
          at=seq(0, 1, length=100),
          scales=list(draw=FALSE))


## ----plots,echo=FALSE,fig.show='hide',fig.width=6,fig.height=4-----------
levelplot(chaparral ~ x + y, data=cruz2, aspect="iso",
          xlab="", ylab="", main="Chaparral and survey plots",
          scales=list(draw=FALSE),
          col.regions=heat.colors(100),
          panel = function(...) {
              panel.levelplot(...)
#                  grid.points(jayData$x, jayData$y, pch=0,
#                              size=unit(0.01, "npc"))
              lpoints(jayData$x, jayData$y, pch=0, col=1, cex=0.5)
              }
          )


## ----jayData,size='scriptsize'-------------------------------------------
head(jayData)


## ----fm1,size='scriptsize'-----------------------------------------------
fm1 <- lm(jays ~ elevation, data=jayData)
summary(fm1)


## ----pred1,echo=FALSE,fig.width=7,fig.height=5.5-------------------------
nseq <- 20
elev.seq <- seq(0, 2300, length=nseq)
predData1 <- data.frame(elevation=elev.seq)
pred1 <- predict(fm1, newdata=predData1, interval="confidence", se.fit=TRUE)
plot(jays ~ elevation, jayData, xlab="Elevation (m)", ylab="Jays", cex.lab=1.3)
lines(predData1$elevation, pred1$fit[,1])
lines(predData1$elevation, pred1$fit[,2], lty=3)
lines(predData1$elevation, pred1$fit[,3], lty=3)


## ----fm2,size='scriptsize'-----------------------------------------------
fm2 <- lm(jays ~ elevation+forest, data=jayData)
summary(fm2)


## ----pred2,echo=FALSE,fig.width=7,fig.height=6---------------------------
forest.seq <- seq(0,1,length=nseq)
predData2 <- data.frame(elevation=rep(elev.seq, each=nseq),
                        forest=rep(forest.seq, times=nseq))
pred2 <- predict(fm2, newdata=predData2, interval="confidence", se.fit=TRUE)
res <- persp(forest.seq, elev.seq, matrix(pred2$fit[,1],nseq),
             theta=40, phi=20, col=rgb(0,0,1,0.5), zlab="Expected number of jays",
             ylab="Elevation", xlab="Forest cover", zlim=c(30, 55),
             ticktype="detailed")
#points(trans3d(jayData$forest, jayData$elev, jayData$jays, pmat=res),
#       col="red", cex=1.5, pch=16)


## ----fm3,size='scriptsize'-----------------------------------------------
fm3 <- lm(jays ~ habitat, data=jayData)
summary(fm3)


## ----pred3,echo=FALSE,fig.width=6,fig.height=6,fig.show='hide'-----------
predData3 <- data.frame(habitat=levels(jayData$habitat))
pred3 <- predict(fm3, newdata=predData3, interval="confidence", se.fit=TRUE)
xmid <- barplot(pred3$fit[,1], xlab="Habitat", names=predData3$habitat,
                cex.lab=1.5,
                ylim=c(0, 45), col="darkcyan", ylab="Expected number of jays")
arrows(xmid, pred3$fit[,1], xmid, pred3$fit[,1]+pred3$se, angle=90, length=0.05, code=3)
box()


## ----fm4,size='scriptsize'-----------------------------------------------
fm4 <- lm(jays ~ elevation+habitat, data=jayData)
summary(fm4)


## ----pred4,echo=FALSE,fig.width=7,fig.height=5.5-------------------------
predData4 <- data.frame(elevation=rep(elev.seq, 3),
                        habitat=rep(levels(jayData$habitat), each=nseq))
pred4 <- predict(fm4, newdata=predData4, interval="confidence", se.fit=TRUE)
pred4dat <- data.frame(pred4$fit, predData4)
plot(jays ~ elevation, jayData, xlab="Elevation (m)", ylab="Jays", cex.lab=1.3)
lines(fit~elevation, data=pred4dat, subset=habitat=="Bare", col="tan", lwd=3)
lines(fit~elevation, data=pred4dat, subset=habitat=="Oak", col="brown", lwd=3)
lines(fit~elevation, data=pred4dat, subset=habitat=="Pine", col="darkseagreen", lwd=3)
legend(0, 47, c("Oak", "Pine", "Bare"), col=c("brown","darkseagreen", "tan"), lwd=3)


## ----fm5,size='tiny'-----------------------------------------------------
fm5 <- lm(jays ~ elevation*habitat, data=jayData)
summary(fm5)


## ----pred5,echo=FALSE,fig.width=7,fig.height=5.5-------------------------
pred5 <- predict(fm5, newdata=predData4, interval="confidence", se.fit=TRUE)
pred5dat <- data.frame(pred5$fit, predData4)
plot(jays ~ elevation, jayData, xlab="Elevation (m)", ylab="Jays", cex.lab=1.3)
lines(fit~elevation, data=pred5dat, subset=habitat=="Bare", col="tan", lwd=3)
lines(fit~elevation, data=pred5dat, subset=habitat=="Oak", col="brown", lwd=3)
lines(fit~elevation, data=pred5dat, subset=habitat=="Pine", col="darkseagreen", lwd=3)
legend(0, 47, c("Oak", "Pine", "Bare"), col=c("brown","darkseagreen", "tan"), lwd=3)


## ----fm6,size='scriptsize'-----------------------------------------------
fm6 <- lm(jays ~ elevation+I(elevation^2), data=jayData)
summary(fm6)


## ----pred6,echo=FALSE,fig.width=7,fig.height=5.5-------------------------
nseq <- 20
elev.seq <- seq(0, 2300, length=nseq)
predData6 <- data.frame(elevation=elev.seq)
pred6 <- predict(fm6, newdata=predData6, interval="confidence", se.fit=TRUE)
plot(jays ~ elevation, jayData, xlab="Elevation (m)", ylab="Jays", cex.lab=1.3)
lines(predData6$elevation, pred6$fit[,1])
lines(predData6$elevation, pred6$fit[,2], lty=3)
lines(predData6$elevation, pred6$fit[,3], lty=3)


## ----fm7,size='tiny'-----------------------------------------------------
fm7 <- lm(jays ~ habitat * forest + elevation +
          I(elevation^2), data=jayData)
summary(fm7)


## ----E7------------------------------------------------------------------
E7 <- predict(fm7, type="response", newdata=cruz2,
              interval="confidence")


## ----E72-----------------------------------------------------------------
E7 <- cbind(cruz2[,c("x","y")], E7)
head(E7)


## ----Ejay,fig.show='hide',fig.width=6,fig.height=4,echo=FALSE------------
levelplot(fit ~ x + y, data=E7, aspect="iso",
          xlab="", ylab="", main="Expected number of jays per grid cell",
##          col.regions=heat.colors(101),
          at=22:55, colorkey=list(space="bottom"),
          scales=list(draw=FALSE))


## ----Ljay,fig.show='hide',fig.width=6,fig.height=4,echo=FALSE------------
levelplot(lwr ~ x + y, data=E7, aspect="iso",
          xlab="", ylab="", main="Lower CI",
          at=25:55, colorkey=list(space="bottom"),
          scales=list(draw=FALSE))


## ----Ujay,fig.show='hide',fig.width=6,fig.height=4,echo=FALSE------------
levelplot(upr ~ x + y, data=E7, aspect="iso",
          xlab="", ylab="", main="Upper CI",
          at=25:55, colorkey=list(space="bottom"),
          scales=list(draw=FALSE))


## ----future1,echo=FALSE--------------------------------------------------
future1 <- cruz2
future1$habitat[] <- "Bare"
future.pred1 <- predict(fm6, type="response", newdata=future1,
                        interval="confidence")
future.pred1 <- cbind(cruz2[,c("x","y")], future.pred1)

## ----future1fig,fig.show='hide',fig.width=6,fig.height=4,echo=FALSE------
levelplot(fit ~ x + y, data=future.pred1, aspect="iso",
          xlab="", ylab="", main="Expected values",
          at=25:55, colorkey=list(space="bottom"),
          scales=list(draw=FALSE))


## ----future2,echo=FALSE--------------------------------------------------
future2 <- cruz2
future2$elevation <- future2$elevation - 1000
future2$elevation[future2$elevation < 0] <- NA
future.pred2 <- predict(fm6, type="response", newdata=future2,
                        interval="confidence")
future.pred2 <- cbind(cruz2[,c("x","y")], future.pred2)

## ----future2fig,echo=FALSE,fig.show='hide',fig.width=6,fig.height=4------
levelplot(fit ~ x + y, data=future.pred2, aspect="iso",
          panel = function(...) {
              panel.fill(col="skyblue1")
              panel.levelplot(...)
          },
          xlab="", ylab="", main="Expected values",
          at=25:55, colorkey=list(space="bottom"),
          scales=list(draw=FALSE))


## ----fm1-2,size='scriptsize',eval=FALSE----------------------------------
## fm1 <- lm(jays ~ elevation, data=jayData)


## ----X1,size='scriptsize'------------------------------------------------
X1 <- model.matrix(fm1)
head(X1, n=5) # First 5 rows of design matrix


## ----beta-hat,size='scriptsize'------------------------------------------
beta.hat1 <- coef(fm1) # Estimates of beta0 and beta1
beta.hat1


## ----Ey1,size='footnotesize'---------------------------------------------
Ey1 <- X1 %*% beta.hat1 # Expected number of jays at each site
head(Ey1, 5)


## ----fm4-2,size='scriptsize',eval=FALSE----------------------------------
## fm4 <- lm(jays ~ elevation + habitat, data=jayData)


## ----X4,size='scriptsize'------------------------------------------------
X4 <- model.matrix(fm4)
head(X4, n=5) # First 5 rows of design matrix


## ----beta-hat4,size='scriptsize'-----------------------------------------
beta.hat4 <- coef(fm4) # Estimates of beta0 and beta1
beta.hat4


## ----Ey4,size='footnotesize'---------------------------------------------
Ey4 <- X4 %*% beta.hat4 # Expected number of jays at each site
head(Ey4, 5)

