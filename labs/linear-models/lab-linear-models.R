## ----cruzData------------------------------------------------------------
cruzData <- read.csv("cruzData.csv")
head(cruzData, n=7)


## ----elevJay,include=TRUE,fig.width=6,fig.height=4,size='scriptsize'-----
library(lattice)
levelplot(elevation ~ x + y, data=cruzData,
          aspect="iso", # Puts x and y axes on same scale
          xlab="Easting", ylab="Northing", main="Elevation")


## ----forestJay,fig.width=6,fig.height=4,size='scriptsize'----------------
levelplot(forest ~ x + y, data=cruzData, aspect="iso",
          xlab="",ylab="",main="Forest Cover",
          scales=list(draw=FALSE)) # suppress axes


## ----habitat,fig.width=6,fig.height=4,size='scriptsize'------------------
levelplot(habitat ~ x + y, data=cruzData, aspect="iso",
          xlab="", ylab="", main="Habitat type",
          scales=list(draw=FALSE))


## ----jayData, size='scriptsize'------------------------------------------
library(latticeExtra) ## install.packages("latticeExtra") ## Do this!
jayData <- read.csv("jayData.csv")
head(jayData)

## ----jayData-str, size='scriptsize'--------------------------------------
str(jayData) ## 100 rows


## ----plots,fig.width=6,fig.height=4,size='scriptsize',fig.show='hide'----
levelplot(chaparral ~ x + y, data=cruzData, aspect="iso",
          xlab="", ylab="", main="Chaparral and survey plots",
          scales=list(draw=FALSE)) +
    xyplot(y ~ x, jayData, pch=0, col=1, cex=0.5)


## ----fm1-----------------------------------------------------------------
fm1 <- lm(jays ~ elevation, data=jayData)


## ----fm1s----------------------------------------------------------------
summary(fm1)


## ----fm2,size='scriptsize'-----------------------------------------------
fm2 <- lm(jays ~ elevation + I(elevation^2), data=jayData)


## ----fm2s,size='scriptsize'----------------------------------------------
summary(fm2)


## ----fm3,size='scriptsize'-----------------------------------------------
fm3 <- lm(jays ~ elevation + chaparral, data=jayData)


## ----fm3s,size='scriptsize'----------------------------------------------
summary(fm3)


## ----fm4,size='scriptsize'-----------------------------------------------
fm4 <- lm(jays ~ habitat, data=jayData)


## ----fm4s,size='scriptsize'----------------------------------------------
summary(fm4)


## ----fm5,size='scriptsize'-----------------------------------------------
fm5 <- lm(jays ~ habitat + elevation, data=jayData)
summary(fm5)


## ----fm6,size='tiny'-----------------------------------------------------
fm6 <- lm(jays ~ habitat + elevation +
          I(elevation^2) + chaparral, data=jayData)
summary(fm6)


## ----nd1,size='footnotesize'---------------------------------------------
nd1 <- data.frame(habitat = "Oak",
    elevation=seq(min(jayData$elev), max(jayData$elev), length=100),
    chaparral=mean(jayData$chaparral))


## ----E6pred,size='footnotesize'------------------------------------------
E6.elev <- predict(fm6, newdata=nd1, type="response", se.fit=TRUE,
                   interval="confidence")
E6.elev <- cbind(E6.elev$fit, nd1)
head(E6.elev)


## ----E6elev,fig.width=8,fig.height=6,fig.show='hide',size='scriptsize'----
plot(fit ~ elevation, E6.elev, type="l", ylim=c(20,50),
     xlab="Elevation (m)", ylab="Expected number of jays")
points(jays ~ elevation, jayData)
lines(lwr ~ elevation, E6.elev, lty=2)
lines(upr ~ elevation, E6.elev, lty=2)


## ----E6------------------------------------------------------------------
E6 <- predict(fm6, type="response", newdata=cruzData, interval="confidence")


## ----E62-----------------------------------------------------------------
E6 <- cbind(cruzData[,c("x","y")], E6)
head(E6, n=4)


## ----X6------------------------------------------------------------------
X <- model.matrix(~habitat+elevation+I(elevation^2)+
                  chaparral, data=cruzData)
beta <- coef(fm6) # beta estimates
E <- X %*% beta   # expected number of jays at each pixel
head(E, n=4)


## ----Ejay,include=FALSE,fig.width=6,fig.height=4-------------------------
levelplot(fit ~ x + y, data=E6, aspect="iso",
          xlab="", ylab="", main="Expected number of jays per grid cell",
          at=25:55, colorkey=list(space="bottom"),
          scales=list(draw=FALSE))


## ----Ljay,include=FALSE,fig.width=6,fig.height=4-------------------------
levelplot(lwr ~ x + y, data=E6, aspect="iso",
          xlab="", ylab="", main="Lower CI",
          at=25:55, colorkey=list(space="bottom"),
          scales=list(draw=FALSE))


## ----Ujay,include=FALSE,fig.width=6,fig.height=4-------------------------
levelplot(upr ~ x + y, data=E6, aspect="iso",
          xlab="", ylab="", main="Upper CI",
          at=25:55, colorkey=list(space="bottom"),
          scales=list(draw=FALSE))


## ----elev,include=FALSE,fig.width=6,fig.height=4-------------------------
Switzerland <- read.csv("Switzerland.csv")
levelplot(elevation ~ x + y, data=Switzerland, aspect="iso",
          col.regions=terrain.colors(100), scales=list(draw=FALSE),
          xlab="", ylab="", main="Elevation")


## ----forest,include=FALSE,fig.width=6,fig.height=4-----------------------
levelplot(forest ~ x + y, data=Switzerland, aspect="iso",
          col.regions=terrain.colors(100), scales=list(draw=FALSE),
          xlab="", ylab="", main="Forest cover")


## ----water,include=FALSE,fig.width=6,fig.height=4------------------------
levelplot(water ~ x + y, data=Switzerland, aspect="iso",
          col.regions=terrain.colors(100), scales=list(draw=FALSE),
          xlab="", ylab="", main="Water")


## ----swissData-----------------------------------------------------------
swissData <- read.csv("swissData.csv")
head(swissData, n=7)

