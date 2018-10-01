## ----voleData------------------------------------------------------------
voleData <- read.csv("microtus_data.csv")
head(voleData, 7)
str(voleData)

## ----factor--------------------------------------------------------------
voleData$food <- factor(voleData$food)
str(voleData)

## ----table---------------------------------------------------------------
table(voleData$predators, voleData$food)

## ----box1,fig.show='hide',fig.width=8,fig.height=6,size='scriptsize'-----
boxplot(voles ~ food + predators, data=voleData, ylab="Voles", cex.lab=1.5)

## ----aov1----------------------------------------------------------------
aov1 <- aov(voles ~ food * predators, data=voleData)
summary(aov1)

## ----aov2----------------------------------------------------------------
aov2 <- aov(voles ~ food + predators, data=voleData)
summary(aov2)

## ----subset1,size='scriptsize'-------------------------------------------
summary(aov(voles ~ food, data=voleData, subset=predators=="Present"))

## ----subset2,size='scriptsize'-------------------------------------------
summary(aov(voles ~ food, data=voleData, subset=predators=="Absent"))

## ----tuk,size='scriptsize'-----------------------------------------------
tuk.out <- TukeyHSD(aov1)
tuk.out$'food:predators'

## ----mtable,size='tiny'--------------------------------------------------
ybar_ij.SE <- model.tables(aov1, type="means", se=TRUE)
ybar_ij.SE

## ----gmeans--------------------------------------------------------------
ybar_ij. <- ybar_ij.SE$tables$"food:predators"
ybar_ij.

## ----SE------------------------------------------------------------------
SE_ij. <- as.numeric(ybar_ij.SE$se$"food:predators")
SE_ij.

## ----ybarSE2,fig.show='hide',fig.width=8,fig.height=6,size='tiny'--------
plot(1:3, ybar_ij.[,1], xaxt="n", xlim=c(0.5, 3.5), ylim=c(0, 50), type="b",
     pch=16, col="blue", xlab="Food", ylab="Voles", cex=1.5, cex.lab=1.5, lwd=2)
lines(1:3, ybar_ij.[,2], pch=16, col="black", type="b", cex=1.5, lty=2, lwd=2)
axis(1, 1:3, labels=c("0", "1", "2"))
arrows(1:3, ybar_ij.[,1]-SE_ij., 1:3, ybar_ij.[,1]+SE_ij., code=3, angle=90,
       length=0.05, lwd=2, col="blue")
arrows(1:3, ybar_ij.[,2]-SE_ij., 1:3, ybar_ij.[,2]+SE_ij., code=3, angle=90,
       length=0.05, lwd=2)
legend(0.5, 50, c("Absent", "Present"), col=c("blue", "black"), lty=c(1,2), title="Predators", pch=16)

## ----ybarSE3,fig.show='hide',fig.width=8,fig.height=6,size='tiny'--------
bp <- barplot(ybar_ij., xlab="Predators", args.legend=list(title="Food"),
              cex.lab=1.5, cex.names=1.4, col=c("linen", "lightblue", "turquoise"),
              ylab="Voles", beside=TRUE, legend=TRUE, ylim=c(0, 50)); box()
arrows(bp, ybar_ij., bp, ybar_ij.+SE_ij., code=2, angle=90, length=0.05, lwd=1)

