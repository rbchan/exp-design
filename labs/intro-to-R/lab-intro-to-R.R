## ----add2----------------------------------------------------------------
2+2


## ----sq3, size='small'---------------------------------------------------
sqrt(3)


## ----six2, size='small'--------------------------------------------------
6^2


## ----cos-pi, size='small'------------------------------------------------
cos(pi)


## ----y-def---------------------------------------------------------------
y <- 2


## ----y-------------------------------------------------------------------
y


## ----y-math, size='small'------------------------------------------------
y*2+1


## ----z-def, size='small'-------------------------------------------------
z <- c(-1, 9, 33, -4)
z


## ----x1, size='small'----------------------------------------------------
x1 <- 1:3 # Same as: x1 <- c(1, 2, 3)
          # Note: anything after "#" is a comment
x1


## ----x2, size='small'----------------------------------------------------
x2 <- seq(from=1, to=7, by=2)
x2


## ----rep, size='small'---------------------------------------------------
rep(x2, times=2)


## ----help,eval=FALSE-----------------------------------------------------
## ?rep
## help(rep)


## ----y1-class, size='small'----------------------------------------------
y1 <- c(2.1, 3.5, 99.0)
class(y1)


## ----y2-factor, size='small'---------------------------------------------
y2 <- factor(c("Treatment", "Control", "Treatment"))
y2


## ----wt-ht---------------------------------------------------------------
weight <- c(60, 72, 57, 90, 95, 72)
height <- c(1.8, 1.8, 1.7, 1.9, 1.7, 1.9)


## ----bmi, size='footnotesize'--------------------------------------------
BMI <- weight/height^2
BMI


## ----y-sum, size='footnotesize'------------------------------------------
y <- c(4,7,2,3,150)
sum(y)


## ----y-mean, size='footnotesize'-----------------------------------------
mean(y)


## ----y-var, size='footnotesize'------------------------------------------
var(y)


## ----y-sub1, size='small'------------------------------------------------
y <- c(2, 4, 8, 4, 25)
y.sub1 <- y[c(1,3)]
y.sub1


## ----y-sub2, size='small'------------------------------------------------
y.sub2 <- y[-2]
y.sub2


## ----y-sub3, size='small'------------------------------------------------
y.re <- y[c(5,4,3,2,1)]
y.re


## ----y-g4, size='small'--------------------------------------------------
y <- c(2, 4, 6, 4, 25)
y>4


## ----y-g4-sub, size='small'----------------------------------------------
y.sub4 <- y[y>4]
y.sub4


## ----datfr1, size='small'------------------------------------------------
y <- c(3, 9, 7, 4)
x1 <- factor(c('High', 'High', 'Low', 'Low'))
x2 <- c(2.2, 3.4, 4.4, 3.9)
mydata <- data.frame(Goats=y, Elev=x1, Temp=x2)
rownames(mydata) <- c('Site1', 'Site2', 'Site3', 'Site4')
mydata


## ----mydat-sub1, size='footnotesize'-------------------------------------
mydata[1,c(1,3)]


## ----mydat-sub2, size='footnotesize'-------------------------------------
mydata[c('Site2', 'Site3'), c('Goats', 'Temp')]


## ----mydat-sub3, size='footnotesize'-------------------------------------
mydata$Elev


## ----eval=FALSE, size='footnotesize'-------------------------------------
## View(mydata)


## ----smry, size='scriptsize'---------------------------------------------
summary(mydata)


## ----str, size='scriptsize'----------------------------------------------
str(mydata)


## ----export, size='small'------------------------------------------------
write.csv(mydata, file="mydata.csv")


## ----lsf, size='scriptsize', eval=FALSE----------------------------------
## getwd() # Go to this location and look for 'mydata.csv'


## ----read-csv, size='small'----------------------------------------------
mydata2 <- read.csv("mydata.csv")


## ----getwd, size='small'-------------------------------------------------
getwd()


## ----setwd, size='small', eval=FALSE-------------------------------------
## ## Note the forward slashes, which could be replaced by "\\"
## setwd("C:/work/courses/")


## ----ls, size='scriptsize'-----------------------------------------------
ls()


## ----rm, size='scriptsize'-----------------------------------------------
rm(x1, x2, mydata2, y, y1, y2, y.re,
   y.sub1, y.sub2, y.sub4, height, weight, BMI, z)
ls()


## ----save-image,eval=TRUE------------------------------------------------
save.image("myimage.RData")  ## Save workspace
rm(list=ls())                ## Remove all objects
ls()                         ## All are gone


## ----load-image,eval=TRUE,size='small'-----------------------------------
load("myimage.RData")        ## Load the saved workspace
ls()                         ## Check that the objects are back


## ----eval=FALSE----------------------------------------------------------
## help.start()

