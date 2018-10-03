## ----import,size='small'-------------------------------------------------
gypsyData <- read.csv("gypsyData.csv")
str(gypsyData)

## ----to-factor,size='small'----------------------------------------------
gypsyData$Plot <- factor(gypsyData$Plot)
table(gypsyData$Treatment, gypsyData$Plot)

## ----aov-wrong,size='small'----------------------------------------------
aov.wrong <- aov(larvae ~ Treatment + Plot,
                 data=gypsyData)

## ----aov-wrong-out,size='small'------------------------------------------
summary(aov.wrong)

## ----aov-correct,size='small'--------------------------------------------
aov.correct <- aov(larvae ~ Treatment + Error(Plot),
                   data=gypsyData)

## ----aov-correct-out,size='small'----------------------------------------
summary(aov.correct)

## ----agg-----------------------------------------------------------------
plotData <- aggregate(formula=larvae ~ Treatment + Plot,
                      data=gypsyData, FUN=mean)

## ----agg-out-------------------------------------------------------------
plotData

## ----aov-plot,size='scriptsize'------------------------------------------
aov.plot <- aov(larvae ~ Treatment, data=plotData)
summary(aov.plot)

## ----aov-plot-out,size='scriptsize'--------------------------------------
summary(aov.correct)

## ----lme1----------------------------------------------------------------
library(nlme)
library(multcomp)
lme1 <- lme(larvae ~ Treatment, random=~1|Plot,
            data=gypsyData)

## ----anova-lme1----------------------------------------------------------
anova(lme1, Terms="Treatment")

## ----VarCorr-------------------------------------------------------------
VarCorr(lme1)

## ----ranef---------------------------------------------------------------
round(ranef(lme1), 2)

## ----tuk,size='scriptsize'-----------------------------------------------
tuk <- glht(lme1, linfct=mcp(Treatment="Tukey"))

## ----tuk-out,size='scriptsize'-------------------------------------------
summary(tuk)

