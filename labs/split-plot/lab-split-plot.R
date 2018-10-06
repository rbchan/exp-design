## ----meatData,size='small'-----------------------------------------------
meatData <- read.csv("meatData.csv")
str(meatData)

## ----factors,size='small'------------------------------------------------
meatData$time <- factor(meatData$time)
meatData$carcass <- factor(meatData$carcass)
meatData$roast <- factor(meatData$roast)

## ----aov1,size='small'---------------------------------------------------
aov.meat1 <- aov(Wbscore ~ tenderizer * time + carcass +
                 Error(roast), data=meatData)

## ----saov1,size='small'--------------------------------------------------
summary(aov.meat1)

## ----lme1,size='small'---------------------------------------------------
library(nlme)
lme.meat1 <- lme(Wbscore ~ tenderizer*time,
    data=meatData,
    correlation=corCompSymm(), # To make results same as aov()
    random = ~1|carcass/roast)


## ----alme1,size='small'--------------------------------------------------
anova(lme.meat1)

## ----lmeP,size='tiny'----------------------------------------------------
lme.meatP <- lme(Wbscore ~ time, data=meatData,
                 random = ~1|carcass/roast, correlation=corCompSymm(),
                 subset=tenderizer=="P")

## ----lmeV,size='tiny'----------------------------------------------------
lme.meatV <- lme(Wbscore ~ time, data=meatData,
                 random = ~1|carcass/roast,correlation=corCompSymm(),
                 subset=tenderizer=="V")

## ----lmeC,size='tiny'----------------------------------------------------
lme.meatC <- lme(Wbscore ~ time, data=meatData,
                 random = ~1|carcass/roast, correlation=corCompSymm(),
                 subset=tenderizer=="C")

## ----lmePVC,size='tiny'--------------------------------------------------
anova(lme.meatP, Terms="time")
anova(lme.meatV, Terms="time")
anova(lme.meatC, Terms="time")

## ----tuk,size='scriptsize'-----------------------------------------------
# install.packages("multcomp")
library(multcomp)
mcP <- glht(lme.meatP, linfct=mcp(time="Tukey"))
summary(mcP)
# confint(mcP)
# plot(mcP)

## ----mcp,include=FALSE,fig.width=8,fig.height=6--------------------------
plot(mcP) # Should do this for the other 2 levels of tenderizer too

