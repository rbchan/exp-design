---
layout: page
title: Labs
---

## Overview

These labs are meant for incoming graduate students. No prior
experience with [R](https://www.r-project.org/) is assumed, but
students should have taken a basic undergraduate-level statistics
course.



## Schedule



### [Week 1 - Introduction to R](intro-to-R/intro-to-R.md)

Overview of R basics. Topics include:
- Installing R
- Creating and indexing vectors and data.frames
- Computing summary statistics
- Importing and exporting data
- Help pages



### [Week 2 - *t*-tests](t-tests/t-tests.md)

*t*-tests with and without the `t.test` function. Topics include:
- Two-sample *t*-test
- Paired *t*-test
- Graphics
  * Histograms
  * Boxplots


### [Week 3 - One-way ANOVA](ANOVA/ANOVA.md)

Analysis of data from completely randomized designs. Topics include:
- `aov` vs `lm`
- Means, effects, and SEs with `model.tables`
- Computing the ANOVA table by hand
- Multiple comparisons with `TukeyHSD`


### [Week 4 - Contrasts, estimation, and power](estimation-power/estimation-power.md)

Three somewhat unrelated topics in this lab.

1. Creating orthogonal contrasts to test specific hypotheses
2. Moving beyond null hypothesis tests to focus on effect sizes and
   confidence intervals
3. Power analysis for *t*-tests and one-way ANOVA
   - `power.t.test` and `power.anova.test`


### [Week 5 - Assumptions of ANOVA, transformations, and nonparametrics](assump-nonpar/assump-nonpar.md)

Testing assumptions and dealing with violations using transformations
or nonparametrics.

- Extracting residuals with `resid`
- Shapiro-Wilk test with `shapiro.test`
- Graphical assessments
- Log, sqrt, asin, transformations
- Kruskal-Wallis test with `kruskal.test`
- Wilcoxon rank sum test, aka Mann-Whitney test, with `wilcox.test`


### [Week 6 - Randomized complete block design](blocking/blocking.md)

Accounting for extraneous sources of variation using blocked designs

- Blocked ANOVA by hand and with `aov`
- Random block effects using `Error` in `aov`



### [Week 7 - AxB Factorial](factorial/factorial.md)

Factorial designs with just two factors. Analysis and options for
presenting the results.


### [Week 8 - Nested Designs](nested/nested.md)

Nested designs in which experimental units are subsampled.

- `aov` with multiple `Error` strata
- More flexible model fitting with the `lme` function in the `nlme` package
- The `multcomp` package for multiple comparisons



### [Week 9 - Split-plot Designs](split-plot/split-plot.md)

Split-plot designs in which treatments are applied to both whole-units
and sub-units in a blocked design.

- `aov` with multiple `Error` strata
- More flexible model fitting with the `lme` function in the `nlme` package
- The `multcomp` package for multiple comparisons


### [Week 10 - Repeated Measures Designs](repeated-measures/repeated-measures.md)

Repeated measures designs in which observations are recorded on each
"subject" on multiple time periods. The interaction of the treatment
variable and time is of interest.

- Univariate approach using `aov` and adjusted p-values
- Multivariate approach using `manova` with Wilks' lambda or Pillai's trace


### [Week 11 - ANCOVA](ANCOVA/ANCOVA.md)

Analysis of covariance in which one explantory variable is a factor,
and the other is a continuous variable.

- Model fitting with `lm`
- Predictions with `predict`



### Week 12 - Linear models

Stepping back to look at linear models



### [Week 13 - Generalized linear models](GLMs/GLMs.md)

Logistic regression and Poisson regression with `glm`
