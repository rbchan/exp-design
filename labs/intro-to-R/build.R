
## Load knitr or install if need be
reqval <- require(knitr)
if(!reqval) {
    install.packages("knitr")
    library(knitr)
}

## Create the .tex file
knit("lab-intro-to-R.Rnw")


## Create the PDF
tools::texi2dvi("lab-intro-to-R.tex", pdf=TRUE, clean=TRUE)


## Open the PDF
## This will only work if Rcmd open is installed
sysval <- system("open lab-intro-to-R.pdf")

if(sysval != 0) {
    warning("\n\nThe PDF should be in your working directory")
}


