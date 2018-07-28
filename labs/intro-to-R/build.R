
if(!require(knitr)) {
    install.packages("knitr")
}

knit("lab-intro-to-R.Rnw")
tools::texi2dvi("lab-intro-to-R.tex", pdf=TRUE,
                clean=TRUE)

if(system("open lab-intro-to-R.pdf") != 0) {
    warning("\n\nThe PDF should be in your working directory")
}


