rnw2pdf <- function(filestub, tangle=FALSE, open=TRUE, clean=TRUE) {

    ## Load knitr or install if need be
    reqval <- require(knitr)
    if(!reqval) {
        install.packages("knitr")
        library(knitr)
    }

    ## Create the .tex file
    rnw.file <- paste(filestub, ".Rnw", sep="")
    tex.file <- knit(rnw.file)

    ## Create the .R file
    if(tangle)
        knit(rnw.file, tangle=TRUE)

    ## Create the PDF
    ##tools::texi2dvi("lab-intro-to-R.tex", pdf=TRUE)
    tools::texi2dvi(tex.file, pdf=TRUE, clean=clean)

    ## Open the PDF
    ## This will only work if Rcmd open is installed
    if(open) {
        open.pdf <- paste("open ", filestub, ".pdf", sep="")
        sysval <- system(open.pdf)
        if(sysval != 0) {
            warning("\n\nThe PDF should be in your working directory")
        }
    }

}
