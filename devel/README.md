# Developer Guide

Building the package

    make clean
    cd cochrane; R -e 'devtools::document()'; cd ..
    make build
    make file=cochrane_0.1.tar.gz check

Install and test in R

    detach("package:cochrane", unload=TRUE)

    R CMD REMOVE cochrane
    R CMD INSTALL cochrane_0.1.tar.gz
