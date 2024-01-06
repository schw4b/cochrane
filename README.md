# cochrane: Import Data from the Cochrane Database of Systematic Reviews (CDSR)

The Cochrane Library is probably the largest body of evidence in health care. This packages loads datasets from systematic reviews from https://www.cochranelibrary.com that include all the study data, effect estimates, and the combined effect estimates from meta-analyses.

## User guide
### Installation from github
    remotes::install_github("schw4b/cochrane")


### Loading package
    library(cochrane)

## Developer guide
### Package building
    export LC_CTYPE='C'

    make clean
    cd cochrane; R -e 'devtools::document()'; cd ..
    make build
    make doc
    make file=cochrane_0.1.tar.gz check

### Install package locally
    detach("package:swt", unload=TRUE)

    R CMD REMOVE cochrane
    R CMD INSTALL cochrane_0.1.tar.gz