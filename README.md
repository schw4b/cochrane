# cochrane: Import Data from the Cochrane Database of Systematic Reviews (CDSR) <img src="man/figures/cochrane_hex.png" align="right" width=120 height=139 alt="" />

The Cochrane Library is probably the largest body of evidence in health care. This packages loads datasets from systematic reviews from https://www.cochranelibrary.com that include all the study data, effect estimates, and the combined effect estimates from meta-analyses.

## User guide

### Installation from github

    remotes::install_github("schw4b/cochrane")

To read a vignette (see below), the package must be installed as follows. Be aware that this process requires [Pandoc](https://pandoc.org/installing.html) for users who work with R directly rather than through RStudio (RStudio includes its own copy of Pandoc.).
    
    remotes::install_github("schw4b/cochrane", build_vignettes = TRUE)

### Loading package

    library(cochrane)
    
### Please cite this package in your research output

    citation("cochrane")
    
### Open vignette
    
    vignette("cochrane")

## Developer guide

### Package building

    export LC_CTYPE='C'

    make clean
    cd cochrane; R -e 'devtools::document()'; cd ..
    make build
    make file=cochrane_0.1.tar.gz check

### Install package locally

    detach("package:swt", unload=TRUE)

    R CMD REMOVE cochrane
    R CMD INSTALL cochrane_0.1.tar.gz
