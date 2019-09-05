[![Travis build status](https://travis-ci.org/adimajo/glmdisc.svg?branch=master)](https://travis-ci.org/adimajo/glmdisc)
[![Coverage status](https://codecov.io/gh/adimajo/glmdisc/branch/master/graph/badge.svg)](https://codecov.io/github/adimajo/glmdisc?branch=master)

# glmdisc

The goal of glmdisc is to discretize continuous features, merge factor levels, and sparsely introduce interactions in logistic regression models.

## Installation

You can install the development version of `glmdisc` from Github with:

``` r
# install.packages("devtools")
devtools::install_github("adimajo/glmdisc", build_vignettes = TRUE)
```

Or alternatively directly from CRAN:
``` r
install.packages("glmdisc")
```

## Example

For further instructions, go to vignettes > glmdisc.Rmd or go through the vignette directly in RStudio by typing "vignette('glmdisc')" once the package is installed and loaded.