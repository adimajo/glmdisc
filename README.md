![CRAN package](https://www.r-pkg.org/badges/version-ago/glmdisc)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/glmdisc)
![R package](https://github.com/adimajo/glmdisc/workflows/R%20package/badge.svg)
[![Travis build status](https://travis-ci.org/adimajo/glmdisc.svg?branch=master)](https://travis-ci.org/adimajo/glmdisc)
[![Coverage status](https://codecov.io/gh/adimajo/glmdisc/branch/master/graph/badge.svg)](https://codecov.io/github/adimajo/glmdisc?branch=master)

# Feature quantization for parsimonious and interpretable models

Table of Contents
-----------------

* [Documentation](https://adimajo.github.io/glmdisc)
* [Installation instructions](#-installation)
* [Open an issue](https://github.com/adimajo/glmdisc/issues/new/choose)

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

## Documentation

### Through R(Studio)

#### Getting help

The help pages of this package can be reached by typing `help(glmdisc)` (or `?glmdisc`) once the package is installed and loaded.

#### Vignette

For further instructions, go to `vignettes` > `glmdisc.Rmd` or go through the vignette directly in RStudio by typing `vignette('glmdisc')` once the package is installed and loaded.

### Online

Both the help pages and the vignette are available online as [a Github page built with `pkgdown`](http://adimajo.github.io/glmdisc).
