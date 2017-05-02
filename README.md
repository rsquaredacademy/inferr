
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/inferr)](https://cran.r-project.org/package=inferr) [![Travis-CI Build Status](https://travis-ci.org/rsquaredacademy/inferr.svg?branch=master)](https://travis-ci.org/rsquaredacademy/inferr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rsquaredacademy/inferr?branch=master&svg=true)](https://ci.appveyor.com/project/rsquaredacademy/inferr) [![Coverage Status](https://img.shields.io/codecov/c/github/rsquaredacademy/inferr/master.svg)](https://codecov.io/github/rsquaredacademy/inferr?branch=master) [![](https://cranlogs.r-pkg.org/badges/grand-total/inferr)](https://cran.r-project.org/package=inferr)

Overview
--------

Inferential statistics allows us to make generalizations about populations using data drawn from the population. We use them when it is impractical or impossible to collect data about the whole population under study and instead, we have a sample that represents the population under study and using inferential statistics technique, we make generalizations about the population from the sample.

The **inferr** package:

-   builds upon the statistical tests provided in **stats**
-   provides additional and flexible input options
-   more detailed and structured test results

As of version 0.1, **inferr** includes a select set of parametric and non-parametric statistical tests which are listed below:

-   One Sample t Test
-   Paired Sample t Test
-   Independent Sample t Test
-   One Sample Proportion Test
-   Two Sample Proportion Test
-   One Sample Variance Test
-   Two Sample Variance Test
-   Binomial Test
-   ANOVA
-   Chi Square Goodness of Fit Test
-   Chi Square Independence Test
-   Levene's Test
-   Cochran's Q Test
-   McNemar Test
-   Runs Test for Randomness

Installation
------------

``` r
# install inferr from CRAN
install.packages("inferr")

# the development version from github
# install.packages("devtools")
devtools::install_github("rsquaredacademy/inferr")
```

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
