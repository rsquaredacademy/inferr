
<!-- README.md is generated from README.Rmd. Please edit that file -->
inferr
======

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/inferr)](https://cran.r-project.org/package=inferr) [![Travis-CI Build Status](https://travis-ci.org/rsquaredacademy/inferr.svg?branch=master)](https://travis-ci.org/rsquaredacademy/inferr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rsquaredacademy/inferr?branch=master&svg=true)](https://ci.appveyor.com/project/rsquaredacademy/inferr) [![](https://cranlogs.r-pkg.org/badges/grand-total/inferr)](https://cran.r-project.org/package=inferr)

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

Shiny App
---------

If you want to explore the package using a shiny app, please click [here](http://rsquaredlabs.com:3838/explorer/).

Vignettes
---------

-   [Introduction to inferr](http://www.rsquaredacademy.com/inferr/articles/index.html)

Usage
-----

##### One Sample t Test

``` r
ttest(hsb$write, mu = 50, type = 'all')
#>                               One-Sample Statistics                               
#> ---------------------------------------------------------------------------------
#>  Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> ---------------------------------------------------------------------------------
#>   write      200    52.775     0.6702       9.4786       51.4537    54.0969   
#> ---------------------------------------------------------------------------------
#> 
#>                                Ho: mean(write) ~=50                              
#> 
#>         Ha: mean < 50              Ha: mean ~= 50               Ha: mean > 50        
#>          t = 4.141                   t = 4.141                   t = 4.141         
#>        P < t = 1.0000             P > |t| = 0.0001             P > t = 0.0000
```

##### ANOVA

``` r
owanova(hsb, 'write', 'prog')
#>                                 ANOVA                                  
#> ----------------------------------------------------------------------
#>                    Sum of                                             
#>                    Squares     DF     Mean Square      F        Sig.  
#> ----------------------------------------------------------------------
#> Between Groups    3175.698      2      1587.849      21.275    0.0000 
#> Within Groups     14703.177    197      74.635                        
#> Total             17878.875    199                                    
#> ----------------------------------------------------------------------
#> 
#>                  Report                   
#> -----------------------------------------
#>  Category      N      Mean     Std. Dev. 
#> -----------------------------------------
#>     1         45     51.333        9.398 
#>     2         105    56.257        7.943 
#>     3         50     46.760        9.319 
#> -----------------------------------------
#> 
#> Number of obs = 200       R-squared     = 0.1776 
#> Root MSE      = 8.6392    Adj R-squared = 0.1693
```

##### Chi Square Test of Independence

``` r
chisq_test(as.factor(hsb$female), as.factor(hsb$schtyp))
#>                Chi Square Statistics                 
#> 
#> Statistics                     DF    Value      Prob 
#> ----------------------------------------------------
#> Chi-Square                     1    0.0470    0.8284
#> Likelihood Ratio Chi-Square    1    0.0471    0.8282
#> Continuity Adj. Chi-Square     1    0.0005    0.9822
#> Mantel-Haenszel Chi-Square     1    0.0468    0.8287
#> Phi Coefficient                     0.0153          
#> Contingency Coefficient             0.0153          
#> Cramer's V                          0.0153          
#> ----------------------------------------------------
```

##### Levene's Test

``` r
levene_test(hsb$read, group_var = hsb$race)
#>            Summary Statistics             
#> Levels    Frequency    Mean     Std. Dev  
#> -----------------------------------------
#>   1          24        46.67      10.24   
#>   2          11        51.91      7.66    
#>   3          20        46.8       7.12    
#>   4          145       53.92      10.28   
#> -----------------------------------------
#> Total        200       52.23      10.25   
#> -----------------------------------------
#> 
#>                              Test Statistics                              
#> -------------------------------------------------------------------------
#> Statistic                            Num DF    Den DF         F    Pr > F 
#> -------------------------------------------------------------------------
#> Brown and Forsythe                        3       196      3.44    0.0179 
#> Levene                                    3       196    3.4792     0.017 
#> Brown and Forsythe (Trimmed Mean)         3       196    3.3936     0.019 
#> -------------------------------------------------------------------------
```

##### Cochran's Q Test

``` r
cochran_test(exam)
#>    Test Statistics     
#> ----------------------
#> N                   15 
#> Cochran's Q       4.75 
#> df                   2 
#> p value          0.093 
#> ----------------------
```

##### McNemar Test

``` r
himath <- ifelse(hsb$math > 60, 1, 0)
hiread <- ifelse(hsb$read > 60, 1, 0)
mcnemar_test(table(himath, hiread))
#>            Controls 
#> ---------------------------------
#> Cases       0       1       Total 
#> ---------------------------------
#>   0        135      21        156 
#>   1         18      26         44 
#> ---------------------------------
#> Total      153      47        200 
#> ---------------------------------
#> 
#>        McNemar's Test        
#> ----------------------------
#> McNemar's chi2        0.2308 
#> DF                         1 
#> Pr > chi2              0.631 
#> Exact Pr >= chi2      0.7493 
#> ----------------------------
#> 
#>        Kappa Coefficient         
#> --------------------------------
#> Kappa                     0.4454 
#> ASE                        0.075 
#> 95% Lower Conf Limit      0.2984 
#> 95% Upper Conf Limit      0.5923 
#> --------------------------------
#> 
#> Proportion With Factor 
#> ----------------------
#> cases             0.78 
#> controls         0.765 
#> ratio           1.0196 
#> odds ratio      1.1667 
#> ----------------------
```

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
