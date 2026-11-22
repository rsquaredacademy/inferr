
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inferr

> Tools for Statistical Inference

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/inferr)](https://cran.r-project.org/package=inferr)
[![R-CMD-check](https://github.com/rsquaredacademy/inferr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rsquaredacademy/inferr/actions/workflows/R-CMD-check.yaml)
[![Coverage
status](https://app.codecov.io/gh/rsquaredacademy/inferr/branch/master/graph/badge.svg)](https://app.codecov.io/github/rsquaredacademy/inferr?branch=master)
<!-- badges: end -->

## Overview

inferr builds upon the statistical tests provided in **stats**, provides
additional and flexible input options and more detailed and structured
test results. As of version 0.3, **inferr** includes a select set of
parametric and non-parametric statistical tests which are listed below:

- One Sample t Test
- Paired Sample t Test
- Independent Sample t Test
- One Sample Proportion Test
- Two Sample Proportion Test
- One Sample Variance Test
- Two Sample Variance Test
- Binomial Test
- ANOVA
- Chi Square Goodness of Fit Test
- Chi Square Independence Test
- Levene’s Test
- Cochran’s Q Test
- McNemar Test
- Runs Test for Randomness

## Installation

``` r
# install inferr from CRAN
install.packages("inferr")

# the development version from github
# install.packages("devtools")
devtools::install_github("rsquaredacademy/inferr")
```

## Articles

- [Introduction to
  inferr](https://inferr.rsquaredacademy.com/articles/intro.html)

## Usage

The examples use `wholesale_customers` (UCI, CC BY 4.0) and the
simulated `campaign_responses`.

#### One Sample t Test

``` r
w <- transform(wholesale_customers, log_grocery = log(grocery))
```

``` r
# Is average grocery spending different from 4,000? Use logs because spending is right-skewed.
ifr_os_t_test(log(wholesale_customers$grocery), mu = log(4000))
#>                               One-Sample Statistics                               
#> ---------------------------------------------------------------------------------
#>  Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> ---------------------------------------------------------------------------------
#>     x        440    8.4412     0.0532       1.1162        8.3366     8.5457   
#> ---------------------------------------------------------------------------------
#> 
#>                                   Two Tail Test                                  
#>                                  ---------------                                  
#> 
#>                           Ho: mean(x) ~=8.29404964010203                         
#>                           Ha: mean(x) !=8.29404964010203                          
#> --------------------------------------------------------------------------------
#>  Variable      t      DF       Sig       Mean Diff.    [95% Conf. Interval] 
#> --------------------------------------------------------------------------------
#>     x        2.765    439    0.00594       0.1471        0.0425     0.2517   
#> --------------------------------------------------------------------------------
```

#### ANOVA

``` r
# Does average grocery spending differ by region? Use logs because spending is right-skewed.
ifr_oneway_anova(w, log_grocery, region)
#>                                 ANOVA                                  
#> ----------------------------------------------------------------------
#>                   Sum of                                              
#>                   Squares    DF     Mean Square      F        Sig.    
#> ----------------------------------------------------------------------
#> Between Groups     3.866      2        1.933       1.556    0.2122285 
#> Within Groups     543.057    437       1.243                          
#> Total             546.923    439                                      
#> ----------------------------------------------------------------------
#> 
#>                  Report                   
#> -----------------------------------------
#> Category     N       Mean      Std. Dev. 
#> -----------------------------------------
#>  lisbon     77      8.358        1.069   
#>  porto      47      8.703        0.905   
#>  other      316     8.423        1.153   
#> -----------------------------------------
#> 
#> Number of obs = 440       R-squared     = 0.0071 
#> Root MSE      = 1.1148    Adj R-squared = 0.0025
```

#### Chi Square Test of Independence

``` r
# Is customer channel associated with region?
ifr_chisq_assoc_test(wholesale_customers, channel, region)
#>                Chi Square Statistics                 
#> 
#> Statistics                     DF    Value      Prob 
#> ----------------------------------------------------
#> Chi-Square                     2    4.3491    0.1137
#> Likelihood Ratio Chi-Square    2    4.4609    0.1075
#> Phi Coefficient                         NA          
#> Contingency Coefficient             0.0989          
#> Cramer's V                          0.0994          
#> ----------------------------------------------------
```

#### Levene’s Test

``` r
# Do retail and hotel/restaurant/cafe customers have equal variance in log grocery spending?
ifr_levene_test(w, log_grocery, group_var = channel)
#>                   Summary Statistics                    
#>        Levels            Frequency    Mean    Std. Dev  
#> -------------------------------------------------------
#> hotel_restaurant_cafe       298       7.93      0.93    
#>        retail               142       9.51       0.6    
#> -------------------------------------------------------
#>         Total               440       8.44      1.12    
#> -------------------------------------------------------
#> 
#>                              Test Statistics                               
#> --------------------------------------------------------------------------
#> Statistic                            Num DF    Den DF          F    Pr > F 
#> --------------------------------------------------------------------------
#> Brown and Forsythe                        1       438    12.2545     5e-04 
#> Levene                                    1       438    12.0327     6e-04 
#> Brown and Forsythe (Trimmed Mean)         1       438    12.2628     5e-04 
#> --------------------------------------------------------------------------
```

#### Cochran’s Q Test

``` r
# Did the three campaigns get the same response rate?
ifr_cochran_qtest(campaign_responses, promo_1, promo_2, promo_3)
#>          Test Statistics           
#> ----------------------------------
#> N                              200 
#> Cochran's Q              0.4592593 
#> df                               2 
#> p value                     0.7948 
#> ----------------------------------
```

#### McNemar Test

``` r
# Did the promotion change the proportion of customers who bought?
ifr_mcnemar_test(campaign_responses, bought_before, bought_after)
#>            Controls 
#> ---------------------------------
#> Cases      0        1       Total 
#> ---------------------------------
#>   0        74       70        144 
#>   1        20       36         56 
#> ---------------------------------
#> Total      94      106        200 
#> ---------------------------------
#> 
#>         McNemar's Test         
#> ------------------------------
#> McNemar's chi2         27.7778 
#> DF                           1 
#> Pr > chi2             < 0.0001 
#> Exact Pr >= chi2      < 0.0001 
#> ------------------------------
#> 
#>        Kappa Coefficient         
#> --------------------------------
#> Kappa                     0.1231 
#> ASE                       0.0609 
#> 95% Lower Conf Limit      0.0039 
#> 95% Upper Conf Limit      0.2424 
#> --------------------------------
#> 
#> Proportion With Factor 
#> ----------------------
#> cases             0.72 
#> controls          0.47 
#> ratio           1.5319 
#> odds ratio         3.5 
#> ----------------------
```

## Getting Help

If you encounter a bug, please file a minimal reproducible example using
[reprex](https://reprex.tidyverse.org/index.html) on github. For
questions and clarifications, ask on Stack Overflow with the `r` tag.

## Code of Conduct

Please note that the inferr project is released with a [Contributor Code
of Conduct](https://inferr.rsquaredacademy.com/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

<div data-rsa-signup="" data-site="inferr.rsquaredacademy.com"
data-privacy-url="https://www.rsquaredacademy.com/privacy-policy.html">

</div>
