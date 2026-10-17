# One Sample Variance Comparison Test

`ifr_os_var_test` performs tests on the equality of standard deviations
(variances).It tests that the standard deviation of a sample is equal to
a hypothesized value.

## Usage

``` r
ifr_os_var_test(
  data,
  x,
  sd,
  confint = 0.95,
  alternative = c("both", "less", "greater", "all"),
  ...
)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- x:

  numeric; column in `data`

- sd:

  hypothesised standard deviation

- confint:

  confidence level

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of "both" (default), "greater", "less" or "all". You can specify just
  the initial letter

- ...:

  additional arguments passed to or from other methods

## Value

`ifr_os_var_test` returns an object of class `"ifr_os_var_test"`. An
object of class `"ifr_os_var_test"` is a list containing the following
components:

- n:

  number of observations

- sd:

  hypothesised standard deviation of `x`

- sigma:

  observed standard deviation

- se:

  estimated standard error

- chi:

  chi-square statistic

- df:

  degrees of freedom

- p_lower:

  lower one-sided p-value

- p_upper:

  upper one-sided p-value

- p_two:

  two-sided p-value

- xbar:

  mean of `x`

- c_lwr:

  lower confidence limit of standard deviation

- c_upr:

  upper confidence limit of standard deviation

- var_name:

  name of `x`

- conf:

  confidence level

- type:

  alternative hypothesis

## Deprecated Function

`infer_os_var_test()` has been deprecated. Instead use
`ifr_os_var_test()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## See also

[`var.test`](https://rdrr.io/r/stats/var.test.html)

## Examples

``` r
# lower tail
ifr_os_var_test(mtcars, mpg, 5, alternative = 'less')
#>                             One-Sample Statistics                             
#> -----------------------------------------------------------------------------
#>  Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> -----------------------------------------------------------------------------
#>    mpg       32     20.0906     1.0654       6.0269        3.8737    10.6527   
#> -----------------------------------------------------------------------------
#> 
#>             Lower Tail Test           
#>             ---------------           
#>            Ho: sd(mpg) >= 5           
#>             Ha: sd(mpg) < 5            
#> 
#>     Chi-Square Test for Variance      
#> -------------------------------------
#>  Variable       c       DF      Sig       
#> -------------------------------------
#>    mpg       45.042     31     0.9506  
#> -------------------------------------

# upper tail
ifr_os_var_test(mtcars, mpg, 5, alternative = 'greater')
#>                             One-Sample Statistics                             
#> -----------------------------------------------------------------------------
#>  Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> -----------------------------------------------------------------------------
#>    mpg       32     20.0906     1.0654       6.0269        3.8737    10.6527   
#> -----------------------------------------------------------------------------
#> 
#>             Upper Tail Test           
#>             ---------------           
#>            Ho: sd(mpg) <= 5           
#>             Ha: sd(mpg) > 5            
#> 
#>     Chi-Square Test for Variance      
#> -------------------------------------
#>  Variable       c       DF      Sig    
#> -------------------------------------
#>    mpg       45.042     31     0.0494  
#> -------------------------------------

# both tails
ifr_os_var_test(mtcars, mpg, 5, alternative = 'both')
#>                             One-Sample Statistics                             
#> -----------------------------------------------------------------------------
#>  Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> -----------------------------------------------------------------------------
#>    mpg       32     20.0906     1.0654       6.0269        3.8737    10.6527   
#> -----------------------------------------------------------------------------
#> 
#>              Two Tail Test            
#>             ---------------           
#>             Ho: sd(mpg) = 5           
#>            Ha: sd(mpg) != 5            
#> 
#>     Chi-Square Test for Variance      
#> -------------------------------------
#>  Variable       c       DF      Sig    
#> -------------------------------------
#>    mpg       45.042     31     0.0988  
#> -------------------------------------

# all tails
ifr_os_var_test(mtcars, mpg, 5, alternative = 'all')
#>                             One-Sample Statistics                             
#> -----------------------------------------------------------------------------
#>  Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> -----------------------------------------------------------------------------
#>    mpg       32     20.0906     1.0654       6.0269        3.8737    10.6527   
#> -----------------------------------------------------------------------------
#> 
#>                                 Ho: sd(mpg) = 5                                
#> 
#>          Ha: sd < 5                Ha: sd != 5                 Ha: sd > 5         
#>         c = 45.0419                c = 45.0419                c = 45.0419       
#>      Pr(C < c) = 0.9506       2 * Pr(C > c) = 0.0988       Pr(C > c) = 0.0494    
```
