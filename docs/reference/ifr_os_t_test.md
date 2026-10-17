# One Sample t Test

`ifr_os_t_test` performs t tests on the equality of means. It tests the
hypothesis that a sample has a mean equal to a hypothesized value.

## Usage

``` r
ifr_os_t_test(
  data,
  x,
  mu = 0,
  alpha = 0.05,
  alternative = c("both", "less", "greater", "all"),
  ...
)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- x:

  numeric; column in `data`

- mu:

  a number indicating the true value of the mean

- alpha:

  acceptable tolerance for type I error

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of "both" (default), "greater", "less" or "all". You can specify just
  the initial letter

- ...:

  additional arguments passed to or from other methods

## Value

`ifr_os_t_test` returns an object of class `"ifr_os_t_test"`. An object
of class `"ifr_os_t_test"` is a list containing the following
components:

- mu:

  a number indicating the true value of the mean

- n:

  number of observations

- df:

  degrees of freedom

- Mean:

  observed mean of `x`

- stddev:

  standard deviation of `x`

- std_err:

  estimate of standard error

- test_stat:

  t statistic

- confint:

  confidence interval for the mean

- mean_diff:

  mean difference

- mean_diff_l:

  lower confidence limit for mean difference

- mean_diff_u:

  upper confidence limit for mean difference

- p_l:

  lower one-sided p-value

- p_u:

  upper one-sided p-value

- p:

  two sided p-value

- conf:

  confidence level

- type:

  alternative hypothesis

- var_name:

  name of `x`

## Deprecated Function

`infer_os_t_test()` has been deprecated. Instead use `ifr_os_t_test()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html)

## Examples

``` r
# lower tail
ifr_os_t_test(hsb, write, mu = 50, alternative = 'less')
#>                               One-Sample Statistics                               
#> ---------------------------------------------------------------------------------
#>  Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> ---------------------------------------------------------------------------------
#>   write      200    52.775     0.6702       9.4786         -Inf     53.8828   
#> ---------------------------------------------------------------------------------
#> 
#>                                  Lower Tail Test                                 
#>                                  ---------------                                  
#> 
#>                                Ho: mean(write) >=50                              
#>                                 Ha: mean(write) <50                               
#> --------------------------------------------------------------------------------
#>  Variable      t      DF       Sig       Mean Diff.    [95% Conf. Interval] 
#> --------------------------------------------------------------------------------
#>   write      4.141    199    0.99997       2.775          -Inf      3.8828   
#> --------------------------------------------------------------------------------

# upper tail
ifr_os_t_test(hsb, write, mu = 50, alternative = 'greater')
#>                               One-Sample Statistics                               
#> ---------------------------------------------------------------------------------
#>  Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> ---------------------------------------------------------------------------------
#>   write      200    52.775     0.6702       9.4786       51.6678      Inf     
#> ---------------------------------------------------------------------------------
#> 
#>                                  Upper Tail Test                                 
#>                                  ---------------                                  
#> 
#>                                Ho: mean(write) <=50                              
#>                                Ha: mean(write) >50                                
#> --------------------------------------------------------------------------------
#>  Variable      t      DF       Sig       Mean Diff.    [95% Conf. Interval] 
#> --------------------------------------------------------------------------------
#>   write      4.141    199    0.00003       2.775         1.6678      Inf     
#> --------------------------------------------------------------------------------

# both tails
ifr_os_t_test(hsb, write, mu = 50, alternative = 'both')
#>                               One-Sample Statistics                               
#> ---------------------------------------------------------------------------------
#>  Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> ---------------------------------------------------------------------------------
#>   write      200    52.775     0.6702       9.4786       51.4537    54.0969   
#> ---------------------------------------------------------------------------------
#> 
#>                                   Two Tail Test                                  
#>                                  ---------------                                  
#> 
#>                                Ho: mean(write) ~=50                              
#>                                Ha: mean(write) !=50                               
#> --------------------------------------------------------------------------------
#>  Variable      t      DF       Sig       Mean Diff.    [95% Conf. Interval] 
#> --------------------------------------------------------------------------------
#>   write      4.141    199    0.00005       2.775         1.4537     4.0969   
#> --------------------------------------------------------------------------------

# all tails
ifr_os_t_test(hsb, write, mu = 50, alternative = 'all')
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
#>        P < t = 0.99997            P > |t| = 0.00005            P > t = 0.00003      
```
