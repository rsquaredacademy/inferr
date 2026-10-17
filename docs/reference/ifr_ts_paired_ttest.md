# Paired t test

`ifr_ts_paired_ttest` tests that two samples have the same mean,
assuming paired data.

## Usage

``` r
ifr_ts_paired_ttest(
  data,
  x,
  y,
  confint = 0.95,
  alternative = c("both", "less", "greater", "all")
)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- x:

  numeric; column in `data`

- y:

  numeric; column in `data`

- confint:

  confidence level

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of "both" (default), "greater", "less" or "all". You can specify just
  the initial letter.

## Value

`ifr_ts_paired_ttest` returns an object of class
`"ifr_ts_paired_ttest"`. An object of class `"ifr_ts_paired_ttest"` is a
list containing the following components:

- Obs:

  number of observations

- b:

  mean, standard deviation and standard error of `x`, `y` and their
  difference

- tstat:

  t statistic

- p_lower:

  lower one-sided p-value

- p_upper:

  upper one-sided p-value

- p_two_tail:

  two sided p-value

- corr:

  Correlation of `x` and `y`

- corsig:

  p-value of correlation test

- conf_int1:

  confidence interval for mean of `x`

- conf_int2:

  confidence interval for mean of `y`

- conf_int_diff:

  confidence interval for mean of difference of `x` and `y`

- df:

  degrees of freedom

- confint:

  confidence level

- alternative:

  alternative hypothesis

- var_names:

  names of `x` and `y`

- xy:

  string used in printing results of the test

## Deprecated Function

`infer_ts_paired_ttest()` has been deprecated. Instead use
`ifr_ts_paired_ttest()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html)

## Examples

``` r
# lower tail
ifr_ts_paired_ttest(hsb, read, write, alternative = 'less')
#>                          Paired Samples Statistics                           
#> ----------------------------------------------------------------------------
#> Variables    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> ----------------------------------------------------------------------------
#>    read       200    52.23     0.7249921    10.25294        50.8      53.66   
#>    write      200    52.775    0.6702372    9.478586       51.45       54.1    
#> ----------------------------------------------------------------------------
#>    diff       200    -0.545    0.6283822    8.886666       -1.78       0.69    
#> ----------------------------------------------------------------------------
#> 
#>          Paired Samples Correlations         
#> -------------------------------------------
#>   Variables      Obs    Correlation    Sig.
#>  read & write    200       0.60        0 
#> -------------------------------------------
#> 
#>           Paired Samples Test            
#>           -------------------            
#>        Ho: mean(read - write) = 0        
#>        Ha: mean(read - write) < 0        
#> 
#> ----------------------------------------
#>   Variables         t       df     Sig.  
#> ----------------------------------------
#>  read - write    -0.8673    199    0.193 
#> ----------------------------------------

# upper tail
ifr_ts_paired_ttest(hsb, read, write, alternative = 'greater')
#>                          Paired Samples Statistics                           
#> ----------------------------------------------------------------------------
#> Variables    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> ----------------------------------------------------------------------------
#>    read       200    52.23     0.7249921    10.25294        50.8      53.66   
#>    write      200    52.775    0.6702372    9.478586       51.45       54.1    
#> ----------------------------------------------------------------------------
#>    diff       200    -0.545    0.6283822    8.886666       -1.78       0.69    
#> ----------------------------------------------------------------------------
#> 
#>          Paired Samples Correlations         
#> -------------------------------------------
#>   Variables      Obs    Correlation    Sig.
#>  read & write    200       0.60        0 
#> -------------------------------------------
#> 
#>           Paired Samples Test            
#>           -------------------            
#>        Ho: mean(read - write) = 0        
#>        Ha: mean(read - write) > 0        
#> 
#> ----------------------------------------
#>   Variables         t       df     Sig.  
#> ----------------------------------------
#>  read - write    -0.8673    199    0.807 
#> ----------------------------------------

# both tails
ifr_ts_paired_ttest(hsb, read, write, alternative = 'both')
#>                          Paired Samples Statistics                           
#> ----------------------------------------------------------------------------
#> Variables    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> ----------------------------------------------------------------------------
#>    read       200    52.23     0.7249921    10.25294        50.8      53.66   
#>    write      200    52.775    0.6702372    9.478586       51.45       54.1    
#> ----------------------------------------------------------------------------
#>    diff       200    -0.545    0.6283822    8.886666       -1.78       0.69    
#> ----------------------------------------------------------------------------
#> 
#>          Paired Samples Correlations         
#> -------------------------------------------
#>   Variables      Obs    Correlation    Sig.
#>  read & write    200       0.60        0 
#> -------------------------------------------
#> 
#>           Paired Samples Test            
#>           -------------------            
#>        Ho: mean(read - write) = 0        
#>       Ha: mean(read - write) ~= 0        
#> 
#> ----------------------------------------
#>   Variables         t       df     Sig.  
#> ----------------------------------------
#>  read - write    -0.8673    199    0.387 
#> ----------------------------------------

# all tails
ifr_ts_paired_ttest(hsb, read, write, alternative = 'all')
#>                          Paired Samples Statistics                           
#> ----------------------------------------------------------------------------
#> Variables    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> ----------------------------------------------------------------------------
#>    read       200    52.23     0.7249921    10.25294        50.8      53.66   
#>    write      200    52.775    0.6702372    9.478586       51.45       54.1    
#> ----------------------------------------------------------------------------
#>    diff       200    -0.545    0.6283822    8.886666       -1.78       0.69    
#> ----------------------------------------------------------------------------
#> 
#>          Paired Samples Correlations         
#> -------------------------------------------
#>   Variables      Obs    Correlation    Sig.
#>  read & write    200       0.60        0 
#> -------------------------------------------
#> 
#>                 Ho: mean(read - write) = mean(diff) = 0                  
#> 
#>    Ha: mean(diff) < 0      Ha: mean(diff) ~= 0       Ha: mean(diff) > 0    
#>        t = -0.8673              t = -0.8673              t = -0.8673       
#>      P < t = 0.193           P > |t| = 0.387           P > t = 0.807       
```
