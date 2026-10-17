# Two Independent Sample t Test

`ifr_ts_ind_ttest` compares the means of two independent groups in order
to determine whether there is statistical evidence that the associated
population means are significantly different.

## Usage

``` r
ifr_ts_ind_ttest(
  data,
  x,
  y,
  confint = 0.95,
  alternative = c("both", "less", "greater", "all"),
  ...
)
```

## Arguments

- data:

  a data frame

- x:

  factor; a column in `data`

- y:

  numeric; a column in `data`

- confint:

  confidence level

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of "both" (default), "greater", "less" or "all". You can specify just
  the initial letter

- ...:

  additional arguments passed to or from other methods

## Value

`ifr_ts_ind_ttest` returns an object of class `"ifr_ts_ind_ttest"`. An
object of class `"ifr_ts_ind_ttest"` is a list containing the following
components:

- levels:

  levels of `x`

- obs:

  number of observations of `y` for each level of `x`

- n:

  total number of observations

- mean:

  mean of `y` for each level of `x`

- sd:

  standard deviation of `y` for each level of `x`

- se:

  estimate of standard error of `y` for each level of `x`

- lower:

  lower limit for the mean of `y` for each level of `x`

- upper:

  upper limit for the mean of `y` for each level of `x`

- combined:

  a data frame; mean, standard deviation, standard error and confidence
  limit of mean of `y`

- mean_diff:

  difference in mean of `y` for the two groups of `x`

- se_dif:

  estimate of the standard error for difference in mean of `y` for the
  two groups of `x`

- sd_dif:

  degrees of freedom

- conf_diff:

  confidence interval for `mean_diff`

- df_pooled:

  degrees of freedom for the pooled method

- df_satterthwaite:

  degrees of freedom for the Satterthwaite method

- t_pooled:

  t statistic for the pooled method

- t_satterthwaite:

  t statistic for the Satterthwaite method

- sig_pooled:

  two-sided p-value for the pooled method

- sig_pooled_l:

  lower one-sided p-value for the pooled method

- sig_pooled_u:

  upper one-sided p-value for the pooled method

- sig:

  two-sided p-value for the Satterthwaite method

- sig_l:

  lower one-sided p-value for the Satterthwaite method

- sig_u:

  upper one-sided p-value for the Satterthwaite method

- num_df:

  numerator degrees of freedom for folded f test

- den_df:

  denominator degrees of freedom for folded f test

- f:

  f value for the equality of variances test

- f_sig:

  p-value for the folded f test

- var_y:

  name of `y`

- confint:

  confidence level

- alternative:

  alternative hypothesis

## Deprecated Function

`infer_ts_ind_ttest()` has been deprecated. Instead use
`ifr_ts_ind_ttest()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html)

## Examples

``` r
# lower tail
ifr_ts_ind_ttest(hsb, female, write, alternative = 'less')
#>                               Group Statistics                                
#> -----------------------------------------------------------------------------
#>   Group       Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> -----------------------------------------------------------------------------
#>     0          91    50.121      1.080       10.305       47.97539   52.26661  
#>     1         109    54.991      0.779        8.134       53.44689   56.53511  
#> -----------------------------------------------------------------------------
#>  combined     200    52.775      0.67         9.479       51.45379   54.09621  
#> -----------------------------------------------------------------------------
#>    diff       200    -4.87       1.304        9.231       -7.42623   -2.31377  
#> -----------------------------------------------------------------------------
#> 
#>                       Independent Samples Test                        
#>                       ------------------------                        
#> 
#>                   Ho: mean(0) - mean(1) = diff = 0                    
#>                             Ha: diff < 0                              
#> 
#> ---------------------------------------------------------------------
#>  Variable        Method        Variances    DF     t Value     P < t  
#> ---------------------------------------------------------------------
#>   write          Pooled          Equal      198    -3.7341    0.0001  
#>   write       Satterthwaite     Unequal     170    -3.6564    0.0002  
#> ---------------------------------------------------------------------
#> 
#>                 Test for Equality of Variances                  
#> ---------------------------------------------------------------
#>  Variable      Method     Num DF    Den DF    F Value    P > F  
#> ---------------------------------------------------------------
#>   write       Folded F      90       108       1.605     0.0188 
#> ---------------------------------------------------------------

# upper tail
ifr_ts_ind_ttest(hsb, female, write, alternative = 'greater')
#>                               Group Statistics                                
#> -----------------------------------------------------------------------------
#>   Group       Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> -----------------------------------------------------------------------------
#>     0          91    50.121      1.080       10.305       47.97539   52.26661  
#>     1         109    54.991      0.779        8.134       53.44689   56.53511  
#> -----------------------------------------------------------------------------
#>  combined     200    52.775      0.67         9.479       51.45379   54.09621  
#> -----------------------------------------------------------------------------
#>    diff       200    -4.87       1.304        9.231       -7.42623   -2.31377  
#> -----------------------------------------------------------------------------
#> 
#>                       Independent Samples Test                        
#>                       ------------------------                        
#> 
#>                   Ho: mean(0) - mean(1) = diff = 0                    
#>                             Ha: diff > 0                              
#> 
#> ---------------------------------------------------------------------
#>  Variable        Method        Variances    DF     t Value     P > t  
#> ---------------------------------------------------------------------
#>   write          Pooled          Equal      198    -3.7341    0.9999  
#>   write       Satterthwaite     Unequal     170    -3.6564    0.9998  
#> ---------------------------------------------------------------------
#> 
#>                 Test for Equality of Variances                  
#> ---------------------------------------------------------------
#>  Variable      Method     Num DF    Den DF    F Value    P > F  
#> ---------------------------------------------------------------
#>   write       Folded F      90       108       1.605     0.0188 
#> ---------------------------------------------------------------

# both tails
ifr_ts_ind_ttest(hsb, female, write, alternative = 'both')
#>                               Group Statistics                                
#> -----------------------------------------------------------------------------
#>   Group       Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> -----------------------------------------------------------------------------
#>     0          91    50.121      1.080       10.305       47.97539   52.26661  
#>     1         109    54.991      0.779        8.134       53.44689   56.53511  
#> -----------------------------------------------------------------------------
#>  combined     200    52.775      0.67         9.479       51.45379   54.09621  
#> -----------------------------------------------------------------------------
#>    diff       200    -4.87       1.304        9.231       -7.42623   -2.31377  
#> -----------------------------------------------------------------------------
#> 
#>                       Independent Samples Test                        
#>                       ------------------------                        
#> 
#>                   Ho: mean(0) - mean(1) = diff = 0                    
#>                             Ha: diff ~= 0                             
#> 
#> ---------------------------------------------------------------------
#>  Variable        Method        Variances    DF     t Value    P > |t| 
#> ---------------------------------------------------------------------
#>   write          Pooled          Equal      198    -3.7341    0.0002  
#>   write       Satterthwaite     Unequal     170    -3.6564    0.0003  
#> ---------------------------------------------------------------------
#> 
#>                 Test for Equality of Variances                  
#> ---------------------------------------------------------------
#>  Variable      Method     Num DF    Den DF    F Value    P > F  
#> ---------------------------------------------------------------
#>   write       Folded F      90       108       1.605     0.0188 
#> ---------------------------------------------------------------

# all tails
ifr_ts_ind_ttest(hsb, female, write, alternative = 'all')
#>                               Group Statistics                                
#> -----------------------------------------------------------------------------
#>   Group       Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
#> -----------------------------------------------------------------------------
#>     0          91    50.121      1.080       10.305       47.97539   52.26661  
#>     1         109    54.991      0.779        8.134       53.44689   56.53511  
#> -----------------------------------------------------------------------------
#>  combined     200    52.775      0.67         9.479       51.45379   54.09621  
#> -----------------------------------------------------------------------------
#>    diff       200    -4.87       1.304        9.231       -7.42623   -2.31377  
#> -----------------------------------------------------------------------------
#> 
#>                         Independent Samples Test                         
#>                       ------------------------                        
#> 
#>                     Ho: mean(0) - mean(1) = diff = 0                     
#> 
#>       Ha: diff < 0            Ha: diff ~= 0             Ha: diff > 0       
#> 
#>                                   Pooled                                   
#> ------------------------------------------------------------------------
#>        t = -3.7341              t = -3.7341              t = -3.7341       
#>      P < t = 0.0001          P > |t| = 0.0002          P > t = 0.9999      
#> 
#>                               Satterthwaite                                
#> ------------------------------------------------------------------------
#>        t = -3.6564              t = -3.6564              t = -3.6564       
#>      P < t = 0.0002          P > |t| = 0.0003          P > t = 0.9998      
#> 
#> 
#>                 Test for Equality of Variances                  
#> ---------------------------------------------------------------
#>  Variable      Method     Num DF    Den DF    F Value    P > F  
#> ---------------------------------------------------------------
#>   write       Folded F      90       108       1.605     0.0188 
#> ---------------------------------------------------------------
```
