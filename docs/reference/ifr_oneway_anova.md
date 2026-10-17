# One Way ANOVA

One way analysis of variance

## Usage

``` r
ifr_oneway_anova(data, x, y, ...)
```

## Arguments

- data:

  a `data.frame` or a `tibble`

- x:

  numeric; column in `data`

- y:

  factor; column in `data`

- ...:

  additional arguments passed to or from other methods

## Value

`ifr_oneway_anova` returns an object of class `"ifr_oneway_anova"`. An
object of class `"ifr_oneway_anova"` is a list containing the following
components:

- adjusted_r2:

  adjusted r squared value

- df_btw:

  between groups degress of freedom

- df_within:

  within groups degress of freedom

- df_total:

  total degress of freedom

- fstat:

  f value

- group_stats:

  group statistics

- ms_btw:

  between groups mean square

- ms_within:

  within groups mean square

- obs:

  number of observations

- pval:

  p value

- r2:

  r squared value

- rmse:

  root mean squared error

- ss_between:

  between group sum of squares

- ss_within:

  within group sum of squares

- ss_total:

  total sum of squares

## Deprecated Function

`infer_oneway_anova()` has been deprecated. Instead use
`ifr_oneway_anova()`

## References

Kutner, M. H., Nachtsheim, C., Neter, J., & Li, W. (2005). Applied
linear statistical models. Boston: McGraw-Hill Irwin.

## See also

[`anova`](https://rdrr.io/r/stats/anova.html)

## Examples

``` r
ifr_oneway_anova(mtcars, mpg, cyl)
#>                                ANOVA                                 
#> --------------------------------------------------------------------
#>                    Sum of                                           
#>                   Squares     DF    Mean Square      F        Sig.  
#> --------------------------------------------------------------------
#> Between Groups    824.785     2       412.392      39.699      0    
#> Within Groups     301.263     29      10.388                        
#> Total             1126.048    31                                    
#> --------------------------------------------------------------------
#> 
#>                  Report                  
#> ----------------------------------------
#> Category    N       Mean      Std. Dev. 
#> ----------------------------------------
#>    4        11     26.664       4.510   
#>    6        7      19.743       1.454   
#>    8        14     15.100       2.560   
#> ----------------------------------------
#> 
#> Number of obs = 32        R-squared     = 0.7325 
#> Root MSE      = 3.2231    Adj R-squared = 0.714 
#> 
ifr_oneway_anova(hsb, write, prog)
#>                                 ANOVA                                  
#> ----------------------------------------------------------------------
#>                    Sum of                                             
#>                    Squares     DF     Mean Square      F        Sig.  
#> ----------------------------------------------------------------------
#> Between Groups    3175.698      2      1587.849      21.275      0    
#> Within Groups     14703.177    197      74.635                        
#> Total             17878.875    199                                    
#> ----------------------------------------------------------------------
#> 
#>                  Report                   
#> -----------------------------------------
#> Category     N       Mean      Std. Dev. 
#> -----------------------------------------
#>    1        45      51.333       9.398   
#>    2        105     56.257       7.943   
#>    3        50      46.760       9.319   
#> -----------------------------------------
#> 
#> Number of obs = 200       R-squared     = 0.1776 
#> Root MSE      = 8.6392    Adj R-squared = 0.1693 
#> 
```
