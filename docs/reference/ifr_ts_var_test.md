# Two Sample Variance Comparison Test

`ifr_ts_var_test` performs tests on the equality of standard deviations
(variances).

## Usage

``` r
ifr_ts_var_test(
  data,
  ...,
  group_var = NULL,
  alternative = c("less", "greater", "all")
)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- ...:

  numeric; column(s) in `data`

- group_var:

  factor; column in `data`

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of "both" (default), "greater", "less" or "all". You can specify just
  the initial letter.

## Value

`ifr_ts_var_test` returns an object of class `"ifr_ts_var_test"`. An
object of class `"ifr_ts_var_test"` is a list containing the following
components:

- f:

  f statistic

- lower:

  lower one-sided p-value

- upper:

  upper one-sided p-value

- two_tail:

  two-sided p-value

- vars:

  variances for each level of the grouping variable

- avgs:

  means for each level of the grouping variable

- sds:

  standard deviations for each level of the grouping variable

- ses:

  standard errors for each level of the grouping variable

- avg:

  combined mean

- sd:

  combined standard deviation

- se:

  estimated combined standard error

- n1:

  numerator degrees of freedom

- n2:

  denominator degrees of freedom

- lens:

  number of observations for each level of grouping variable

- len:

  number of observations

- lev:

  levels of the grouping variable

- type:

  alternative hypothesis

## Deprecated Function

`infer_ts_var_test()` has been deprecated. Instead use
`ifr_ts_var_test()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## See also

[`var.test`](https://rdrr.io/r/stats/var.test.html)

## Examples

``` r
# using grouping variable
ifr_ts_var_test(hsb, read, group_var = female, alternative = 'less')
#>                Variance Ratio Test                 
#> --------------------------------------------------
#>   Group      Obs    Mean     Std. Err.    Std. Dev. 
#> --------------------------------------------------
#>    0        91     52.82       1.1         10.51   
#>    1        109    51.73      0.96         10.06   
#> --------------------------------------------------
#>  combined    200    52.23      0.72         10.25   
#> --------------------------------------------------
#> 
#>            Lower Tail Test           
#>            ---------------           
#>          ratio = sd(0) / (1)         
#>             Ho: ratio = 1            
#>             Ha: ratio < 1             
#> 
#>         Variance Ratio Test          
#> ------------------------------------
#>    F       Num DF    Den DF      p    
#> ------------------------------------
#>  1.0913      90       108      0.6694 
#> ------------------------------------

# using two variables
ifr_ts_var_test(hsb, read, write, alternative = 'less')
#>                Variance Ratio Test                 
#> --------------------------------------------------
#>   Group      Obs    Mean     Std. Err.    Std. Dev. 
#> --------------------------------------------------
#>   read      200    52.23      0.72         10.25   
#>  write      200    52.77      0.67         9.48    
#> --------------------------------------------------
#>  combined    400    52.5       0.49         9.86    
#> --------------------------------------------------
#> 
#>            Lower Tail Test           
#>            ---------------           
#>       ratio = sd(read) / (write)     
#>             Ho: ratio = 1            
#>             Ha: ratio < 1             
#> 
#>         Variance Ratio Test          
#> ------------------------------------
#>    F       Num DF    Den DF      p    
#> ------------------------------------
#>  1.1701     199       199      0.8656 
#> ------------------------------------
```
