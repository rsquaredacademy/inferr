# Levene's test for equality of variances

`ifr_levene_test` reports Levene's robust test statistic for the
equality of variances and the two statistics proposed by Brown and
Forsythe that replace the mean in Levene's formula with alternative
location estimators. The first alternative replaces the mean with the
median. The second alternative replaces the mean with the 10

## Usage

``` r
ifr_levene_test(data, ...)

# Default S3 method
ifr_levene_test(data, ..., group_var = NULL, trim_mean = 0.1)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- ...:

  numeric; columns in `data`

- group_var:

  factor; column in `data`

- trim_mean:

  trimmed mean

## Value

`ifr_levene_test` returns an object of class `"ifr_levene_test"`. An
object of class `"ifr_levene_test"` is a list containing the following
components:

- bf:

  Brown and Forsythe f statistic

- p_bf:

  p-value for Brown and Forsythe f statistic

- lev:

  Levene's f statistic

- p_lev:

  p-value for Levene's f statistic

- bft:

  Brown and Forsythe f statistic using trimmed mean

- p_bft:

  p-value for Brown and Forsythe f statistic using trimmed mean

- avgs:

  mean for each level of the grouping variable

- sds:

  standard deviations for each level of the grouping variable

- avg:

  combined mean

- sd:

  combined standard deviation

- n:

  number of observations

- n_df:

  numerator degrees of freedom

- d_df:

  denominator degrees of freedom

- levs:

  levels of the grouping variable

- lens:

  number of observations for each level of the grouping variable

- type:

  alternative hypothesis

## Deprecated Function

`infer_levene_test()` has been deprecated. Instead use
`ifr_levene_test()`.

## References

Bland, M. 2000. An Introduction to Medical Statistics. 3rd ed. Oxford:
Oxford University Press.

Brown, M. B., and A. B. Forsythe. 1974. Robust tests for the equality of
variances. Journal of the American Statistical Association 69: 364–367.

Carroll, R. J., and H. Schneider. 1985. A note on Levene’s tests for
equality of variances. Statistics and Probability Letters 3: 191–194.

## Examples

``` r
# using grouping variable
ifr_levene_test(hsb, read, group_var = race)
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

# using  variables
ifr_levene_test(hsb, read, write, socst)
#>            Summary Statistics             
#> Levels    Frequency    Mean     Std. Dev  
#> -----------------------------------------
#>   0          200       52.23      10.25   
#>   1          200       52.77      9.48    
#>   2          200       52.41      10.74   
#> -----------------------------------------
#> Total        600       52.47      10.15   
#> -----------------------------------------
#> 
#>                              Test Statistics                              
#> -------------------------------------------------------------------------
#> Statistic                            Num DF    Den DF         F    Pr > F 
#> -------------------------------------------------------------------------
#> Brown and Forsythe                        2       597    1.1683    0.3116 
#> Levene                                    2       597    1.3803    0.2523 
#> Brown and Forsythe (Trimmed Mean)         2       597    1.3258    0.2664 
#> -------------------------------------------------------------------------
```
