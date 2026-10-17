# Two Sample Test of Proportion

Tests on the equality of proportions using large-sample statistics. It
tests that a sample has the same proportion within two independent
groups or two samples have the same proportion.

## Usage

``` r
ifr_ts_prop_test(
  data,
  var1,
  var2,
  alternative = c("both", "less", "greater", "all"),
  ...
)

ifr_ts_prop_group(
  data,
  var,
  group,
  alternative = c("both", "less", "greater", "all")
)

ifr_ts_prop_calc(
  n1,
  n2,
  p1,
  p2,
  alternative = c("both", "less", "greater", "all"),
  ...
)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- var1:

  factor; column in `data`

- var2:

  factor; column in `data`

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of "both" (default), "greater", "less" or "all". You can specify just
  the initial letter

- ...:

  additional arguments passed to or from other methods

- var:

  factor; column in `data`

- group:

  factor; column in `data`

- n1:

  sample 1 size

- n2:

  sample 2 size

- p1:

  sample 1 proportion

- p2:

  sample 2 proportion

## Value

an object of class `"ifr_ts_prop_test"`. An object of class
`"ifr_ts_prop_test"` is a list containing the following components:

- n1:

  sample 1 size

- n2:

  sample 2 size

- phat1:

  sample 1 proportion

- phat2:

  sample 2 proportion

- z:

  z statistic

- sig:

  p-value for z statistic

- alt:

  alternative hypothesis

## Deprecated Functions

`infer_ts_prop_test()`, `infer_ts_prop_grp()` and `infer_ts_prop_calc()`
have been deprecated. Instead use `ifr_ts_prop_test()`,
`ifr_ts_prop_group()` and `ifr_ts_prop_calc()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## See also

[`prop.test`](https://rdrr.io/r/stats/prop.test.html)

## Examples

``` r
# using variables
# lower tail
ifr_ts_prop_test(treatment, treatment1, treatment2,
alternative = 'less')
#>        Test Statistics        
#> -----------------------------
#> Total Observations        100 
#> z                       0.403 
#> Pr(Z < z)               0.656 
#> 

# using groups
# lower tail
ifr_ts_prop_group(treatment2, outcome, female,
alternative = 'less')
#>        Test Statistics        
#> -----------------------------
#> Total Observations        200 
#> z                       0.351 
#> Pr(Z < z)               0.637 
#> 

# using sample size and proportions
# lower tail
ifr_ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5, alternative = 'less')
#>        Test Statistics         
#> ------------------------------
#> Total Observations          55 
#> z                       -1.514 
#> Pr(Z < z)                0.065 
#> 
```
