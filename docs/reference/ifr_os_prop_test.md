# One Sample Test of Proportion

`ifr_os_prop_test` compares proportion in one group to a specified
population proportion.

## Usage

``` r
ifr_os_prop_test(
  data,
  variable = NULL,
  prob = 0.5,
  phat = 0.5,
  alternative = c("both", "less", "greater", "all")
)

# Default S3 method
ifr_os_prop_test(
  data,
  variable = NULL,
  prob = 0.5,
  phat = 0.5,
  alternative = c("both", "less", "greater", "all")
)
```

## Arguments

- data:

  numeric vector of length 1 or a `data.frame` or `tibble`

- variable:

  factor; column in `data`

- prob:

  hypothesised proportion

- phat:

  observed proportion

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of "both" (default), "greater", "less" or "all". You can specify just
  the initial letter.

## Value

`ifr_os_prop_test` returns an object of class `"ifr_os_prop_test"`. An
object of class `"ifr_os_prop_test"` is a list containing the following
components:

- n:

  number of observations

- phat:

  proportion of 1's

- p:

  assumed probability of success

- z:

  z statistic

- sig:

  p-value for z statistic

- alt:

  alternative hypothesis

- obs:

  observed number of 0's and 1's

- exp:

  expected number of 0's and 1's

- deviation:

  deviation of observed from expected

- std:

  standardized resiudals

## Deprecated Function

`infer_os_prop_test()` has been deprecated. Instead use
`ifr_os_prop_test()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## See also

[`prop.test`](https://rdrr.io/r/stats/prop.test.html)
[`binom.test`](https://rdrr.io/r/stats/binom.test.html)

## Examples

``` r
# use as a calculator
ifr_os_prop_test(200, prob = 0.5, phat = 0.3)
#>      Test Statistics       
#> --------------------------
#> Sample Size            200 
#> Exp Prop               0.5 
#> Obs Prop               0.3 
#> z                  -5.6569 
#> Pr(|Z| > |z|)            0 
#> 
#> -----------------------------------------------------------------
#> Category    Observed    Expected    % Deviation    Std. Residuals 
#> -----------------------------------------------------------------
#>    0          140         100          40.00            4.00      
#>    1           60         100         -40.00           -4.00      
#> -----------------------------------------------------------------

# using data set
ifr_os_prop_test(hsb, female, prob = 0.5)
#>      Test Statistics      
#> -------------------------
#> Sample Size           200 
#> Exp Prop              0.5 
#> Obs Prop            0.545 
#> z                  1.2728 
#> Pr(|Z| > |z|)      0.2031 
#> 
#> -----------------------------------------------------------------
#> Category    Observed    Expected    % Deviation    Std. Residuals 
#> -----------------------------------------------------------------
#>    0           91         100          -9.00           -0.90      
#>    1          109         100           9.00            0.90      
#> -----------------------------------------------------------------
```
