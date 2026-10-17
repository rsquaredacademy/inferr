# Chi Square Goodness of Fit Test

Test whether the observed proportions for a categorical variable differ
from hypothesized proportions

## Usage

``` r
ifr_chisq_gof_test(data, x, y, correct = FALSE)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- x:

  factor; column in `data`

- y:

  expected proportions

- correct:

  logical; if TRUE continuity correction is applied

## Value

`ifr_chisq_gof_test` returns an object of class `"ifr_chisq_gof_test"`.
An object of class `"ifr_chisq_gof_test"` is a list containing the
following components:

- categories:

  levels of `x`

- chisquare:

  chi square statistic

- deviation:

  deviation of observed from frequency

- degrees_of_freedom:

  chi square degrees of freedom

- expected_frequency:

  expected frequency/proportion

- n_levels:

  number of levels of `x`

- observed_frequency:

  observed frequency/proportion

- pvalue:

  p-value

- sample_size:

  number of observations

- std_residuals:

  standardized residuals

- varname:

  name of categorical variable

## Deprecated Function

`infer_chisq_gof_test()` has been deprecated. Instead use
`ifr_chisq_gof_test()`

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)

## Examples

``` r
ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140))
#>     Test Statistics     
#> -----------------------
#> Chi-Square       5.0286 
#> DF                    3 
#> Pr > Chi Sq      0.1697 
#> Sample Size         200 
#> 
#>                          Variable: race                           
#> -----------------------------------------------------------------
#> Category    Observed    Expected    % Deviation    Std. Residuals 
#> -----------------------------------------------------------------
#>    1           24          20          20.00            0.89      
#>    2           11          20         -45.00           -2.01      
#>    3           20          20           0.00            0.00      
#>    4          145         140           3.57            0.42      
#> -----------------------------------------------------------------

# apply continuity correction
ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140), correct = TRUE)
#>     Test Statistics     
#> -----------------------
#> Chi-Square       4.3821 
#> DF                    3 
#> Pr > Chi Sq      0.2231 
#> Sample Size         200 
#> 
#>                          Variable: race                           
#> -----------------------------------------------------------------
#> Category    Observed    Expected    % Deviation    Std. Residuals 
#> -----------------------------------------------------------------
#>    1           24          20          17.50            0.78      
#>    2           11          20         -47.50           -2.12      
#>    3           20          20          -2.50           -0.11      
#>    4          145         140           3.21            0.38      
#> -----------------------------------------------------------------
```
