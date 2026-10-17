# Chi Square Test of Association

Chi Square test of association to examine if there is a relationship
between two categorical variables.

## Usage

``` r
ifr_chisq_assoc_test(data, x, y)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- x:

  factor; column in `data`

- y:

  factor; column in `data`

## Value

`ifr_chisq_assoc_test` returns an object of class
`"ifr_chisq_assoc_test"`. An object of class `"ifr_chisq_assoc_test"` is
a list containing the following components:

- chisquare:

  chi square

- chisquare_lr:

  likelihood ratio chi square

- chisquare_mantel_haenszel:

  mantel haenszel chi square

- chisquare_adjusted:

  continuity adjusted chi square

- contingency_coefficient:

  contingency coefficient

- cramers_v:

  cramer's v

- df:

  degrees of freedom

- ds:

  product of dimensions of the table of `x` and `y`

- phi_coefficient:

  phi coefficient

- pval_chisquare:

  p-value of chi square

- pval_chisquare_adjusted:

  p-value of continuity adjusted chi square

- pval_chisquare_lr:

  p-value of likelihood ratio chi square

- pval_chisquare_mantel_haenszel:

  p-value of mantel haenszel chi square

## Deprecated Function

`infer_chisq_assoc_test()` has been deprecated. Instead use
`ifr_chisq_assoc_test()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)

## Examples

``` r
ifr_chisq_assoc_test(hsb, female, schtyp)
#>                Chi Square Statistics                 
#> 
#> Statistics                     DF    Value      Prob 
#> ----------------------------------------------------
#> Chi-Square                     1    0.0470    0.8284
#> Likelihood Ratio Chi-Square    1    0.0471    0.8282
#> Continuity Adj. Chi-Square     1    0.0005    0.9822
#> Mantel-Haenszel Chi-Square     1    0.0468    0.8287
#> Phi Coefficient                     0.0153          
#> Contingency Coefficient             0.0153          
#> Cramer's V                          0.0153          
#> ----------------------------------------------------

ifr_chisq_assoc_test(hsb, female, ses)
#>                Chi Square Statistics                 
#> 
#> Statistics                     DF    Value      Prob 
#> ----------------------------------------------------
#> Chi-Square                     2    4.5765    0.1014
#> Likelihood Ratio Chi-Square    2    4.6789    0.0964
#> Phi Coefficient                     0.1513          
#> Contingency Coefficient             0.1496          
#> Cramer's V                          0.1513          
#> ----------------------------------------------------
```
