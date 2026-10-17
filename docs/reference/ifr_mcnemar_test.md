# McNemar Test

Test if the proportions of two dichotomous variables are equal in the
same population.

## Usage

``` r
ifr_mcnemar_test(data, x = NULL, y = NULL)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- x:

  factor; column in `data`

- y:

  factor; column in `data`

## Value

`ifr_mcnemar_test` returns an object of class `"ifr_mcnemar_test"`. An
object of class `"ifr_mcnemar_test"` is a list containing the following
components:

- statistic:

  chi square statistic

- df:

  degrees of freedom

- pvalue:

  p-value

- exactp:

  exact p-value

- cstat:

  continuity correction chi square statistic

- cpvalue:

  continuity correction p-value

- kappa:

  kappa coefficient; measure of interrater agreement

- std_err:

  asymptotic standard error

- kappa_cil:

  95% kappa lower confidence limit

- kappa_ciu:

  95% kappa upper confidence limit

- cases:

  cases

- controls:

  controls

- ratio:

  ratio of proportion with factor

- odratio:

  odds ratio

- tbl:

  two way table

## Deprecated Function

`infer_mcnermar_test()` has been deprecated. Instead use
`ifr_mcnemar_test()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## See also

[`mcnemar.test`](https://rdrr.io/r/stats/mcnemar.test.html)

## Examples

``` r
# using variables from data
hb <- hsb
hb$himath <- ifelse(hsb$math > 60, 1, 0)
hb$hiread <- ifelse(hsb$read > 60, 1, 0)
ifr_mcnemar_test(hb, himath, hiread)
#>            Controls 
#> ---------------------------------
#> Cases       0       1       Total 
#> ---------------------------------
#>   0        135      21        156 
#>   1         18      26         44 
#> ---------------------------------
#> Total      153      47        200 
#> ---------------------------------
#> 
#>        McNemar's Test        
#> ----------------------------
#> McNemar's chi2        0.2308 
#> DF                         1 
#> Pr > chi2              0.631 
#> Exact Pr >= chi2      0.7493 
#> ----------------------------
#> 
#>        Kappa Coefficient         
#> --------------------------------
#> Kappa                     0.4454 
#> ASE                        0.075 
#> 95% Lower Conf Limit      0.2984 
#> 95% Upper Conf Limit      0.5923 
#> --------------------------------
#> 
#> Proportion With Factor 
#> ----------------------
#> cases             0.78 
#> controls         0.765 
#> ratio           1.0196 
#> odds ratio      1.1667 
#> ----------------------

# test if the proportion of students in himath and hiread group is same
himath <- ifelse(hsb$math > 60, 1, 0)
hiread <- ifelse(hsb$read > 60, 1, 0)
ifr_mcnemar_test(table(himath, hiread))
#>            Controls 
#> ---------------------------------
#> Cases       0       1       Total 
#> ---------------------------------
#>   0        135      21        156 
#>   1         18      26         44 
#> ---------------------------------
#> Total      153      47        200 
#> ---------------------------------
#> 
#>        McNemar's Test        
#> ----------------------------
#> McNemar's chi2        0.2308 
#> DF                         1 
#> Pr > chi2              0.631 
#> Exact Pr >= chi2      0.7493 
#> ----------------------------
#> 
#>        Kappa Coefficient         
#> --------------------------------
#> Kappa                     0.4454 
#> ASE                        0.075 
#> 95% Lower Conf Limit      0.2984 
#> 95% Upper Conf Limit      0.5923 
#> --------------------------------
#> 
#> Proportion With Factor 
#> ----------------------
#> cases             0.78 
#> controls         0.765 
#> ratio           1.0196 
#> odds ratio      1.1667 
#> ----------------------

# using matrix
ifr_mcnemar_test(matrix(c(135, 18, 21, 26), nrow = 2))
#>            Controls 
#> ---------------------------------
#> Cases       0       1       Total 
#> ---------------------------------
#>   0        135      21        156 
#>   1         18      26         44 
#> ---------------------------------
#> Total      153      47        200 
#> ---------------------------------
#> 
#>        McNemar's Test        
#> ----------------------------
#> McNemar's chi2        0.2308 
#> DF                         1 
#> Pr > chi2              0.631 
#> Exact Pr >= chi2      0.7493 
#> ----------------------------
#> 
#>        Kappa Coefficient         
#> --------------------------------
#> Kappa                     0.4454 
#> ASE                        0.075 
#> 95% Lower Conf Limit      0.2984 
#> 95% Upper Conf Limit      0.5923 
#> --------------------------------
#> 
#> Proportion With Factor 
#> ----------------------
#> cases             0.78 
#> controls         0.765 
#> ratio           1.0196 
#> odds ratio      1.1667 
#> ----------------------
```
