# Binomial Test

Test whether the proportion of successes on a two-level categorical
dependent variable significantly differs from a hypothesized value.

## Usage

``` r
ifr_binom_calc(n, success, prob = 0.5, ...)

ifr_binom_test(data, variable, prob = 0.5)
```

## Arguments

- n:

  number of observations

- success:

  number of successes

- prob:

  assumed probability of success on a trial

- ...:

  additional arguments passed to or from other methods

- data:

  a `data.frame` or a `tibble`

- variable:

  factor; column in `data`

## Value

`ifr_binom_test` returns an object of class `"ifr_binom_test"`. An
object of class `"ifr_binom_test"` is a list containing the following
components:

- exp_k:

  expected number of successes

- exp_p:

  expected probability of success

- k:

  number of successes

- n:

  number of observations

- obs_p:

  assumed probability of success

- pval_lower:

  lower one sided p value

- pval_upper:

  upper one sided p value

## Deprecated Functions

`infer_binom_calc()` and `infer_binom_test()` have been deprecated.
Instead use `ifr_binom_cal()` and `ifr_binom_test()`.

## References

Hoel, P. G. 1984. Introduction to Mathematical Statistics. 5th ed. New
York: Wiley.

## See also

[`binom.test`](https://rdrr.io/r/stats/binom.test.html)

## Examples

``` r
# using calculator
ifr_binom_calc(32, 13, prob = 0.5)
#>             Binomial Test              
#>  --------------------------------------
#>   Group    N     Obs. Prop    Exp. Prop 
#>  --------------------------------------
#>     0      19      0.59375        0.500 
#>     1      13      0.40625        0.500 
#>  --------------------------------------
#> 
#> 
#>                  Test Summary                 
#>  --------------------------------------------
#>   Tail              Prob             p-value  
#>  --------------------------------------------
#>   Lower         Pr(k <= 13)          0.188543 
#>   Upper         Pr(k >= 13)          0.892336 
#>  --------------------------------------------

# using data set
ifr_binom_test(hsb, female, prob = 0.5)
#>              Binomial Test              
#>  ---------------------------------------
#>   Group     N     Obs. Prop    Exp. Prop 
#>  ---------------------------------------
#>     0       91        0.455        0.500 
#>     1      109        0.545        0.500 
#>  ---------------------------------------
#> 
#> 
#>                   Test Summary                  
#>  ----------------------------------------------
#>   Tail               Prob              p-value  
#>  ----------------------------------------------
#>   Lower          Pr(k <= 109)          0.910518 
#>   Upper          Pr(k >= 109)          0.114623 
#>  ----------------------------------------------
```
