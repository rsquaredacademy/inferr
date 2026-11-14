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

`ifr_binom_test` returns an object of class
`c("ifr_binom_test", "ifr_binom_calc")`. An object of this class is a
list containing the following components:

- exp_k:

  expected number of successes

- exp_p:

  expected probability of success

- k:

  number of successes

- n:

  number of observations

- obs_p:

  observed probability of success

- pval_lower:

  lower one sided p value

- pval_upper:

  upper one sided p value

- two_tail:

  two sided exact p value

## Details

The success is the **second** level of the factor. For a character
column the levels are ordered alphabetically, as
[`factor()`](https://rdrr.io/r/base/factor.html) does, so for a binary
column the second (last) level is the success. For a logical column the
levels are `FALSE` and `TRUE`, so `TRUE` is the success; for a numeric
0/1 column the levels are `0` and `1`, so `1` is the success.
`ifr_binom_test()` requires the column to be a factor, so convert
character, logical or 0/1 columns with
[`factor()`](https://rdrr.io/r/base/factor.html) first and check the
level order with [`levels()`](https://rdrr.io/r/base/levels.html).

## Deprecated Functions

`infer_binom_calc()` and `infer_binom_test()` have been deprecated.
Instead use `ifr_binom_calc()` and `ifr_binom_test()`.

## References

Hoel, P. G. 1984. Introduction to Mathematical Statistics. 5th ed. New
York: Wiley.

Conover, W. J. (1999). *Practical Nonparametric Statistics*, 3rd ed. New
York: Wiley.

Hollander, M., Wolfe, D. A. and Chicken, E. (2014). *Nonparametric
Statistical Methods*, 3rd ed. Hoboken, NJ: Wiley.

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
#>                Test Summary                
#>  -----------------------------------------
#>   Tail              Prob          p-value  
#>  -----------------------------------------
#>   Lower          Pr(k <= 13)      0.188543 
#>   Upper          Pr(k >= 13)      0.892336 
#>   Two-sided    Pr(k) <= Pr(13)    0.377086 
#>  -----------------------------------------

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
#>                 Test Summary                
#>  ------------------------------------------
#>   Tail               Prob          p-value  
#>  ------------------------------------------
#>   Lower          Pr(k <= 109)      0.910518 
#>   Upper          Pr(k >= 109)      0.114623 
#>   Two-sided    Pr(k) <= Pr(109)    0.229247 
#>  ------------------------------------------
```
