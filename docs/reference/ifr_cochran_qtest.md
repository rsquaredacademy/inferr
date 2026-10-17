# Cochran Q Test

Test if the proportions of 3 or more dichotomous variables are equal in
the same population.

## Usage

``` r
ifr_cochran_qtest(data, ...)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- ...:

  columns in `data`

## Value

`ifr_cochran_qtest` returns an object of class `"ifr_cochran_qtest"`. An
object of class `"ifr_cochran_qtest"` is a list containing the following
components:

- df:

  degrees of freedom

- n:

  number of observations

- pvalue:

  p value

- q:

  cochran's q statistic

## Deprecated Function

`infer_cochran_test()` has been deprecated. Instead use
`ifr_cochran_qtest()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

## Examples

``` r
ifr_cochran_qtest(exam, exam1, exam2, exam3)
#>    Test Statistics     
#> ----------------------
#> N                   15 
#> Cochran's Q       4.75 
#> df                   2 
#> p value          0.093 
#> ----------------------
```
