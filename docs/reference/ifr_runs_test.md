# Test for Random Order

runtest tests whether the observations of `x` are serially independent
i.e. whether they occur in a random order, by counting how many runs
there are above and below a threshold. By default, the median is used as
the threshold. A small number of runs indicates positive serial
correlation; a large number indicates negative serial correlation.

## Usage

``` r
ifr_runs_test(
  data,
  x,
  drop = FALSE,
  split = FALSE,
  mean = FALSE,
  threshold = NA
)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- x:

  numeric; column in `data`

- drop:

  logical; if TRUE, values equal to the threshold will be dropped from
  `x`

- split:

  logical; if TRUE, data will be recoded in binary format

- mean:

  logical; if TRUE, mean will be used as threshold

- threshold:

  threshold to be used for counting runs, specify 0 if data is coded as
  a binary.

## Value

`infer_runs_test` returns an object of class `"ifr_runs_test"`. An
object of class `"ifr_runs_test"` is a list containing the following
components:

- n:

  number of observations

- threshold:

  within group sum of squares

- n_below:

  number below the threshold

- n_above:

  number above the threshold

- mean:

  expected number of runs

- var:

  variance of the number of runs

- n_runs:

  number of runs

- z:

  z statistic

- p:

  p-value of `z`

## Deprecated Function

`runs_test()` has been deprecated. Instead use `ifr_runs_test()`.

## References

Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
Statistical Procedures, 4th edition. : Chapman & Hall/CRC.

Edgington, E. S. 1961. Probability table for number of runs of signs of
first differences in ordered series. Journal of the American Statistical
Association 56: 156–159.

Madansky, A. 1988. Prescriptions for Working Statisticians. New York:
Springer.

Swed, F. S., and C. Eisenhart. 1943. Tables for testing randomness of
grouping in a sequence of alternatives. Annals of Mathematical
Statistics 14: 66–87.

## Examples

``` r
ifr_runs_test(hsb, read)
#> Runs Test
#>  Total Cases:  200 
#>  Test Value :  50 
#>  Cases < Test Value:  101 
#>  Cases > Test Value:  99 
#>  Number of Runs:  95 
#>  Expected Runs:  100.99 
#>  Variance (Runs):  49.73874 
#>  z Statistic:  -0.8493358 
#>  p-value:  0.3956945 

ifr_runs_test(hsb, read, drop = TRUE)
#> Runs Test
#>  Total Cases:  200 
#>  Test Value :  50 
#>  Cases < Test Value:  83 
#>  Cases > Test Value:  99 
#>  Number of Runs:  89 
#>  Expected Runs:  91.2967 
#>  Variance (Runs):  44.54805 
#>  z Statistic:  -0.3441046 
#>  p-value:  0.7307676 

ifr_runs_test(hsb, read, split = TRUE)
#> Runs Test
#>  Total Cases:  200 
#>  Test Value :  50 
#>  Cases < Test Value:  101 
#>  Cases > Test Value:  99 
#>  Number of Runs:  95 
#>  Expected Runs:  100.99 
#>  Variance (Runs):  49.73874 
#>  z Statistic:  -0.8493358 
#>  p-value:  0.3956945 

ifr_runs_test(hsb, read, mean = TRUE)
#> Runs Test
#>  Total Cases:  200 
#>  Test Value :  52.23 
#>  Cases < Test Value:  115 
#>  Cases > Test Value:  85 
#>  Number of Runs:  93 
#>  Expected Runs:  98.75 
#>  Variance (Runs):  47.52418 
#>  z Statistic:  -0.8340854 
#>  p-value:  0.4042329 

ifr_runs_test(hsb, read, threshold = 0)
#> Runs Test
#>  Total Cases:  200 
#>  Test Value :  0 
#>  Cases < Test Value:  0 
#>  Cases > Test Value:  200 
#>  Number of Runs:  1 
#>  Expected Runs:  1 
#>  Variance (Runs):  0 
#>  z Statistic:  NaN 
#>  p-value:  NaN 
```
