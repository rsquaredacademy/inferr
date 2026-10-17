# Changelog

## inferr 0.3.2.9000

## inferr 0.3.2

CRAN release: 2024-11-11

This is a patch release to fix minor issues.

## inferr 0.3.1

CRAN release: 2021-05-28

This is a patch release to fix error on R-devel on Debian and Fedora.

## inferr 0.3.0

CRAN release: 2018-02-13

This is a minor release for bug fixes and API changes. We have
completely revamped the API. All the functions now take a `data.frame`
or `tibble` as the first argument followed by the variable names. The
variable names need not be surrounded by single/double quotes anymore.
Please view the guide for more details.

#### Bug Fixes

- using if/while statement is used with a condition of lenght greater
  than 1 will result in a runtime error and not warning
  ([\#9](https://github.com/rsquaredacademy/inferr/issues/9))

- error in binomial test
  ([\#11](https://github.com/rsquaredacademy/inferr/issues/11))

THe shiny app has been updated to reflect the changes in the API.

## inferr 0.2.0

CRAN release: 2017-12-11

#### New Features

- shiny app for interactive analysis

## inferr 0.1.1

CRAN release: 2017-05-02

#### Bug Fixes

- `binom_test()` accepts non-binary variables
  ([\#1](https://github.com/rsquaredacademy/inferr/issues/1)).
- `ind_ttest()` should throw an error when the grouping variable has
  more than 2 levels
  ([\#2](https://github.com/rsquaredacademy/inferr/issues/2)).
- `ts_prop_test()` should accept only binary variables
  ([\#3](https://github.com/rsquaredacademy/inferr/issues/3)).
- `var_test()` should accept only binary variables for the `group_var`
  input. The number of continuous variables must also not exceed two.
  ([\#4](https://github.com/rsquaredacademy/inferr/issues/4)).

## inferr 0.1.0

CRAN release: 2017-02-14

- First release
