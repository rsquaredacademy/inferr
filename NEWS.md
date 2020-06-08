# inferr 0.3.0.9000

The following functions were deprecated in `0.3.0` and have been removed:

- `binom_calc`
- `binom_test`
- `chisq_gof`
- `chisq_test`
- `cochran_test`
- `ind_ttest`
- `launch_inferr`
- `levene_test`
- `mcnemar_test`
- `os_vartest`
- `owanova`
- `paired_ttest`
- `prop_test`
- `runs_test`
- `ts_prop_calc`
- `ts_prop_grp`
- `ts_prop_test`
- `ttest`
- `var_test`

# inferr 0.3.0

This is a minor release for bug fixes and API changes. We have completely revamped the API. All the functions now take a `data.frame` or `tibble` as the first argument followed by the variable names. The variable names need not be surrounded by single/double quotes anymore. Please view the guide for more details.

### Bug Fixes

- using if/while statement is used with a condition of lenght greater than 1 will result in a runtime error and not warning  ([#9](https://github.com/rsquaredacademy/inferr/issues/9))

- error in binomial test ([#11](https://github.com/rsquaredacademy/inferr/issues/11))

THe shiny app has been updated to reflect the changes in the API.

# inferr 0.2.0

### New Features

- shiny app for interactive analysis

# inferr 0.1.1

### Bug Fixes

* `binom_test()` accepts non-binary variables (#1).
* `ind_ttest()` should throw an error when the grouping variable has more than 2 levels (#2).
* `ts_prop_test()` should accept only binary variables (#3).
* `var_test()`  should accept only binary variables for the `group_var` input. The number of continuous variables must also not exceed two. (#4).

# inferr 0.1.0

* First release
