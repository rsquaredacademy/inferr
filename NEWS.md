# inferr 0.3.2.9000

## Bug Fixes

- `ifr_os_var_test()` now reports the correct confidence interval for the
  standard deviation.
- `ifr_ts_ind_ttest()` now uses t quantiles, rather than normal quantiles, for
  the confidence interval of the mean difference and reports both the pooled
  and Satterthwaite intervals.
- `ifr_cochran_qtest()` now recodes logical, 0/1, 1/2 and two-level factor
  columns to 0/1, and rejects other inputs.
- `ifr_oneway_anova()` now drops incomplete cases and computes the test
  statistics from unrounded sums of squares; the printed table shows the
  p-value instead of `0` and keeps it within the `Sig.` column.
- `ifr_chisq_assoc_test()` no longer returns `NaN` for the likelihood ratio
  statistic when a contingency table contains an empty cell.
- `ifr_chisq_gof_test()` applies Yates correction only when `correct = TRUE`.
- `library(inferr)` no longer contacts CRAN or offers to update packages when
  the package is attached.
- `ifr_os_t_test()` and `ifr_ts_ind_ttest()` now compute p-values and
  confidence intervals from unrounded statistics.
- `ifr_levene_test()` now works with a character grouping variable and reports
  the correct number of observations and denominator degrees of freedom when
  the data contain missing values.
- `ifr_ts_var_test()` now works with a character grouping variable and requires
  exactly two groups.
- `ifr_ts_prop_test()`, `ifr_ts_prop_group()` and `ifr_os_prop_test()` now
  reject variables that are not binary.
- `ifr_ts_paired_ttest()` now drops incomplete pairs instead of failing.
- `ifr_os_t_test()`, `ifr_ts_ind_ttest()` and `ifr_os_var_test()` now drop
  missing values instead of failing.
- `ifr_mcnemar_test()` now requires a 2 x 2 table when variables are supplied.
- `ifr_runs_test()` now reports the number of cases remaining after
  `drop = TRUE`.
- `ifr_chisq_assoc_test()` now returns `NA` for the phi coefficient when the
  table is not 2 x 2.
- The two-sample proportion test output now labels the two-sided p-value as
  `Pr(|Z| > |z|)`.
- Missing suggested packages now produce an informative error message.
- The documentation for `ifr_ts_var_test()` now lists the accepted
  `alternative` values, and the README example runs again.
- `ifr_binom_test()` and `ifr_binom_calc()` now return and print the
  two-sided exact p-value, computed with the same rule as
  `stats::binom.test()`.
- `ifr_binom_test()` now returns an object of class `ifr_binom_test` (which
  also inherits from `ifr_binom_calc`), as documented.
- The binomial test documentation now states that the success is the second
  factor level.
- `ifr_ts_var_test()` now returns the documented two-sided p-value.
- `ifr_ts_prop_group()` and `ifr_ts_prop_calc()` now compute all p-values
  from the unrounded z statistic.
- `ifr_ts_paired_ttest()` now computes p-values and the confidence interval
  of the mean difference from the unrounded t statistic and standard
  deviation.

# inferr 0.3.2

This is a patch release to fix minor issues.

# inferr 0.3.1

This is a patch release to fix error on R-devel on Debian and Fedora.

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
