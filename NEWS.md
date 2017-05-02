# inferr 0.1.1

### Bug Fixes

* `binom_test()` accepts non-binary variables (#1).
* `ind_ttest()` should throw an error when the grouping variable has more than 2 levels (#2).
* `ts_prop_test()` should accept only binary variables (#3).
* `var_test()`  should accept only binary variables for the `group_var` input. The number of continuous variables must also not exceed two. (#4).

# inferr 0.1.0

* First release
