# inferr c25 — report

Job id: `inferr-c25`
Worktree: `F:/R/Packages/_worktrees/inferr-c25`
Branch: `task/inferr-c25` (from `origin/develop`, base `2a2c786`)
Date: 2026-11-18
Scope: issues #100, #99 and #89; build only (no push, no GitHub, no CRAN
submission).

## Summary

All three issues are fixed and covered by tests:

- `ifr_oneway_anova()` rejects a response with no variation instead of
  returning `NaN` F and a spurious R-squared.
- Invalid inputs now error with the argument name and valid range:
  `sd <= 0` in `ifr_os_var_test()`; `confint` outside `(0, 1)` in
  `ifr_os_var_test()`, `ifr_ts_ind_ttest()` and `ifr_ts_paired_ttest()`;
  `alpha` outside `(0, 1)` in `ifr_os_t_test()`; non-positive or
  non-integer `n1`/`n2` and `p1`/`p2` outside `[0, 1]` in
  `ifr_ts_prop_calc()`; negative counts in `ifr_chisq_gof_test()` and
  `ifr_mcnemar_test()`.
- `ifr_ts_ind_ttest()` stores the Satterthwaite degrees of freedom
  unrounded and prints them with two decimals (`169.71` for
  `hsb/female/write`), while the stored value is `169.7074`.

Ordinary valid calls return the same numbers as before, except the
intended unrounded Satterthwaite df. A regression test records the values
captured before the change.

## Files changed

```text
NEWS.md
R/ifr-anova.R
R/ifr-chisq-gof-test.R
R/ifr-mcnemar-test.R
R/ifr-os-t-test.R
R/ifr-os-var-test.R
R/ifr-output.R
R/ifr-ts-ind-ttest.R
R/ifr-ts-paired-ttest.R
R/ifr-ts-prop-test.R
R/ifr-utils.R
tests/testthat/_snaps/indttest.md
tests/testthat/test-anova.R
tests/testthat/test-chisqgof.R
tests/testthat/test-indttest.R
tests/testthat/test-mcnemar.R
tests/testthat/test-os-vartest.R
tests/testthat/test-paired-test.R
tests/testthat/test-regression.R   (new)
tests/testthat/test-two-prop-test.R
docs-dev/inferr-c25-report.md      (this report)
```

`devtools::document()` did not require a documentation change. The installed
roxygen2 8.1.1 rewrote `DESCRIPTION` (`RoxygenNote`/`Config/roxygen2/version`),
`NAMESPACE` formatting and `man/inferr.Rd`; those unrelated rewrites were
reverted, so the source still declares `RoxygenNote: 7.3.2`.

## Reprex before/after

### #100 — constant ANOVA response

Before:

```text
Warning message:
In summary.lm(model) : essentially perfect fit: summary may be unreliable
fstat: NaN  r2: 0.5152  pval: NaN
                            ANOVA
--------------------------------------------------------------
                  Sum of
                  Squares    DF    Mean Square     F     Sig.
--------------------------------------------------------------
Between Groups       0       1          0         NaN    NaN
Within Groups        0       4          0
Total                0       5
...
Number of obs = 6    R-squared     = 0.5152
Root MSE      = 0    Adj R-squared = 0.3939
```

After:

```text
ERROR: response 'x' has no variation; a one-way ANOVA is undefined
```

### #99 — standard deviation and confidence level

Before:

```text
sd = 0  -> chi: Inf        c_lwr: 9.337
sd = -1 -> chi: 20919.42   c_lwr: 9.337
os_var confint = 0   -> OK
os_var confint = 1   -> OK
os_var confint = -0.5 -> OK
os_var confint = 1.5 -> OK
os_var confint = NA  -> OK
ind confint = 0 / 1 / -0.5 / 1.5 -> OK (NaN warnings)
paired confint = 0 / 1 / -0.5 / 1.5 -> OK (NaN warnings)
```

After:

```text
sd = 0  -> ERROR: sd must be greater than 0
sd = -1 -> ERROR: sd must be greater than 0
os_var confint = 0 / 1 / -0.5 / 1.5 / NA ->
  ERROR: confint must be between 0 and 1 (exclusive)
ind confint = 0 / 1 / -0.5 / 1.5 ->
  ERROR: confint must be between 0 and 1 (exclusive)
paired confint = 0 / 1 / -0.5 / 1.5 ->
  ERROR: confint must be between 0 and 1 (exclusive)
```

### #99 — two-sample proportion calculator and negative counts

Before:

```text
0,25,0.3,0.5   -> z: 0       n1: 0
-1,25,0.3,0.5  -> z: NaN     n1: -1
30,0,0.3,0.5   -> z: 0       n1: 30
30,25,1.5,0.5  -> z: NaN
30,25,-0.1,0.5 -> z: -5.861
30,25,0.3,1.5  -> z: -12.259
30.5,25,0.3,0.5   -> z: -1.52
30,25.5,0.3,0.5   -> z: -1.521
gof negative y -> chisq: -2397.65  p: 1
```

After:

```text
0,25,0.3,0.5 / -1,25,0.3,0.5 / 30.5,25,0.3,0.5 ->
  ERROR: n1 must be a positive integer
30,0,0.3,0.5 / 30,-1,0.3,0.5 / 30,25.5,0.3,0.5 ->
  ERROR: n2 must be a positive integer
30,25,1.5,0.5 / 30,25,-0.1,0.5 / 30,25,NA,0.5 ->
  ERROR: p1 must be between 0 and 1
30,25,0.3,1.5 / 30,25,0.3,-0.1 / 30,25,0.3,NA ->
  ERROR: p2 must be between 0 and 1
binom success = -1 -> ERROR: success must be an integer between 0 and n
gof negative y     -> ERROR: y must contain non-negative expected counts or proportions
```

### #89 — Satterthwaite df

Before (stored as an integer, printed without decimals):

```text
stored df_satterthwaite: 170

 Variable        Method        Variances    DF     t Value    P > |t|
  write          Pooled          Equal      198    -3.7341    0.0002
  write       Satterthwaite     Unequal     170    -3.6564    0.0003
```

After (stored unrounded, printed with two decimals):

```text
stored df_satterthwaite: 169.7074

 Variable        Method        Variances      DF      t Value    P > |t|
  write          Pooled          Equal       198      -3.7341    0.0002
  write       Satterthwaite     Unequal     169.71    -3.6564    0.0003
```

## Decisions

- **ANOVA constant guard**: the response's total sum of squares about its
  mean must be finite and positive; this catches the exact-constant reprex
  before `lm()` can manufacture a finite R-squared from floating-point
  noise. The error names the response column.
- **One validation helper**: `check_probability(x, name)` in
  `R/ifr-utils.R` validates both `confint` and `alpha` as a single finite
  number strictly inside `(0, 1)`. Existing "must be numeric" behaviour for
  non-numeric arguments is preserved, and the range message names the
  argument and interval.
- **Satterthwaite printing**: two decimals were chosen (matching the
  `188.46` form in #89), applied through `format_satt_df()` wherever the
  Satterthwaite df is printed. The stored value is no longer rounded.
- **Negative counts**: `ifr_chisq_gof_test()` also rejects non-finite or
  all-zero `y`, and `ifr_mcnemar_test()` rejects negative/non-finite matrix
  counts. Integer-only checks for McNemar were not added because the issue
  only asks for negative counts; it is listed as a follow-up.
- **NEWS**: a new top heading `# inferr (development version)` was created
  because no `(development version)` heading existed. `DESCRIPTION` still
  says `Version: 0.3.3`; the version was not bumped.

## Tests

Each changed behaviour has a test that was also run against the pre-fix
source (`git archive HEAD` copy with the new test files overlaid): the new
expectations failed there (more than 40 failures before the reporter's
limit), and all pass on the fixed source.

Final source suite (`devtools::test()`):

```text
files: 20
test blocks: 197
failed: 0  errors: 0  warnings: 0  skipped: 0  passed: 1141
```

New tests:

- `test-anova.R`: constant response errors and names the response; a
  non-constant response still works.
- `test-os-vartest.R`: `sd` 0, negative, `NA`, length > 1; `confint` 0, 1,
  negative, > 1, `NA`, non-numeric; a valid `confint = 0.9` result.
- `test-indttest.R`: `confint` outside `(0, 1)` and non-numeric; stored df
  matches `t.test()$parameter`; formatted df and printed output; regression
  values.
- `test-paired-test.R`: `confint` outside `(0, 1)` and non-numeric; valid
  `confint = 0.9` matches `t.test()`.
- `test-two-prop-test.R`: `n1`/`n2` zero, negative, fractional and `NA`;
  `p1`/`p2` negative, > 1 and `NA`; boundaries remain valid; regression
  values.
- `test-chisqgof.R`: negative, non-finite and all-zero `y`.
- `test-mcnemar.R`: negative and non-finite matrix counts.
- `test-regression.R`: ordinary ANOVA, one-sample variance, independent and
  paired t, proportion calculator, one-sample proportion, binomial and
  goodness-of-fit results recorded before the change.

The `indttest` snapshot was updated for the wider DF column and the printed
`169.71`.

## Check results

Built from `git archive HEAD` (no `.git` in the tarball):

```text
R CMD build .../c25-src
R CMD check --as-cran --no-manual inferr_0.3.3.tar.gz
Status: 1 WARNING
```

The warning is only the CRAN incoming-feasibility version check:

```text
* checking CRAN incoming feasibility ... WARNING
Insufficient package version (submitted: 0.3.3, existing: 0.3.3)
```

It is unavoidable while `Version: 0.3.3` is retained (the task forbids a
version bump before the 2027-01-13 patch window). The same tarball checked
with the incoming feasibility check disabled, which removes only that
CRAN-index comparison:

```text
_R_CHECK_CRAN_INCOMING_=false R CMD check --as-cran --no-manual inferr_0.3.3.tar.gz
Status: OK
```

So the package itself checks cleanly: **0 errors, 0 warnings, 0 notes**;
the only warning under the unmodified `--as-cran` run is the expected
version warning above. There are no NOTEs to list.

## Commands run

```text
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/c25-before.R
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/c25-test-baseline.R
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/c25-document.R
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/c25-test.R
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/c25-snap.R
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/c25-devtools-test.R
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/c25-count.R
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/c25-head-test.R
git subtree/archive helpers: git archive HEAD | tar -x -C .../c25-head
git archive HEAD | tar -x -C C:/Users/DELL/AppData/Local/Temp/opencode/c25-final-src
R CMD build C:/Users/DELL/AppData/Local/Temp/opencode/c25-final-src
R CMD check --as-cran --no-manual inferr_0.3.3.tar.gz
_R_CHECK_CRAN_INCOMING_=false R CMD check --as-cran --no-manual inferr_0.3.3.tar.gz
```

(`devtools::document()` and `devtools::test()` were run from Rscript files,
not `Rscript -e`.)

## Follow-ups

1. **CRAN incoming version warning.** When the patch release window opens
   after 2027-01-13, bump `DESCRIPTION` from `0.3.3` to the next patch
   version (for example `0.3.4`), rerun
   `R CMD check --as-cran --no-manual` and expect the
   "Insufficient package version" warning to disappear.
2. **Roxygen version drift.** `DESCRIPTION` declares `RoxygenNote: 7.3.2`
   while the environment has roxygen2 8.1.1, which reformats `NAMESPACE`
   and `man/inferr.Rd`. Regenerate documentation with roxygen2 7.3.2, or
   migrate deliberately to 8.x and update `RoxygenNote` in a separate docs
   commit.
3. **Zero expected cells in GOF.** `ifr_chisq_gof_test()` now rejects
   negative and all-zero `y`, but a zero expectation in one cell with a
   positive observed count still gives `Inf`; consider rejecting zero
   expectations for cells with observed counts.
4. **Fractional McNemar counts.** `ifr_mcnemar_test()` rejects negative or
   non-finite matrix counts but still accepts fractional counts; consider
   requiring non-negative integers if that input is considered in scope.
5. **Satterthwaite df in the `alternative = "all"` layout.** That layout
   does not print a DF row at all; if #89 is expected to cover every
   alternative layout, add the formatted Satterthwaite df to the
   all-alternatives header in a follow-up.

## Not done

- No push, no `gh`/GitHub operation, no branch switch, no other worktree
  touched.
- No version bump, no CRAN submission, no win-builder/R-hub/macOS run.
- No win-builder reverse-dependency run.
