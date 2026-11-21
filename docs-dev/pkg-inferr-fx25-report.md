# inferr 0.3.4 fix round from the rx25 audit (pkg-inferr-fx25)

Branch: `task/inferr-fx25`, base `develop` at `5722a08`.
Audit: `F:/R/Packages/_worktrees/inferr-rx25/docs-dev/pkg-inferr-rx25-audit.md`.
Rules applied: `F:/pm/notes/package-audit-stop-rule.md`.
Build: Windows, R 4.6.1, one commit on the task branch. No push, no `gh`, no
GitHub access, no branch switch, no other worktree changed.

## Summary

All High findings (H1-H4), all Medium findings (M1-M7, including M7 which the
brief did not list separately), and L1 were addressed. The supported domain is
now stated and enforced: finite data with `|x| <= 1e150`, sample sizes
`<= 1e15`. Ordinary teaching results are unchanged. The ordinary regression
grid and every finding have value tests; no finding is covered only by a
snapshot.

The package version was advanced to `0.3.4` to match the requested development
release. `DESCRIPTION` keeps `RoxygenNote: 7.3.2`; help pages were generated for
the touched pages only.

## Finding -> change -> test -> reprex before/after

| Finding | Change | Test name (file) | Before (audit reprex) | After (this build) |
|---|---|---|---|---|
| H1 | Any zero expected count is rejected, including a zero/zero cell; message names the cell and category. | `GOF rejects zero expected cells with any observed count (H1)` (`test-chisqgof.R`) | `NaN 2 NaN`; print shows `NaN` | `ERROR: y[3] for category 'c' of 'x' is zero; the chi-square goodness-of-fit test is undefined` |
| H2 | Calculator rejects sample sizes above `1e15`; `n1`/`n2` named in the error. Inside the domain the pooled z is still computed, and an underflowed tail prints as a bound. | `H2: proportion calculator enforces the sample-size domain` (`test-rx25-fixes.R`) | `c(k$z, k$sig)` = `-Inf 0`; total observations `Inf` | `ERROR: n1 must be a positive integer no greater than 1e15`; at `n1=n2=1e12`, `z = -288675.1`, printed `Pr(|Z| > |z|) < 0.001` |
| H3 | All three proportion interfaces retain the unrounded p (`sig_raw`) and print a bound below display resolution. The same rule was applied to every other inferr printer that rounded a p-value to a fixed number of decimals: one-sample/independent/paired t, one-sample/two-sample variance, GOF, McNemar, chi-square association, Cochran Q and Levene. | `H3: the three proportion interfaces print p-value bounds`; `H3: other rounding printers print p-value bounds` (`test-rx25-fixes.R`) | `k$sig` = `0`; printed `Pr(|Z| > |z|) 0` (true p `1.205298e-13`) | `sig = 0; sig_raw = 1.205298258444639e-13`; printed `Pr(|Z| > |z|) < 0.001` |
| H4 | Domain guard rejects `|x| > 1e150` for variance and paired inputs, naming the argument and range; non-finite computed moments also stop. No numerical re-engineering. | `H4: variance and paired inputs outside the domain stop clearly` (`test-rx25-fixes.R`) | variance `sigma` = `Inf`, printed `Std. Dev. Inf` and `chi-square Inf`; paired returned infinite SDs and `NaN` correlation | variance: `ERROR: x must contain only finite values with absolute value <= 1e150`; paired: same error naming `x` |
| M1 | ANOVA guard is purely relative, `sd == 0` or `sd <= 1e-12 * mean(abs(x))`, computed after scaling so it cannot underflow/overflow. Computation is rescaled only for extreme finite magnitudes. | `M1: ANOVA's relative guard works at any finite response scale` (`test-rx25-fixes.R`); existing `ANOVA accepts real spread independently of response scale (RX1)` | `ERROR: response 'x' has no variation` at `1e-170`; expected `F = 13.5`, `p = 0.02131164`. At `1e160` the guard also misfired. | `f = 13.5`, `p = 0.02131164`; at `1e160`: `ERROR: x must contain only finite values with absolute value <= 1e150` |
| M2 | GOF expected values are counts unless they sum to one (tolerance-based); a count vector must total the sample size. | `M2: GOF expected counts must total the sample size` (`test-rx25-fixes.R`) | `c(k$chisquare, k$pvalue)` = `12.8 0.0003` for expected `c(10,10)` and `n=4` | `ERROR: expected counts in y must sum to the sample size (4); expected values in y are counts unless they sum to 1` |
| M3 | Missing observations are removed, including an explicit `NA` factor level, with a message; remaining categories are re-leveled. | `M3: GOF drops an explicit NA factor level with a message` (`test-rx25-fixes.R`) | printed statistics, then `Error: invalid 'width' argument` | `MESSAGE: removing 1 observation(s) with missing 'x'`; with `y = c(1,1)` the table prints cleanly (`Chi-Square 0`, `DF 1`, `Pr > Chi Sq 1`, `Sample Size 2`). With the audit's original `y = c(1,1,1)` the call now reports the expected length error after the drop, because only two categories remain. |
| M4 | McNemar requires an exact 2 x 2 matrix and non-negative integer counts before calculation. | `M4 and M7: McNemar checks shape, integer counts and empty tables` (`test-rx25-fixes.R`) | `1 x 4` matrix: `Error: subscript out of bounds`; fractional counts: `exactp = NaN` plus binomial warnings | `ERROR: data must be a 2 x 2 matrix`; `ERROR: data must contain non-negative integer counts` |
| M5 | Variance requires at least two non-missing observations; paired t requires at least two complete pairs, checked before any table/index operation. Correlation significance is `NA` below three pairs. | `M5: variance and paired tests require usable samples` (`test-rx25-fixes.R`) | variance singleton: `Error: missing value where TRUE/FALSE needed`; paired singleton returned `NA`/`NaN` then print failed with `invalid 'width' argument`; empty paired: `subscript out of bounds` | `ERROR: x must contain at least two non-missing observations`; `ERROR: x and y must contain at least two complete pairs` for both singleton and empty inputs |
| M6 | Each two-sample proportion sample must have at least one non-missing observation; the empty argument is named. | `M6: empty two-sample proportions name the empty sample` (`test-rx25-fixes.R`) | `c(n1,n2,z,sig)` = `0 0 NaN NaN`; printed `NaN` z and p | `ERROR: var1 must contain at least one non-missing observation` (or `var2` when that sample is empty) |
| M7 | McNemar rejects a table with no observations before `mccomp()`; the zero-discordance convention (statistic 0, p 1) is documented. | `M4 and M7: McNemar checks shape, integer counts and empty tables` (`test-rx25-fixes.R`) | `c(statistic, pvalue, exactp, kappa)` = `0 1 1 NaN` | `ERROR: data must contain at least one observation` |
| L1 | Each touched help page now has a `Details` paragraph beginning `Supported inputs: ...` with the finite range, sample-size limit, minimum sample size, missing-data policy and output precision. `NEWS.md` states the same limits. | `L1: touched help pages state the supported input domain` (`test-rx25-fixes.R`) | ANOVA, GOF and proportion family had partial ranges; t, variance, paired and McNemar pages did not state complete ranges, minimum sizes, degenerate handling or bounds | All nine touched pages (`ifr_oneway_anova`, `ifr_chisq_gof_test`, `ifr_ts_prop_test`, `ifr_os_prop_test`, `ifr_os_t_test`, `ifr_os_var_test`, `ifr_ts_ind_ttest`, `ifr_ts_paired_ttest`, `ifr_mcnemar_test`) contain `Supported inputs`, `1e150` and `1e15` |

### Full audit reprex output after this build

```text
H1
ERROR: y[3] for category 'c' of 'x' is zero; the chi-square goodness-of-fit test is undefined

H2
ERROR: n1 must be a positive integer no greater than 1e15

H2 inside domain (n1 = n2 = 1e12)
z = -288675.1
sig = 0
sig_raw = 0            # the true tail is below double representation
printed: Pr(|Z| > |z|) < 0.001

H3
sig = 0; sig_raw = 1.205298258444639e-13
        Test Statistics
-------------------------------
Total Observations           55
z                        -7.416
Pr(|Z| > |z|)           < 0.001

H4 variance
ERROR: x must contain only finite values with absolute value <= 1e150

H4 paired
ERROR: x must contain only finite values with absolute value <= 1e150

M1
f = 13.50000000
p = 0.02131164

M1 at 1e160
ERROR: x must contain only finite values with absolute value <= 1e150

M2
ERROR: expected counts in y must sum to the sample size (4); expected values in y are counts unless they sum to 1

M3 audit call (y = c(1,1,1))
MESSAGE: removing 1 observation(s) with missing 'x'
ERROR: Length of y must be equal to the number of categories in x after removing missing observations

M3 with matching expectations (y = c(1,1))
MESSAGE: removing 1 observation(s) with missing 'x'
 Test Statistics
------------------
Chi-Square       0
DF               1
Pr > Chi Sq      1
Sample Size      2
(category table shows a and b, each observed 1, expected 1)

M4 shape
ERROR: data must be a 2 x 2 matrix

M4 fractional
ERROR: data must contain non-negative integer counts

M5 variance
ERROR: x must contain at least two non-missing observations

M5 paired singleton
ERROR: x and y must contain at least two complete pairs

M5 paired empty
ERROR: x and y must contain at least two complete pairs

M6
ERROR: var1 must contain at least one non-missing observation

M7
ERROR: data must contain at least one observation
```

### Note on H2 and underflow

The audit's `1e308` sample sizes are outside the documented domain and now
stop. Inside the domain (`1e15` and below) the pooled z and the printed bound
are correct. For an astronomically large finite sample such as `1e12` with
proportions `.3` and `.5`, the exact two-sided tail is below the smallest
representable double, so `sig_raw` is `0`; the printer detects the finite
non-zero z statistic and still prints `< 0.001` rather than a false exact zero.
There is no representable unrounded value to store in that case.

## Regression grid

`tests/testthat/test-ordinary-grid.R` compares every touched interface against
base R on mtcars/hsb-style teaching data:

- `ifr_oneway_anova(mtcars, mpg, cyl)` vs `anova(lm(...))`
- `ifr_chisq_gof_test(hsb, race, c(20,20,20,140))` vs `chisq.test()`
- `ifr_mcnemar_test(matrix(c(135,18,21,26), 2))` vs `mcnemar.test()` and the
  exact binomial tail
- `ifr_os_t_test(hsb, write, mu = 50)` vs `t.test()`
- `ifr_os_var_test(mtcars, mpg, 5)` vs the chi-square formula and quantiles
- `ifr_ts_ind_ttest(hsb, female, write)` vs pooled and Welch `t.test()`
- `ifr_ts_paired_ttest(hsb, read, write)` vs `t.test(paired = TRUE)`
- `ifr_ts_prop_test(mtcarz, am, vs)` vs `prop.test(correct = FALSE)`
- `ifr_ts_prop_group(mtcarz, am, vs)` vs `prop.test(correct = FALSE)`
- `ifr_ts_prop_calc(30, 25, .3, .5)` vs the pooled z formula
- `ifr_os_prop_test(hsb, female, prob = .5)` vs `prop.test(correct = FALSE)`

All grid assertions are value assertions, not snapshots. The grid stayed green
through the fix round and through the clean archive check.

## NEWS

`NEWS.md` development bullets were added under `# inferr (development version)`
for each finding group: p-value bounds, GOF zero/total checks, domain guards and
documented limits, the relative ANOVA guard, NA-level handling, McNemar
shape/integer/empty checks, minimum sample sizes, empty two-sample proportions,
and the help-page precision/domain statements.

## Checks

### Disposable archive and test run

Commands (the working shell is bash on this Windows host):

```bash
rm -rf .pkg-inferr-fx25-tmp
mkdir -p .pkg-inferr-fx25-tmp/clean
git archive HEAD -o .pkg-inferr-fx25-tmp/head.tar
tar -xf .pkg-inferr-fx25-tmp/head.tar -C .pkg-inferr-fx25-tmp/clean
NOT_CRAN=true Rscript -e "devtools::test('.pkg-inferr-fx25-tmp/clean', reporter='summary', stop_on_failure=TRUE)"
NOT_CRAN=true Rscript -e "testthat::test_dir('.pkg-inferr-fx25-tmp/clean/tests/testthat', reporter='check', stop_on_failure=FALSE, load_package='source')"
```

Results:

- `devtools::test()` in the disposable copy: completed with no failures.
- `testthat::test_dir(..., reporter='check')` with `NOT_CRAN=true`:
  **1495 pass, 0 fail, 0 warnings, 0 skips, 0 test errors**.

### Build and CRAN check

```bash
cd .pkg-inferr-fx25-tmp
LC_ALL=C _R_CHECK_CRAN_INCOMING_REMOTE_=false R CMD build clean
LC_ALL=C _R_CHECK_CRAN_INCOMING_REMOTE_=false R CMD check --as-cran --no-manual inferr_0.3.4.tar.gz
```

Result: **Status: OK. 0 errors, 0 warnings, 0 notes.** The built tarball is
`inferr_0.3.4.tar.gz`. Vignettes built and re-built successfully.

The check's own testthat summary:

```text
[ FAIL 0 | WARN 0 | SKIP 41 | PASS 1420 ]
```

The 41 skips are the expected CRAN-mode snapshot skips (33) and
repository-relative documentation checks unavailable in the built archive (8);
there were no test errors and no failures. The `NOT_CRAN=true` run above
includes those checks and passes them.

### Disposable library

No package was installed into any persistent library. The only installation
was the normal temporary installation done by `R CMD build` / `R CMD check`
inside `.pkg-inferr-fx25-tmp/`. The temporary folder was deleted after the
checks and report were written.

## Files changed

- `R/ifr-utils.R`: domain guards, moment guard, `format_p_bound()`.
- `R/ifr-anova.R`: relative scaled guard, finite-range guard, rescaling for
  extreme finite magnitudes, moment checks.
- `R/ifr-chisq-gof-test.R`: zero-expected rejection, count/proportion total
  validation, NA-level removal with message, `pvalue_raw`.
- `R/ifr-ts-prop-test.R`, `R/ifr-os-prop-test.R`: sample-size domain, empty
  sample errors, `sig_raw`, underflow-aware bound printing.
- `R/ifr-os-t-test.R`, `R/ifr-os-var-test.R`, `R/ifr-ts-paired-ttest.R`,
  `R/ifr-ts-ind-ttest.R`: domain guards, minimum samples, moment checks, raw p
  fields, paired correlation convention.
- `R/ifr-mcnemar-test.R`: 2 x 2 shape, integer counts, empty-table rejection,
  raw p fields.
- `R/ifr-output.R`: bound-aware p display for every printer that previously
  rounded a positive p to zero.
- `R/ifr-chisq-assoc-test.R`, `R/ifr-cochran-q-test.R`, `R/ifr-levene-test.R`,
  `R/ifr-ts-var-test.R`: raw p fields for their printers.
- `man/*.Rd`: regenerated help for the nine touched pages.
- `NEWS.md`, `DESCRIPTION`: development bullets and version `0.3.4`.
- `tests/testthat/test-ordinary-grid.R`, `tests/testthat/test-rx25-fixes.R`,
  `tests/testthat/test-chisqgof.R`, `tests/testthat/_snaps/paired-test.md`.

## Not done / limitations

- H2 and H4 numerics were not re-engineered, per the brief. The response was to
  document and enforce the supported domain and reject non-finite moments.
- No push, no GitHub issue creation, no `gh`, no branch switch, and no change to
  another worktree or repository.
- `docs/` (the pkgdown site) was not regenerated. It was not part of this fix
  round and is excluded from the package build. `docs-dev/` reports remain
  excluded from the build.
- A positive p-value that underflows below the smallest double cannot be stored
  as a positive unrounded number; the printer bounds it from the finite
  statistic instead. This is noted above for H2.
- The `M3` audit call with `y = c(1,1,1)` now gets the documented
  missing-observation message followed by the category-length error, because
  removing the explicit `NA` level leaves two categories. Supplying the two
  remaining expected values prints cleanly.
