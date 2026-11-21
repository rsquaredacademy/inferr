# inferr 0.3.4 fix round 2, from the rx25b re-audit (pkg-inferr-fx25b)

Branch: `task/inferr-fx25`, continued from the fx25 commit `2b1f83c`.
Base for regression comparison: `develop` at `5722a08`.
Re-audit: `F:/R/Packages/_worktrees/inferr-rx25b/docs-dev/pkg-inferr-rx25b-audit.md`.
Earlier report: `docs-dev/pkg-inferr-fx25-report.md`.
Rules: `F:/pm/notes/package-audit-stop-rule.md`.
Environment: Windows 11, R 4.6.1 (2026-06-24 ucrt), one commit on the task
branch. No push, no `gh`/GitHub, no branch switch, no other worktree or
repository changed. Installations were confined to disposable temporary
libraries; temporary folders and tarballs were removed at the end.

## Summary

All eight High findings (H1-H8), both Medium findings (M1-M2) and L1 from
the rx25b re-audit are addressed. The two named regressions (H1 unused levels
after a missing value, H2 valid paired prints and the valid constant-control
paired t test) plus the H3 large-scale ANOVA numeric regression are restored,
with value tests against the `5722a08` result. The rest of the ordinary
teaching grid is unchanged and still green.

Where a limit is hard to get right numerically, the documented domain is
narrowed and a clear named error is returned instead of re-engineering the
numerics: ANOVA sums of squares that cannot be represented at the response
scale stop with an underflow/overflow error, and two-sample variance stops
when a group variance is not positive and representable. All returned
p-values stay in `[0, 1]`; printers never show a positive probability as an
exact zero.

`NEWS.md` and the help pages for every changed interface now state the actual
conditions, the raw fields and the output precision. `DESCRIPTION` remains
at version `0.3.4` with `RoxygenNote: 7.3.2`.

## Finding -> change -> test -> audit reprex before/after

The "Before" column is the rx25b audit/HEAD (`2b1f83c`) result. The "After"
column is this build.

| Finding | Change | Test (file) | Before (audit reprex) | After (this build) |
|---|---|---|---|---|
| H1 | GOF removes missing observations and the explicit `NA` level only; every other declared level is retained, including an unused non-NA category. A vector of ones is the equal-proportions shorthand. | `H1: GOF keeps an unused declared level and restores the 5722a08 result`; `H1: GOF retains an unused non-NA level without missing values`; `H1: GOF drops an explicit NA level but keeps the others` (`test-rx25b-fixes.R`) | `removing 1 observation(s)` then `ERROR: Length of y must be equal...` | `chisq = 1`, `df = 2`, `p = 0.6065`; categories `a,b,c`; observed `1,1,0`; expected `2/3,2/3,2/3` |
| H2 (constant control) | The correlation is ancillary; when undefined, `corr`/`corsig` are labelled `NA` and the paired t test is still computed. | `H2: paired t test computes with a constant control column` (`test-rx25b-fixes.R`) | `ERROR: computed correlation ... is not finite` | `t = 1.4`, `df = 3`, `p = 0.2560073`, `corr = NA`, `corsig = NA`; prints with `NA` |
| H2 (two pairs) | Missing labels are printed as `NA`; widths no longer become `NA`. | `H2: paired t test prints a complete two-pair object` (`test-rx25b-fixes.R`) | first table then `ERROR: invalid 'width' argument` | full object prints: `t = 0`, `df = 1`, `p = 1.000`, correlation `1.00`, significance `NA` |
| H3 (1e149) | Sums of squares and mean squares are kept unrounded until converted back to original units, then rounded once. | `H3: ANOVA converts unrounded sums of squares back to original units` (`test-rx25b-fixes.R`) | within SS `3.996e298`, within MS `1.008e298` | within SS `4e298`, within MS `1e298`, F `13.5`; between `1.35e299`, total `1.75e299` |
| H3 (1e-170) | A positive sum of squares that underflows at the response scale stops with a named error; overflow is also rejected. | `H3: ANOVA is scale invariant at a representable tiny scale`; `M1: ANOVA's relative guard works at any representable response scale` (`test-rx25b-fixes.R`, `test-rx25-fixes.R`) | all SS/MS zero, F `13.5` with no bound or error | `ERROR: sums of squares for response 'x' underflow at this response scale; rescale 'x' to a larger magnitude`; at `1e-140` SS `4e-280`, MS `1e-280`, F `13.5` |
| H4 | The standard deviation is computed from a scaled sample for magnitudes below `1e-100`; confidence limits use `sigma * sqrt(df/q)`; `chi_raw` retains the unrounded statistic. | `H4: one-sample variance test is scale invariant at tiny scales` (`test-rx25b-fixes.R`) | `chi = 0`, `p_two = 0` | `chi = 5`, `p_two = 0.3435943`, identical after rescaling data and null SD by `1e170` |
| H5 (two-sample, subnormal) | Pooled z uses the direct expression for ordinary inputs and a factored square root (`p_sum`, `q_sum`) when the pooled product would underflow; `z_raw` retains the unrounded value. | `H5: two-sample proportions stay finite near the smallest doubles` (`test-rx25b-fixes.R`) | `z = Inf`; two-sided/upper p `0` | `z_raw = 7.02898e-162`; two-sided p `1`, lower/upper `0.5` |
| H5 (two-sample, near one) | The pooled complement is computed separately, so a pooled proportion that rounds to one no longer gives an infinite z. | same test | `z = Inf`; p `0` | `z_raw = 1.0536712e-7`; two-sided p `0.9999999` (printed `1` to three decimals) |
| H5 (one-sample) | The standard error uses a factored square root for subnormal probabilities; `z_raw` is retained; an overflowing percentage deviation prints as a bound. | `H5: one-sample proportion stays finite and prints honest bounds` (`test-rx25b-fixes.R`) | `z = Inf`; both/upper p `0`; deviation `Inf` | `z_raw = 7.113407e161`; p `0` but printed `< 0.0001`; deviation `> 1e308` |
| H6 (ANOVA) | ANOVA prints `< 0.0001` when the F tail underflows; representable small p-values keep their scientific display. | `H6: ANOVA prints an underflowed positive p-value as a bound` (`test-rx25b-fixes.R`) | `Sig. 0.0000` for F `12475` (`log` tail finite below `log(0.0001)`) | `Sig. < 0.0001` |
| H6 (independent-t variance row) | The folded-F row is printed from `f_sig_raw` and `f_raw`. | `H6: independent-t variance row prints an underflowed p-value bound` (`test-rx25b-fixes.R`) | `P > F 0` with raw `7.864835e-51` | `P > F < 0.0001` |
| H6 (one-sample variance) | The printer uses `chi_raw`, not the rounded chi statistic, to detect underflow. | `H6: variance printers use unrounded statistics for p bounds` (`test-rx25b-fixes.R`) | `Sig 0.0000` | `Sig < 0.0001` |
| H6 (two-sample variance) | The printer uses `f_raw`; a non-positive or unrepresentable variance stops clearly. | same test | `p 0.0000` | `p < 0.0001`; tiny unrepresentable variance errors with `positive, finite variances` |
| H7 | Two-sided p-values are selected from the unrounded lower and upper tails and capped at one, for pooled and Satterthwaite results. | `H7: independent-t two-sided p-values stay in [0, 1]` (`test-rx25b-fixes.R`) | `sig_raw = 1.0000077`, `sig_pooled_raw = 1.0000077`; small case `1.00008`/`1.0001` | `sig_raw = sig_pooled_raw = 0.9999923`, matching `t.test`; small case `0.99992`, printed `0.9999` |
| H8 | Proportion and count totals use separate tolerances; each expected value, the sum finiteness and the value range are validated before comparison; no silent normalization. | `H8: GOF rejects proportion totals and oversized expectations` (`test-rx25b-fixes.R`) | `c(.5,.501)` accepted: chi `0.1996`, expected `50000,50100`; `c(1e308,1e308)` gives `Inf`/`NaN` | `ERROR: expected counts in y must sum to the sample size (100000)...`; `ERROR: expected values in y must have absolute value <= 1e150` |
| M1 | GOF requires at least two retained categories. | `M1: GOF requires at least two categories` (`test-rx25b-fixes.R`) | chi `0`, df `0`, p `1` | `ERROR: the chi-square goodness-of-fit test requires at least two categories with positive expected values` |
| M2 | Satterthwaite degrees of freedom are computed from scaled variances; the two-sided p is selected from unrounded tails. | `M2: Welch t test handles large finite data without overflow` (`test-rx25b-fixes.R`) | `ERROR: missing value where TRUE/FALSE needed` | `t = -2.8284`, `df = 2`, `p = 0.1056`; identical after scaling by `1e-80` |
| L1 | Help pages state the actual domain, minimum samples, degenerate cases, raw fields and precision; raw fields were added to the association, Cochran, Levene and two-sample variance pages. | `L1: touched help pages state the supported input domain` (`test-rx25-fixes.R`); `L1: raw fields are documented on the remaining touched pages` (`test-rx25b-fixes.R`) | range/precision text incomplete or contradicted by H2-H6; raw fields undocumented | nine touched pages contain `Supported inputs`, `1e150`, `1e15`; the remaining touched pages document their `*_raw` fields; paired page documents `NA` |

### Full audit reprex output after this build

```text
H1
MESSAGE: removing 1 observation(s) with missing 'x'
 chisq     df      p
1.0000 2.0000 0.6065

H1 explicit NA level (the audit's c(.4,.4,.2) call)
ERROR: Length of y must be equal to the number of categories in x after removing missing observations
# with the two remaining expectations it prints cleanly (categories a,b)

H2 constant control
    t  df       p corr corsig
1.400   3 0.2560073   NA     NA

H2 two pairs
# full object prints; correlation 1.00, Sig. NA; t 0.00, df 1, Sig. 1.000

H3 1e149
ss_within ms_within         f
 4.00e+298 1.00e+298  1.35e+01

H3 1e-170
ERROR: sums of squares for response 'x' underflow at this response scale; rescale 'x' to a larger magnitude

H4 variance tiny
      chi     p_two
5.0000000 0.3435943

H5 two-sample subnormal
z_raw = 7.0289803374404621e-162
sig: both = 1, less = 0.5, greater = 0.5

H5 two-sample near one
z_raw = 1.0536712127723506e-07
sig_raw: both = 0.9999999, less = 0.5, greater = 0.5

H5 one-sample subnormal
z_raw = 7.1134072937536514e+161
sig: two-both = 0, less = 1, greater = 0
printed: Pr(|Z| > |z|) < 0.0001 ; Pr(Z > z) < 0.0001
deviation: "-50.00" "> 1e308"

H6 ANOVA
Between Groups ... F 12475   Sig. < 0.0001

H6 independent-t variance row
    x         Folded F      29        29       10000     < 0.0001

H6 one-sample variance
    x        0    99    < 0.0001

H6 two-sample variance
 0     100       100      < 0.0001

H7 independent t
sig_raw = sig_pooled_raw = 0.9999923 (reference t.test = 0.9999923)
small case: sig_raw = sig_pooled_raw = 0.99992 (printed 0.9999)

H8 proportions total
ERROR: expected counts in y must sum to the sample size (100000); expected values in y are counts unless they sum to 1

H8 huge expectations
ERROR: expected values in y must have absolute value <= 1e150

M1 one level
ERROR: the chi-square goodness-of-fit test requires at least two categories with positive expected values

M2 Welch 1e80
      t      df       p
-2.8284  2.0000  0.1056
```

## Regressions restored (5722a08 versus HEAD)

| Regression | 5722a08 output | HEAD (`2b1f83c`) output | This build |
|---|---|---|---|
| H1: GOF missing observation with an unused declared level `c` | `chisq 1`, `df 2`, `p 0.6065` | message then length error | `chisq 1`, `df 2`, `p 0.6065`; category `c` retained |
| H2: paired t test with constant control `y = rep(2,4)` | `t 1.4`, `df 3`, `p 0.2560073`, `corr NA`, `corsig NA` (warning from `cor`) | non-finite-correlation error | same t/df/p, `corr NA`, `corsig NA`, no error |
| H2: printing two valid pairs | printed the object (`corr 1.00`, `Sig. NaN`, `t 0`, `df 1`, `p 1`) | first table then `invalid 'width' argument` | prints the full object (`corr 1.00`, `Sig. NA`, `t 0`, `df 1`, `p 1.000`) |
| H3: ANOVA at `1e149` | within SS `4.00e298`, within MS `1.00e298` | within SS `3.996e298`, within MS `1.008e298` | within SS `4e298`, within MS `1e298` |

The H1 explicit-NA-level variant previously printed until the `NA` category was
reached. This build drops the NA level: the audit's three-expectation call is
now a clear category-length error, and the matching two-expectation call
prints cleanly. This is the honest version of the re-audit's instruction to
remove the explicit NA level only.

The ordinary regression grid (`tests/testthat/test-ordinary-grid.R`) stayed
green before and after: ANOVA on `mtcars`, GOF on `hsb`, McNemar, one-sample
t and variance, independent and paired t, grouped and calculator proportions,
and one-sample proportions all still match their base R references. No
snapshot was accepted or changed.

## Tests

New tests are in `tests/testthat/test-rx25b-fixes.R`; the ANOVA M1 test in
`test-rx25-fixes.R` was updated because `1e-170` is now an explicit
underflow error instead of a table of zeroes. Every new assertion is a value
or whole-print assertion; there are no snapshot-only tests. Heavy work is
limited to a handful of fixed vectors, so the suite stays fast.

## Checks

### Disposable archive and test run

The working tree was staged and written as a tree object, then extracted with
`git archive` into a disposable folder outside the repository:

```bash
git add -A
TREE=$(git write-tree)
git archive "$TREE" | tar -x -C <disposable>/clean
Rscript -e "Sys.setenv(NOT_CRAN='true'); devtools::test('<disposable>/clean', reporter='summary', stop_on_failure=FALSE)"
```

Result: all files passed, no failures, warnings or errors.

### Build and CRAN check

```bash
cd <disposable>
LC_ALL=C _R_CHECK_CRAN_INCOMING_REMOTE_=false R CMD build clean
R_LIBS_USER=<disposable>/lib LC_ALL=C _R_CHECK_CRAN_INCOMING_REMOTE_=false \
  R CMD check --as-cran --no-manual inferr_0.3.4.tar.gz
```

Final result: **Status: OK. 0 errors, 0 warnings, 0 notes.** Vignettes built
and rebuilt successfully.

Check test summary from `inferr.Rcheck/tests/testthat.Rout`:

```text
[ FAIL 0 | WARN 0 | SKIP 42 | PASS 1537 ]
```

The 42 skips are 33 CRAN snapshot skips plus 9 repository-relative
documentation checks unavailable in the built archive. The final
`00check.log` test time is:

```text
* checking tests ... [15s] OK
  Running 'testthat.R' [15s]
```

This is well under the 4 minute local limit. (The first verification run on
the same code path took 22 s; no test was slow enough to need `skip_on_cran()`
for time.)

### Cleanup

The disposable check folders, libraries and tarballs under
`C:/Users/DELL/AppData/Local/Temp/opencode/` were removed after the report was
written. No installation was made into a persistent library.

## Files changed

- `R/ifr-utils.R`: NA-aware p formatting; stable one- and two-sample z helpers.
- `R/ifr-chisq-gof-test.R`: retain declared non-NA levels; drop only missing
  values/NA level; equal-proportions shorthand; separate proportion/count
  tolerances; finite and range checks; one-category error.
- `R/ifr-ts-paired-ttest.R`: ancillary correlation may be `NA`; p capped at 1.
- `R/ifr-anova.R`: unrounded sum-of-squares conversion; underflow/overflow
  errors; `pval_raw`; bound printing on underflow.
- `R/ifr-os-var-test.R`: scaled standard deviation and confidence limits;
  `chi_raw`; p capped at 1.
- `R/ifr-ts-prop-test.R`, `R/ifr-os-prop-test.R`: stable z and complements;
  `z_raw`; p capped at 1; deviation overflow bound.
- `R/ifr-ts-ind-ttest.R`: scaled Satterthwaite df; unrounded two-sided tail
  selection and cap; `f_raw`; bound variance row.
- `R/ifr-ts-var-test.R`: `f_raw`; finite/sample-size guard; positive-variance
  error; bound printer.
- `R/ifr-output.R`: ANOVA, paired, independent-t variance, one- and two-sample
  variance and proportion printers use the raw statistic and printed bounds.
- `R/ifr-chisq-assoc-test.R`, `R/ifr-cochran-q-test.R`, `R/ifr-levene-test.R`,
  `R/ifr-ts-var-test.R`: documented raw fields and enforced documented limits.
- `man/*.Rd`: regenerated help for the touched pages (manually kept in sync
  for the final ANOVA and two-sample variance wording).
- `NEWS.md`: corrected the earlier overbroad claims and added the rx25b fixes.
- `tests/testthat/test-rx25-fixes.R`, `tests/testthat/test-rx25b-fixes.R`.
- `docs-dev/pkg-inferr-fx25b-report.md` (this report).

## Backlog (out of domain, not fixed)

These remain outside the documented domain or are degenerate edge cases and
were deliberately left for the package backlog, per the stop rule:

- McNemar with no discordances (`matrix(c(5,0,0,0),2)`): the documented
  convention returns statistic 0 and p 1, but kappa and its interval are
  `NaN` because kappa is undefined when expected agreement is 1. The re-audit
  itself lists this as backlog; no finite estimate was fabricated.
- Inputs above `|x| = 1e150` (for example `1e160`) for the variance, paired
  and ANOVA interfaces are rejected with the documented range error. More
  robust handling of such extreme finite scales is a separate numerical
  project.
- One-sample and two-sample variance outputs at sub-`1e-4` scales still store
  their ancillary standard deviations and confidence limits at the documented
  four-decimal precision, so those fields display `0` even when the
  chi-square statistic and p-value are now correct. The statistic and p-value
  are the tested quantities; increasing the precision of the ancillary fields
  is a display-precision backlog item, not a wrong test result.

## Boundary restoration from the rx25c regression audit (pkg-inferr-fx25c)

The regression-only audit (`pkg-inferr-rx25c-audit.md`) found two guards added
in rx25b that rejected boundary cases the `5722a08` baseline and base R accept.
Both are restored without changing any other fx25/fx25b behaviour.

| case | 5722a08 | before (`e326122`) | after |
|---|---|---|---|
| Two variances, constant first sample (`x = rep(2,5)`, `y = 1:5`) | F `0`; lower `0`; upper `1`; two-sided `0`; prints | error requiring positive, finite variances | F `0`; lower `0`; upper `1`; two-sided `0`; prints; matches `var.test` |
| Two variances, constant second sample (`y` first, `x` second) | F `Inf`; lower `1`; upper `0`; two-sided `0`; prints | same error | F `Inf`; lower `1`; upper `0`; two-sided `0`; prints; matches `var.test` |
| Two variances, grouped form with one constant group | F `0` or `Inf`; matching tails `0`/`1`; prints | same error | same as `5722a08`; matches `var.test` |
| Two variances, both samples constant | `NaN` (undefined) | error | error (undefined, as instructed) |
| Exact-fit one-way ANOVA (between SS `1.5`, within SS `0`) | F `Inf`; p `0`; prints | error: computed F statistic is not finite | between SS `1.5`; within SS `0`; F `Inf`; p `0`; prints |
| One-way ANOVA with no total variation | error: response has no variation | error | error (unchanged) |

A genuinely constant group is now distinguished from a non-constant group
whose variance is not positive and representable: the latter still stops, as
does the all-constant variance case. Tests in
`tests/testthat/test-rx25c-fixes.R` assert the restored values against both
`stats::var.test` and the `5722a08` values, and exercise the printed output.

### Checks

From a clean `git archive` of the staged tree (`037c56b`) in a disposable
folder: `devtools::test()` passed all files with no failures, warnings or
errors. `R CMD build --no-manual` produced `inferr_0.3.4.tar.gz`, and
`R CMD check --as-cran --no-manual` on that tarball completed with
**Status: OK** (exit 0; 0 errors, 0 warnings, 0 notes). The check test summary
was `[ FAIL 0 | WARN 0 | SKIP 42 | PASS 1614 ]` in `[13s]`, below the
four-minute limit. Temporary folders, the disposable library and the tarball
were removed afterwards.

