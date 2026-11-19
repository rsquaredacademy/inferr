# pkg-inferr-c25-fu2: follow-up 2

Date: 2026-11-19. Branch: `task/inferr-c25`.
Starting HEAD: `490dd6605cbacf22cb98a69ec8e8d8da10ffd7f2`.

Read `F:/pm/notes/package-audit-stop-rule.md` and
`docs-dev/pkg-inferr-c25-rx-audit.md`. The worktree was clean initially.
This change addresses RX1, L3 and L4 from that re-check.

## Changes and supported domain

- **RX1 / ANOVA:** Removed the absolute floor from the constant-response
  guard. It now rejects population SD at most `1e-12 * mean(abs(response))`.
  The two-group response `(1:6) * 1e-13` is accepted and agrees with both
  the unscaled response and `anova(lm(...))`: F = 13.5,
  p approximately 0.0213116411287567. Exact constants, including all-zero
  data, and the original M1 one-ulp example remain rejected.
- **L3 / pooled proportion z:** Added a shared boundary guard to
  `ifr_ts_prop_calc()`, `ifr_ts_prop_test()` and `ifr_ts_prop_group()`.
  Both-zero and both-one samples now error, for example:
  `p1 and p2: both samples have proportion 0; the pooled z test is undefined`.
  The both-one case substitutes `1` for `0`. No documented API promise
  required retaining the previous silent NaN. The calculator still accepts
  positive integer sample sizes and finite proportions in `[0, 1]`, except
  these two undefined combinations. Opposite boundary proportions, a single
  homogeneous sample and equal interior proportions remain accepted.
- **L4 / GOF:** Rejects a zero expected cell with a positive observed count
  before either corrected or uncorrected calculation. For the requested hsb
  example, the error starts:
  `y[1] for category '1' of 'race' is zero but its observed count is positive`.
  It continues `the chi-square goodness-of-fit test is undefined`.
  Expected values remain finite, non-negative counts or proportions in
  factor-level order; proportions sum to one.

Updated the development NEWS and the three functions' roxygen/Rd help with
the relevant boundaries and stored precision: ANOVA F and proportion z/p
use three decimals; GOF chi-square/p use four; ANOVA p remains unrounded.
P-values continue to be computed from unrounded statistics. No version or
dependency change: DESCRIPTION still contains the worktree's `0.3.3`, despite
the brief's reference to the next 0.3.4 development release.

## Regression evidence: fail before, pass after

Added the tests first and ran them against the starting HEAD's unchanged
implementation, then ran the same tests after the fixes. No branch switch
or revert of implementation files was needed.

| New block | Before fixes | After fixes |
|---|---|---|
| RX1 response-scale acceptance | 1 errored block at the small-scale call | 23 passes |
| L3 pooled proportion boundaries | 24 failed expectations, 10 passes | 34 passes |
| L4 zero expected GOF cells | 5 failed expectations | 5 passes |

The targeted command was `devtools::test(filter =
"anova|two-prop-test|chisqgof", reporter = "summary", stop_on_failure = FALSE)`.
Structured totals from `as.data.frame(res)`:

```text
Before: 3 files, 46 blocks, 29 failed, 1 error, 0 warnings, 0 skips, 163 passed
After:  3 files, 46 blocks,  0 failed, 0 errors, 0 warnings, 0 skips, 215 passed
```

The scale regression checks multipliers `1e-13`, `1e-8`, `1`, `1e6` and
`-1e-13` against the unscaled result and base R, plus constants `0`, `0.3`
and `1e-13`. The existing M1 test using `0.3` versus `0.1 + 0.2` is unchanged.
Proportion boundary tests cover both common boundaries, all four
alternatives and all three public entry points, with nearby valid cases
checked against uncorrected `prop.test()`. GOF tests cover counts and
proportions, both correction settings, and a renamed category at cell 2.
Existing ordinary-input, reference and snapshot tests remain in the suite.

## Full validation

Environment: Windows 11 x64, R 4.6.1 (2026-06-24 ucrt), devtools 2.5.2,
testthat 3.3.2. R commands used script files. Build/check artifacts and logs
are outside the repository. The initial before-fix run emitted four inherited
`C.UTF-8` startup warnings, outside testthat. Subsequent R launches used
`LANG=C` and `LC_ALL=C`; the check script also sets the R locale to `C`,
following the re-check's documented Windows locale workaround.

Full `devtools::test(reporter = "summary", stop_on_failure = FALSE)`:

```text
20 files, 201 blocks, 1,209 passed expectations
0 failed expectations, 0 errors, 0 warnings, 0 skips
```

Ran the full requested check:

```r
res <- devtools::check(
  document = FALSE, args = "--no-manual",
  check_dir = file.path(scratch_root, "check-locale-C"), error_on = "never"
)
```

```text
CHECK TOTAL errors=0 warnings=0 notes=1
Installed tests: [ FAIL 0 | WARN 0 | SKIP 7 | PASS 1331 ]
```

The sole note is the existing `Found the following hidden files and
directories: .git` note for the worktree pointer file. The seven installed
test skips are the existing file-existence guards in `test-docs.R`, where
source-tree help, README, DESCRIPTION and build-ignore paths are unavailable
in the installed test context. Source tests skip none. The source structured
expectation count and installed console count are separate totals.

The check completed installation, compiled-code checks, examples, tests,
vignettes and vignette rebuilding. `error_on = "never"` permits collecting
the check result; no failure was suppressed or reclassified. Devtools used
`--as-cran` in addition to `--no-manual`, with CRAN incoming and incoming
remote checks disabled, force-suggests false and `NOT_CRAN=true`. This is
the requested devtools check, not a remote CRAN incoming gate. No check
retry was necessary. `git diff --check` also passed.

## Evidence and scope

Scratch root: `C:/Users/DELL/AppData/Local/Temp/inferr-c25-fu2-20261119/`.
Evidence includes `targeted.R`, `targeted-before.log/.rds`,
`targeted-after.log/.rds`, `test-evidence.R/.log`, `full-tests.R/.log/.rds/.csv`,
`full-check.R/.log` and `check-locale-C.rds`.
The check log is `check-locale-C/inferr.Rcheck/00check.log`; installed-test
output is `check-locale-C/inferr.Rcheck/tests/testthat.Rout`.

Scope is the three requested fixes, regression tests, help, development NEWS
and this report. All requested runtime findings are addressed and the
ordinary suite passes. This focused fix and verification does not replace
the independent cross-family release gate required by the stop rule.
No push, gh/GitHub use, branch switch or version bump.
