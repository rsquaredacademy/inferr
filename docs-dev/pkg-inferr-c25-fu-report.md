# pkg-inferr-c25-fu — follow-up to the C25 audit

- Worktree: `F:/R/Packages/_worktrees/inferr-c25`, branch `task/inferr-c25`
- Base for this round: `2f1bdc4` "Audit pkg-inferr-c25 build" (audit report
  committed); package build commit `6b0541a`
- Audit being followed up: `docs-dev/pkg-inferr-c25-aud-report.md`
- Date: 2026-11-19
- Scope: M1, L1, L2, L5 only. L3 and L4 are left as written follow-ups
  (reproduced in full at the end). No version bump; NEWS dev section only; no
  push, no gh/GitHub, no branch switch.

## Files changed

| File | Item | Change |
|---|---|---|
| `R/ifr-anova.R` | M1 | Relative-tolerance constant-response guard |
| `tests/testthat/test-anova.R` | M1, L5 | New M1 test block (+2 ordinary cases); fixed the joined line in the H8 test |
| `R/ifr-mcnemar-test.R` | L1 | Error message names the data argument |
| `tests/testthat/test-mcnemar.R` | L1 | Expectations updated to the new message |
| `NEWS.md` | L2 | Dropped the incorrect `stats::t.test()` comparison |
| `R/ifr-utils.R` | L2 | Comment above `format_satt_df()` says "two decimals" only |

No other file, doc, version or dependency was touched.

## M1 — ANOVA near-constant response guard

`R/ifr-anova.R` now tests the response spread relative to its own magnitude:

```r
response     <- fdata[[x1]]
response_ss  <- sum((response - mean(response)) ^ 2)
response_sd  <- sqrt(response_ss / length(response))

if (!is.finite(response_sd) ||
    response_sd <= 1e-12 * max(1, mean(abs(response)))) {
  stop(paste0("response '", x1,
              "' has no variation; a one-way ANOVA is undefined"),
       call. = FALSE)
}
```

This is the audit's suggested rule, with the same "has no variation" error.
The old guard rejected only `response_ss <= 0`, so a response whose values
differ by one ulp passed and `lm()` produced cancellation noise.

### Before / after (audit reprex)

Input: `data.frame(x = c(0.3, 0.1 + 0.2, 0.3, 0.1 + 0.2, 0.3, 0.1 + 0.2),
g = factor(rep(c("a", "b"), each = 3)))`.

Before (old guard, `HEAD` code):

```text
WARNING: essentially perfect fit: summary may be unreliable
fstat = 6  pval = 0.070484  r2 = 0.673  adj_r2 = 0.5912
ss_between = 0  ss_within = 0
```

After (fixed code):

```text
ERROR: response 'x' has no variation; a one-way ANOVA is undefined
```

### Ordinary responses unaffected

The new test block also compares two ordinary responses against
`stats::anova(lm(...))`. Outputs are identical before and after the fix:

```text
mtcars mpg/cyl: F = 39.698  p = 4.97892e-09  r2 = 0.7325
hsb write/prog: F = 21.275  p = 4.31016e-09  r2 = 0.1776
```

Additional spot checks from the audit's valid-input grid are still accepted
after the fix: a response at `1e-8` scale (`F = 100`), the same response at a
`1e9` offset (`F = 100`), and a response constant within groups but different
between groups (`F = Inf`, the pre-existing `lm()` warning only).

### Test (fails on HEAD)

`tests/testthat/test-anova.R`:
"a response constant up to double precision is rejected (M1)". It asserts the
error on the reprex and checks the two ordinary cases above.

## L1 — McNemar message names the argument

`R/ifr-mcnemar-test.R` (`mcdata()`):

Before: `stop("counts must be non-negative", call. = FALSE)`

After: `stop("data must contain non-negative counts", call. = FALSE)`

Before / after for `ifr_mcnemar_test(matrix(c(10, 2, -1, 5), 2))`:

```text
Before: ERROR: counts must be non-negative
After:  ERROR: data must contain non-negative counts
```

The invalid-NA case uses the same message and the valid matrix case is
unchanged. `tests/testthat/test-mcnemar.R` expectations were updated to the
new wording (that updated block is the failing test on HEAD).

## L2 — NEWS/comment `stats::t.test()` claim

`stats::t.test()` prints the df with about 5 significant digits, not two
decimals (audit examples: `18.332` vs `18.33`, `4.3479` vs `4.35`). The
behaviour stays two decimals; only the claim is removed.

Before (`NEWS.md`):

```text
- `ifr_ts_ind_ttest()` now stores the Satterthwaite degrees of freedom
  unrounded and prints them with two decimals, as `stats::t.test()` does.
```

After:

```text
- `ifr_ts_ind_ttest()` now stores the Satterthwaite degrees of freedom
  unrounded and prints them with two decimals.
```

Before (`R/ifr-utils.R`, above `format_satt_df()`):

```r
# Satterthwaite degrees of freedom are printed with two decimals, as
# stats::t.test() does, while the stored value remains unrounded
```

After:

```r
# Satterthwaite degrees of freedom are printed with two decimals, while the
# stored value remains unrounded
```

No behaviour changed, so no test is added; the existing #89 tests still check
the stored unrounded value and the printed `169.71`.

## L5 — `tests/testthat/test-anova.R` formatting

Before (joined lines):

```r
test_that("ANOVA works when columns are named x or y (H8)", {  d <- data.frame(
```

After:

```r
test_that("ANOVA works when columns are named x or y (H8)", {
  d <- data.frame(
```

Cosmetic only; the test content is unchanged.

## Proof that the new tests fail on `HEAD`

The R fixes were stashed (`git stash push -- R/ifr-anova.R
R/ifr-mcnemar-test.R`), leaving the new tests in place, and the two test files
were run against the old code:

```text
ℹ Testing inferr
WARNING: 'test-anova.R:149:3' ---------------------
essentially perfect fit: summary may be unreliable
FAILURE: 'test-anova.R:149:3' ---------------------
Expected `ifr_oneway_anova(d, x, g)` to throw a error.

ERROR: 'test-mcnemar.R:149:3' ---------------------
Error: counts must be non-negative

[ FAIL 2 | WARN 1 | SKIP 0 | PASS 113 ]
```

The stash was then restored (`git stash pop`) and the same files pass:

```text
ℹ Testing inferr
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 117 ]
```

L2 and L5 change no behaviour, so they have no failing test; L5 is test-file
formatting and L2 is NEWS/comment text.

## Test totals

`devtools::test()` on the fixed tree:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1284 ]

files: 20  blocks: 198  failed: 0  errors: 0  skipped: 0  warnings: 0  expectations passed: 1147
```

Baseline in the C25 audit was 20 files, 197 blocks, 1141 expectations passed.
This round adds one block and six expectations (1 `expect_error` + 4
`expect_equal` + 1 `expect_true`). `git status` shows only the six files
above plus this report.

## Commands run

```text
Rscript <temp>/run-tests.R "anova|mcnemar"      # targeted: FAIL 0, PASS 117
git stash push -m fu-proof-old-code -- R/ifr-anova.R R/ifr-mcnemar-test.R
Rscript <temp>/run-tests.R "anova|mcnemar"      # old code: FAIL 2, WARN 1
git stash pop
Rscript <temp>/reprex.R                         # after outputs (M1, L1, ordinary)
git stash push -m fu-before-output -- R/ifr-anova.R R/ifr-mcnemar-test.R
Rscript <temp>/reprex.R                         # before outputs
git stash pop
Rscript <temp>/edges.R                          # 1e-8, 1e9 and F = Inf accept checks
Rscript <temp>/totals.R                         # devtools::test() totals
```

Helper scripts are in `C:/Users/DELL/AppData/Local/Temp/inferr-c25-fu/`
(`run-tests.R`, `totals.R`, `reprex.R`, `edges.R`). All R was run from script
files with `Rscript`; `pkgload::load_all()` was used inside the scripts and
`devtools::test()` for the totals.

## Follow-ups left as written (not fixed in this round)

**L3. `ifr_ts_prop_calc()` accepts boundary proportions that make the pooled
z undefined (pre-existing).** `ifr_ts_prop_calc(30, 25, 0, 0)` and
`(30, 25, 1, 1)` return `z = NaN`, `sig = NaN` with no message, the same as
origin/develop. `[0, 1]` is the range the brief asked for, so this is not a
regression. A clear error, or `z = 0, p = 1` with a note, would be better than
NaN. (`(30, 25, 0, 1)` gives the correct finite `z = -7.416`.)

**L4. Goodness-of-fit with one zero expected cell and a positive observed count
still gives `chisquare = Inf, pvalue = 0` (pre-existing).** The builder lists
this as follow-up 3. Reprex: `ifr_chisq_gof_test(hsb, race, c(0, 20, 40, 140))`.
