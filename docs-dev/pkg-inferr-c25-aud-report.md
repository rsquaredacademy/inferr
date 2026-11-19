# pkg-inferr-c25-aud — audit of the pkg-inferr-c25 build

- Worktree: `F:/R/Packages/_worktrees/inferr-c25`, branch `task/inferr-c25`
- Audited range: `git diff origin/develop..HEAD` = one commit `6b0541a`
  "Validate inputs, reject constant ANOVA response, print Satterthwaite df
  (#100, #99, #89)" on base `origin/develop` `2a2c786`
- Brief: `F:/pm/notes/2026-11-15-day/briefs/pkg-inferr-c25-brief.md`
- Builder report: committed as `docs-dev/inferr-c25-report.md` (not
  `docs-dev/pkg-inferr-c25-report.md`; contents are as the brief requires)
- Date: 2026-11-18. Read and verify only: no package code changed, no push, no gh,
  no branch switch.

## Verdict

All three issues are fixed. Ordinary inputs give the same numbers as
`origin/develop` in a 241-call grid. The only differences are the intended
unrounded `df_satterthwaite` and how it prints. The new errors do not reject
the valid inputs tried. Every new test fails on the old code.
`devtools::test()` passes (0 failures). There are no High findings. One Medium finding is a
pre-existing near-constant ANOVA edge that the #100 guard does not cover.
The rest are Low or Info.

## Findings

### High

None.

### Medium

**M1. ANOVA: a response that is constant up to floating-point representation
still produces a spurious F, p and R-squared (#100 class only partly covered;
pre-existing, unchanged by the build).**

The guard at `R/ifr-anova.R` rejects only `sum((x - mean(x))^2) <= 0`. If a
response's values differ only by one ulp (e.g. some computed as `0.1 + 0.2`,
others typed as `0.3`), the guard passes. `lm()` and `anova_split()` then
return cancellation noise. The printed table shows every sum of squares as
`0` but F as `6`, which contradicts itself.

```r
d <- data.frame(x = c(0.3, 0.1 + 0.2, 0.3, 0.1 + 0.2, 0.3, 0.1 + 0.2),
                g = factor(rep(c("a", "b"), each = 3)))
ifr_oneway_anova(d, x, g)
```

- Actual (HEAD and origin/develop identical): `Between Groups 0 1 0 6 0.070484`,
  `Within Groups 0 4 0`, `R-squared = 0.673`, `Adj R-squared = 0.5912`, and an `lm()`
  "essentially perfect fit" warning.
- Expected: either the same "has no variation" error (the response cannot be
  told apart from a constant at double precision) or the exact result for the
  stored doubles. In units of one ulp the data are `0 1 0 / 1 0 1`, which gives
  F = 0.5, p = 0.5185, R² = 0.111. `stats::anova(lm(x ~ g))` on the raw doubles
  is also noise (F = 0.1153).
- Severity: Medium, not High. The input is ill-conditioned: its variation is at
  machine precision, which double-precision arithmetic cannot resolve
  (`stats` fails the same way). The behaviour predates the build. The issue
  reprex (an exact constant) and the other exact-constant cases are fixed.
- Suggested follow-up: use a relative-tolerance guard, e.g. reject when
  `sqrt(response_ss / n) <= 1e-12 * max(1, mean(abs(response)))`, or compute
  sums of squares from `x - median(x)`. Add the reprex above as a test.

### Low

**L1. The McNemar negative-count message does not name the argument.**
The brief asks that "Error messages name the argument and the valid range".
`ifr_mcnemar_test(matrix(c(135, -18, 21, 26), 2))` stops with
`counts must be non-negative`. Expected something like
`data must contain non-negative counts`. The behaviour is correct; only the
wording is off.

**L2. The NEWS/comment claim "as `stats::t.test()` does" is only true for some
data.** `t.test()` prints the df with 5 significant digits
(`format(x, digits - 2)`), not two decimals. hsb/female/write agree
(`169.71`), but mtcars/am/mpg prints `18.33` in inferr and `df = 18.332` in
`t.test()`, and a small sample prints `4.35` against `4.3479`. The brief
allowed choosing two decimals, so the behaviour is fine. The NEWS bullet and
the `format_satt_df()` comment should say "two decimals" without the
`t.test()` comparison.

**L3. `ifr_ts_prop_calc()` accepts boundary proportions that make the pooled
z undefined (pre-existing).** `ifr_ts_prop_calc(30, 25, 0, 0)` and
`(30, 25, 1, 1)` return `z = NaN`, `sig = NaN` with no message, the same as
origin/develop. `[0, 1]` is the range the brief asked for, so this is not a
regression. A clear error, or `z = 0, p = 1` with a note, would be better than
NaN. (`(30, 25, 0, 1)` gives the correct finite `z = -7.416`.)

**L4. Goodness-of-fit with one zero expected cell and a positive observed count
still gives `chisquare = Inf, pvalue = 0` (pre-existing).** The builder lists
this as follow-up 3. Reprex: `ifr_chisq_gof_test(hsb, race, c(0, 20, 40, 140))`.

**L5. Formatting slip in `tests/testthat/test-anova.R`.** The diff joins two
lines: `test_that("ANOVA works when columns are named x or y (H8)", {  d <- data.frame(`.
The test still runs; this is a cosmetic, unrelated change in an
otherwise test-only hunk.

### Info

- I1. In the `alternative = "all"` layout of `ifr_ts_ind_ttest()`, which prints
  no DF row, the "------------------------" underline under "Independent
  Samples Test" moves 2 characters, because `df_w` now uses the formatted
  Satterthwaite df. Only whitespace changes; the snapshot was updated.
- I2. `ifr_ts_prop_calc()` uses a strict integer check, so `n1 = 30 + 1e-12`
  is now rejected. This matches the existing checks in `ifr_binom_calc()` and
  `ifr_os_prop_test()`. `0.3 * 100` is still accepted (it equals 30 in R).
- I3. The builder's report file is `docs-dev/inferr-c25-report.md`, not
  `docs-dev/pkg-inferr-c25-report.md`.
- I4. `man/`, `DESCRIPTION` and `NAMESPACE` are unchanged. The version stays
  `0.3.3` and the NEWS bullets are under a new `# inferr (development version)`
  heading, as the brief requires.

## Passing checks

### (1) Each issue is fixed — reprexes re-run (HEAD vs origin/develop)

| Call | origin/develop | HEAD |
|---|---|---|
| #100 `ifr_oneway_anova(data.frame(x = rep(1, 6), g = factor(rep(c("a","b"), each = 3))), x, g)` | `fstat=NaN pval=NaN r2=0.5152` + lm warning | `ERROR: response 'x' has no variation; a one-way ANOVA is undefined` |
| same with `rep(0.1,6)`, `rep(1/3,9)` (3 groups), `rep(1e6+0.1,8)`, `rep(0.7,7)`, constant after NA drop, character group | NaN F, spurious R² (0.5, 0.5385, 0.5625, NaN) | same clear error naming the response |
| #99 `ifr_os_var_test(hsb, read, sd = 0)` / `sd = -1` | `chi=Inf` / `chi=20919.42` | `ERROR: sd must be greater than 0` |
| `sd = Inf` / `sd = c(1, 2)` | `chi=0` / "the condition has length > 1" | `ERROR: sd must be a finite number greater than 0` |
| `confint` = 0, 1, -0.5, 1.5, NaN in `ifr_os_var_test`, `ifr_ts_ind_ttest`, `ifr_ts_paired_ttest` | numbers (degenerate/inverted CIs, NaN with warnings) | `ERROR: confint must be between 0 and 1 (exclusive)` |
| `confint = NA`, `c(0.9, 0.95)`, `"0.95"` in the same three | NA CIs / vector CIs / cryptic errors | `ERROR: confint must be numeric` |
| `ifr_os_t_test(..., alpha = 0/1/-0.5/1.5)` | already errored | still errors via the shared `check_probability()` helper (message now ends "(exclusive)") |
| `ifr_ts_prop_calc` n1/n2 = 0, -1, 30.5/25.5, Inf | `z=0`, NaN, `z=-1.52` … | `ERROR: n1 (n2) must be a positive integer` |
| `ifr_ts_prop_calc` p1/p2 = 1.5, -0.1, NA | `z=NaN`, `-5.861`, `-12.259`, `4.576`, NA | `ERROR: p1 (p2) must be between 0 and 1` |
| `ifr_chisq_gof_test(hsb, race, c(-20, 20, 40, 160))` | `chisquare=-81.3438 pvalue=1` | `ERROR: y must contain non-negative expected counts or proportions` |
| GOF `y` all zero / containing NA | `chisquare=Inf` / cryptic error | clear errors |
| `ifr_mcnemar_test(matrix(c(135, -18, 21, 26), 2))` (also `as.table`) | `statistic=507 pvalue=0 kappa=0.9405` | `ERROR: counts must be non-negative` |
| #89 `ifr_ts_ind_ttest(hsb, female, write)$df_satterthwaite` | `170` | `169.70738` (= `t.test()$parameter`, diff < 1e-13) |
| #89 printed Satterthwaite row | `write Satterthwaite Unequal 170 -3.6564 0.0003` | `write Satterthwaite Unequal 169.71 -3.6564 0.0003` (also `less`/`greater`; `18.33`, `4.35`, and `6.00` for an integral df) |

`grep` for `confint`/`alpha` arguments across `R/` finds exactly four
user-facing functions: `ifr_os_var_test`, `ifr_ts_ind_ttest`,
`ifr_ts_paired_ttest` (confint) and `ifr_os_t_test` (alpha). All four use
`check_probability()`. The remaining "n = 0 / p > 1 / negative counts" inputs
(`ifr_binom_calc`, `ifr_binom_test`, `ifr_os_prop_test`) were already
validated on origin/develop and still error.

### (2) Ordinary inputs give the same numbers as origin/develop

I extracted the base with `git archive origin/develop` into
`%TEMP%/inferr-c25-aud-base` and ran `pkgload::load_all` on it in a
separate `Rscript`. The same grid script ran on both trees and stored
`unclass(result)`, the `print()` output and the warnings for 241 calls:

- ANOVA (6 designs, including F = Inf with constant-within/different-between)
- os_var (4 alternatives × 3 confint × 3 sd, plus 2 more)
- os_t (4 × 3 alpha × 3 mu)
- ind t (5 datasets × 4 alternatives × 3 confint)
- paired t (2 × 4 × 3)
- ts_prop_calc (7 settings × 4 alternatives, including boundaries and integer input)
- ts_prop_test/group, os_prop, binom, GOF (7, including proportions, integers and correct = TRUE)
- McNemar (6, including matrix/table/data input, zero discordant cells and integer matrices)
- chisq_assoc, cochran, levene, runs, ts_var

Result: `error-diff 0, warn-diff 0, value-diff 60, print-diff 60`. All 60
value differences are in `ifr_ts_ind_ttest` and only in `df_satterthwaite`
(e.g. `170 -> 169.7074`, `188 -> 188.4631`, `18 -> 18.33225`). All 60 print
differences are the DF column, its width and the centering. A token-level
check found 0 changes outside the Satterthwaite DF. Every other value, in
every other function, is `identical()`. Two grid calls error on both trees in
the same way (`ifr_ts_var_test` does not accept `"both"`, which is my grid's
mistake and unchanged).

### (3) New errors don't reject valid inputs

These are accepted, with the same numbers as base:

- `confint`/`alpha` = 0.001, 0.5, 0.9, 0.95, 0.99, 0.999
- `sd` = 1e-10, 0.5, 10, `10L`
- `ifr_ts_prop_calc` with `30L`, `n = 1`, `p = 0`, `p = 1`, `(0, 1)`
- GOF with proportions summing to 1, integer counts and fractional expected counts
- McNemar with zero cells, integer matrices and tables
- ANOVA with real variation at 1e-8 scale and at a 1e9 offset (F = 10 on both trees)
- ANOVA with a response constant within groups but different between groups (F = Inf)

The only newly rejected inputs are outside the documented or brief domains,
plus `n1 = 30 + 1e-12` (I2).

### (4) Tests are meaningful

I copied the origin/develop source to `%TEMP%/inferr-c25-aud-oldcode`,
overlaid HEAD's `tests/` and ran `devtools::test()` there. The result was 13
failed and 6 errored test blocks, 40 failed expectations. Every new or changed
block fails on the old code:

- the #100 ANOVA block
- GOF negative/zero
- McNemar negative counts
- os_var sd and confint
- ind t confint and #89 df
- the 4 updated ind t snapshots and the expected-result block
- paired confint
- ts_prop_calc validation

`test-regression.R` passes on the old code, as a regression guard should. On
HEAD it records values from before the change. The #89 test checks the stored
value against `t.test()`, checks it is non-integral, and checks the printed
`169.71`.

### (5) Docs/NEWS match behaviour

The NEWS bullets list exactly the functions and behaviours changed. The only
wording issue is the "as `stats::t.test()` does" claim (L2). The roxygen and
Rd text needed no change: the parameters are documented as "confidence level"
and "hypothesised standard deviation", the new checks enforce what those terms
mean, and `df_satterthwaite` is documented as "degrees of freedom for the
Satterthwaite method".

### (6) `devtools::test()` on HEAD

`files: 20, blocks: 197, failed: 0, errors: 0, skipped: 0, warnings: 0,
expectations passed: 1141`. This matches the builder's report. `git status`
was clean after the run. I did not re-run R CMD check; it is outside this
audit's checklist. The builder reports `Status: OK` with the incoming check
disabled, and only the expected "Insufficient package version" warning
without it.

## Commands run

```text
Get-Content F:/pm/notes/2026-11-15-day/briefs/pkg-inferr-c25-brief.md
git status; git log --oneline origin/develop..HEAD; git diff origin/develop..HEAD --stat
git diff origin/develop..HEAD -- R NEWS.md
git diff origin/develop..HEAD -- tests/testthat/*.R
Select-String (grep) for confint/alpha/sd/prob and exported functions across R/, man/, vignettes/, README
git archive --format=tar -o $env:TEMP/inferr-c25-aud-base.tar origin/develop; tar -xf (into $env:TEMP/inferr-c25-aud-base)
Rscript scratch/grid.R <base> grid-base.rds;  Rscript scratch/grid.R <HEAD> grid-head.rds   # 241-call grid
Rscript scratch/compare.R                                                               # base vs HEAD diff
Rscript scratch/reprex.R <HEAD>;  Rscript scratch/reprex.R <base>                        # issue reprexes + edge cases
Rscript scratch/printdiff.R                                                             # print diffs, df vs t.test, edges
Rscript scratch/nearconst.R <HEAD>;  Rscript scratch/nearconst.R <base>                  # M1
Copy base -> $env:TEMP/inferr-c25-aud-oldcode, overlay HEAD tests/
Rscript scratch/runtests.R <HEAD>      # devtools::test(): 0 failures, 1141 passed
Rscript scratch/runtests.R <oldcode>   # new tests on old code: 40 failures / 6 errors
```

(Scratch scripts are in the auditor's artifact directory
`C:/Users/DELL/.gemini/antigravity-cli/brain/4a4cbf26-1685-4485-a884-5a1c3c110e5f/scratch/`.
All R was run with `pkgload::load_all()` from script files.)

Any wrong result left inside the domain: no
