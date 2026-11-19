# pkg-inferr-c25-rx: independent re-check

Date: 2026-11-19. Auditor: Codex. Branch: `task/inferr-c25`.
Audited HEAD: `03ef81a1faa81d47be9d95e5417373d1c304cce8`.
Pre-follow-up baseline: `2f1bdc4885f21c4195571abc186726f88e02347e`.

Read `F:/pm/notes/package-audit-stop-rule.md`, the original audit and the
follow-up report. Verified the follow-up diff and reran the evidence rather
than accepting its claims. Package code and tests were not edited. All R
commands used script files, not `Rscript -e`. Scratch archives, scripts,
serialized results and logs are outside the repository at
`C:/Users/DELL/AppData/Local/Temp/inferr-c25-rx-20261119/`.

## Verdict

The four claimed fixes address their original findings. The new ANOVA guard
introduces a Medium regression: it rejects a well-conditioned response solely
because its units make the values small. This blocks merging under the stop
rule's requirement that ordinary valid inputs remain accepted. No High finding
was found in this focused re-check.

The full development test suite passes. The completed package check has
0 errors, 0 warnings and 1 note about the worktree's `.git` file. An initial
check attempt failed because of the inherited Windows locale; both attempts
are reported below.

## Each original Medium/Low

| Finding | Status | Independently observed evidence |
|---|---|---|
| M1: one-ulp ANOVA spread produces misleading results | **FIXED for the original reprex** | Parent: F = 6, p = 0.070484, R-squared = 0.673, plus an essentially-perfect-fit warning. HEAD: `response 'x' has no variation; a one-way ANOVA is undefined`. The replacement guard causes RX1 below. |
| L1: McNemar negative-count message omits argument | **FIXED** | Negative and NA matrices now report `data must contain non-negative counts`; the parent reports `counts must be non-negative`. A negative `as.table()` input also gets the new message. |
| L2: inaccurate `stats::t.test()` printing comparison | **FIXED** | Both NEWS and the helper comment now say two decimals without that comparison. Stored df = 18.33225163840046 and formatted df = `18.33` on factor-coded mtcars/am/mpg. |
| L5: joined lines in H8 ANOVA test | **FIXED** | The `test_that(..., {` line and `d <- data.frame(` are separate at test-anova.R:82-83. Test content is unchanged and the test passes. |
| L3: pooled proportion z undefined at common boundaries | **STILL OPEN**, explicitly deferred by follow-up | `(30, 25, 0, 0)` and `(30, 25, 1, 1)` both return `z = NaN`, `sig = NaN`, without warning, on parent and HEAD. |
| L4: zero expected GOF cell | **STILL OPEN**, explicitly deferred by follow-up | `ifr_chisq_gof_test(hsb, race, c(0, 20, 40, 140))` returns `chisquare = Inf`, `pvalue = 0`, on parent and HEAD. |

The follow-up claimed fixes only for M1/L1/L2/L5. L3/L4 are unchanged
pre-existing findings, not newly claimed fixes or regressions.

Text verification output:

```text
NEWS.md:15-16:
- `ifr_ts_ind_ttest()` now stores the Satterthwaite degrees of freedom
  unrounded and prints them with two decimals.

R/ifr-utils.R:130-131:
# Satterthwaite degrees of freedom are printed with two decimals, while the
# stored value remains unrounded

tests/testthat/test-anova.R:82-83:
test_that("ANOVA works when columns are named x or y (H8)", {
  d <- data.frame(

Printed Satterthwaite row:
mpg        Satterthwaite     Unequal     18.33    3.7671     0.0014
```

## RX1: new Medium ANOVA regression; scale-dependent false rejection

The guard at `R/ifr-anova.R:62-63` computes population SD, using divisor n,
and compares it with `1e-12 * max(1, mean(abs(response)))`. The `max(1, ...)`
introduces an absolute tolerance for responses whose mean magnitude is below
one. The follow-up's description of the guard as relative to the response's
own magnitude is therefore incomplete.

Reproducer, run on both archived revisions:

```r
d <- data.frame(
  x = (1:6) * 1e-13,
  g = factor(rep(c("a", "b"), each = 3))
)
ifr_oneway_anova(d, x, g)
anova(lm(x ~ g, data = d))
```

```text
population SD       = 1.707825127659933e-13
mean(abs(x))        = 3.5e-13
HEAD threshold      = 1e-12
pure relative limit = 3.5e-25

HEAD:
ERROR: response 'x' has no variation; a one-way ANOVA is undefined

Parent:
fstat = 13.5; pval = 0.02131164; r2 = 0.7714

stats reference:
F = 13.500000000000005
p = 0.021311641128756702
R-squared = 0.77142857142857146
```

HEAD accepts the identical design at scales 1, 1e-8 and 1e-12, giving
F = 13.5 and the same p-value, but rejects it at scale 1e-13. This is real
spread, not cancellation around a large offset: SD / mean(abs(x)) is about
0.488. The numbers are also far from floating-point underflow.

The ANOVA help documents a numeric response and gives no minimum magnitude
or spread. NEWS says the error is for a response with no variation; it does
not document this absolute restriction. Accordingly, this is an in-domain
regression in a change of measurement units, not an excluded ill-conditioned
input to send to the exotic-input backlog. The ordinary grid below remains
green, but the broader claim that valid inputs do not regress is false.

Recommend **pure relative scaling**, removing the `max(1, ...)` floor:

```r
!is.finite(response_sd) ||
  response_sd <= 1e-12 * mean(abs(response))
```

This is a recommendation only; no implementation was changed. Evaluating
the predicates on HEAD's inputs gives:

```text
input                 current rejects   pure relative rejects
(1:6) * 1e-13         TRUE              FALSE
original M1 one ulp   TRUE              TRUE
rep(0, 6)            TRUE              TRUE
rep(0.3, 6)          TRUE              TRUE
```

A future fix should keep the M1 rejection test and add the small-scale
reproducer as an acceptance/reference test, including invariance under
multiplication by a nonzero scale.

## New/updated tests: fail with fixes reverted; pass on HEAD

Created `head/` from `git archive HEAD` and `reverted/` from that same archive.
Only in `reverted/`, replaced `R/ifr-anova.R` and `R/ifr-mcnemar-test.R`
with their `HEAD^` versions. All tests remained HEAD's tests. No stash,
checkout, branch switch or source edit was performed in the worktree.

Verified file identity with `git hash-object`:

```text
ANOVA: HEAD and scratch head
036f869e2480ade17a3b9c736eba91f4578f6541
ANOVA: parent and scratch reverted
1f01a023fa319190cafc05d35f6a2c1b855f69ce
ANOVA tests: HEAD and scratch reverted
7a6a27995c4fd59b5d4185224a8e0b29464d6399
```

Ran `devtools::test(pkg = ..., filter = "anova|mcnemar", reporter = "summary",
stop_on_failure = FALSE)` from `targeted.R` on each scratch tree.

```text
REVERTED:
Warning test-anova.R:149:3:
  essentially perfect fit: summary may be unreliable
Failure test-anova.R:149:3:
  Expected `ifr_oneway_anova(d, x, g)` to throw a error.
Error test-mcnemar.R:149:3:
  Error: counts must be non-negative

Structured totals: 2 files, 18 blocks, 1 failed expectation, 1 errored block,
                  1 warning, 0 skips, 101 passed expectations.

HEAD:
Structured totals: 2 files, 18 blocks, 0 failures, 0 errors, 0 warnings,
                  0 skips, 105 passed expectations.
```

Specifically, the new M1 block has six passing expectations on HEAD versus
five passes, one failure and one warning when reverted. The updated L1 block
has three passes on HEAD; when reverted its first mismatched error aborts
the block. Separate probes confirmed both negative and NA message changes.
L2 and L5 are comment/NEWS/formatting corrections; there is no new behavioral
test to fail for those edits.

## Ordinary-input regression grid

Ran `grid.R` in separate Rscript processes on complete archives of HEAD and
HEAD^, then compared every result object, printed output, warning and error
using `identical()`. The final grid contains 150 successful calls:

- 77 ANOVA calls: seeds 103 through 108 from `100 + seed + ng`, seed indices
  1:4 and 2/3/4 groups, six observations per group; five scales (1e-8, 1e-4,
  1, 100, 1e6); twelve additional 1e9-offset designs; mtcars and hsb; NA
  deletion, character/numeric groups, and constant-within/different-between
  groups.
- 37 McNemar calls: six count configurations, three integer multipliers,
  matrix/table forms, and the documented dataframe example. Includes zero
  discordances, zero cells, balanced discordances and asymmetric counts.
- 32 independent t tests exercising the unchanged df formatter: factor-coded
  mtcars/am/mpg and hsb/female/write, four alternatives and four confidence
  levels (0.5, 0.9, 0.95, 0.99).
- Four direct `format_satt_df()` calls, including an integral df.

Reference checks cover ANOVA F/p and McNemar uncorrected statistic/p and
corrected p. Zero discordances use the package's defined statistic 0, p 1
rather than base R's undefined 0/0 calculation.

```text
GRID calls=150 errors=0 reference checks=228 reference failures=0
                 anova format mcnemar ttest
calls               77      4      37    32
value differences:    0
print differences:    0
warnings differences: 0
error differences:    0
Parent reference failures: 0 HEAD reference failures: 0
```

The one warning in each tree is the same pre-existing essentially-perfect-fit
warning for the constant-within-group ANOVA. No ordinary-grid regression was
found. RX1 was tested separately and is the demonstrated regression.

## Full tests and package check

Environment: Windows 11 x64, R 4.6.1 (2026-06-24 ucrt), devtools 2.5.2,
testthat 3.3.2, GCC 14.3.0. Both full commands ran against the actual worktree
at the audited HEAD, with build/check artifacts directed outside the repo.

`full-tests.R` ran the entire `devtools::test()` suite, without a filter.
Totals below are independently summed from `as.data.frame(res)`; this
reproduces the follow-up's structured 1,147-pass total.

```text
TOTAL files=20 blocks=198 failed=0 errors=0 skipped=0 warnings=0 passed=1147
```

The first `devtools::check(document = FALSE, args = "--no-manual", ...)`
attempt stopped at DESCRIPTION metadata after child R processes emitted
`Setting LC_COLLATE=C.UTF-8 failed` and corresponding CTYPE/MONETARY/TIME
startup warnings. Its reported totals were **1 error, 1 warning, 1 note**;
it did not complete the remaining checks.

Reran the full command after setting `LANG=C`, `LC_ALL=C` and
`Sys.setlocale("LC_ALL", "C")` inside `full-check-locale-C.R`:

```r
res <- devtools::check(
  document = FALSE, args = "--no-manual",
  check_dir = file.path(args[2], "check-locale-C"), error_on = "never"
)
```

`error_on = "never"` only permits collecting the result; no check failure
was suppressed or converted to a pass. The run completed installation,
compiled-code checks, examples, tests, vignettes and vignette rebuilding:

```text
* checking tests ...
  Running 'testthat.R' [14s]
 [14s] OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE
CHECK TOTAL errors=0 warnings=0 notes=1
```

The sole note is `Found the following hidden files and directories: .git`.
Here `.git` is the worktree pointer file, which was included in the build;
`.Rbuildignore` was not changed by the follow-up. This note is separate from
the runtime regression. The installed-package test log reports:

```text
[ FAIL 0 | WARN 0 | SKIP 7 | PASS 1269 ]
```

Those seven skips are source-file-existence guards in test-docs.R: source
Rd/README/DESCRIPTION/build-ignore files are unavailable
at their source-tree paths during installed-package testing. The full
source-tree `devtools::test()` run above skips none. The installed console
tally and the source suite's structured expectation total are reported
separately, without treating them as interchangeable counts.

Devtools set `_R_CHECK_CRAN_INCOMING_=FALSE`, incoming remote FALSE,
`_R_CHECK_FORCE_SUGGESTS_=FALSE`, and `NOT_CRAN=true`; these are recorded
in both check logs. This is the requested devtools check, not a claim of a
successful remote CRAN incoming check.

## Evidence and repository scope

Scratch scripts: `full-tests.R`, `full-check.R`, `full-check-locale-C.R`,
`targeted.R`, `probes.R`, `numeric.R`, `evidence.R`, `grid.R`, `compare.R`.
Corresponding logs and RDS results are in the scratch root named above.
Completed check log:
`check-locale-C/inferr.Rcheck/00check.log`; installed test output:
`check-locale-C/inferr.Rcheck/tests/testthat.Rout`.

The worktree was clean at the start and after all test/check runs. Only this
requested audit report is added for the audit commit. No package code, tests,
help, NEWS, dependency or version change; no push, gh/GitHub use or branch
switch. Stop-rule result: M1's original case and L1/L2/L5 are fixed, but
RX1 is a newly introduced valid-input regression. Recommend fixing the
relative scaling before merging.

Ready to merge into develop: no
