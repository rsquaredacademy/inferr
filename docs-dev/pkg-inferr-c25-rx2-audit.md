# pkg-inferr-c25-rx2: re-check 2 of inferr follow-up 2

Date: 2026-11-19. Auditor: DeepSeek (independent re-check).
Branch: `task/inferr-c25`.
Audited HEAD: `863613ada559260757100692e153d3d89e2ff2c1` (fu2).
Parent commit: `490dd6605cbacf22cb98a69ec8e8d8da10ffd7f2` (audit commit; its code/tests are
`03ef81a` plus only `docs-dev/pkg-inferr-c25-rx-audit.md`).

Read `F:/pm/notes/package-audit-stop-rule.md`,
`docs-dev/pkg-inferr-c25-rx-audit.md` and
`docs-dev/pkg-inferr-c25-fu2-report.md`. The fu2 report was treated as a claim
to verify, not as evidence. Package code, tests, help, NEWS and data were not
edited. Package test, functional-grid and check runs used script files.
Archives, scripts, logs and RDS results are outside the repository at
`C:/Users/DELL/AppData/Local/Temp/inferr-c25-rx2-20261119/`.

## Verdict

All three requested runtime fixes behave as claimed inside the documented
domain, the ordinary-input grid is unchanged from the parent, and the full
source suite and package check pass. The 1,209 / 1,269 / 1,346 test-count
difference is a reporting-context artifact, not lost tests: the default
reporter counts 1,346 expectations on HEAD, the same suite's
`as.data.frame()` "passed" column undercounts by 137 because of repeated test
descriptions, and the installed check skips the seven `test-docs.R` blocks
(15 expectations), giving 1,331.

One edge case remains, outside the requested ordinary grid: a zero expected
cell with zero observed count returns `NaN` instead of an error. Base R
`chisq.test()` also returns `NaN` for that input, so this is recorded as a
Low/backlog item, not a release blocker. No High or Medium finding was found
in this focused re-check.

## 1. ANOVA guard

`(1:6) * 1e-13` with two groups is accepted and agrees with the unscaled
response and with `anova(lm())` to display precision:

```text
scale      package F   package p              base F              base p
1          13.5        0.021311641128756727   13.5                0.021311641128756702
1e-13      13.5        0.021311641128756741   13.500000000000005  0.021311641128756702
1e-8       13.5        0.021311641128756741   13.5                0.021311641128756727
1e6        13.5        0.021311641128756727   13.5                0.021311641128756702
-1e-13     13.5        0.021311641128756741   13.500000000000005  0.021311641128756702
```

F is identical at all scales (stored to three decimals). The p-values differ
only in the last bits: the largest package-vs-base difference above is
`3.9e-17`, and the largest scaled-vs-unscaled package difference is
`1.4e-17`; all print as `0.0213` and the tests' `1e-12` tolerance is met. The
parent rejects the `1e-13` and `-1e-13` cases with the constant-response
error, so this is the requested RX1 fix, not an accident of the test data.

Exact constants and the M1 one-ulp case are rejected on HEAD with
`response 'x' has no variation; a one-way ANOVA is undefined`:

```text
x = rep(0, 6)                          error
x = rep(0.3, 6)                        error
x = rep(1e-13, 6)                      error
x = c(0.3, 0.1+0.2, ...) (M1)          error
```

Twenty ordinary datasets were run through both archived revisions and
compared with `identical()` on the full result object, printed output,
warnings and error state. The grid covers mtcars (five responses/groupings),
hsb (five), five seeded designs with two to four groups and unequal sizes, a
constant-within/different-between design, numeric and character group
codings, NA row deletion, and a `1e9` offset design. Result: **20/20
identical**, including the constant-within-group case that legitimately
returns `F = Inf, p = 0` and the same pre-existing perfect-fit warning on
both revisions.

## 2. Proportion z boundary errors

Both-zero and both-one samples now error for all four alternatives and all
three public entry points:

```text
ifr_ts_prop_calc(30, 25, 0, 0)   -> p1 and p2: both samples have proportion 0; the pooled z test is undefined
ifr_ts_prop_calc(30, 25, 1, 1)   -> p1 and p2: both samples have proportion 1; the pooled z test is undefined
ifr_ts_prop_test(d, a, b)        -> same messages (both-zero and both-one)
ifr_ts_prop_group(g, outcome, grp) -> same messages (both-zero and both-one)
```

Each was checked under `alternative = "both"`, `"less"`, `"greater"` and
`"all"` (24 errors, all with the exact message). The parent returns silent
`NaN`/`NaN` (and three `NaN`s for `"all"`), so the new behaviour is a real
change at exactly the undefined boundary.

Ordinary and boundary-valid inputs are unchanged: 11 count/proportion cases
(including `(0, 1)`, `(1, 0)`, `(0, 0.4)`, `(1, 0.4)`, equal interior
proportions and the ordinary `0.3/0.48` calculator case) under four alternatives
through all three entry points, 44 result groups, **44/44 `identical()` to
the parent**. Against `prop.test(correct = FALSE)` there were **99/99
matching** `z`/p comparisons for `both`, `less` and `greater`, including the
one-boundary cases; no mismatch remained after constructing the grouped data
with the same per-group counts.

## 3. Goodness of fit

A zero expected cell with a positive observed count errors before either
calculation, names the cell and category, and works for counts, proportions
and both `correct` settings:

```text
ifr_chisq_gof_test(hsb, race, c(0, 20, 40, 140))
  y[1] for category '1' of 'race' is zero but its observed count is positive;
  the chi-square goodness-of-fit test is undefined

ifr_chisq_gof_test(hsb, race, c(0, 0.1, 0.2, 0.7))
  same message (proportions scaled by n)

renamed factor, cell 2
  y[2] for category 'beta' of 'kind' is zero but its observed count is positive; ...
```

Ordinary GOF inputs are unchanged: six calls (race counts corrected and
uncorrected, mtcars cyl/gear, hsb female corrected, hsb ses) are **6/6
`identical()` to the parent**. For the four uncorrected ordinary calls the
statistic and p-value equal `chisq.test(obs, p = expected/sum(expected),
correct = FALSE)` after the package's documented four-decimal rounding
(e.g. race 5.0286/0.1697, cyl 1.3333/0.5134, gear 6.9833/0.0305, ses
23.8236/0). The `correct = TRUE` calls use the package's documented
Yates-style correction on every cell; base R's `chisq.test(correct = TRUE)`
does not correct a GOF test, so those calls have no direct base-R
counterpart and are judged by the package formula and the unchanged
parent output, not by that comparison.

Edge observation, not part of the requested fix: a declared but unobserved
level with expected zero (e.g. observed `c(0, 10, 10, 10)`, expected
`c(0, 0.25, 0.25, 0.5)`) still returns `chisquare = NaN`, `pvalue = NaN`
rather than an error. Base R `chisq.test()` returns the same `NaN` for this
input, so the reference comparison is not violated, but the output is not
self-explanatory. This is a Low/backlog item for a future fix (either define
the zero/zero cell as a zero contribution or reject it with a named error).

## 4. Test count: 1,209 vs 1,269 vs 1,346

Archives were made with `git archive HEAD` and `git archive HEAD^` and each
was tested with `devtools::test(pkg = <archive>, stop_on_failure = FALSE)`
and the default reporter in a fresh non-interactive R process (no
`reporter` argument). Under Rscript the default is testthat's progress
reporter, whose final line is the console tally.

```text
HEAD 863613a (source archive):  [ FAIL 0 | WARN 0 | SKIP 0 | PASS 1346 ]
PARENT 490dd66 (source archive):[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1284 ]
```

The default reporter executed 20 files and 201 test blocks on HEAD (198 on
the parent), with no failures, warnings or skips. The increase is exactly the
62 expectations added by the fixes: 23 in the ANOVA block (5 scales x 4 plus
3 constants), 5 in the GOF block, and 34 in the proportion block (2
boundaries x 4 alternatives x 3 functions plus 5 valid cases x 2).
`1284 + 62 = 1346`, so nothing was lost.

The fixer's 1,209 is the `passed` column of `as.data.frame(result)`, not the
console expectation count. testthat 3.3.2's `ListReporter` reuses the open
block when consecutive `test_that()` blocks have the same description
(`start_test()` only resets when the context or test name changes), so
expectations from later same-named blocks are not attached to a fresh block
in the returned results object and are dropped from it, while the progress
reporter still counts them. On HEAD the structured table
sums to 1,209 while the progress reporter counts 1,346; the same suite on
the parent gives 1,147 structured versus 1,284 console, i.e. the identical
137-expectation undercount on both revisions. The standalone
`test-prop-test.R` reproduces it directly: 9 blocks, four of them named
"output from ifr_os_prop_test matches expected result", 55 console
expectations versus 51 in the structured table, with the third and fourth
occurrences of that name returning zero recorded results.

The 1,269 is the earlier re-check's *installed check* console tally for
`03ef81a`, which is the same suite as the parent commit (the parent adds only
the audit report). The reconciliation is:

```text
1269  earlier installed-check console total (03ef81a)
 +62  expectations added by fu2
=1331  installed-check console total on HEAD   (observed in this check)
 +15  expectations in the 7 test-docs.R blocks skipped when installed
=1346  source-tree console total on HEAD        (observed above)

1346  source-tree console total on HEAD
-137  ListReporter duplicate-test-name undercount
=1209  "passed" column reported by the fixer
```

The 15 skipped expectations are the seven `test-docs.R` blocks (2+2+2+4+2+2+1)
whose source-tree `man/`, `README*`, `DESCRIPTION` and `.Rbuildignore` paths
are absent under `R CMD check`; they are the only skips. These are different
counters (source progress reporter vs installed `test_check`), not lost
tests. `NOT_CRAN` was inherited unset in the source runs, but
`devtools::test()` sets `NOT_CRAN=true` for both of them (confirmed in
`devtools:::r_env_vars()`), as it did in the check. That setting changes
nothing in this suite because there is no `skip_on_cran()` and the only skips
are the file-existence guards.

## 5. Full package check

`devtools::check(document = FALSE, args = "--no-manual")` was run against the
worktree at HEAD, with `check_dir` outside the repository and
`error_on = "never"` only so the result object could be collected. Devtools
used `--no-manual --as-cran`, with
`_R_CHECK_CRAN_INCOMING_=FALSE`,
`_R_CHECK_CRAN_INCOMING_REMOTE_=FALSE`,
`_R_CHECK_FORCE_SUGGESTS_=FALSE` and `NOT_CRAN=true`. No failure was
suppressed or reclassified.

```text
Status: 1 NOTE
R CMD check results: 0 errors | 0 warnings | 1 note
Note: Found the following hidden files and directories: .git
Installed tests: [ FAIL 0 | WARN 0 | SKIP 7 | PASS 1331 ]
```

The check completed installation, compiled-code checks, examples, tests,
vignettes and vignette rebuilding. The sole note is the worktree `.git`
pointer file already documented by the earlier re-check; `.Rbuildignore` was
not changed. Installed test log skips are exactly the seven `test-docs.R`
blocks listed above. Environment: Windows 11 x64, R 4.6.1 (2026-06-24 ucrt),
devtools 2.5.2, testthat 3.3.2, GCC 14.3.0; `LC_ALL=C` set in the scripts.

## Evidence and repository scope

Scratch root: `C:/Users/DELL/AppData/Local/Temp/inferr-c25-rx2-20261119/`.
Evidence includes `run-tests.R`, `tests-head.log`, `tests-parent.log`,
`tests-head.rds`, `tests-parent.rds`, `instrument.R`,
`reporter-rows-head.rds`, `prop-blocks.R/.log`, `functional.R`,
`functional-head.rds`, `functional-parent.rds`, `analyze.R/.log`,
`base-probe.R/.log`, `full-check.R/.log`, `check-result.rds`.
Check log: `check/inferr.Rcheck/00check.log`; installed test output:
`check/inferr.Rcheck/tests/testthat.Rout`.

The worktree was clean before and after the runs. No package code, test,
help, NEWS, data or version change; no stash, checkout, branch switch, push,
`gh` or GitHub use. Only this report is added for the audit commit.

Ready to merge into develop: yes
