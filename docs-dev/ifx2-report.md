# inferr 0.3.3 fix round 2 — report

Job id: `ifx2`
Branch: `task/inferr-fix-033` (same branch as round 1)
Worktree: `F:/R/Packages/statistics/inferr-wt/fix-033`
Date: 2026-11-01
Re-audit: `F:/pm/notes/2026-11-01-pkg-audits/ifxx-reaudit.md`
Shared rules: `F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`
Audited base for round 2: `d77dc8c`

## Summary

Every finding that the re-audit marked still wrong, partly fixed or new is
fixed: **H11, M4, M5, M6, M7, L1, L2 and L4**. Each numerical fix has a
value-asserting testthat test that reproduces the re-audit's exact case (plus a
nearby case) and was run red before the fix. The final source suite passes 680
assertions; the clean installed suite passes 769 with 0 failures.

No exported function name or argument changed. Behaviour changes are the
intended consequences of the fixes:

- `ifr_binom_calc()`/`ifr_binom_test()`, `ifr_os_prop_test()`, `ifr_os_t_test()`,
  `ifr_runs_test()` and `ifr_cochran_qtest()` now raise clear errors for invalid
  or degenerate input instead of returning plausible-looking values.
- The runs and Cochran tests now remove incomplete observations consistently.
- A grouping factor with unused levels is accepted by `ifr_ts_ind_ttest()` after
  dropping the unused levels.

The re-audit's question is answered: **no known wrong numerical result remains.**

## Findings

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| **H11** McNemar loses representable upper-tail probabilities | `tests/testthat/test-mcnemar.R` "McNemar upper-tail p-values are not lost to cancellation (H11)" | `R/ifr-mcnemar-test.R:142,163` (`pchisq(..., lower.tail = FALSE)` in both helpers) | Exact matrix `matrix(c(10, 100, 0, 10), 2)`: `cpvalue` is now `4.16275043898642e-23` and equals `mcnemar.test(m, correct = TRUE)$p.value` (ratio 1, tolerance 1e-10); the uncorrected helper equals `mcnemar.test(m)$p.value`; nearby `matrix(c(20, 30, 2, 20), 2)` also matches with a positive tail | Fixed |
| **M4** accepted encodings fail in homogeneous groups | `tests/testthat/test-two-prop-test.R` "ifr_ts_prop_group handles homogeneous groups for accepted encodings (M4)" | `R/ifr-ts-prop-test.R:150-170` (one shared success level, counts by level) | Re-audit character case now gives `n = c(2, 2)`, `phat = c(0, 1)`, `z = -2`, `p = 0.046`, matching `prop.test(c(0, 2), c(2, 2), correct = FALSE)`; all-success/failure nearby case gives `z = 2`, `p = 0.046`; logical and 0/1 encodings agree; factor level order is preserved | Fixed |
| **M5** invalid/degenerate inputs returned wrong values | `test-binom.R` "rejects fractional and out-of-range counts (M5)" and "requires exactly two factor levels (M5)"; `test-prop-test.R` "rejects invalid proportions and sample sizes (M5)"; `test-ttest.R` "rejects degenerate samples and invalid alpha (M5)"; `test-runs.R` "handles ties, missing values and degenerate samples (M5)"; `test-cochran.R` "handles missing values consistently across encodings (M5)" | `R/ifr-binom-test.R:58-76,118-142`; `R/ifr-os-prop-test.R:55-84`; `R/ifr-os-t-test.R:79-88,130-150`; `R/ifr-runs-test.R:72-138`; `R/ifr-cochran-q-test.R:40-48,98-108,136` | `ifr_binom_calc(4, 1.5)` and counts outside `0..n` error; `ifr_os_prop_test(100, phat = 2)` errors instead of returning `obs = c(-100, 200)`; `ifr_os_t_test(rep(1, 4))` errors "essentially constant" and a singleton errors clearly instead of a `qt()` warning plus indirect error; all-ties `drop = TRUE` errors clearly instead of the native NULL conversion error; `c(1, 2, NA, 4)` gives `n = 3`, `n_below = 2`, `n_above = 1`, `z = -0.7071`, `p = 0.4795` by hand; logical-NA and integer-NA Cochran input both give `n = 2`, `q = 1`, `p = 0.6065`. Boundary binomial and two-observation t cases still work. Cochran's p-value also uses the upper tail (line 136). | Fixed |
| **M6** unused grouping levels reject independent t data | `tests/testthat/test-indttest.R` "independent t test drops unused grouping levels (M6)" | `R/ifr-ts-ind-ttest.R:92-101` (`droplevels()` after complete cases; retained group count must be 2) | Re-audit case now returns `levels = c("a", "b")`, `n = 6`, `t = 0`, `p = 1`, matching `t.test(val ~ grp, var.equal = TRUE)`; a one-group design hidden behind a declared second level now errors | Fixed |
| **M7** coverage gaps and weak tiny-p protection | `test-anova.R` H10 test now asserts `p < 1e-18`, a unit ratio and equal logs; `test-docs.R` "the exam data source documentation is stable (M3)" (accurate name); new `test-deprecated.R` covers deprecated aliases and the launcher's soft-deprecation path with a mocked suggest check | `tests/testthat/test-anova.R:106-119`; `tests/testthat/test-docs.R:39`; `tests/testthat/test-deprecated.R` | The H10 ratio and log assertions fail if the magnitude changes while an absolute tolerance near 1e-21 would not. The source-tree suite (used here) runs the documentation assertions that the installed check skips by design. The launcher test emits the deprecation message and stops at a mocked `check_suggests()`, without launching Shiny. | Fixed |
| **L1** README and rendered site stale | regenerated output (no new test) | `README.md`; `docs/` (pkgdown 2.2.1 rebuild) | README shows `Between Groups ... 4.310163e-09` (was `0`), the Levene/Brown–Forsythe rows now match the code, and the chi-square probabilities match L4. `docs/index.html` reports `0.3.3`, and `docs/news/index.md` contains the 0.3.3 news. | Fixed |
| **L2** remaining semantic documentation errors | `tests/testthat/test-docs.R` "binomial and runs documentation use correct names and meanings (L2)" | `R/ifr-binom-test.R:28,35`; `R/ifr-runs-test.R:26`; matching `man/ifr_binom_calc.Rd:36,58`, `man/ifr_runs_test.Rd:38` | Roxygen and Rd agree: `obs_p` is the observed probability, the replacement is `ifr_binom_calc()`, and the runs threshold is the threshold used to count runs (not within-group SS). | Fixed |
| **L4** premature statistic rounding changes p-values | `test-prop-test.R` "one-sample proportion p-values use the unrounded z (L4)"; `test-chisq.R` "association p-values use the unrounded chi-square (L4)" | `R/ifr-os-prop-test.R:139-144` (raw z for probabilities, rounded z returned); `R/ifr-chisq-assoc-test.R:151-170,184-205` (raw Pearson/LR/MH statistics for p-values, rounded statistics returned) | Re-audit example now returns `0.5637, 0.2819, 0.7181`, matching `prop.test(1, 3, correct = FALSE)`; the `matrix(c(1, 1, 2, 3), 2)` association table now returns `0.8091` (not `0.8092`), matching `chisq.test(m, correct = FALSE)`. Existing hsb/schtyp probabilities moved from `0.8284/0.8282` to `0.8283/0.8281`, which `chisq.test()` confirms; the snapshot was updated. LR and MH p-values are asserted from the raw statistics. | Fixed |

## Deferred

No re-audit finding is deferred. The following are outside the finding list or
outside this job's environment:

- No win-builder, R-hub, macOS, reverse-dependency or manual-PDF run was
  performed (same scope as round 1).
- Installed-check documentation assertions still skip because their
  source-relative files are not in the installed package. The source-tree suite
  run here executes them; this is intentional, not a check failure.
- `docs-dev/ifxx-reaudit.md` remains uncommitted. It is the auditor's report, not
  a change from this fix round; `.Rbuildignore` already excludes `docs-dev/`.

## Checks

Source test suite (`testthat::test_local()`):

```text
failed skipped   error warning  passed
     0       0       0       0     680
```

Clean build and check from `git archive HEAD` (`b419c1e`) in a temporary
directory outside the source tree:

```text
R CMD build --no-manual src
R CMD check --as-cran --no-manual inferr_0.3.3.tar.gz
```

Final status: **0 errors, 0 warnings, 1 NOTE**. The NOTE is the expected CRAN
incoming-feasibility maintainer change retained from round 1 (M1):

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

Examples, tests and vignette rebuilding pass. Installed tests:
`[ FAIL 0 | WARN 0 | SKIP 40 | PASS 769 ]`; the 40 skips are 33 CRAN snapshot
skips plus seven source-file guards. R 4.6.1 (2026-06-24 ucrt), Windows 11 x64,
Rtools45/GCC 14.3.0.

## Commands that changed something

- `Rscript -e "testthat::test_local('.', reporter = 'summary')"` — used to see
  each new test fail before its fix and to confirm the final suite.
- `Rscript -e "testthat::snapshot_accept('chisq', path = 'tests/testthat')"` —
  accepted the corrected association p-values in the chisq snapshot.
- `R CMD INSTALL --library=<temp>` — installed the fixed package so the README
  and vignettes could run `library(inferr)`.
- `Rscript -e "rmarkdown::render('README.Rmd')"` — regenerated `README.md`.
- `Rscript -e "pkgdown::build_site(preview = FALSE, devel = TRUE, install = FALSE)"`
  — regenerated `docs/` with version 0.3.3 and the corrected output.
- `git archive`, `tar`, `R CMD build`, `R CMD check` — clean final check above.
- `git checkout -- tests/testthat/_snaps` — removed line-ending-only churn that
  running the snapshot tests left in the worktree.

## Not done

- Nothing was pushed; no GitHub/`gh` operation and no branch switch.
- No new dependency and no API change.
- No further statistical defects were found in the re-audit's numerical sweep
  beyond H11 and L4, and both are fixed.

## Commits

```text
439c776 fix: compute McNemar p-values from the upper tail (H11) (ifx2)
acc8b7f fix: use a shared success level in grouped proportion tests (M4) (ifx2)
2c4ba6d fix: validate invalid and degenerate inputs explicitly (M5) (ifx2)
9f0827b docs: use a two-sided threshold in runs test examples (M5) (ifx2)
b471ac8 fix: drop unused grouping levels in the independent t test (M6) (ifx2)
e71399d test: strengthen numerical and deprecation coverage (M7) (ifx2)
b419c1e test: cover the deprecated launcher path (M7) (ifx2)
159a6c8 docs: correct binomial and runs test descriptions (L2) (ifx2)
28e526d fix: compute proportion and association p-values from raw statistics (L4) (ifx2)
67919f7 docs: regenerate README and pkgdown site (L1) (ifx2)
```
