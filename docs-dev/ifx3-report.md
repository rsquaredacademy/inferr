# inferr 0.3.3 fix round 3 — report

Job id: `ifx3`
Branch: `task/inferr-fix-033`
Worktree: `F:/R/Packages/statistics/inferr-wt/fix-033`
Date: 2026-11-01
Re-audit: `F:/pm/notes/2026-11-01-pkg-audits/ifxx2-reaudit.md`
Shared rules: `F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`
Base for round 3: `20a981b`

## Summary

Every open finding in the second re-audit is fixed: **H12, M5, M7, M8 and M9**.
Each numerical fix has a value-asserting testthat test that reproduces the
audit's exact case (plus a nearby case) and was run red before the fix. The
final source suite passes 748 assertions; the clean installed suite passes 837
with 0 failures. The clean `--as-cran` check is 0 errors, 0 warnings and the one
expected maintainer-change NOTE.

The re-audit's question is answered: **no known wrong result remains**, and no
valid input found by the re-audit is rejected.

## Findings

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| **H12** runs-test upper-tail cancellation returns 0 for a representable p-value | `tests/testthat/test-runs.R:120` "runs test keeps a representable upper-tail p-value (H12)" | `R/ifr-runs-test.R:146` (`2 * pnorm(abs(test_stat), lower.tail = FALSE)`) | Re-audit case `c(rep(-1, 100), rep(1, 100))`, `threshold = 0`: `n = 200`, `runs = 2`, `mean = 101`, `var = 49.74874371859296`, `z = -14.0360250783475`, `p = 9.382981388157838e-45`, exactly `2 * pnorm(abs(z), lower.tail = FALSE)` (ratio 1). The nearby `c(rep(-1, 10), rep(1, 10))` case also asserts the value, ratio and ordinary magnitude. Both fail before the fix (`p = 0`; ratio 0). | Fixed |
| **M8** one-sample t constant-data guard depends on units | `tests/testthat/test-ttest.R:74` "one-sample t is scale invariant and rejects relative degeneracy (M8)" | `R/ifr-os-t-test.R:146-158` (positive finite SD and SE; `stderr < 10 * .Machine$double.eps * abs(Mean)`, matching `stats:::t.test.default`) | `c(1, 2, 4, 8) * 1e-9` now returns `t = 2.423`, `p = 0.09393995513578184`, exactly `t.test(x)` and exactly the unscaled result; it previously errored "data are essentially constant". `1e8 + c(-1e-7, 0, 1e-7, 2e-7)` now errors "data are essentially constant" like `t.test()`, instead of returning `t = 1549812827816400`, `p = 5.92e-46`. The existing constant and two-observation tests still pass. | Fixed |
| **M9** Cochran Q rejects a homogeneous binary factor column | `tests/testthat/test-cochran.R:78` "declared factor levels survive homogeneous columns (M9)" | `R/ifr-cochran-q-test.R:102-108` (validate `nlevels(x) == 2` without `droplevels()`, preserving the declared success mapping) | Re-audit case: numeric `d` and the factor copy declared at levels `c(0, 1)` both return `n = 4`, `df = 2`, `q = 2`, `p = 0.3679`; the logical copy agrees. All-zero, all-one and complete-case-homogeneous factor columns agree with the 0/1 and logical encodings; a genuinely three-level factor still errors. The factor case errors before the fix. | Fixed |
| **M5** remaining degenerate designs return `NaN` | `test-runs.R:95` (two-point runs error) and `test-cochran.R:141` "Cochran Q rejects designs without within-subject discordance (M5)" | `R/ifr-runs-test.R:140-143` (positive finite runs variance required); `R/ifr-cochran-q-test.R:160-166` (positive finite Cochran denominator required) | `ifr_runs_test(data.frame(v = c(1, 2)), v)` now errors "the variance of the number of runs is zero" instead of `z = NaN`, `p = NaN`. A perfectly concordant three-column design now errors "no within-subject discordance" instead of `q = NaN`, `pvalue = NaN`, for both 0/1 and factor encodings. A nearby single-discordant design still returns its hand-computed `Q` and `pchisq()` p-value. Both cases return `NaN` before the fix. | Fixed |
| **M7** uncorrected McNemar tiny-p test used the corrected reference; remaining coverage gaps | `test-mcnemar.R:73-97` (raw helper against `mcnemar.test(m, correct = FALSE)`, corrected reference kept separately, ratio/log assertions, explicit check that the references differ); `test-deprecated.R:13` (all 19 aliases); `.github/workflows/R-CMD-check.yaml:56` (source documentation tests) | Test and CI only (`tests/testthat/test-mcnemar.R:76,91-95`; `tests/testthat/test-deprecated.R:13-39`; workflow step) | For `matrix(c(10, 100, 0, 10), 2)`, the uncorrected helper equals `mcnemar.test(m, correct = FALSE)$p.value` with ratio and log ratio 1 (the corrected reference is `4.162750438986416e-23`, ratio 0.366, so the two are distinguishable); the nearby `matrix(c(20, 30, 2, 20), 2)` case does the same. All nineteen deprecated aliases now warn in the suite (previously five). The source-tree documentation assertions (previously skipped by installed checks) now run in CI on Linux release. | Fixed |

## Deferred

- No re-audit finding is deferred.
- Installed-check documentation assertions still skip by design because their
  source-relative files are not in the installed tarball; the new CI step runs
  them from the source tree instead.
- No win-builder, R-hub, macOS, reverse-dependency or manual-PDF run was
  performed (same scope as earlier rounds).
- `docs-dev/ifxx-reaudit.md` and `docs-dev/ifxx2-reaudit.md` remain untracked
  auditor reports. `docs-dev/` is excluded from the build; neither is a change
  from this round.

## Checks

Source suite (`testthat::test_local('.')`):

```text
failed=0 skipped=0 error=0 warning=0 passed=748
```

Clean build and check from `git archive HEAD` in
`C:/Users/DELL/AppData/Local/Temp/opencode/ifx3-check`:

```text
R CMD build --no-manual src
R CMD check --as-cran --no-manual inferr_0.3.3.tar.gz
```

Final status: **0 errors, 0 warnings, 1 NOTE**. The NOTE is the expected CRAN
incoming-feasibility maintainer change retained from round 1 (M1):

```text
* checking CRAN incoming feasibility ... [31s] NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

Examples, vignette rebuilding and installed tests pass. Installed tests:

```text
[ FAIL 0 | WARN 0 | SKIP 40 | PASS 837 ]
```

The 40 skips are the 33 CRAN snapshot skips plus the seven source-file
documentation guards that the new CI step covers. R 4.6.1 (2026-06-24 ucrt),
Windows 11 x64, Rtools45/GCC 14.3.0.

## Commands run that changed something

- `Rscript -e "testthat::test_local('.', filter = ...)"` — ran each new test
  first to see it fail, then to confirm the fix; final full-source run above.
- `Rscript -e "testthat::test_local('.', filter = 'docs')"` — confirmed the
  source documentation tests that the new CI step runs.
- `git archive --format=tar --output=<tmp>/head.tar HEAD`, `tar --force-local
  -xf ...`, `R CMD build --no-manual src`, `R CMD check --as-cran
  --no-manual inferr_0.3.3.tar.gz` — clean build/check above.
- `rm -rf .ifxx2-tmp` — deleted the untracked round-two audit scratch directory
  (scripts, base/HEAD archives, tarball, installed check tree).
- `git checkout -- tests/testthat/_snaps` — removed line-ending-only churn left
  by running the snapshot tests.

## Not done

- Nothing was pushed; no GitHub/`gh` operation and no branch switch.
- No new dependency and no API change.
- No other statistical defect was found while re-checking the audit cases.

## Commits

```text
7fd0282 fix: keep representable runs-test upper tail (H12) (ifx3)
70d7997 fix: make the one-sample t constant-data check scale aware (M8) (ifx3)
9a64de9 fix: keep declared binary factor levels in Cochran Q (M9) (ifx3)
393ea1b fix: reject degenerate runs and Cochran statistics (M5) (ifx3)
6554c84 test: use the matching McNemar reference and cover all aliases (M7) (ifx3)
776a0fa docs: note the round-three user-visible fixes in NEWS (ifx3)
```
