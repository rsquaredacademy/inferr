# inferr 0.3.3 fix round 4 — report

Job id: `ifx4`
Branch: `task/inferr-fix-033`
Worktree: `F:/R/Packages/statistics/inferr-wt/fix-033`
Date: 2026-11-01
Re-audit: `F:/pm/notes/2026-11-01-pkg-audits/ifxx3-reaudit.md`
Shared rules: `F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`
Base for round 4: `7a5ae00`

## Summary

The single High finding and both partly-fixed findings left open by the third
re-audit are fixed: **H13, M5 and L1**. H13 and M5 each have a value-asserting
testthat test that reproduces the audit's exact case (plus a nearby case) and
was confirmed to fail before the fix. L1 is a publication defect, fixed by
regenerating the pkgdown news/site after the final source and NEWS changes.

The third re-audit's question is answered: **no known wrong result remains**.

## Findings

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| **H13** a homogeneous 1/2-coded Cochran column is recoded as 0/1, so equivalent encodings disagree | `tests/testthat/test-cochran.R:141` "a homogeneous 1/2-coded column follows the rest of the design (H13)"; `:183` all-success/NA-homogenized 1/2 columns; `:224` mixed 0/1+1/2 rejection | `R/ifr-cochran-q-test.R:49` (recodes the whole selection at once) and `:95-159` (`cochran_recode()` resolves one numeric coding across all columns before recoding, then maps 0/1 or 1/2 consistently) | Audit case `a=c(0,0,0,0)`, `b=c(0,1,1,1)`, `c=c(0,1,1,1)`: the 0/1 frame, its `+1` 1/2 frame and the declared-factor 1/2 frame now all return `n=4`, `df=2`, `q=6`, `p=0.0498`. Hand reference `Q = (k-1)(k·Σc²−Σm²)/(k·Σm−ΣrowSums²)` on the 0/1 matrix gives `Q=6`, `pchisq(6,2,lower=F)=0.04978707`. Nearby `b=c(0,0,0,1)`, `c=c(0,0,1,1)` gives `q=3`, `p=0.2231` in all three encodings (`pchisq(3,2,lower=F)=0.2231302`). Before the fix the numeric 1/2 frame returned `q=2`, `p=0.3679` (nearby `q=4.6667`). | Fixed |
| **M5** degenerate null probabilities `prob=0`/`prob=1` still return `NaN` | `tests/testthat/test-prop-test.R:64-69` (endpoint error assertions inside the M5 rejection block) | `R/ifr-os-prop-test.R:55` (`prob <= 0 || prob >= 1`, message "prob must be strictly between 0 and 1") | `ifr_os_prop_test(100, phat = 0, prob = 0)` and `(100, phat = 1, prob = 1)` now error "prob must be strictly between 0 and 1" instead of `z=NaN`, `sig=NaN`, matching base `prop.test(0, 100, p = 0, correct = FALSE)` which errors "elements of 'p' must be in (0,1)". The existing boundary case `phat=0` with the interior null `prob=.5` still returns `z=-10`, `sig=0`. | Fixed |
| **L1** rendered news omits the round-three NEWS bullets | none (site regeneration) | `docs/news/index.md`, `docs/news/index.html`, `docs/search.json`, `docs/pkgdown.yml` (regenerated with `pkgdown::build_site()`) | `docs/news/index.md` and `index.html` now open 0.3.3 Bug Fixes with the two new round-four bullets (Cochran coding, one-proportion null boundary) followed by the four round-three bullets (runs upper tail, scale-aware one-sample t guard, homogeneous Cochran factors, undefined runs/Cochran), and the "Other changes" section now includes "The README and pkgdown site were regenerated…". No other page content changed. | Fixed |

## Deferred

- No re-audit finding is deferred.
- `docs-dev/ifxx-reaudit.md`, `docs-dev/ifxx2-reaudit.md` and
  `docs-dev/ifxx3-reaudit.md` remain untracked auditor reports (preserved, not
  committed); `docs-dev/` is excluded from the build.
- No win-builder, R-hub, macOS, reverse-dependency or manual-PDF run was
  performed (same scope as earlier rounds).

## Checks

Source suite (`devtools::test()`):

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 909 ]
```

Clean build and check from `git archive HEAD` in
`C:/Users/DELL/AppData/Local/Temp/opencode/ifx4-check`:

```text
R CMD build --no-manual inferr
R CMD check --as-cran --no-manual inferr_0.3.3.tar.gz
```

Final status: **0 errors, 0 warnings, 1 NOTE**. The NOTE is the expected CRAN
incoming-feasibility maintainer change retained from round 1 (M1):

```text
* checking CRAN incoming feasibility ... [88s] NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

There were no WARNINGs and no other NOTEs. Examples, vignette rebuilding and
installed tests pass. Installed tests:

```text
[ FAIL 0 | WARN 0 | SKIP 40 | PASS 861 ]
```

The 40 skips are the 33 CRAN snapshot skips plus the seven source-file
documentation guards. R 4.6.1 (2026-06-24 ucrt), Windows 11 x64, Rtools45/GCC
14.3.0.

## Commands run that changed something

- `Rscript -e "pkgdown::build_site()"` — regenerated the pkgdown news/site
  (`docs/news/index.{md,html}`, `docs/search.json`, `docs/pkgdown.yml`).
- `rm -rf .ifxx3-tmp` — deleted the untracked round-three audit scratch
  directory (scripts, base/HEAD archives, tarball, installed check tree).
- `git checkout -- tests/testthat/_snaps` — removed line-ending-only churn left
  by running the snapshot tests.
- `git archive …` + `R CMD build` + `R CMD check --as-cran --no-manual` — clean
  build/check above.

## Not done

- Nothing was pushed; no GitHub/`gh` operation and no branch switch.
- No new dependency and no API change.
- No other statistical defect was found while re-checking the audit cases.

## Commits

```text
670e2b7 fix: recode homogeneous Cochran columns by the design's coding (H13) (ifx4)
0befbf0 fix: reject degenerate one-proportion null probabilities (M5) (ifx4)
77fc053 docs: note the round-four user-visible fixes in NEWS (ifx4)
7f26b14 docs: regenerate pkgdown news after round four (L1) (ifx4)
```
