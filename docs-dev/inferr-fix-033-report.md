# inferr 0.3.3 fix round — report

Job id: `inferr-fix-033`
Branch: `task/inferr-fix-033` (from `release/v0.3.3`, base commit `ded89d5`)
Date: 2026-11-01
Audit: `F:/pm/notes/2026-11-01-pkg-audits/cx-inferr-audit.md`
Shared rules: `F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`

## Summary

All ten High findings and the three must-fix Medium findings from the audit
were fixed, together with L3 (`docs-dev/` excluded from the build). The
incorrect `sd_dif` label from L2 was corrected as part of H6. Each fix has a
testthat test that reproduces the audit case and at least one nearby case;
each new test was run before the fix and failed, and the final source suite is
green.

No exported function signature changed. Three behaviour changes follow
directly from the fixes: `ifr_ts_var_test()` now rejects more than two
variables (or more than one outcome with `group_var`), `ifr_oneway_anova()`
rejects a design with no residual degrees of freedom, and the `corsig`
component of `ifr_ts_paired_ttest()` is stored with four decimals instead of
two.

The final `R CMD check --as-cran --no-manual` result is **0 errors, 0
warnings, 1 note**. The note is the CRAN incoming-feasibility notice for the
required maintainer email change (M1). The previous invalid-URL note is gone
(M3).

## Findings

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| H1 missing categorical observations counted as failures | `test-binom.R` "ignores missing outcomes in n and k (H1)"; `test-prop-test.R` "ignores missing outcomes (H1)"; `test-chisqgof.R` "ignores missing observations (H1)"; `test-two-prop-test.R` two H1 tests | `R/ifr-binom-test.R:130`; `R/ifr-os-prop-test.R:64,71-72`; `R/ifr-ts-prop-test.R:141-147,242-243`; `R/ifr-chisq-gof-test.R:51,96` | Audit cases plus nearby cases compared with `binom.test()`, `prop.test(correct = FALSE)` and `chisq.test()`; NA insertion now leaves `n`, `k`, observed/expected counts and p-values unchanged | Fixed |
| H2 McNemar exact p > 1 and correction creates evidence | `test-mcnemar.R` "McNemar exact and corrected p-values are bounded (H2)" | `R/ifr-mcnemar-test.R:141-156` | `exactp` capped at 1 and checked against `binom.test(2, 4)`; corrected statistic/p checked against `mcnemar.test()` for `b = 3, c = 9`; no-discordant table defined as statistic 0, p = 1 | Fixed |
| H3 Yates correction not bounded by the deviation | `test-chisq.R` "Yates correction is bounded by the observed deviation (H3)"; `test-chisqgof.R` "GOF continuity correction is bounded at the observed deviation (H3)" | `R/ifr-chisq-assoc-test.R:177-179`; `R/ifr-chisq-gof-test.R:84,118-131` | 2 x 2 equal table and `hsb/female x schtyp` compared with `chisq.test()` (corrected); GOF checked with the explicit hand calculation; corrected deviation/residuals use the signed bounded deviation | Fixed |
| H4 paired correlation p-value > 1 | `test-paired-test.R` "paired correlation p-value matches cor.test for both signs (H4)" | `R/ifr-ts-paired-ttest.R:116-117,165-170` | Correlation `-0.9` and `+0.9` cases compared with `cor.test()`; p-value is `2 * pt(abs(t), n - 2, lower.tail = FALSE)` from the unrounded correlation | Fixed |
| H5 Levene and Brown–Forsythe interchanged | `test-levene.R` new reference test; existing expectations and character/NA tests updated | `R/ifr-levene-test.R:153-154` | Mean-centred ANOVA on `abs(x - group mean)` equals `lev`; median-centred equals `bf`; cross-checked with `car::leveneTest(center = "mean"/"median")` | Fixed |
| H6 pooled SD divides by `n1 + n2 - 4` | `test-indttest.R` "pooled SD uses n1 + n2 - 2 in the denominator (H6)"; `test-utils.R` `sd_diff`/`se_diff` test rewritten to pass variances | `R/ifr-ts-ind-ttest.R:276-285` (doc line 31 and `man/ifr_ts_ind_ttest.Rd` for L2 `sd_dif`) | Audit case now 1.581 (was 2.236); `hsb/female/write` now 9.185 (was 9.231); checked against the pooled-variance identity and `t.test(var.equal = TRUE)` | Fixed |
| H7 singleton ANOVA group gives NA | `test-anova.R` "a singleton group does not make the ANOVA missing (H7)" and "rejects a design with no residual degrees of freedom (H7)" | `R/ifr-anova.R:99-110,141-143` | `ss_within`, `df_within`, `fstat`, `pval` compared with `anova(lm())` for one and two singleton groups; zero residual df now errors | Fixed |
| H8 columns named `x`/`y` break ANOVA and t tests | `test-anova.R` "ANOVA works when columns are named x or y (H8)"; `test-indttest.R` "independent t test works when columns are named x or y (H8)" | `R/ifr-anova.R:96-110`; `R/ifr-ts-ind-ttest.R:177-190`; `R/inferr.R:15-18` | Renamed copies of the same data compared with `anova(lm())` and `t.test(var.equal = TRUE)`; private data-frame columns avoid name lookup in the data mask | Fixed |
| H9 variance test silently uses two of three samples | `test-two-var-test.R` two H9 tests | `R/ifr-ts-var-test.R:69-71,79-97` | More than two columns and more than one outcome with `group_var` now error; accepted two-column and grouped calls compared with `var.test()`; retained-group count is checked after missing values are removed | Fixed |
| H10 ANOVA p-value underflows to zero | `test-anova.R` "ANOVA p-value uses the upper tail without cancellation (H10)" | `R/ifr-anova.R:148` | Audit case now `1.119967745e-21` and equals `anova(lm())` / `pf(..., lower.tail = FALSE)` | Fixed |
| M1 wrong maintainer email | `test-docs.R` "DESCRIPTION uses the release maintainer email (M1)" | `DESCRIPTION:5`; `man/inferr.Rd:25` | `DESCRIPTION` and the generated maintainer field checked; check output reports `aravindhebbali@rsquaredacademy.com` | Fixed |
| M2 stale/unfinished `cran-comments.md` | none (metadata) | `cran-comments.md` | Replaced TODOs with the checks actually run, the final check outcome and the maintainer-note explanation | Fixed |
| M3 invalid external documentation URL | `test-docs.R` "the exam data source is reachable and stable (M3)" | `R/ifr-data-exam.R:12`; `man/exam.Rd:16` | The SAS/SPSS tutorial URL timed out and returned 520 from the checker; replaced with the stable Wikipedia Cochran's Q page (`200` here); the check no longer reports an invalid URL | Fixed |
| L3 `docs-dev/` not build-ignored | `test-docs.R` "development reports are excluded from the build (L3)" | `.Rbuildignore:18` | Rule added; a build after the report was committed does not contain `docs-dev/` | Fixed |

## Deferred

- **M4** — Non-factor binary encodings (character/logical/numeric) can still fail in a homogeneous group because `table(...)[[2]]` has no shared success level; the documented input is a factor. Not changed.
- **M5** — Malformed/degenerate inputs still need explicit validation (fractional binomial counts, `phat` outside 0–1, constant one-sample t, runs with ties/NA, Cochran encodings); these are invalid-input improvements, not wrong results on valid data.
- **M6** — `ifr_ts_ind_ttest()` and the binary-outcome validators still reject unused factor levels; the variance test now drops unused grouping levels as part of H9. The remaining unused-level case is not a wrong result.
- **M7** — Broader coverage gaps remain (mocked Shiny launcher, deprecation paths, installed-documentation assertions); H1–H10 now have numeric base-R comparisons. Not expanded further.
- **L1** — `README.md` and the rendered pkgdown site still show pre-fix output/version; these files are excluded from the CRAN tarball and were not regenerated.
- **L2** — The remaining Rd/roxygen wording errors were not changed: the `ifr_binom_cal()` typo, `obs_p` described as "assumed" probability, and the runs-test threshold described as within-group sum of squares. The `sd_dif` label was corrected with H6.

## Checks

Source test suite (`NOT_CRAN=true`, `testthat::test_local()`):

```text
failed skipped error warning passed
     0       0     0       0    574
```

Build and check, from a clean `git archive HEAD` copy with the build/check
directory outside the source tree:

```powershell
$env:LC_ALL = 'C'; $env:LANG = 'C'; $env:LANGUAGE = 'en'
& 'C:/Program Files/R/R-4.6.1/bin/R.exe' CMD build --no-manual src
& 'C:/Program Files/R/R-4.6.1/bin/R.exe' CMD check --as-cran --no-manual inferr_0.3.3.tar.gz
```

Final status: **0 errors, 0 warnings, 1 note**.

The note is:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

It is the expected consequence of M1 (the required maintainer address change).
Examples, the testthat suite, and vignette rebuilding pass. R 4.6.1
(2026-06-24 ucrt), Windows 11 x64, Rtools45.

## Commands that changed something

- `Rscript -e "testthat::test_local('.', reporter = 'summary')"` — used to
  observe each new test fail before its fix and to confirm the final suite.
- `Rscript -e "testthat::snapshot_accept(path = 'tests/testthat')"` — updated
  the `chisq`, `indttest` and `levene` snapshots for the corrected values.
- `R CMD build --no-manual src` and
  `R CMD check --as-cran --no-manual inferr_0.3.3.tar.gz` — final build/check
  above.
- `git archive` / `tar` — produced the clean source copy used for the build.

## Not done

- No win-builder, R-hub, macOS or reverse-dependency run.
- No manual PDF, no pkgdown/README regeneration (L1), no fixes for M4–M7 or the
  remaining L2 wording.
- Nothing was pushed; no GitHub/`gh` operation was used.

## Commits

```text
4ffbc0b fix: correct categorical test denominators and continuity corrections (H1, H3)
6cce1db fix: bound McNemar exact and continuity-corrected p-values (H2)
9d76ee7 fix: compute paired-test correlation p-value from the upper tail (H4)
7720a2c fix: stop interchanging Levene and Brown-Forsythe statistics (H5)
81684a6 fix: correct pooled SD and x/y column handling in the independent t test (H6, H8)
6bd6501 fix: handle singleton groups, x/y names and the upper tail in ANOVA (H7, H8, H10)
ce08489 fix: require two samples and validate retained groups in the variance test (H9)
91302f3 chore: update maintainer email, data source URL and build ignores (M1, M3, L3)
dff3b97 docs: add NEWS entries for the 0.3.3 fixes
61f6a86 chore: declare data.table globals used by the ANOVA and t test helpers
2255e3e fix: define the McNemar statistic when there are no discordant pairs (H2)
a09c883 docs: record the completed check results in cran-comments.md (M2)
b38a81a docs: add the inferr 0.3.3 fix-round report
```
