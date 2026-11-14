This is a patch release. It corrects wrong test statistics and p-values in
several tests: missing categorical observations are no longer counted as
failures, the McNemar exact and continuity-corrected probabilities are
bounded, the Yates correction is bounded at zero, the paired-test correlation
p-value is computed from the upper tail, the Levene and Brown-Forsythe
statistics are no longer interchanged, the independent-test pooled standard
deviation uses the correct denominator, and one-way ANOVA handles singleton
groups and upper-tail p-values. Cochran's Q test now reads numeric, logical and factor
codings of the binary variables consistently. Data columns named `x` or `y` no longer break
the ANOVA or independent t test. The release also adds input checks to the
proportion, Levene, variance and McNemar tests, requires two samples in the
variance test, handles missing values in the t and variance tests, and stops
`library(inferr)` from contacting CRAN when the package is attached. See
`NEWS.md` for the full list.

## Test environments

- local: Windows 11 x64 build 26300, R 4.6.1 (2026-06-24 ucrt), Rtools45
- win-builder: R-devel (2026-11-13 r90888 ucrt)
- R-hub: linux, windows, macos, macos-arm64 (R-devel)
- GitHub Actions: ubuntu-latest (devel, release, oldrel-1), windows-latest
  (release), macos-latest (release)
- Ubuntu 26.04, R 4.5.2

## R CMD check results

0 errors | 0 warnings | 1 note

The single note is the CRAN incoming feasibility note reporting the
maintainer email change in `DESCRIPTION` from `hebbali.aravind@gmail.com` to
`aravindhebbali@rsquaredacademy.com`. It is expected for this submission. No
invalid-URL note is reported; the `exam` data source that previously returned
HTTP 520 was replaced with a stable page.

The check was run from a clean `git archive HEAD` copy (build directory
outside the source tree) with `LC_ALL=C`, `LANG=C` and `LANGUAGE=en`:

```text
R CMD build src
R CMD check --as-cran inferr_0.3.3.tar.gz
```

DESCRIPTION, dependencies, loading/unloading, native code, R code, Rd files,
examples, tests and vignette rebuilding all pass.

## Reverse dependencies

There are no reverse dependencies on CRAN.
