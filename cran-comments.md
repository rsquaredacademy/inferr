This is a patch release. It fixes incorrect confidence intervals and p-values in
several tests (one-sample variance, independent and paired t, one-way ANOVA,
likelihood ratio chi-square), adds the two-sided exact p-value to the binomial
test, adds input checks to the proportion, Levene,
variance and McNemar tests, handles missing values in the t and variance tests,
and stops `library(inferr)` from contacting CRAN when the package is attached.
See `NEWS.md` for the full list.

## Test environments

- local: Windows 11, R 4.6.1
- TODO: win-builder (devel, release)
- TODO: R-hub
- TODO: macOS

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

TODO: revdepcheck
