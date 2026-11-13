# ifx6 — inferr 0.3.3 fix report

2026-11-14 (Asia/Calcutta). Worktree
`F:/R/Packages/statistics/inferr-wt/fix-033`, branch `task/inferr-fix-033`.
Started at `9f7663a`; scope is M10, M11, L1 and L5 in the preserved,
untracked `docs-dev/ifxx5-reaudit.md`. Read the shared
`F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`; no applicable
AGENTS.md or CLAUDE.md was found in this worktree or its ancestors.

## Findings

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| M10 | `tests/testthat/test-cochran.R:317` covers the exact audit design and nearby Q=3 design with different factor labels, both level orders, reversed column selection and incomplete rows; `:360` covers mixed factor/numeric 0/1 and 1/2 coding, numeric precedence and unresolved conflicting hints | `R/ifr-cochran-q-test.R:159`, `:179`: collect factor hints separately and consult them only for ambiguous numeric columns without explicit numeric evidence; `NEWS.md:5` | Both new tests failed before the fix; exact and nearby examples separately reproduced against the original source; numerical assertions and the unchanged H13 tests pass | fixed |
| M11 | Metadata inspection; existing launcher-mock test exercises `local_mocked_bindings()` | `DESCRIPTION:25`: `testthat (>= 3.1.7)` | Installed testthat 3.3.2 exports the helper; full suite passes. A separate installation/run of testthat 3.1.7 was not performed | fixed |
| L1 | Compared rendered news with NEWS.md | `docs/news/index.html:47`, `docs/news/index.md:8`, plus generated search index and pkgdown build metadata | Site regenerated successfully; round-five pre-filter inference/ambiguity entry and round-six independent-factor entry appear in both rendered news formats and search data | fixed |
| L5 | Evaluated the corrected scalar expression in the reproduction script | `docs-dev/ifx5-report.md:37` | Column sums 0,3,3; row sums 0,2,2,2; `2 * (3*18 - 36) / (3*6 - 12) = 6` | fixed |

## Coding rule and numerical evidence

Every two-level factor still maps its first declared level to 0 and its
second to 1, including an unobserved declared level. Factor labels do not
constrain other factors. Numeric columns must share one 0/1 or 1/2 coding;
their explicit 0/2 evidence takes precedence over factor labels. When the
numeric columns contain only ambiguous ones, consistent numeric-labelled
factor hints remain available as the H13 fallback. Conflicting hints with
otherwise unresolved numeric ones still error. Coding evidence is read
before complete-case filtering, as in round five. No API or dependency was
added; the public p-value retains its existing four-decimal representation.

The exact audit example was rerun:

```r
d <- data.frame(a = c(0, 0, 0, 0),
                b = c(0, 1, 1, 1),
                c = c(0, 1, 1, 1))
f <- data.frame(a = factor(d$a, levels = c(0, 1)),
                b = factor(d$b + 1, levels = c(1, 2)),
                c = factor(d$c + 1, levels = c(1, 2)))
ifr_cochran_qtest(f, a, b, c)
```

At `9f7663a` this errors with "Cannot determine a single 0/1 or 1/2 coding
across the numeric columns." After the fix: `n=4`, `df=2`, `q=6`,
`pvalue=0.0498`. The independent calculation uses column and row sums:

```text
Q = 2 * (3*18 - 36) / (3*6 - 12) = 6
pchisq(6, 2, lower.tail=FALSE) = 0.049787068367863944
```

Tests assert the unrounded reference `0.04978706836786394` at tolerance
1e-14 from the returned Q/df and assert the stored rounded p-value 0.0498;
there is no unrounded p-value field in the existing public object.

For the nearby design `a=(0,0,0,0)`, `b=(0,0,0,1)`, `c=(0,0,1,1)`, with the
same differing factor labels, the old source gives the same coding error;
the fix returns `n=4`, `df=2`, `q=3`, `pvalue=0.2231` and the independent
unrounded p is `0.22313016014842982`. Logs:
`.ifx6-tmp/cochran-before.log`, `cochran-after.log`, and `reproduce.log`.

All existing H13 assertions remain unchanged and pass: original and nearby
mixed designs, pre-filter numeric evidence, all-success and NA-homogenized
columns, homogeneous numeric/factor/logical encodings, and the 27 mixed
choices (26 numerical agreements and one expected ambiguity error).

## Checks

`devtools::test()` with `NOT_CRAN=true`, R 4.6.1, testthat 3.3.2:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1188 ]
```

Log: `.ifx6-tmp/test-all.log`.

Clean archive build/check at implementation-and-docs commit `89155db`:

```text
R CMD build --no-manual source/inferr
R CMD check --as-cran --no-manual inferr_0.3.3.tar.gz
0 errors | 0 warnings | 1 NOTE
[ FAIL 0 | WARN 0 | SKIP 40 | PASS 1140 ]
```

Examples, installed tests, package vignettes and vignette rebuilding pass.
The 40 skips are the existing CRAN snapshot/source-document guards. The
single NOTE is the expected maintainer email change:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

There are no other NOTEs. Platform: Windows 11 x64 build 26300,
R 4.6.1 (2026-06-24 ucrt), Rtools45/GCC 14.3.0. Logs are
`.ifx6-tmp/check-before-report/build.log`, `check.log` and
`inferr.Rcheck/00check.log`; installed suite output is
`inferr.Rcheck/tests/testthat.Rout`.

The built tarball contains 109 entries and excludes development reports,
audit scratch, rendered site files and cran-comments. The report commit is
followed by a second clean `git archive HEAD` build/check; its commit ID,
build output and check output are retained under `.ifx6-tmp/check-final/`.
No further commit is made after that final check.

## Commands run that changed something

All heavy R commands were invoked through
`bash ~/.claude/bin/heavy-slot.sh pkg-ifx6 <command>`. Reproduction scripts,
logs, temporary directories, the local installed library, archives and check
trees were created only under `.ifx6-tmp/`.

```text
Rscript .ifx6-tmp/test-cochran.R
Rscript .ifx6-tmp/environment.R
Rscript .ifx6-tmp/render-news.R
Rscript .ifx6-tmp/test-all.R
Rscript .ifx6-tmp/reproduce.R
Rscript .ifx6-tmp/render-site.R
Rscript .ifx6-tmp/refresh-news.R
bash .ifx6-tmp/check-archive.sh check-before-report
bash .ifx6-tmp/check-archive.sh check-final
```

The render script installs this package with `R CMD INSTALL` into
`.ifx6-tmp/library`, then runs
`pkgdown::build_site(preview=FALSE, devel=TRUE, install=FALSE)`. The final
news-only refresh runs `pkgdown::build_news()`, the pkgdown Markdown-docs
builder and `pkgdown::build_search()` after clarifying the round-five NEWS
wording to match the final coding rule. README results were already correct
and do not need changes.

The first full-site attempt stopped because its separate article-rendering
process could not load inferr; installing into the local scratch library
resolved this and the full render completed. Pkgdown/Pandoc printed existing
site metadata/accessibility and deprecated math-option notices; these are
outside this minimal fix scope. A first archive-extraction attempt failed
because GNU tar interpreted a Windows drive colon as a remote path; using
Bash's physical POSIX working directory resolved it before the build/check.

```text
git restore -- tests/testthat/_snaps docs/articles/intro.html
git add R/ifr-cochran-q-test.R tests/testthat/test-cochran.R NEWS.md
git commit -m "fix: map Cochran factors independently of numeric coding (M10) (ifx6)"
git add DESCRIPTION
git commit -m "fix: require testthat 3.1.7 for mocked bindings (M11) (ifx6)"
git add docs/news/index.html docs/news/index.md docs/pkgdown.yml docs/search.json
git commit -m "docs: refresh rendered news for rounds five and six (L1) (ifx6)"
git add docs-dev/ifx5-report.md
git commit -m "docs: correct Cochran hand calculation in round-five report (L5) (ifx6)"
git add docs-dev/ifx6-report.md
git commit -m "docs: record round-six fixes and validation (ifx6)"
```

The restore removed only test-generated snapshot line-ending changes and
an unrelated regenerated article date. Initial tracked files were clean.

## Deferred and not done

No requested finding is deferred. The five pre-existing untracked re-audit
reports and `.ifxx5-tmp/` were left untouched. `.ifx6-tmp/` remains untracked
with the evidence for this round. No branch switch, push, gh/GitHub operation
or other-worktree access occurred. No CRAN submission, external builder,
macOS, reverse-dependency check or manual-PDF build was performed. The exact
testthat minimum was not separately installed; checks used 3.3.2.
