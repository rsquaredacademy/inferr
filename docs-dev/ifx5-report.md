# ifx5 — inferr fix report

2026-11-01. Worktree `F:/R/Packages/statistics/inferr-wt/fix-033`, branch
`task/inferr-fix-033`. Scope: only H13 as left in `ifxx4-reaudit.md` — the
equivalent mixed numeric/factor Cochran input still returning `Q=2, p=0.3679`
instead of `Q=6, p=0.0498`.

## Findings fixed

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| H13 | `tests/testthat/test-cochran.R:232` "mixed numeric/factor columns share one success coding (H13)"; `:281` "every equivalent mixed encoding of the H13 design agrees" | `R/ifr-cochran-q-test.R:41-47` (coding resolved before complete-case filtering), `:93-130` (`cochran_recode()`), `:132-203` (new `cochran_coding()`) | hand-calculated `Q`/`p`; 26/27 mixed encodings give `Q=6, p=0.0498`; the genuinely ambiguous one errors | fixed |

## Fix

`cochran_recode()` now normalises every column to the same 0/1 success coding
by one explicit rule — logical `FALSE/TRUE`, factor first/second level, and a
single design-wide numeric 0/1 or 1/2 coding — applied after the coding is
resolved but before complete-case filtering can erase the evidence it relies
on.

The coding is established by a new `cochran_coding()` helper from every
unambiguous source: numeric columns that contain a `0` (0/1), a `2` (1/2), and
factor columns whose declared levels are `0/1` or `1/2`. A homogeneous numeric
column of all `1`s — the success code of a 0/1 design or the failure code of a
1/2 design — inherits that coding, which is exactly what the re-audit's mixed
`numeric/factor/factor`, `numeric/logical/factor` and `numeric/factor/logical`
cases needed. Conflicting declarations (a 0/1 column beside a 1/2 column) still
error with the existing "single 0/1 or 1/2 coding" message. A homogeneous
numeric column beside only logical (or non-numeric-level factor) columns leaves
the coding genuinely undetermined and now errors clearly instead of silently
guessing `0/1`.

## Hand-calculated verification

The re-audit design `a=c(0,0,0,0)`, `b=c(0,1,1,1)`, `c=c(0,1,1,1)` has column
sums `0,3,3`, row sums `0,2,2,2`, `k=3`, so

```
Q = (k-1) * (k*sum(colSums(m)^2) - sum(m)^2) / (k*sum(m) - sum(rowSums(m)^2))
  = 2 * (3*18 - 36) / (3*6 - 12)
  = 6
p = pchisq(6, 2, lower.tail = FALSE) = 0.04978706836786394 -> 0.0498
```

The four encodings `d01` (numeric 0/1), `d12` (numeric 1/2), `f12` (factor
1/2) and `mixed` (numeric + factor 1/2) all now return `n=4, df=2, q=6,
p=0.0498`. The nearby design (`b=c(0,0,0,1)`, `c=c(0,0,1,1)`) returns `Q=3,
p=0.2231`; the pre-filter-evidence case (a `c(2,NA,1)` row appended before
`b/c` become factors) returns `Q=6, p=0.0498`. Of the 27 per-column choices of
numeric-1/2 / factor-1/2 / logical for the design, 26 return `Q=6` and the one
`numeric/logical/logical` mix errors with "homogeneous numeric column".

## Deferred

None. H13 was the only open finding; the other 25 IDs were confirmed fixed in
the round-four re-audit and were not touched.

## Check output

`devtools::test()` (with `NOT_CRAN=true`) — all green. From a clean
`git archive HEAD` copy, `R CMD build --no-manual` then
`R CMD check --as-cran --no-manual`: **0 errors, 0 warnings, 1 NOTE**. The
entire NOTE is the expected CRAN incoming maintainer change:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'
New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

Installed tests: `[ FAIL 0 | WARN 0 | SKIP 40 | PASS 937 ]` (40 skips are the
usual CRAN snapshot and source-file documentation guards). Source suite with
`NOT_CRAN=true`: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 985 ]`.

## Commands run that changed something

```text
git add NEWS.md R/ifr-cochran-q-test.R tests/testthat/test-cochran.R
git commit -m "fix: resolve mixed Cochran coding across all column types (H13) (ifx5)"
```

(plus the report commit below). `NEWS.md`'s round-four Cochran bullet was
updated to describe the now-complete fix rather than the earlier partial one.

## Anything not done

The `.ifxx4-tmp/` audit scratch directory (left untracked by the re-audit after
its cleanup was policy-blocked) was removed as instructed. No API change: the
function signature and exported return object are unchanged; the only new
internal symbol is the non-exported `cochran_coding()` helper.
