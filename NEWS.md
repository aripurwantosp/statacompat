# statacompat 1.0

First release. It contains a small set of vectorised helpers that reproduce
selected Stata expression semantics.

* Comparisons: `stata_eq()`, `stata_ne()`, `stata_gt()`, `stata_ge()`,
  `stata_lt()`, `stata_le()`. Numeric missing values are larger than all
  numbers. Strings are compared by bytes, independent of locale. The result
  never contains `NA`.
* Membership and ranges: `stata_inlist()` and `stata_inrange()`. They follow
  Stata's rules for missing values, including the open-bound rules of
  `inrange()`.
* `stata_cond()` reproduces Stata's truth rules: a missing numeric condition
  counts as true unless `missing` is supplied. A logical `NA` condition is an
  error.
* `stata_missing()` treats `""` as missing. A string of spaces is not missing.
* `egen` row functions: `stata_rowtotal()` (with a `missing` option),
  `stata_rowmean()`, `stata_rowmiss()`, and `stata_rownonmiss()` (with a
  `strok` option).
* Extended missing values (`.a`-`.z`, via `haven::tagged_na()`) are
  detected. Results that depend on distinguishing one missing value from
  another raise an error instead of returning a guess.
