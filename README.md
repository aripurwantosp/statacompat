# statacompat

A lightweight R package that reproduces selected Stata expression and
missing-value semantics in R.

When Stata data-management code is translated into R, an expression can look
identical in both languages and still return different results. Most of these
differences come from missing values. statacompat provides a small set of
vectorised helpers, all prefixed `stata_`, that behave like their Stata
counterparts and work inside ordinary `dplyr::mutate()` calls.

It is meant for researchers who work in both Stata and R, including those
replicating Stata results in R or migrating `.do` files.

## Why base R and Stata disagree

In Stata, a numeric missing value (`.`) is **larger than every number**.
Relational and logical expressions therefore never return missing. In R, any
comparison with `NA` returns `NA`.

```stata
gen high_income = income > 10000000
```

```r
income <- c(5000000, 25000000, NA)

income > 10000000                # naive translation
#> [1] FALSE  TRUE    NA

stata_gt(income, 10000000)       # what Stata computes
#> [1] FALSE  TRUE  TRUE
```

In Stata, the missing income is coded as high income. That is often not what
the analyst intended, and the usual Stata safeguard is `income > 10000000 &
income < .`. The point is that statacompat gives the same answer as the
original Stata code, including the cases where that code is questionable, so
a translation can be checked line by line:

```r
stata_gt(income, 10000000) & stata_lt(income, NA)   # Stata: ... & income < .
#> [1] FALSE  TRUE FALSE
```

## What is in version 1.0

| Stata | statacompat | Note |
|---|---|---|
| `==`, `!=`, `>`, `>=`, `<`, `<=` | `stata_eq()`, `stata_ne()`, `stata_gt()`, `stata_ge()`, `stata_lt()`, `stata_le()` | Missing is greater than all numbers. Strings are compared by bytes. Never returns `NA`. |
| `inlist(z, a, b, ...)` | `stata_inlist(x, ...)` | Missing is an ordinary member. Candidates are evaluated row by row. |
| `inrange(z, a, b)` | `stata_inrange(x, lower, upper)` | Inclusive. Missing `x` gives FALSE. A missing bound means unbounded. |
| `cond(x, a, b[, c])` | `stata_cond(condition, yes, no, missing)` | A missing numeric condition counts as **true**. |
| `missing(x1, ...)` | `stata_missing(...)` | For strings, `""` is missing but `" "` is not. |
| `egen rowtotal()` / `rowmean()` / `rowmiss()` / `rownonmiss()` | `stata_rowtotal()`, `stata_rowmean()`, `stata_rowmiss()`, `stata_rownonmiss()` | The `missing` and `strok` options are supported. |

Comparisons and predicates return logical vectors. Use `as.integer()` when you
need a 0/1 indicator variable.

```r
age <- c(12, 15, 64, 65, NA)
age >= 15 & age <= 64              # base R
#> [1] FALSE  TRUE  TRUE FALSE    NA
stata_inrange(age, 15, 64)         # Stata inrange(age, 15, 64)
#> [1] FALSE  TRUE  TRUE FALSE FALSE
stata_inrange(age, 15, NA)         # 15 and older, missing excluded
#> [1] FALSE  TRUE  TRUE  TRUE FALSE

kbli <- c(1, 4, NA)
stata_inlist(kbli, 1, 2, 3)
#> [1]  TRUE FALSE FALSE
stata_inlist(kbli, 1, NA)          # Stata inlist(kbli, 1, .)
#> [1]  TRUE FALSE  TRUE

r1 <- c(1, 1, NA); r2 <- c(2, NA, NA); r3 <- c(3, 3, NA)
rowMeans(cbind(r1, r2, r3), na.rm = TRUE)
#> [1]   2   2 NaN
stata_rowmean(r1, r2, r3)
#> [1]  2  2 NA
stata_rowtotal(r1, r2, r3)                  # egen rowtotal()
#> [1] 6 4 0
stata_rowtotal(r1, r2, r3, missing = TRUE)  # egen rowtotal(), missing
#> [1]  6  4 NA

wage <- c(0, 1500, NA)
ifelse(wage, "paid", "unpaid")
#> [1] "unpaid" "paid"   NA
stata_cond(wage, "paid", "unpaid")          # Stata cond(wage, "paid", "unpaid")
#> [1] "unpaid" "paid"   "paid"
```

String comparisons in base R depend on the session locale. Stata compares
bytes. In an Indonesian or English UTF-8 locale:

```r
x <- c("apel", "Bayam", "")
x > "B"                # depends on the locale
#> [1] FALSE  TRUE FALSE
stata_gt(x, "B")       # byte order, as in Stata, in every locale
#> [1]  TRUE  TRUE FALSE
```

### In a pipeline

```r
library(dplyr)

dta <- data.frame(
  kbli = c(1, 5, NA, 3),
  age  = c(14, 30, 70, NA),
  r10a = c(1, 2, NA, 2),
  r10b = c(2, 2, NA, 1)
)

dta |>
  mutate(
    sector      = stata_inlist(kbli, 1, 2, 3),
    working_age = stata_inrange(age, 15, 64),
    atwork      = stata_cond(stata_eq(r10a, 1) | stata_eq(r10b, 1), 1, 0)
  )
#>   kbli age r10a r10b sector working_age atwork
#> 1    1  14    1    2   TRUE       FALSE      1
#> 2    5  30    2    2  FALSE        TRUE      0
#> 3   NA  70   NA   NA  FALSE       FALSE      0
#> 4    3  NA    2    1   TRUE       FALSE      1
```

The row functions take vectors or data frames, so `dplyr::pick()` works:
`stata_rowtotal(pick(r1:r5))`.

## Where the rules come from

- Every rule is taken from Stata's manuals: [U] 12.2.1, [U] 13.2.3–13.2.4,
  [U] 12.4.2, [FN] Programming functions and [D] egen.
- A few cases are not spelled out in the manuals: `inlist()` with missing
  values, `rowmiss()` with string variables, and the storage type `egen`
  uses when none is given. The help pages describe what statacompat does in
  these cases and state that the behaviour is inferred from documented rules.
- The expected values in the test suite are written from the manuals; they
  are not computed by the package.

statacompat does **not** claim full Stata compatibility. It reproduces the
functions listed above, within the limits below.

## Limitations and non-goals

- **Extended missing values** (`.a`–`.z`, read by `haven::read_dta()` as
  tagged `NA`s) are only partly supported. A result that depends on
  telling one missing value from another, such as `.a == .`, raises an error.
  All other results match Stata. To treat every missing value as `.`, convert
  first with `x[is.na(x)] <- NA_real_`.
- **`NA_character_`** has no Stata equivalent, so string comparisons reject it.
  Stata's missing string is `""`.
- **Storage precision.** `egen` stores its result as `float` unless a type is
  given, and `float` rounds integers above 16,777,216. statacompat always
  computes in double precision, so its results correspond to
  `egen double ...`. This matters for large Rupiah totals.
- **Arithmetic is not emulated.** In Stata `1/0` gives `.`, but in R it gives
  `Inf`. statacompat treats `Inf` as an ordinary number.
- **Types.** Factors are rejected. Dates can be compared only with dates of
  the same class, because Stata's date origin (1960) differs from R's (1970).
- Out of scope: translating `.do` files, `recode`, `_n`/`_N`, `bysort`,
  `egen group()`, estimation commands, and aliases for Stata commands.

## Installation

From a local copy of the source:

```r
# local
# install.packages("devtools")
devtools::install("path/to/statacompat")
```
or from github:

```r
# github
# install.packages("remotes")
remotes::install_github("aripurwantosp/statacompat", build_vignettes = TRUE)
```

statacompat has no runtime dependencies beyond base R (R >= 4.1).

## Learn more

- `vignette("stata-and-r-semantics", package = "statacompat")`: *When Stata
  and R Disagree: Expressions and Missing Values*.
- `?stata_comparison`, `?stata_inrange`, `?stata_cond`, `?stata_row*`.

## AI-Assisted Development

Generative AI tools, particularly Claude by Anthropic, were used to assist in the development of this package, including code drafting, debugging, refactoring, documentation, and test development. All AI-assisted outputs were reviewed, evaluated, and validated by the package author.
