#' Stata-style relational comparisons
#'
#' Vectorised equivalents of Stata's relational operators `==`, `!=`, `>`,
#' `>=`, `<` and `<=`. Unlike R's operators, they never return `NA`:
#' following Stata, a numeric missing value is treated as larger than every
#' number, and two missing values are equal.
#'
#' @section Semantics:
#' In Stata, `. > 1` is true and `. == .` is true (Stata manual \[U\] 13.2.3: "A missing
#' value is greater than any nonmissing value"). In R, `NA > 1` is `NA`,
#' and code such as `x > 1` silently drops or propagates missing rows.
#'
#' | Stata         | R base     | statacompat           | result |
#' |---------------|------------|-----------------------|--------|
#' | `. == 1`      | `NA == 1`  | `stata_eq(NA, 1)`     | FALSE  |
#' | `. != 1`      | `NA != 1`  | `stata_ne(NA, 1)`     | TRUE   |
#' | `. > 1`       | `NA > 1`   | `stata_gt(NA, 1)`     | TRUE   |
#' | `. >= 1`      | `NA >= 1`  | `stata_ge(NA, 1)`     | TRUE   |
#' | `. < 1`       | `NA < 1`   | `stata_lt(NA, 1)`     | FALSE  |
#' | `. <= 1`      | `NA <= 1`  | `stata_le(NA, 1)`     | FALSE  |
#' | `. == .`      | `NA == NA` | `stata_eq(NA, NA)`    | TRUE   |
#' | `. != .`      | `NA != NA` | `stata_ne(NA, NA)`    | FALSE  |
#'
#' Every row follows from the documented ordering of missing values
#' (\[U\] 12.2.1) and from the rule that relational operators never return
#' missing (\[U\] 13.2.3).
#'
#' **Missing values.** `NA` and `NaN` both stand for Stata's system missing
#' value `.`. `Inf` and `-Inf` are ordinary (very large / very small)
#' numbers; Stata has no infinities (`1/0` is `.` in Stata).
#'
#' **Extended missing values.** Stata orders `. < .a < .b < ... < .z` and
#' treats them as distinct. Extended missing values imported with
#' `haven::read_dta()` are R `NA`s carrying a tag. Comparing an extended
#' missing value with a *non-missing* value gives the Stata result; comparing
#' it with a *different* missing value is an error, because version 1.0
#' does not reproduce the ordering among missing values.
#'
#' **Strings.** Strings are compared by their UTF-8 bytes, as in Stata
#' (Stata manual \[U\] 12.4.2), so `"a" > "B"` is `TRUE` regardless of the R session's
#' locale. Stata's missing string is `""`, which is simply the smallest
#' string. `NA_character_` has no Stata equivalent and is an error.
#'
#' **Types.** `x` and `y` must be of the same kind: numbers (double,
#' integer, logical, numeric `haven_labelled`), strings, `Date`s, or
#' `POSIXct` date-times. Mixing kinds is an error, as in Stata. A bare `NA`
#' may be compared with numbers, dates or date-times. Factors are rejected.
#'
#' **Lengths.** `x` and `y` must have the same length, or one of them must
#' have length 1. Any other combination is an error (no silent recycling).
#'
#' @param x,y Vectors to compare. See *Types* and *Lengths*.
#' @return A logical vector without `NA`s, of the common length of `x` and
#'   `y`. Use [as.integer()] for a Stata-style 0/1 indicator.
#' @seealso [stata_missing()] to test for missing values, [stata_inrange()]
#'   for closed ranges that exclude missing values.
#' @name stata_comparison
#' @examples
#' income <- c(5e6, 2e7, NA)
#'
#' # Stata: gen high_income = income > 10000000
#' income > 1e7               # base R: NA for the missing income
#' stata_gt(income, 1e7)      # Stata: the missing income counts as "high"
#'
#' # The usual Stata idiom `income > 10000000 & income < .`:
#' stata_gt(income, 1e7) & stata_lt(income, NA)
#'
#' stata_eq(c(1, NA), NA)     # Stata: x == .
#' stata_gt("a", "B")         # byte order, as in Stata
NULL

#' @rdname stata_comparison
#' @export
stata_eq <- function(x, y) {
  sc_compare3(x, y, "stata_eq") == 0L
}

#' @rdname stata_comparison
#' @export
stata_ne <- function(x, y) {
  sc_compare3(x, y, "stata_ne") != 0L
}

#' @rdname stata_comparison
#' @export
stata_gt <- function(x, y) {
  sc_compare3(x, y, "stata_gt") > 0L
}

#' @rdname stata_comparison
#' @export
stata_ge <- function(x, y) {
  sc_compare3(x, y, "stata_ge") >= 0L
}

#' @rdname stata_comparison
#' @export
stata_lt <- function(x, y) {
  sc_compare3(x, y, "stata_lt") < 0L
}

#' @rdname stata_comparison
#' @export
stata_le <- function(x, y) {
  sc_compare3(x, y, "stata_le") <= 0L
}
