#' Stata's `cond()`: vectorised if-else with Stata truth rules
#'
#' `stata_cond(condition, yes, no, missing)` reproduces Stata's
#' `cond(x, a, b[, c])`: it returns `yes` where `condition` is true, `no`
#' where it is false and, if supplied, `missing` where `condition` is a
#' missing number.
#'
#' @section How the condition is read:
#' Stata reads a numeric condition as **false if zero and true otherwise,
#' and a missing value is "otherwise"**. `stata_cond()` follows that rule
#' for numeric conditions (Stata manual \[FN\] Programming functions,
#' `cond()`):
#'
#' | `condition`               | 3 arguments | with `missing = c` |
#' |---------------------------|-------------|--------------------|
#' | nonzero number, `Inf`     | `yes`       | `yes`              |
#' | `0`                       | `no`        | `no`               |
#' | `NA` / `NaN` (Stata `.`)  | **`yes`**   | `c`                |
#' | `TRUE`                    | `yes`       | `yes`              |
#' | `FALSE`                   | `no`        | `no`               |
#' | logical `NA`              | error       | error              |
#'
#' This is **not** what [ifelse()] or `dplyr::if_else()` do: both return
#' `NA` for a missing condition.
#'
#' A **logical `NA` condition is an error**. In R it almost always comes
#' from an ordinary R comparison such as `x < 2` with missing `x`, and
#' there is no safe way to guess what Stata would have done: Stata's
#' `cond(x < 2, a, b)` gives `b` for a missing `x` (because `. < 2` is
#' false), whereas reading the `NA` produced by R's `x < 2` as Stata's
#' missing value would give `a`. Build the condition with the
#' `stata_*()` comparisons, which never return `NA`, or convert it with
#' `as.numeric()` if you really want a missing condition to count as true.
#'
#' Stata ignores the fourth argument when the condition is a relational
#' expression, because such expressions are never missing. The same holds
#' here when the condition comes from a `stata_*()` predicate.
#'
#' @section Result type:
#' `yes`, `no` and `missing` must all be numbers (logical, integer, double,
#' numeric `haven_labelled`), all strings, or all dates of one class. A bare
#' `NA` branch is allowed with numbers and dates. The result is logical if
#' every branch is logical, integer if every branch is integer or logical,
#' and double otherwise; strings give character, dates keep their class
#' (date-times take the time zone of the first date-time branch). Value
#' labels are dropped. Mixing numbers and strings is an error, as in Stata.
#'
#' @param condition A numeric or logical vector (numeric `haven_labelled`
#'   allowed). Strings, factors and dates are rejected.
#' @param yes,no Values used where `condition` is true or false.
#' @param missing Optional value used where `condition` is a missing
#'   number. If omitted, such rows get `yes`, as in Stata.
#' @return A vector of the common length of the inputs. Each input must have
#'   length 1 or that common length.
#' @export
#' @examples
#' r10a <- c(1, 2, NA)
#' r10b <- c(2, 1, NA)
#'
#' # Stata: gen atwork = cond(r10a == 1 | r10b == 1, 1, 0)
#' stata_cond(stata_eq(r10a, 1) | stata_eq(r10b, 1), 1, 0)
#'
#' # Numeric conditions: missing counts as true unless `missing` is given
#' wage <- c(0, 1500, NA)
#' stata_cond(wage, "paid", "unpaid")
#' stata_cond(wage, 1, 0, missing = NA)  # Stata: cond(wage, 1, 0, .)
#'
#' # A logical NA is rejected:
#' try(stata_cond(wage > 0, 1, 0))
stata_cond <- function(condition, yes, no, missing) {
  fn <- "stata_cond"
  has_missing <- !base::missing(missing)

  pc <- sc_prepare(condition, fn, "condition")
  if (pc$kind != "numeric") {
    sc_abort(fn, sprintf(paste(
      "`condition` must be numeric or logical, not %s. In Stata, a string",
      "condition is a type mismatch."), pc$kind), "statacompat_error_type")
  }
  cv <- pc$value
  if (is.logical(cv) && anyNA(cv)) {
    sc_abort(fn, paste(
      "`condition` is a logical vector containing NA. Stata conditions are",
      "never logical NA: build the condition with stata_eq(), stata_gt(),",
      "etc., which never return NA, or use as.numeric(condition) to apply",
      "Stata's rule that a missing number counts as true."),
      "statacompat_error_na_condition")
  }

  branches <- list(yes = yes, no = no)
  if (has_missing) {
    branches$missing <- missing
  }
  bargs <- names(branches)
  preps <- Map(sc_prepare, branches, fn, bargs)
  kind <- sc_common_kind(preps, fn, bargs)

  lens <- c(length(cv), vapply(preps, function(p) length(p$value), integer(1)))
  n <- sc_common_size(lens, fn, c("condition", bargs))
  cv <- sc_recycle(cv, n)

  is_miss <- is.na(cv)
  is_true <- !is_miss & cv != 0
  is_false <- !is_miss & cv == 0
  if (!has_missing) {
    is_true <- is_true | is_miss
    is_miss <- logical(n)
  }

  values <- lapply(preps, `[[`, "value")
  if (kind == "string") {
    out <- character(n)
  } else if (kind == "numeric") {
    types <- vapply(values, typeof, character(1))
    out <- if (all(types == "logical")) {
      logical(n)
    } else if (all(types %in% c("logical", "integer"))) {
      integer(n)
    } else {
      double(n)
    }
  } else {
    out <- double(n)
  }

  fill <- function(out, where, value) {
    if (any(where)) {
      out[where] <- sc_recycle(value, n)[where]
    }
    out
  }
  out <- fill(out, is_true, values$yes)
  out <- fill(out, is_false, values$no)
  if (has_missing) {
    out <- fill(out, is_miss, values$missing)
  }

  if (kind == "Date") {
    out <- structure(out, class = "Date")
  } else if (kind == "POSIXct") {
    typed <- branches[vapply(preps, function(p) !p$untyped, logical(1))]
    tz <- attr(typed[[1L]], "tzone")
    out <- structure(out, class = c("POSIXct", "POSIXt"), tzone = tz)
  }
  out
}
