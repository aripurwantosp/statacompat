#' Stata's `inlist()`: is a value equal to any of the candidates?
#'
#' `stata_inlist(x, a, b, ...)` reproduces Stata's `inlist(z, a, b, ...)`:
#' it is `TRUE` where `x` equals at least one candidate, using the same
#' equality as [stata_eq()].
#'
#' @section Semantics:
#' * **Missing values are ordinary members.** `stata_inlist(NA, 1, 2)` is
#'   `FALSE` and `stata_inlist(NA, 1, NA)` is `TRUE`. R's `NA == 1` would
#'   instead give `NA`. Stata's manual does not discuss missing values in
#'   `inlist()` explicitly; this rule is inferred from the fact that
#'   `inlist()` uses the same equality as `==`, under which `. == .` is true
#'   (\[U\] 13.2.3).
#' * **Candidates are evaluated row by row.** Each candidate may be a single
#'   value or a vector as long as `x` (a "variable"). With a variable
#'   candidate, row `i` of `x` is compared with row `i` of the candidate,
#'   exactly as Stata evaluates `inlist(x, a, b)` observation by
#'   observation.
#' * **A vector of codes is not a set.** `stata_inlist(x, codes)` with
#'   `codes <- c(1, 2, 3)` does *not* ask whether `x` is one of 1, 2, 3; it
#'   is an error unless `length(codes)` is 1 or `length(x)`, in which case
#'   `codes` is treated as a variable. To test membership in a vector of
#'   codes, splice it:
#'   `do.call(stata_inlist, c(list(x), as.list(codes)))`.
#' * **Types.** All arguments must be numbers, or all strings (Stata's rule),
#'   or all dates of one class. `NA_character_` is an error; Stata's missing
#'   string is `""`, which is an ordinary member.
#' * **Extended missing values.** Comparing an extended missing value
#'   (`.a`-`.z`, see [stata_comparison]) with a different missing value is
#'   an error.
#' * Stata limits `inlist()` to 250 numeric or 10 string arguments. That is
#'   a Stata implementation limit and is not enforced here.
#'
#' @param x A vector.
#' @param ... One or more candidate values, each of length 1 or
#'   `length(x)`.
#' @return A logical vector without `NA`s.
#' @seealso [stata_eq()], [stata_inrange()]
#' @export
#' @examples
#' kbli <- c(1, 4, NA, 3)
#' stata_inlist(kbli, 1, 2, 3)
#' stata_inlist(kbli, 1, NA)     # Stata: inlist(kbli, 1, .)
#'
#' # A vector of codes must be spliced:
#' codes <- c(1, 2, 3)
#' do.call(stata_inlist, c(list(kbli), as.list(codes)))
#'
#' stata_inlist(c("a", "", "c"), "a", "")
stata_inlist <- function(x, ...) {
  fn <- "stata_inlist"
  cands <- list(...)
  if (length(cands) == 0L) {
    sc_abort(fn, "supply at least one candidate value in `...`.",
             "statacompat_error_size")
  }
  args <- c("x", paste0("..", seq_along(cands)))
  preps <- c(list(sc_prepare(x, fn, "x")),
             Map(sc_prepare, cands, fn, args[-1L]))
  sc_common_kind(preps, fn, args)

  lens <- vapply(preps, function(p) length(p$value), integer(1))
  other <- unique(lens[lens != 1L])
  if (length(other) > 1L) {
    desc <- paste(sprintf("`%s` has length %d", args, lens), collapse = ", ")
    sc_abort(fn, paste0(
      "inputs must have length 1 or a common length; ", desc, ". ",
      "Each candidate is a single value or a variable as long as `x`. ",
      "To test membership in a vector of codes, use ",
      "`do.call(stata_inlist, c(list(x), as.list(codes)))`."),
      "statacompat_error_size")
  }
  n <- if (length(other) == 1L) other else 1L

  out <- logical(n)
  for (j in seq_along(cands)) {
    out <- out | (sc_compare3(x, cands[[j]], fn, "x", args[j + 1L]) == 0L)
  }
  out
}

#' Stata's `inrange()`: is a value known to lie in a closed interval?
#'
#' `stata_inrange(x, lower, upper)` reproduces Stata's `inrange(z, a, b)`,
#' which is `TRUE` "if it is known that a <= z <= b" (Stata manual \[FN\]). Both bounds are
#' inclusive, a missing `x` is never in range, and a **missing bound means
#' "unbounded" on that side**.
#'
#' @section Semantics:
#' Stata applies these rules in order (Stata manual \[FN\] Programming functions):
#'
#' 1. `x` missing: `FALSE`.
#' 2. `lower` and `upper` missing: `TRUE`.
#' 3. `lower` missing: `x <= upper`.
#' 4. `upper` missing: `lower <= x`.
#' 5. Otherwise: `lower <= x & x <= upper`.
#'
#' So `stata_inrange(age, 15, NA)` means "age 15 or older, and not missing",
#' which differs from both `age >= 15` in Stata (true for missing age) and
#' `age >= 15` in R (`NA` for missing age). Reversed bounds give `FALSE`.
#' Because rules 2-4
#' only ask *whether* a bound is missing, extended missing values (`.a`-`.z`)
#' behave exactly like `.` here.
#'
#' **Strings.** `""` is Stata's missing string: a `""` value of `x` is never
#' in range, and a `""` bound is unbounded.
#' Strings are ordered by their UTF-8 bytes. Stata's manual adds that for
#' strings `"."` is interpreted as `""`. statacompat does **not** implement
#' that rule, so `"."` is treated as an ordinary string.
#' `NA_character_` is an error.
#'
#' **Types and lengths.** `x`, `lower` and `upper` must be of one kind
#' (numbers, strings, or dates of one class; a bare `NA` bound is allowed
#' with numbers and dates). Each must have length 1 or a common length;
#' bounds may therefore vary by row.
#'
#' @param x A vector.
#' @param lower,upper Inclusive bounds; `NA` (or `""` for strings) means
#'   unbounded.
#' @return A logical vector without `NA`s.
#' @seealso [stata_inlist()], [stata_comparison]
#' @export
#' @examples
#' age <- c(10, 15, 64, 65, NA)
#' stata_inrange(age, 15, 64)
#'
#' # Open-ended range: 15 and older, missing excluded
#' stata_inrange(age, 15, NA)
#'
#' # Compare: Stata's `age >= 15` is TRUE for the missing age
#' stata_ge(age, 15)
#'
#' stata_inrange(c("m", "", "z"), "a", "n")
stata_inrange <- function(x, lower, upper) {
  fn <- "stata_inrange"
  args <- c("x", "lower", "upper")
  preps <- Map(sc_prepare, list(x, lower, upper), fn, args)
  kind <- sc_common_kind(preps, fn, args)
  n <- sc_common_size(vapply(preps, function(p) length(p$value), integer(1)),
                      fn, args)
  xv <- sc_recycle(preps[[1L]]$value, n)
  lv <- sc_recycle(preps[[2L]]$value, n)
  uv <- sc_recycle(preps[[3L]]$value, n)

  if (kind == "string") {
    sc_check_no_na_string(xv, fn, "x")
    sc_check_no_na_string(lv, fn, "lower")
    sc_check_no_na_string(uv, fn, "upper")
    xm <- xv == ""
    lm <- lv == ""
    um <- uv == ""
    r <- sc_string_rank(c(xv, lv, uv))
    xv <- r[seq_len(n)]
    lv <- r[n + seq_len(n)]
    uv <- r[2L * n + seq_len(n)]
  } else {
    xm <- is.na(xv)
    lm <- is.na(lv)
    um <- is.na(uv)
  }

  ok <- !xm
  lower_ok <- rep_len(TRUE, n)
  idx <- ok & !lm
  lower_ok[idx] <- lv[idx] <= xv[idx]
  upper_ok <- rep_len(TRUE, n)
  idx <- ok & !um
  upper_ok[idx] <- xv[idx] <= uv[idx]
  ok & lower_ok & upper_ok
}
