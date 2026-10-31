#' Stata's `missing()`: is any argument missing?
#'
#' `stata_missing(x1, x2, ...)` reproduces Stata's `missing(x1, x2, ...)`:
#' `TRUE` where at least one argument is missing.
#'
#' @section What counts as missing:
#' * **Numbers and dates:** `NA` and `NaN`, including extended missing
#'   values from `haven::tagged_na()` (Stata `.`, `.a`-`.z`). `Inf` is not
#'   missing.
#' * **Strings:** the empty string `""`, Stata's missing string. A string of
#'   spaces such as `" "` is **not** missing, because `""` is Stata's only
#'   missing string.
#'   `NA_character_`, which cannot occur in Stata data, is also counted as
#'   missing so that data created in R are handled sensibly.
#'
#' Arguments may mix numbers and strings, as in Stata. Each must have
#' length 1 or a common length. Factors are rejected.
#'
#' @param ... One or more vectors.
#' @return A logical vector without `NA`s.
#' @seealso [stata_rowmiss()] to count missing values per row.
#' @export
#' @examples
#' stata_missing(c(1, NA, NaN, Inf))
#' stata_missing(c("a", "", " "))
#'
#' # TRUE where either is missing (Stata: missing(age, name))
#' stata_missing(c(30, NA, 40), c("Ani", "Budi", ""))
stata_missing <- function(...) {
  fn <- "stata_missing"
  dots <- list(...)
  if (length(dots) == 0L) {
    sc_abort(fn, "supply at least one argument.", "statacompat_error_size")
  }
  args <- paste0("..", seq_along(dots))
  preps <- Map(sc_prepare, dots, fn, args)
  n <- sc_common_size(vapply(preps, function(p) length(p$value), integer(1)),
                      fn, args)
  out <- logical(n)
  for (p in preps) {
    out <- out | sc_recycle(sc_is_missing(p), n)
  }
  out
}

sc_is_missing <- function(prep) {
  if (prep$kind == "string") {
    is.na(prep$value) | prep$value == ""
  } else {
    is.na(prep$value)
  }
}
