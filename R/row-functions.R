#' Stata's `egen` row functions
#'
#' Row-wise totals, means and missing-value counts across several
#' variables, reproducing `egen` `rowtotal()`, `rowmean()`, `rowmiss()` and
#' `rownonmiss()`.
#'
#' @section Semantics:
#' The table and the rules below follow Stata manual \[D\] egen, except
#' where noted. Rejecting dates is a statacompat design choice.
#'
#' | Row        | `rowtotal` | `rowtotal(missing = TRUE)` | `rowmean` | `rowmiss` | `rownonmiss` |
#' |------------|-----------:|---------------------------:|----------:|----------:|-------------:|
#' | `1, 2, 3`  | 6          | 6                          | 2         | 0         | 3            |
#' | `1, NA, 3` | 4          | 4                          | 2         | 1         | 2            |
#' | `NA, NA, NA` | **0**    | `NA`                       | `NA`      | 3         | 0            |
#'
#' * `stata_rowtotal()` treats missing values as 0, so a row with no
#'   non-missing values totals **0** by default. `missing = TRUE`
#'   corresponds to Stata's `missing` option and returns `NA` for such rows.
#' * `stata_rowmean()` ignores missing values and returns `NA` (not `NaN`)
#'   when every value in the row is missing.
#' * `stata_rowmiss()` accepts strings without any option and counts `""`
#'   as missing. \[D\] egen restricts string variables only for
#'   `rownonmiss()` (through `strok`) and states no such restriction for
#'   `rowmiss()`; this behaviour is inferred from that.
#' * `stata_rownonmiss()` rejects strings unless `strok = TRUE`, matching
#'   Stata's `strok` option; `""` then counts as missing.
#' * `stata_rowtotal()` and `stata_rowmean()` reject strings, as Stata does
#'   (type mismatch). Dates are also rejected, since summing dates is rarely
#'   meaningful; use `as.numeric()` if you really need it.
#' * Missing means `NA` or `NaN` (including `haven::tagged_na()`), and for
#'   strings `""` or `NA_character_`.
#'
#' @section Precision:
#' Results are doubles. Stata's default storage type for new variables is
#' `float` (see `set type`), and `egen` without an explicit type is assumed
#' to follow it. A `float` holds integers exactly only up to 16,777,216, so
#' 16,777,217 cannot be stored and becomes 16,777,216. `stata_rowtotal()`
#' gives 16,777,217, which corresponds to
#' `egen double t = rowtotal(a b)`. Totals of Rupiah amounts and other
#' large values can therefore differ from Stata output created without
#' `double`. statacompat does not emulate `float` storage.
#'
#' @section Inputs:
#' Pass the variables either as separate vectors or as data frames, whose
#' columns are used as variables, or a mixture. A data frame is never
#' treated as a single vector. Inside `dplyr::mutate()`, use
#' `dplyr::pick()` to select columns, e.g.
#' `stata_rowtotal(dplyr::pick(r1:r5))`. All variables must have the same
#' length; scalars are not recycled. Matrices are rejected; convert them
#' with [as.data.frame()].
#'
#' @param ... Vectors and/or data frames: the variables of the row
#'   operation.
#' @param missing For `stata_rowtotal()`: if `TRUE`, rows in which every
#'   value is missing get `NA` instead of 0 (Stata's `missing` option).
#' @param strok For `stata_rownonmiss()`: allow string variables (Stata's
#'   `strok` option).
#' @return `stata_rowtotal()` and `stata_rowmean()` return a double
#'   vector. `stata_rowmiss()` and `stata_rownonmiss()` return an integer
#'   vector. Every result has one element per row.
#' @name stata_row
#' @examples
#' r1 <- c(1, 1, NA)
#' r2 <- c(2, NA, NA)
#' r3 <- c(3, 3, NA)
#'
#' stata_rowtotal(r1, r2, r3)
#' stata_rowtotal(r1, r2, r3, missing = TRUE)
#' stata_rowmean(r1, r2, r3)
#' stata_rowmiss(r1, r2, r3)
#' stata_rownonmiss(r1, r2, r3)
#'
#' # base R gives NaN for the all-missing row
#' rowMeans(cbind(r1, r2, r3), na.rm = TRUE)
#'
#' # data frames and strings
#' df <- data.frame(x = c(1, NA), s = c("a", ""))
#' stata_rowmiss(df)
#' stata_rownonmiss(df, strok = TRUE)
#'
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   dta <- data.frame(r1 = r1, r2 = r2, r3 = r3)
#'   dplyr::mutate(dta, total = stata_rowtotal(dplyr::pick(r1:r3)))
#' }
NULL

#' @rdname stata_row
#' @export
stata_rowtotal <- function(..., missing = FALSE) {
  fn <- "stata_rowtotal"
  sc_check_flag(missing, fn, "missing")
  cols <- sc_row_columns(list(...), fn)
  sc_row_require_numeric(cols, fn)
  acc <- sc_row_accumulate(cols)
  total <- acc$total
  if (missing) {
    total[acc$nonmissing == 0L] <- NA_real_
  }
  total
}

#' @rdname stata_row
#' @export
stata_rowmean <- function(...) {
  fn <- "stata_rowmean"
  cols <- sc_row_columns(list(...), fn)
  sc_row_require_numeric(cols, fn)
  acc <- sc_row_accumulate(cols)
  out <- acc$total / acc$nonmissing
  out[acc$nonmissing == 0L] <- NA_real_
  out
}

#' @rdname stata_row
#' @export
stata_rowmiss <- function(...) {
  fn <- "stata_rowmiss"
  cols <- sc_row_columns(list(...), fn)
  sc_row_count_missing(cols)
}

#' @rdname stata_row
#' @export
stata_rownonmiss <- function(..., strok = FALSE) {
  fn <- "stata_rownonmiss"
  sc_check_flag(strok, fn, "strok")
  cols <- sc_row_columns(list(...), fn)
  if (!strok) {
    is_str <- vapply(cols$preps, function(p) p$kind == "string", logical(1))
    if (any(is_str)) {
      sc_abort(fn, paste0(
        "string variable(s) ", paste0("`", cols$names[is_str], "`",
                                      collapse = ", "),
        " are not allowed unless `strok = TRUE`, as in Stata."),
        "statacompat_error_type")
    }
  }
  length(cols$preps) - sc_row_count_missing(cols)
}

# ---- internals --------------------------------------------------------------

# Flatten `...` into a list of prepared columns of equal length.
sc_row_columns <- function(dots, fn) {
  if (length(dots) == 0L) {
    sc_abort(fn, "supply at least one variable.", "statacompat_error_size")
  }
  arg_names <- names(dots)
  if (is.null(arg_names)) {
    arg_names <- character(length(dots))
  }
  preps <- list()
  names <- character()
  for (i in seq_along(dots)) {
    d <- dots[[i]]
    label <- if (nzchar(arg_names[i])) arg_names[i] else paste0("..", i)
    if (is.data.frame(d)) {
      for (j in seq_along(d)) {
        nm <- names(d)[j]
        preps[[length(preps) + 1L]] <- sc_prepare(d[[j]], fn, nm)
        names[length(names) + 1L] <- nm
      }
    } else if (!is.null(dim(d))) {
      sc_abort(fn, sprintf(paste(
        "`%s` is a matrix or array. Pass vectors or a data frame",
        "(e.g. as.data.frame(m)) so that each column is one variable."),
        label), "statacompat_error_type")
    } else {
      preps[[length(preps) + 1L]] <- sc_prepare(d, fn, label)
      names[length(names) + 1L] <- label
    }
  }
  if (length(preps) == 0L) {
    sc_abort(fn, "no variables supplied (the data frame has no columns).",
             "statacompat_error_size")
  }
  lens <- vapply(preps, function(p) length(p$value), integer(1))
  if (length(unique(lens)) > 1L) {
    desc <- paste(sprintf("`%s` has length %d", names, lens), collapse = ", ")
    sc_abort(fn, paste0(
      "all variables must have the same length; ", desc, "."),
      "statacompat_error_size")
  }
  list(preps = preps, names = names, n = lens[[1L]])
}

sc_row_require_numeric <- function(cols, fn) {
  kinds <- vapply(cols$preps, `[[`, "", "kind")
  bad <- kinds != "numeric"
  if (any(bad)) {
    sc_abort(fn, paste0(
      "type mismatch: ",
      paste(sprintf("`%s` is %s", cols$names[bad], kinds[bad]),
            collapse = ", "),
      ". Only numeric variables can be totalled or averaged."),
      "statacompat_error_type")
  }
  invisible()
}

sc_row_accumulate <- function(cols) {
  total <- double(cols$n)
  nonmissing <- integer(cols$n)
  for (p in cols$preps) {
    v <- as.double(p$value)
    ok <- !is.na(v)
    total[ok] <- total[ok] + v[ok]
    nonmissing <- nonmissing + ok
  }
  list(total = total, nonmissing = nonmissing)
}

sc_row_count_missing <- function(cols) {
  count <- integer(cols$n)
  for (p in cols$preps) {
    count <- count + sc_is_missing(p)
  }
  count
}
