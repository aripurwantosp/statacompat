#' statacompat: Reproduce Selected Stata Expression and Missing-Value Semantics
#'
#' Vectorised helpers that make Stata-to-R translations behave like the
#' original Stata expressions, particularly where missing values would
#' otherwise silently change results.
#'
#' Scope of version 1.0:
#' * comparisons: [stata_eq()], [stata_ne()], [stata_gt()], [stata_ge()],
#'   [stata_lt()], [stata_le()];
#' * membership and ranges: [stata_inlist()], [stata_inrange()];
#' * conditional values: [stata_cond()];
#' * missing values: [stata_missing()];
#' * `egen` row functions: [stata_rowtotal()], [stata_rowmean()],
#'   [stata_rowmiss()], [stata_rownonmiss()].
#'
#' Ordinary numeric missing values (`NA`, `NaN`) stand for Stata's `.`.
#' Extended missing values `.a`-`.z` are not fully supported: they are
#' detected and rejected wherever they would change a result. See
#' `vignette("stata-and-r-semantics", package = "statacompat")`.
#'
#' Each rule is taken from Stata's manuals, and every help page cites the
#' relevant section. Where the manuals leave a case open, the help page says
#' so and describes the behaviour that statacompat implements.
#'
#' @keywords internal
"_PACKAGE"
