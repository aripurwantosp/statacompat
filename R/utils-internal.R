# Internal helpers shared by all exported functions. None are exported.
#
# Vocabulary used throughout:
#   "kind"    the Stata-relevant type of an input after normalisation:
#             "numeric" (double, integer, logical, numeric haven_labelled),
#             "string"  (character, character haven_labelled),
#             "Date", "POSIXct" (kept apart so they are never compared with
#             plain numbers; Stata's date origin is 1960, R's is 1970).
#   "untyped" a logical vector whose elements are all NA, e.g. a bare `NA`.
#             It stands for Stata's `.` and may be paired with any
#             numeric-like kind.

# ---- conditions -------------------------------------------------------------

sc_abort <- function(fn, message, class = NULL) {
  cnd <- structure(
    class = c(class, "statacompat_error", "error", "condition"),
    list(message = paste0("`", fn, "()`: ", message), call = NULL)
  )
  stop(cnd)
}

# ---- type normalisation -----------------------------------------------------

sc_strip_attributes <- function(x) {
  attributes(x) <- NULL
  x
}

# Returns list(kind, value, untyped). `value` is a bare atomic vector.
sc_prepare <- function(x, fn, arg) {
  if (is.null(x)) {
    sc_abort(fn, sprintf("`%s` must not be NULL.", arg),
             "statacompat_error_type")
  }
  if (is.factor(x)) {
    sc_abort(fn, sprintf(paste(
      "`%s` is a factor. Stata has no factor type; convert it explicitly",
      "with as.character() or as.integer() (codes) first."), arg),
      "statacompat_error_type")
  }
  if (inherits(x, "haven_labelled_spss")) {
    sc_abort(fn, sprintf(paste(
      "`%s` is an SPSS-style labelled vector (haven_labelled_spss),",
      "whose user-defined missing values have no Stata equivalent."), arg),
      "statacompat_error_type")
  }
  if (inherits(x, "haven_labelled")) {
    x <- sc_strip_attributes(x)
  }
  if (inherits(x, "Date")) {
    return(list(kind = "Date", value = as.double(sc_strip_attributes(x)),
                untyped = FALSE))
  }
  if (inherits(x, "POSIXct")) {
    return(list(kind = "POSIXct", value = as.double(sc_strip_attributes(x)),
                untyped = FALSE))
  }
  if (is.object(x) || !is.atomic(x) || is.complex(x) || is.raw(x)) {
    sc_abort(fn, sprintf(
      "`%s` has unsupported class <%s>.", arg,
      paste(class(x), collapse = "/")), "statacompat_error_type")
  }
  x <- sc_strip_attributes(x)
  if (is.character(x)) {
    return(list(kind = "string", value = x, untyped = FALSE))
  }
  untyped <- is.logical(x) && all(is.na(x))
  list(kind = "numeric", value = x, untyped = untyped)
}

# Resolve one common kind for a set of prepared inputs, or abort.
sc_common_kind <- function(preps, fn, args) {
  kinds <- vapply(preps, `[[`, "", "kind")
  untyped <- vapply(preps, `[[`, FALSE, "untyped")
  typed <- unique(kinds[!untyped])
  if (length(typed) == 0L) {
    return("numeric")
  }
  if (length(typed) > 1L) {
    desc <- paste(sprintf("`%s` is %s", args[!untyped], kinds[!untyped]),
                  collapse = ", ")
    sc_abort(fn, paste0(
      "type mismatch: ", desc, ". Stata does not compare strings with ",
      "numbers, and dates are only compared with dates of the same class."),
      "statacompat_error_type")
  }
  if (typed == "string" && any(untyped)) {
    sc_abort(fn, paste0(
      "type mismatch: `", paste(args[untyped], collapse = "`, `"),
      "` is a numeric missing value (NA) but the other inputs are ",
      "strings. Stata's missing string is \"\"."),
      "statacompat_error_type")
  }
  typed
}

# ---- sizes ------------------------------------------------------------------

# Common size under strict recycling: every length must be 1 or n.
sc_common_size <- function(lens, fn, args) {
  other <- unique(lens[lens != 1L])
  if (length(other) > 1L) {
    desc <- paste(sprintf("`%s` has length %d", args, lens), collapse = ", ")
    sc_abort(fn, paste0(
      "inputs must have length 1 or a common length; ", desc, "."),
      "statacompat_error_size")
  }
  if (length(other) == 1L) other else 1L
}

sc_recycle <- function(x, n) {
  if (length(x) == n) x else rep_len(x, n)
}

# ---- extended (tagged) missing values ---------------------------------------

# haven stores Stata's .a-.z as an R NA whose 5th byte (little endian)
# holds the tag letter; R's own NA_real_ and NaN have 0 there. Read the
# bytes directly so haven is not required at run time. Returns an integer
# vector: 0 for ordinary missing / non-missing, the tag byte otherwise.
sc_na_tag <- function(x) {
  out <- integer(length(x))
  if (!is.double(x)) {
    return(out)
  }
  idx <- which(is.na(x) & !is.nan(x))
  if (length(idx)) {
    bytes <- writeBin(x[idx], raw(), size = 8L, endian = "little")
    out[idx] <- as.integer(bytes[seq.int(5L, by = 8L, length.out = length(idx))])
  }
  out
}

# Abort if any position compares two missing values of different kinds
# (e.g. `.` versus `.a`): Stata distinguishes them, R's NA cannot.
sc_check_tags <- function(x, y, both_missing, fn) {
  if (!any(both_missing)) {
    return(invisible())
  }
  tx <- sc_na_tag(x)[both_missing]
  ty <- sc_na_tag(y)[both_missing]
  if (any(tx != ty)) {
    sc_abort(fn, paste(
      "an extended missing value (.a-.z, from haven::tagged_na()) is",
      "compared with a different missing value. Stata treats these as",
      "unequal and ordered (. < .a < ... < .z); statacompat 1.0 does",
      "not reproduce that ordering. Convert extended missing values to",
      "ordinary NA first, e.g. `x[is.na(x)] <- NA_real_`."),
      "statacompat_error_tagged_na")
  }
  invisible()
}

# ---- strings ----------------------------------------------------------------

# Stata orders strings by byte value ([U] 12.4.2). For UTF-8 this equals
# code-point order. R's `<` is locale dependent and radix sort is only
# byte-wise for ASCII in every locale, so non-ASCII strings are compared
# through a fixed-width hexadecimal encoding of their UTF-8 bytes.
sc_string_rank <- function(values) {
  values <- enc2utf8(values)
  u <- unique(values)
  ascii <- !grepl("[^\x01-\x7F]", u, perl = TRUE, useBytes = TRUE)
  if (all(ascii)) {
    key <- u
  } else {
    key <- vapply(u, function(s) {
      paste(as.character(charToRaw(s)), collapse = "")
    }, character(1), USE.NAMES = FALSE)
  }
  rank_u <- match(key, sort(unique(key), method = "radix"))
  rank_u[match(values, u)]
}

sc_check_no_na_string <- function(value, fn, arg) {
  if (anyNA(value)) {
    sc_abort(fn, sprintf(paste(
      "`%s` contains NA_character_. Stata's missing string is \"\";",
      "R's NA_character_ has no Stata equivalent in comparisons.",
      "Recode it explicitly, e.g. `x[is.na(x)] <- \"\"`."), arg),
      "statacompat_error_na_character")
  }
  invisible()
}

# ---- three-way comparison ---------------------------------------------------

# Returns an integer vector of -1/0/1 (x < y, x == y, x > y) under Stata
# ordering, where numeric missing is larger than every number.
sc_compare3 <- function(x, y, fn, xarg = "x", yarg = "y") {
  px <- sc_prepare(x, fn, xarg)
  py <- sc_prepare(y, fn, yarg)
  kind <- sc_common_kind(list(px, py), fn, c(xarg, yarg))
  n <- sc_common_size(c(length(px$value), length(py$value)), fn,
                      c(xarg, yarg))
  xv <- sc_recycle(px$value, n)
  yv <- sc_recycle(py$value, n)

  if (kind == "string") {
    sc_check_no_na_string(xv, fn, xarg)
    sc_check_no_na_string(yv, fn, yarg)
    r <- sc_string_rank(c(xv, yv))
    rx <- r[seq_len(n)]
    ry <- r[n + seq_len(n)]
    return((rx > ry) - (rx < ry))
  }

  xm <- is.na(xv)
  ym <- is.na(yv)
  sc_check_tags(xv, yv, xm & ym, fn)
  out <- integer(n)
  out[xm & !ym] <- 1L
  out[!xm & ym] <- -1L
  ok <- !xm & !ym
  out[ok] <- (xv[ok] > yv[ok]) - (xv[ok] < yv[ok])
  out
}

sc_check_flag <- function(value, fn, arg) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    sc_abort(fn, sprintf("`%s` must be TRUE or FALSE.", arg),
             "statacompat_error_type")
  }
  invisible()
}
