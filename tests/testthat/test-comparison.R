# Expected values are typed from Stata's documentation ([U] 12.2.1,
# [U] 13.2.3, [U] 12.4.2). They are not computed by the package.

test_that("scalar comparisons with missing follow Stata ordering", {
  # Stata: . == 1, . != 1, . > 1, . >= 1, . < 1, . <= 1
  expect_identical(stata_eq(NA, 1), FALSE)
  expect_identical(stata_ne(NA, 1), TRUE)
  expect_identical(stata_gt(NA, 1), TRUE)
  expect_identical(stata_ge(NA, 1), TRUE)
  expect_identical(stata_lt(NA, 1), FALSE)
  expect_identical(stata_le(NA, 1), FALSE)
  # missing on the right
  expect_identical(stata_eq(1, NA), FALSE)
  expect_identical(stata_ne(1, NA), TRUE)
  expect_identical(stata_gt(1, NA), FALSE)
  expect_identical(stata_ge(1, NA), FALSE)
  expect_identical(stata_lt(1, NA), TRUE)
  expect_identical(stata_le(1, NA), TRUE)
  # both missing
  expect_identical(stata_eq(NA, NA), TRUE)
  expect_identical(stata_ne(NA, NA), FALSE)
  expect_identical(stata_gt(NA, NA), FALSE)
  expect_identical(stata_ge(NA, NA), TRUE)
  expect_identical(stata_lt(NA, NA), FALSE)
  expect_identical(stata_le(NA, NA), TRUE)
})

test_that("ordinary values compare as usual", {
  x <- c(0, 1, 2)
  expect_identical(stata_eq(x, 1), c(FALSE, TRUE, FALSE))
  expect_identical(stata_ne(x, 1), c(TRUE, FALSE, TRUE))
  expect_identical(stata_gt(x, 1), c(FALSE, FALSE, TRUE))
  expect_identical(stata_ge(x, 1), c(FALSE, TRUE, TRUE))
  expect_identical(stata_lt(x, 1), c(TRUE, FALSE, FALSE))
  expect_identical(stata_le(x, 1), c(TRUE, TRUE, FALSE))
  expect_identical(stata_lt(-1e300, 1e300), TRUE)
})

test_that("vectorised mixed missing / non-missing (audit data)", {
  x <- c(0, 1, 2, NA)
  expect_identical(stata_ne(x, 1), c(TRUE, FALSE, TRUE, TRUE))
  expect_identical(stata_gt(x, 1), c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(stata_ge(x, 1), c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(stata_eq(x, NA), c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(stata_ne(x, NA), c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(stata_gt(x, NA), c(FALSE, FALSE, FALSE, FALSE))
  expect_identical(stata_ge(x, NA), c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(stata_lt(x, NA), c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(stata_le(x, NA), c(TRUE, TRUE, TRUE, TRUE))
  y <- c(NA, 1, NA, 3)
  expect_identical(stata_lt(x, y), c(TRUE, FALSE, TRUE, FALSE))
  expect_identical(stata_eq(c(NA, NA, 1), c(NA, 1, NA)), c(TRUE, FALSE, FALSE))
})

test_that("results never contain NA and drop attributes", {
  x <- c(a = 1, b = NA)
  for (f in list(stata_eq, stata_ne, stata_gt, stata_ge, stata_lt, stata_le)) {
    r <- f(x, c(NA, 2))
    expect_false(anyNA(r))
    expect_null(names(r))
    expect_type(r, "logical")
  }
})

test_that("NaN is Stata's `.`; Inf is an ordinary number", {
  expect_identical(stata_eq(NaN, NA), TRUE)
  expect_identical(stata_gt(NaN, 1e308), TRUE)
  expect_identical(stata_gt(NA, Inf), TRUE)
  expect_identical(stata_lt(Inf, NA), TRUE)
  expect_identical(stata_gt(Inf, 1e308), TRUE)
  expect_identical(stata_lt(-Inf, -1e308), TRUE)
  expect_identical(stata_eq(Inf, Inf), TRUE)
})

test_that("integer, logical and labelled inputs are numbers", {
  expect_identical(stata_eq(1L, 1), TRUE)
  expect_identical(stata_gt(NA_integer_, 5L), TRUE)
  expect_identical(stata_eq(TRUE, 1), TRUE)
  expect_identical(stata_lt(FALSE, TRUE), TRUE)
  skip_if_not_installed("haven")
  lab <- haven::labelled(c(1, 2, NA), c(Yes = 1, No = 2))
  expect_identical(stata_eq(lab, 1), c(TRUE, FALSE, FALSE))
  expect_identical(stata_gt(lab, 1), c(FALSE, TRUE, TRUE))
})

test_that("zero-length and scalar recycling", {
  expect_identical(stata_eq(numeric(0), 1), logical(0))
  expect_identical(stata_gt(1, numeric(0)), logical(0))
  expect_identical(stata_eq(numeric(0), numeric(0)), logical(0))
  expect_identical(stata_eq(5, c(5, 6, NA)), c(TRUE, FALSE, FALSE))
})

test_that("incompatible lengths are an error, not silent recycling", {
  expect_error(stata_eq(1:3, 1:2), class = "statacompat_error_size")
  expect_error(stata_gt(1:4, 1:2), class = "statacompat_error_size")
  expect_error(stata_eq(numeric(0), 1:2), class = "statacompat_error_size")
})

test_that("type mismatches are errors", {
  expect_error(stata_eq("1", 1), class = "statacompat_error_type")
  expect_error(stata_gt(1, "a"), class = "statacompat_error_type")
  expect_error(stata_eq("a", NA), class = "statacompat_error_type")
  expect_error(stata_eq(factor("a"), "a"), class = "statacompat_error_type")
  expect_error(stata_eq(list(1), 1), class = "statacompat_error_type")
  expect_error(stata_eq(NULL, 1), class = "statacompat_error_type")
  expect_error(stata_eq(1i, 1), class = "statacompat_error_type")
  expect_error(stata_eq(as.Date("2020-01-01"), 18262),
               class = "statacompat_error_type")
  expect_error(stata_eq(as.Date("2020-01-01"),
                        as.POSIXct("2020-01-01", tz = "UTC")),
               class = "statacompat_error_type")
})

test_that("dates compare with dates and with a bare NA", {
  d <- as.Date(c("2020-01-01", NA, "2021-06-30"))
  ref <- as.Date("2020-12-31")
  expect_identical(stata_gt(d, ref), c(FALSE, TRUE, TRUE))
  expect_identical(stata_eq(d, NA), c(FALSE, TRUE, FALSE))
  t <- as.POSIXct(c("2020-01-01 10:00", NA), tz = "UTC")
  expect_identical(stata_lt(t, as.POSIXct("2020-01-01 12:00", tz = "UTC")),
                   c(TRUE, FALSE))
})

test_that("strings use byte order regardless of locale", {
  # Stata: "a" > "B" is 1, "a" > "A" is 1 ([U] 12.4.2)
  expect_identical(stata_gt("a", "B"), TRUE)
  expect_identical(stata_gt("a", "A"), TRUE)
  expect_identical(stata_lt("Z", "a"), TRUE)
  expect_identical(stata_lt("", "a"), TRUE)
  expect_identical(stata_eq("", ""), TRUE)
  expect_identical(stata_gt("abc", "ab"), TRUE)
  expect_identical(stata_gt(e_acute(), "z"), TRUE)
  expect_identical(stata_gt(a_macron(), e_acute()), TRUE)
  expect_identical(stata_eq(c("a", "b", ""), "b"), c(FALSE, TRUE, FALSE))
  latin1 <- iconv(e_acute(), "UTF-8", "latin1")
  expect_identical(stata_eq(latin1, e_acute()), TRUE)
})

test_that("string order is identical in different locales", {
  skip_if_not_installed("withr")
  x <- c("a", "B", "", "Z", "aa", e_acute())
  y <- c("B", "a", "a", "z", "a", "z")
  expected <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)
  # restore the current collation locale when the test finishes
  withr::local_locale(c(LC_COLLATE = Sys.getlocale("LC_COLLATE")))
  tested <- 0L
  for (loc in c("C", "C.UTF-8", "en_US.UTF-8", "id_ID.UTF-8")) {
    if (suppressWarnings(Sys.setlocale("LC_COLLATE", loc)) == "") next
    tested <- tested + 1L
    expect_identical(stata_gt(x, y), expected, info = loc)
  }
  expect_gt(tested, 0L)
})

test_that("NA_character_ is rejected in string comparisons", {
  expect_error(stata_eq(c("a", NA), "a"),
               class = "statacompat_error_na_character")
  expect_error(stata_gt("a", NA_character_),
               class = "statacompat_error_na_character")
})
