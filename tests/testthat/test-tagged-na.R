# Extended missing values (.a-.z) as imported by haven. Stata documents
# that they are distinct and ordered ([U] 12.2.1): (.a == .) is 0,
# (.a > .) is 1.
# Version 0.1.0 reproduces results that do not depend on that ordering and
# raises an error where they would.

test_that("the base-R tag reader agrees with haven", {
  skip_if_not_installed("haven")
  x <- c(1, NA, NaN, haven::tagged_na("a"), haven::tagged_na("z"))
  tags <- sc_na_tag(x)
  expect_identical(tags[1:3], c(0L, 0L, 0L))
  expect_identical(intToUtf8(tags[4:5], multiple = TRUE),
                   haven::na_tag(x)[4:5])
  expect_identical(sc_na_tag(c(1L, NA)), c(0L, 0L))
})

test_that("tagged missing vs non-missing follows Stata", {
  a <- tagged("a")
  expect_identical(stata_gt(a, 1e300), TRUE)
  expect_identical(stata_eq(a, 1), FALSE)
  expect_identical(stata_lt(1, a), TRUE)
  expect_identical(stata_eq(a, a), TRUE)      # .a == .a
  expect_identical(stata_inlist(a, 1, a), TRUE)  # Stata: 1
})

test_that("comparing different missing values is an error", {
  a <- tagged("a")
  b <- tagged("b")
  expect_error(stata_eq(a, NA), class = "statacompat_error_tagged_na")
  expect_error(stata_gt(a, NA), class = "statacompat_error_tagged_na")
  expect_error(stata_ne(a, b), class = "statacompat_error_tagged_na")
  expect_error(stata_inlist(a, 1, NA), class = "statacompat_error_tagged_na")
  # only the positions where both are missing matter
  expect_identical(stata_eq(c(a, 1), c(5, 1)), c(FALSE, TRUE))
  expect_error(stata_eq(c(a, NA), c(5, NA)), NA)
  expect_error(stata_eq(c(a, NA), c(NA, 5)), class = "statacompat_error_tagged_na")
})

test_that("tag-insensitive helpers accept extended missing values", {
  a <- tagged("a")
  b <- tagged("b")
  expect_identical(stata_inrange(5, a, b), TRUE)   # Stata: 1
  expect_identical(stata_inrange(a, 1, 10), FALSE)
  expect_identical(stata_cond(a, 1, 0), 1)
  expect_identical(stata_cond(a, 1, 0, 9), 9)
  expect_identical(stata_missing(a), TRUE)
})

test_that("documented workaround removes tags", {
  x <- c(1, tagged("a"))
  x[is.na(x)] <- NA_real_
  expect_identical(stata_eq(x, NA), c(FALSE, TRUE))
})

test_that("tags survive a read_dta() round trip and are then detected", {
  skip_if_not_installed("haven")
  f <- withr::local_tempfile(fileext = ".dta")
  haven::write_dta(data.frame(v = c(1, NA, haven::tagged_na("c"))), f)
  v <- haven::read_dta(f)$v
  expect_identical(stata_eq(v[1:2], c(1, NA)), c(TRUE, TRUE))
  expect_error(stata_eq(v, NA), class = "statacompat_error_tagged_na")
})
