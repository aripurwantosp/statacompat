# stata_inlist(): expected values from [FN] and [U] 13.2.3.

test_that("inlist basics", {
  expect_identical(stata_inlist(1, 1, 2), TRUE)
  expect_identical(stata_inlist(3, 1, 2), FALSE)
  expect_identical(stata_inlist(c(1, 4, 3), 1, 2, 3), c(TRUE, FALSE, TRUE))
  expect_identical(stata_inlist(2, 2), TRUE)
})

test_that("inlist treats missing as an ordinary member", {
  expect_identical(stata_inlist(NA, 1, 2), FALSE)
  expect_identical(stata_inlist(NA, 1, NA), TRUE)
  expect_identical(stata_inlist(2, 1, NA), FALSE)
  expect_identical(stata_inlist(c(0, 1, 2, NA), 1, NA),
                   c(FALSE, TRUE, FALSE, TRUE))
  expect_identical(stata_inlist(NaN, NA), TRUE)
})

test_that("inlist evaluates variable candidates row by row", {
  x <- c(0, 1, 2, NA)
  a <- c(2, 0, 0, 0)
  b <- c(0, 0, 1, 0)
  expect_identical(stata_inlist(x, a, b), c(TRUE, FALSE, FALSE, FALSE))
  # scalar x against variable candidates
  expect_identical(stata_inlist(0, a, b), c(TRUE, TRUE, TRUE, TRUE))
  expect_identical(stata_inlist(2, a, b), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("inlist strings", {
  expect_identical(stata_inlist("a", "a", "b"), TRUE)
  expect_identical(stata_inlist("", "a", ""), TRUE)
  expect_identical(stata_inlist(c("A", "a", ""), "a", "b"),
                   c(FALSE, TRUE, FALSE))
  expect_error(stata_inlist(c("a", NA), "a"),
               class = "statacompat_error_na_character")
})

test_that("inlist errors", {
  expect_error(stata_inlist(1), class = "statacompat_error_size")
  expect_error(stata_inlist(1, "1"), class = "statacompat_error_type")
  expect_error(stata_inlist("a", 1, "b"), class = "statacompat_error_type")
  expect_error(stata_inlist(factor("a"), "a"), class = "statacompat_error_type")
  # a vector of codes is not a set
  codes <- c(1, 2, 3)
  expect_error(stata_inlist(c(1, 5, 2, 7), codes),
               class = "statacompat_error_size")
  expect_error(stata_inlist(c(1, 5, 2, 7), codes), "do.call")
})

test_that("inlist with spliced codes and zero length", {
  codes <- c(1, 2, 3)
  expect_identical(do.call(stata_inlist, c(list(c(1, 5, NA)), as.list(codes))),
                   c(TRUE, FALSE, FALSE))
  expect_identical(stata_inlist(numeric(0), 1, 2), logical(0))
})

test_that("inlist with dates", {
  d <- as.Date(c("2020-01-01", NA))
  expect_identical(stata_inlist(d, as.Date("2020-01-01"), NA), c(TRUE, TRUE))
  expect_error(stata_inlist(d, 18262), class = "statacompat_error_type")
})

# stata_inrange(): the five ordered rules of [FN].

test_that("inrange inclusive bounds", {
  expect_identical(stata_inrange(c(5, 1, 10, 0, 11), 1, 10),
                   c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(stata_inrange(5, 10, 1), FALSE)          # reversed bounds
  expect_identical(stata_inrange(5, 5, 5), TRUE)
})

test_that("inrange rule 1: missing x is never in range", {
  expect_identical(stata_inrange(NA, 1, 10), FALSE)
  expect_identical(stata_inrange(NA, NA, NA), FALSE)
  expect_identical(stata_inrange(NaN, NA, 10), FALSE)
})

test_that("inrange rules 2-4: missing bounds are unbounded", {
  expect_identical(stata_inrange(5, NA, NA), TRUE)           # rule 2
  expect_identical(stata_inrange(5, NA, 10), TRUE)           # rule 3
  expect_identical(stata_inrange(11, NA, 10), FALSE)
  expect_identical(stata_inrange(5, 1, NA), TRUE)            # rule 4
  expect_identical(stata_inrange(0, 1, NA), FALSE)
  expect_identical(stata_inrange(-Inf, NA, 0), TRUE)
  x <- c(0, 1, 2, NA)
  expect_identical(stata_inrange(x, NA, 1), c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(stata_inrange(x, 1, NA), c(FALSE, TRUE, TRUE, FALSE))
  expect_identical(stata_inrange(x, NA, NA), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("inrange differs from both naive translations", {
  age <- c(10, 15, NA)
  expect_identical(stata_inrange(age, 15, NA), c(FALSE, TRUE, FALSE))
  expect_identical(stata_ge(age, 15), c(FALSE, TRUE, TRUE))   # Stata age >= 15
  expect_identical(age >= 15, c(FALSE, TRUE, NA))             # base R
})

test_that("inrange with row-varying bounds", {
  x <- c(5, 5, 5, NA)
  lo <- c(1, 6, NA, 1)
  hi <- c(10, 10, 4, 10)
  expect_identical(stata_inrange(x, lo, hi), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("inrange strings: \"\" is missing", {
  expect_identical(stata_inrange("m", "a", "z"), TRUE)
  expect_identical(stata_inrange("", "a", "z"), FALSE)
  expect_identical(stata_inrange("m", "", ""), TRUE)
  expect_identical(stata_inrange("m", "", "k"), FALSE)
  expect_identical(stata_inrange("m", "k", ""), TRUE)
  expect_identical(stata_inrange("B", "a", "z"), FALSE)   # byte order
  expect_identical(stata_inrange(e_acute(), "a", "z"), FALSE)
  # "." is an ordinary string: the manual's rule for "." is not implemented
  expect_identical(stata_inrange("m", "a", "."), FALSE)
  expect_error(stata_inrange(NA_character_, "a", "z"),
               class = "statacompat_error_na_character")
})

test_that("inrange types, sizes and dates", {
  expect_error(stata_inrange("m", 1, 2), class = "statacompat_error_type")
  expect_error(stata_inrange(1:3, 1:2, 3), class = "statacompat_error_size")
  expect_identical(stata_inrange(numeric(0), 1, 2), logical(0))
  d <- as.Date(c("2020-06-01", "2019-01-01", NA))
  expect_identical(stata_inrange(d, as.Date("2020-01-01"), NA),
                   c(TRUE, FALSE, FALSE))
  expect_identical(stata_inrange(c(TRUE, FALSE), 1, 1), c(TRUE, FALSE))
})
