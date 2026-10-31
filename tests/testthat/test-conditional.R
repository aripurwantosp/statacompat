test_that("numeric conditions follow Stata truth rules ([FN] cond())", {
  expect_identical(stata_cond(1, 1, 0), 1)
  expect_identical(stata_cond(-2, 1, 0), 1)
  expect_identical(stata_cond(0, 1, 0), 0)
  expect_identical(stata_cond(NA_real_, 1, 0), 1)     # missing is true
  expect_identical(stata_cond(NaN, 1, 0), 1)
  expect_identical(stata_cond(Inf, 1, 0), 1)
  expect_identical(stata_cond(c(1, 0, NA, -2), 10, 20), c(10, 20, 10, 10))
})

test_that("the missing argument is used for missing conditions", {
  expect_identical(stata_cond(NA_real_, 1, 0, 9), 9)
  expect_identical(stata_cond(c(2, 0, NA), 1, 0, missing = NA),
                   c(1, 0, NA))
  expect_identical(stata_cond(c(2L, 0L, NA), "pos", "zero", "miss"),
                   c("pos", "zero", "miss"))
  # predicates are never missing, so the fourth argument is never reached
  x <- c(1, 3, NA)
  expect_identical(stata_cond(stata_gt(x, 2), 50, 70, 99), c(70, 50, 50))
})

test_that("logical conditions", {
  expect_identical(stata_cond(c(TRUE, FALSE), "a", "b"), c("a", "b"))
  expect_error(stata_cond(c(TRUE, NA), 1, 0),
               class = "statacompat_error_na_condition")
  expect_error(stata_cond(NA, 1, 0), class = "statacompat_error_na_condition")
  expect_identical(stata_cond(as.numeric(c(TRUE, NA)), 1, 0), c(1, 1))
})

test_that("the motivating atwork example", {
  r10a <- c(1, 2, NA, NA)
  r10b <- c(2, 1, NA, 1)
  r10c <- c(NA, NA, NA, 2)
  atwork <- stata_cond(
    stata_eq(r10a, 1) | stata_eq(r10b, 1) | stata_eq(r10c, 1), 1, 0)
  expect_identical(atwork, c(1, 1, 0, 1))
})

test_that("vector branches are taken row by row", {
  cond <- c(1, 0, NA, 0)
  expect_identical(stata_cond(cond, c(1, 2, 3, 4), c(10, 20, 30, 40)),
                   c(1, 20, 3, 40))
  expect_identical(stata_cond(cond, 1:4, 10L, -1L), c(1L, 10L, -1L, 10L))
})

test_that("result types", {
  expect_type(stata_cond(c(1, 0), TRUE, FALSE), "logical")
  expect_type(stata_cond(c(1, 0), 1L, 2L), "integer")
  expect_type(stata_cond(c(1, 0), 1L, TRUE), "integer")
  expect_type(stata_cond(c(1, 0), 1L, 2), "double")
  expect_type(stata_cond(c(1, 0), 1, NA), "double")
  expect_identical(stata_cond(c(1, 0), 1, NA), c(1, NA))
  d <- stata_cond(c(1, 0), as.Date("2020-01-01"), NA)
  expect_s3_class(d, "Date")
  expect_identical(d, as.Date(c("2020-01-01", NA)))
  t <- stata_cond(1, as.POSIXct("2020-01-01 10:00", tz = "Asia/Jakarta"), NA)
  expect_s3_class(t, "POSIXct")
  expect_identical(attr(t, "tzone"), "Asia/Jakarta")
  expect_identical(stata_cond(c(1, 0), "a", ""), c("a", ""))
  expect_identical(stata_cond(c(1, 0), "a", NA_character_), c("a", NA))
  skip_if_not_installed("haven")
  lab <- haven::labelled(c(1, 0, NA), c(Yes = 1, No = 0))
  expect_identical(stata_cond(lab, "y", "n", "m"), c("y", "n", "m"))
  expect_identical(stata_cond(c(1, 0), lab[1:2], 5), c(1, 5))
})

test_that("cond errors", {
  expect_error(stata_cond(1, 1, "a"), class = "statacompat_error_type")
  expect_error(stata_cond(1, "a", NA), class = "statacompat_error_type")
  expect_error(stata_cond("1", 1, 0), class = "statacompat_error_type")
  expect_error(stata_cond(factor("a"), 1, 0), class = "statacompat_error_type")
  expect_error(stata_cond(as.Date("2020-01-01"), 1, 0),
               class = "statacompat_error_type")
  expect_error(stata_cond(c(1, 0, 1), 1:2, 0), class = "statacompat_error_size")
  expect_error(stata_cond(1, as.Date("2020-01-01"), 1),
               class = "statacompat_error_type")
})

test_that("zero-length condition", {
  expect_identical(stata_cond(numeric(0), 1, 0), numeric(0))
})

test_that("cond is not ifelse", {
  x <- c(5, NA)
  expect_identical(ifelse(x > 2, "hi", "lo"), c("hi", NA))
  expect_identical(stata_cond(stata_gt(x, 2), "hi", "lo"), c("hi", "hi"))
})
