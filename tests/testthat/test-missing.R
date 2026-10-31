test_that("numeric missing", {
  expect_identical(stata_missing(c(1, NA, NaN, Inf, -Inf)),
                   c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(stata_missing(NA_integer_), TRUE)
  expect_identical(stata_missing(NA), TRUE)
  expect_identical(stata_missing(as.Date(c("2020-01-01", NA))), c(FALSE, TRUE))
})

test_that("string missing is \"\" only (missing(\" \") is 0 in Stata)", {
  expect_identical(stata_missing(c("a", "", " ", ".")),
                   c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(stata_missing(NA_character_), TRUE)
})

test_that("multiple arguments: TRUE if any is missing", {
  expect_identical(stata_missing(c(1, NA), c(1, 1)), c(FALSE, TRUE))
  expect_identical(stata_missing(1, "a"), FALSE)
  expect_identical(stata_missing(1, ""), TRUE)
  expect_identical(stata_missing(c(1, NA, 3), "", c("a", "b", "c")),
                   c(TRUE, TRUE, TRUE))
  expect_identical(stata_missing(c(30, NA, 40), c("Ani", "Budi", "")),
                   c(FALSE, TRUE, TRUE))
  expect_identical(stata_missing(NA, 1:3), c(TRUE, TRUE, TRUE))
})

test_that("labelled and tagged values", {
  skip_if_not_installed("haven")
  expect_identical(stata_missing(haven::labelled(c(1, NA), c(A = 1))),
                   c(FALSE, TRUE))
  expect_identical(stata_missing(c(1, haven::tagged_na("a"))), c(FALSE, TRUE))
})

test_that("errors and edge cases", {
  expect_error(stata_missing(), class = "statacompat_error_size")
  expect_error(stata_missing(1:3, 1:2), class = "statacompat_error_size")
  expect_error(stata_missing(factor("a")), class = "statacompat_error_type")
  expect_identical(stata_missing(numeric(0)), logical(0))
  expect_identical(stata_missing(numeric(0), 1), logical(0))
})
