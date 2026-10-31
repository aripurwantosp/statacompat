# Expected values from [D] egen.

r1 <- c(1, 1, NA)
r2 <- c(2, NA, NA)
r3 <- c(3, 3, NA)

test_that("rowtotal: missing counts as 0; all-missing row is 0", {
  expect_identical(stata_rowtotal(r1, r2, r3), c(6, 4, 0))
})

test_that("rowtotal(missing = TRUE): all-missing row is NA", {
  expect_identical(stata_rowtotal(r1, r2, r3, missing = TRUE), c(6, 4, NA))
})

test_that("rowmean ignores missing; all-missing row is NA, not NaN", {
  out <- stata_rowmean(r1, r2, r3)
  expect_identical(out, c(2, 2, NA))
  expect_false(is.nan(out[3]))
})

test_that("rowmiss and rownonmiss", {
  expect_identical(stata_rowmiss(r1, r2, r3), c(0L, 1L, 3L))
  expect_identical(stata_rownonmiss(r1, r2, r3), c(3L, 2L, 0L))
})

test_that("strings: rowmiss accepts them, others follow Stata", {
  x1 <- c(1, 1, NA)
  s1 <- c("a", "", "")
  expect_identical(stata_rowmiss(x1, s1), c(0L, 1L, 2L))
  expect_error(stata_rownonmiss(x1, s1), class = "statacompat_error_type")
  expect_identical(stata_rownonmiss(x1, s1, strok = TRUE), c(2L, 1L, 0L))
  expect_error(stata_rowtotal(x1, s1), class = "statacompat_error_type")
  expect_error(stata_rowmean(x1, s1), class = "statacompat_error_type")
  # " " is not missing; NA_character_ is
  expect_identical(stata_rowmiss(c(" ", NA)), c(0L, 1L))
})

test_that("float storage is not emulated (matches egen double)", {
  expect_identical(stata_rowtotal(16777217, 0), 16777217)
})

test_that("data frame inputs are split into columns", {
  df <- data.frame(r1 = r1, r2 = r2, r3 = r3)
  expect_identical(stata_rowtotal(df), c(6, 4, 0))
  expect_identical(stata_rowmean(df), c(2, 2, NA))
  expect_identical(stata_rowmiss(df), c(0L, 1L, 3L))
  expect_identical(stata_rowtotal(df[1:2], r3), c(6, 4, 0))
  expect_identical(stata_rownonmiss(data.frame(x = c(1, NA), s = c("a", "")),
                                    strok = TRUE), c(2L, 0L))
})

test_that("types: integer, logical, NaN, labelled, tagged", {
  expect_identical(stata_rowtotal(1:3, c(1L, NA, 1L)), c(2, 2, 4))
  expect_type(stata_rowtotal(1:2, 1:2), "double")
  expect_identical(stata_rowtotal(c(TRUE, FALSE), c(TRUE, NA)), c(2, 0))
  expect_identical(stata_rowmiss(c(NaN, 1)), c(1L, 0L))
  expect_identical(stata_rowmiss(as.Date(c(NA, "2020-01-01"))), c(1L, 0L))
  skip_if_not_installed("haven")
  lab <- haven::labelled(c(1, NA), c(Yes = 1))
  expect_identical(stata_rowtotal(lab, c(2, 2)), c(3, 2))
  expect_identical(stata_rowmiss(c(haven::tagged_na("a"), 1)), c(1L, 0L))
})

test_that("single variable and zero rows", {
  expect_identical(stata_rowtotal(c(5, NA)), c(5, 0))
  expect_identical(stata_rowmean(c(5, NA)), c(5, NA))
  expect_identical(stata_rowtotal(numeric(0), numeric(0)), numeric(0))
  expect_identical(stata_rowmiss(numeric(0)), integer(0))
  expect_identical(stata_rowmean(data.frame(a = numeric(0))), numeric(0))
})

test_that("input errors", {
  expect_error(stata_rowtotal(), class = "statacompat_error_size")
  expect_error(stata_rowtotal(data.frame()), class = "statacompat_error_size")
  expect_error(stata_rowtotal(1:3, 1), class = "statacompat_error_size")
  expect_error(stata_rowtotal(matrix(1:4, 2)), class = "statacompat_error_type")
  expect_error(stata_rowtotal(list(1, 2)), class = "statacompat_error_type")
  expect_error(stata_rowtotal(factor("a")), class = "statacompat_error_type")
  expect_error(stata_rowtotal(as.Date("2020-01-01")),
               class = "statacompat_error_type")
  expect_error(stata_rowtotal(1, missing = NA), class = "statacompat_error_type")
  expect_error(stata_rownonmiss(1, strok = "yes"),
               class = "statacompat_error_type")
})

test_that("row helpers inside dplyr::mutate() with pick()", {
  skip_if_not_installed("dplyr")
  dta <- data.frame(r1 = r1, r2 = r2, r3 = r3)
  out <- dplyr::mutate(
    dta,
    total  = stata_rowtotal(dplyr::pick(r1:r3)),
    totalm = stata_rowtotal(dplyr::pick(r1:r3), missing = TRUE),
    mean   = stata_rowmean(r1, r2, r3),
    nmiss  = stata_rowmiss(dplyr::pick(dplyr::starts_with("r")))
  )
  expect_identical(out$total, c(6, 4, 0))
  expect_identical(out$totalm, c(6, 4, NA))
  expect_identical(out$mean, c(2, 2, NA))
  expect_identical(out$nmiss, c(0L, 1L, 3L))
})
