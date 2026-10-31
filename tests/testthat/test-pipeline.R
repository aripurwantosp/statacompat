test_that("helpers work inside dplyr::mutate()", {
  skip_if_not_installed("dplyr")
  dta <- data.frame(
    kbli   = c(1, 5, NA, 3),
    age    = c(14, 30, 70, NA),
    income = c(5e6, 2e7, NA, 1.2e7),
    r10a   = c(1, 2, NA, 2),
    r10b   = c(2, 2, NA, 1)
  )
  out <- dplyr::mutate(
    dta,
    sector      = stata_inlist(kbli, 1, 2, 3),
    working_age = stata_inrange(age, 15, 64),
    high_income = stata_gt(income, 1e7),
    atwork      = stata_cond(stata_eq(r10a, 1) | stata_eq(r10b, 1), 1, 0),
    any_miss    = stata_missing(age, income)
  )
  expect_identical(out$sector, c(TRUE, FALSE, FALSE, TRUE))
  expect_identical(out$working_age, c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(out$high_income, c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(out$atwork, c(1, 0, 0, 1))
  expect_identical(out$any_miss, c(FALSE, FALSE, TRUE, TRUE))
})
