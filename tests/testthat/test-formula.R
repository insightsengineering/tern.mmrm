library(testthat)

test_that("build_formula gives error message if incorrect covariance structure is given", {
  vars <- list(response = "AVAL", id = "USUBJID", arm = "ARMCD", visit = "AVISIT", covariates = c("RACE", "SEX"))
  expect_error(build_formula(vars, "BLAHHH"))
})

test_that("build_formula builds the correct formula", {
  vars <- list(response = "AVAL", id = "USUBJID", arm = "ARMCD", visit = "AVISIT", covariates = c("RACE", "SEX"))
  result <- as.character(build_formula(vars, "auto-regressive"))
  expected <- as.character(AVAL ~ RACE + SEX + ARMCD * AVISIT + ar1(AVISIT | USUBJID))
  expect_identical(result, expected)
})

test_that("build_formula builds the correct formula with no arm variable given", {
  vars <- list(response = "AVAL", id = "USUBJID", visit = "AVISIT", covariates = c("RACE", "SEX"))
  result <- as.character(build_formula(vars, "auto-regressive"))
  expected <- as.character(AVAL ~ RACE + SEX + AVISIT + ar1(AVISIT | USUBJID))
  expect_identical(result, expected)
})

test_that("build_formula builds the correct formula with no covariates given", {
  vars <- list(response = "AVAL", id = "USUBJID", arm = "ARMCD", visit = "AVISIT")
  result <- as.character(build_formula(vars, "auto-regressive"))
  expected <- as.character(AVAL ~ ARMCD * AVISIT + ar1(AVISIT | USUBJID))
  expect_identical(result, expected)
})
