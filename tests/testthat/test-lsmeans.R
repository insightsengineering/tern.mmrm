# get_mmrm_lsmeans ----

testthat::test_that("get_mmrm_lsmeans can calculate the LS mean results", {
  skip_if_too_deep(5)

  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  fit <- fit_lme4(
    formula = FEV1 ~ ARMCD * AVISIT + (0 + AVISIT | USUBJID),
    data = mmrm_test_data,
    optimizer = "automatic"
  )
  conf_level <- 0.95
  weights <- "proportional"
  averages <- list(
    "VIS1+3" = c("VIS1", "VIS3"),
    "VIS2+4" = c("VIS2", "VIS4")
  )
  expect_silent(result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    averages = averages,
    weights = weights
  ))
  expect_is(result, "list")
  expect_is(result$estimates, "data.frame")
  expect_is(result$contrasts, "data.frame")
})

testthat::test_that("get_mmrm_lsmeans preserves combined arm levels.", {
  skip_if_too_deep(5)

  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )

  fit <- fit_lme4(
    formula = FEV1 ~ ARMCD * AVISIT + (0 + AVISIT | USUBJID),
    data = mmrm_test_data,
    optimizer = "automatic"
  )

  result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = 0.95,
    weights = "proportional"
  )

  testthat::expect_identical(levels(mmrm_test_data$ARMCD), levels(result$estimates$ARMCD))
  testthat::expect_identical(levels(mmrm_test_data$ARMCD)[-1], levels(result$contrasts$ARMCD))
})
