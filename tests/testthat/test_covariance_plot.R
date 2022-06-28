test_that("h_get_timepoint_vars works as expected with string specified", {
  data_wide <- maditr::dcast(mmrm_test_data, USUBJID + ARMCD ~ AVISIT, value.var = "FEV1")
  data_cov <- cov(data_wide[, 3:ncol(data_wide)], use = "pairwise.complete.obs", method = "pearson")
  result <- h_get_timepoint_vars(vcov_matrix = data_cov, string = "VIS")
  expected <- list(
    row_time = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4),
    col_time = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
  )
  expect_identical(result, expected)
})

test_that("h_get_timepoint_vars returns error when string not specified", {
  data_wide <- maditr::dcast(mmrm_test_data, USUBJID + ARMCD ~ AVISIT, value.var = "FEV1")
  data_cov <- cov(data_wide[, 3:ncol(data_wide)], use = "pairwise.complete.obs", method = "pearson")
  expect_error(h_get_timepoint_vars(vcov_matrix = data_cov))
})

test_that("h_get_timepoint_vars works as expected with string argument not needed", {
  data_wide <- maditr::dcast(mmrm_test_data, USUBJID + ARMCD ~ AVISIT, value.var = "FEV1")
  data_cov <- cov(data_wide[, 3:ncol(data_wide)], use = "pairwise.complete.obs", method = "pearson")
  row.names(data_cov) <- c(1, 2, 3, 4)
  colnames(data_cov) <- c(1, 2, 3, 4)
  result <- h_get_timepoint_vars(vcov_matrix = data_cov)
  expected <- list(
    row_time = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4),
    col_time = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
  )
  expect_identical(result, expected)
})
