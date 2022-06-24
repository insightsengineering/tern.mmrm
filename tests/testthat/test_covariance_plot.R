test_that("h_get_timepoint_vars works as expected with string specified", {
  data("mmrm_test_data")
  data_wide <- dcast(mmrm_test_data, USUBJID + ARMCD ~ AVISIT, value.var = "FEV1")
  data_cov <- cov(data_wide[, 3:ncol(data_wide)], use = "pairwise.complete.obs", method = "pearson")
  result <-  h_get_timepoint_vars(vcov_matrix = data_cov, string = "VIS")
  expected <- list(numeric_vcov_matrix_row_label = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4), numeric_vcov_matrix_col_label = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4))
  expect_identical(result, expected)
})

test_that("h_get_timepoint_vars returns error when string not specified", {
  data("mmrm_test_data")
  data_wide <- dcast(mmrm_test_data, USUBJID + ARMCD ~ AVISIT, value.var = "FEV1")
  data_cov <- cov(data_wide[, 3:ncol(data_wide)], use = "pairwise.complete.obs", method = "pearson")
  expect_error(h_get_timepoint_vars(vcov_matrix = data_cov))
})

test_that("h_get_timepoint_vars works as expected with string argument not needed", {
  data("mmrm_test_data")
  data_wide <- dcast(mmrm_test_data, USUBJID + ARMCD ~ AVISIT, value.var = "FEV1")
  data_cov <- cov(data_wide[, 3:ncol(data_wide)], use = "pairwise.complete.obs", method = "pearson")
  row.names(data_cov) <- c(1, 2, 3, 4)
  colnames(data_cov) <- c(1, 2, 3, 4)
  result <-  h_get_timepoint_vars(vcov_matrix = data_cov)
  expected <- list(numeric_vcov_matrix_row_label = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4), numeric_vcov_matrix_col_label = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4))
  expect_identical(result, expected)
})


