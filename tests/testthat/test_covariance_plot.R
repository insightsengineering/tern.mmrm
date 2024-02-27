# h_get_timepoint_vars ----

test_that("h_get_timepoint_vars works in simple example", {
  vcov_matrix <- matrix(
    c(49, 24, 12, 23, 24, 35, 11, 20, 12, 11, 24, 14, 23, 20, 14, 107),
    nrow = 4, ncol = 4,
    dimnames = list(
      c("VIS1", "VIS2", "VIS3", "VIS4"),
      c("VIS1", "VIS2", "VIS3", "VIS4")
    )
  )
  result <- expect_silent(h_get_timepoint_vars(vcov_matrix, time_prefix = "VIS"))
  expected <- list(
    row_time = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4),
    col_time = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
  )
  expect_identical(result, expected)
})

test_that("h_get_timepoint_vars works as expected with time_prefix specified", {
  skip_if_not_installed("maditr")
  data_wide <- maditr::dcast(mmrm_test_data, USUBJID + ARMCD ~ AVISIT, value.var = "FEV1")
  data_cov <- cov(data_wide[, 3:ncol(data_wide)], use = "pairwise.complete.obs", method = "pearson")
  result <- h_get_timepoint_vars(vcov_matrix = data_cov, time_prefix = "VIS")
  expected <- list(
    row_time = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4),
    col_time = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
  )
  expect_identical(result, expected)
})

test_that("h_get_timepoint_vars returns error when time_prefix not specified", {
  skip_if_not_installed("maditr")
  data_wide <- maditr::dcast(mmrm_test_data, USUBJID + ARMCD ~ AVISIT, value.var = "FEV1")
  data_cov <- cov(data_wide[, 3:ncol(data_wide)], use = "pairwise.complete.obs", method = "pearson")
  expect_error(h_get_timepoint_vars(vcov_matrix = data_cov))
})

test_that("h_get_timepoint_vars works as expected with time_prefix argument not needed", {
  skip_if_not_installed("maditr")
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

# h_vectorization ----

test_that("h_vectorization works as expected with time_prefix specified", {
  vcov_matrix <- matrix(
    c(49, 12, 12, 23),
    nrow = 2, ncol = 2,
    dimnames = list(
      c("VIS1", "VIS2"),
      c("VIS1", "VIS2")
    )
  )
  result <- h_vectorization(vcov_matrix, time_prefix = "VIS")
  expected <- data.frame(
    Vect = c(49, 12, 23),
    time_diff = c(0, 1, 0),
    lag = c(0, 1, 0),
    rank_row = c(1, 1, 2),
    rank_col = c(1, 2, 2)
  )
  expect_identical(result, expected)
})

test_that("h_vectorization works as expected when time_prefix not specified", {
  vcov_matrix <- matrix(
    c(49, 12, 12, 23),
    nrow = 2, ncol = 2,
    dimnames = list(
      c(1, 2),
      c(1, 2)
    )
  )
  result <- h_vectorization(vcov_matrix)
  expected <- data.frame(
    Vect = c(49, 12, 23),
    time_diff = c(0, 1, 0),
    lag = c(0, 1, 0),
    rank_row = c(1, 1, 2),
    rank_col = c(1, 2, 2)
  )
  expect_identical(result, expected)
})

# g_covariance ----

test_that("g_covariance works as expected as expected with defaults", {
  vcov_matrix <- matrix(
    c(49, 12, 12, 23),
    nrow = 2, ncol = 2,
    dimnames = list(
      c(1, 2),
      c(1, 2)
    )
  )
  result <- expect_silent(g_covariance(vcov_matrix))

  skip_on_ci()
  vdiffr::expect_doppelganger("g_covariance plot with defaults", result)
})

test_that("g_covariance works as expected as expected with time_prefix specified", {
  vcov_matrix <- matrix(
    c(49, 12, 12, 23),
    nrow = 2, ncol = 2,
    dimnames = list(
      c("VIS1", "VIS2"),
      c("VIS1", "VIS2")
    )
  )
  result <- expect_silent(g_covariance(vcov_matrix, time_prefix = "VIS"))

  skip_on_ci()
  vdiffr::expect_doppelganger("g_covariance plot with time_prefix specified", result)
})

test_that("g_covariance works as expected as expected with time difference", {
  vcov_matrix <- matrix(
    c(49, 12, 12, 23),
    nrow = 2, ncol = 2,
    dimnames = list(
      c(1, 2),
      c(1, 2)
    )
  )
  result <- expect_silent(g_covariance(vcov_matrix, x_var = "time_diff"))

  skip_on_ci()
  vdiffr::expect_doppelganger("g_covariance plot with time diff", result)
})
