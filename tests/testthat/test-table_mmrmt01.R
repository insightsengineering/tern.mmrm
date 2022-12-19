# Test all tables for MMRMT01.

library(dplyr)
library(tern)
library(broom)

mmrm_results <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = c("SEX", "FEV1_BL"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "unstructured",
  weights_emmeans = "proportional"
)

testthat::test_that("LS means table is produced correctly", {
  df <- broom::tidy(mmrm_results)
  result <- basic_table() %>%
    split_cols_by("ARMCD", ref_group = mmrm_results$ref_level) %>%
    add_colcounts() %>%
    split_rows_by("AVISIT") %>%
    summarize_lsmeans(
      show_relative = "increase",
      # Note: We are using less precise formats here to avoid spurious differences
      # between systems while still checking overall structure.
      .formats = c(
        n = "xx.",
        adj_mean_se = sprintf_format("%.1f (%.1f)"),
        adj_mean_ci = "(xx.x, xx.x)",
        diff_mean_se = sprintf_format("%.1f (%.1f)"),
        diff_mean_ci = "(xx.x, xx.x)",
        change = "xx.%",
        p_value = "xx.xx"
      )
    ) %>%
    build_table(df, alt_counts_df = mmrm_test_data)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "VIS1", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "VIS2", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "VIS3", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "VIS4", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "PBO", "(N=420)", "", "68", "32.6 (0.8)", "(31.1, 34.1)",
      "", "", "", "", "", "69", "37.5 (0.6)", "(36.3, 38.7)", "", "",
      "", "", "", "71", "43.0 (0.5)", "(41.9, 44.0)", "", "", "", "",
      "", "67", "48.0 (1.2)", "(45.6, 50.4)", "", "", "", "", "TRT",
      "(N=380)", "", "66", "37.3 (0.8)", "(35.7, 38.8)", "4.7 (1.1)",
      "(2.5, 6.8)", "14%", "0.00", "", "71", "41.9 (0.6)", "(40.7, 43.0)",
      "4.4 (0.9)", "(2.7, 6.1)", "12%", "0.00", "", "58", "46.5 (0.6)",
      "(45.4, 47.6)", "3.6 (0.8)", "(2.1, 5.1)", "8%", "0.00", "",
      "67", "53.0 (1.2)", "(50.6, 55.4)", "5.0 (1.7)", "(1.6, 8.4)",
      "10%", "0.00"
    ),
    dim = c(34L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Fixed effects table is produced correctly", {
  result <- as.rtable(mmrm_results, type = "fixed", format = "xx.xx")
  result_matrix <- to_string_matrix(result)
  expect_identical(result_matrix[1L, -1L], c("Estimate", "Std. Error", "t value", "df", "Pr(>|t|)"))
  expected_matrix <- structure(
    c(
      "", "(Intercept)", "SEXFemale", "FEV1_BL", "ARMCDTRT",
      "AVISITVIS2", "AVISITVIS3", "AVISITVIS4", "ARMCDTRT:AVISITVIS2",
      "ARMCDTRT:AVISITVIS3", "ARMCDTRT:AVISITVIS4", "Estimate", "25.72",
      "-0.00", "0.17", "4.66", "4.86", "10.33", "15.37", "-0.30", "-1.10",
      "0.35", "Std. Error", "1.58", "0.59", "0.03", "1.09", "0.80",
      "0.84", "1.32", "1.13", "1.21", "1.86", "t value", "16.26", "-0.01",
      "5.21", "4.26", "6.05", "12.35", "11.67", "-0.26", "-0.91", "0.19",
      "df", "253", "173", "192", "142", "141", "155", "137", "136",
      "158", "129"
    ),
    dim = c(11L, 5L)
  )
  testthat::expect_identical(result_matrix[, -6L], expected_matrix)
})

testthat::test_that("Covariance matrix table is produced correctly", {
  result <- as.rtable(mmrm_results, type = "cov", format = "xx.xx")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "VIS1", "VIS2", "VIS3", "VIS4", "VIS1", "42.89",
      "15.52", "8.06", "16.59", "VIS2", "15.52", "26.63", "4.63", "10.07",
      "VIS3", "8.06", "4.63", "19.20", "7.79", "VIS4", "16.59", "10.07",
      "7.79", "99.81"
    ),
    dim = c(5L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Model diagnostics table is produced correctly", {
  result <- as.rtable(mmrm_results, type = "diagnostic", format = "xx.xx")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "REML criterion", "AIC", "AICc", "BIC", "Diagnostic statistic value",
      "3429.31", "3449.31", "3449.73", "3482.14"
    ),
    .Dim = c(5L, 2L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
