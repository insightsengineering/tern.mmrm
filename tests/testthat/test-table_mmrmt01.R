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
  expect_snapshot(result)
})

testthat::test_that("Fixed effects table is produced correctly", {
  result <- as.rtable(mmrm_results, type = "fixed", format = "xx.xx")
  testthat::expect_snapshot(result)
})

testthat::test_that("Covariance matrix table is produced correctly", {
  result <- as.rtable(mmrm_results, type = "cov", format = "xx.xx")
  testthat::expect_snapshot(result)
})

testthat::test_that("Model diagnostics table is produced correctly", {
  result <- as.rtable(mmrm_results, type = "diagnostic", format = "xx.xx")
  testthat::expect_snapshot(result)
})
