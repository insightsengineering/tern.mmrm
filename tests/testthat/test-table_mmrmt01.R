# Test all tables for MMRMT01.

library(dplyr)
library(tern)
library(broom)

too_old_lme4 <- compareVersion(as.character(packageVersion("lme4")), "1.1.21") <= 0

mmrm_results <- if (too_old_lme4) {
  NULL
} else {
  fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "unstructured",
    weights_emmeans = "proportional",
    optimizer = "automatic"
  )
}

testthat::test_that("LS means table is produced correctly", {
  testthat::skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  df <- broom::tidy(mmrm_results)
  result <- basic_table() %>%
    split_cols_by("ARMCD", ref_group = mmrm_results$ref_level) %>%
    add_colcounts() %>%
    split_rows_by("AVISIT") %>%
    summarize_lsmeans(show_relative = "increase") %>%
    build_table(df, alt_counts_df = mmrm_test_data)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "VIS1",
      "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "VIS2", "n",
      "Adjusted Mean (SE)", "95% CI", "Difference in Adjusted Means (SE)",
      "95% CI", "Relative Increase (%)", "p-value (MMRM)",
      "VIS3", "n", "Adjusted Mean (SE)",
      "95% CI", "Difference in Adjusted Means (SE)", "95% CI",
      "Relative Increase (%)", "p-value (MMRM)", "VIS4",
      "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "PBO", "(N=420)",
      "", "68", "32.626 (0.767)",
      "(31.111, 34.142)", "", "",
      "", "", "",
      "69", "37.484 (0.605)", "(36.289, 38.679)",
      "", "", "",
      "", "", "71",
      "42.957 (0.514)", "(41.940, 43.974)", "",
      "", "", "",
      "", "67", "47.998 (1.206)",
      "(45.613, 50.383)", "", "",
      "", "", "TRT",
      "(N=380)", "", "66",
      "37.291 (0.781)", "(35.748, 38.835)", "4.665 (1.095)",
      "(2.501, 6.828)", "14.3%", "<0.0001",
      "", "71", "41.852 (0.600)",
      "(40.666, 43.038)", "4.368 (0.852)", "(2.683, 6.052)",
      "11.7%", "<0.0001", "",
      "58", "46.524 (0.568)", "(45.400, 47.648)",
      "3.567 (0.766)", "(2.051, 5.084)", "8.3%",
      "<0.0001", "", "67",
      "53.011 (1.209)", "(50.619, 55.402)", "5.013 (1.708)",
      "(1.636, 8.390)", "10.4%", "0.0039"
    ),
    .Dim = c(34L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Fixed effects table is produced correctly", {
  testthat::skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  result <- as.rtable(mmrm_results, type = "fixed")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "(Intercept)", "SEXFemale", "FEV1_BL", "ARMCDTRT",
      "AVISITVIS2", "AVISITVIS3", "AVISITVIS4", "ARMCDTRT:AVISITVIS2", "ARMCDTRT:AVISITVIS3",
      "ARMCDTRT:AVISITVIS4", "Estimate", "25.7210", "-0.0039", "0.1717",
      "4.6649", "4.8575", "10.3304", "15.3713", "-0.2971",
      "-1.0975", "0.3479", "Std. Error", "1.5822", "0.5889",
      "0.0329", "1.0945", "0.8029", "0.8366", "1.3174",
      "1.1312", "1.2114", "1.8569", "t value", "16.2561",
      "-0.0067", "5.2130", "4.2621", "6.0501", "12.3482",
      "11.6680", "-0.2627", "-0.9060", "0.1873", "df",
      "253", "173", "192", "142", "141",
      "155", "137", "136", "158", "129",
      "Pr(>|t|)", "<0.0001", "0.9947", "<0.0001", "<0.0001",
      "<0.0001", "<0.0001", "<0.0001", "0.7932", "0.3663",
      "0.8517"
    ),
    .Dim = c(11L, 6L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Covariance matrix table is produced correctly", {
  testthat::skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  result <- as.rtable(mmrm_results, type = "cov")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "VIS1", "VIS2", "VIS3", "VIS4", "VIS1", "42.8836", "15.5193", "8.0571", "16.5931", "VIS2", "15.5193",
      "26.6289", "4.6272", "10.0744", "VIS3", "8.0571", "4.6272", "19.2031", "7.7857", "VIS4", "16.5931", "10.0744",
      "7.7857", "99.8111"
    ),
    .Dim = c(5L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Model diagnostics table is produced correctly", {
  testthat::skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  result <- as.rtable(mmrm_results, type = "diagnostic")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "REML criterion", "AIC", "AICc",
      "BIC", "Diagnostic statistic value", "3429.3063", "3449.3063",
      "3449.7326", "3482.1383"
    ),
    .Dim = c(5L, 2L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
