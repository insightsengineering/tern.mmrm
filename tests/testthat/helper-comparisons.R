# Helper function to compare result and expected tables with proper handling of p-value column.
expect_equal_result_tables <- function(result,
                                       expected,
                                       tol = 0.001,
                                       pval_name = "Pr(>|t|)",
                                       pval_threshold = 0.0001) {
  pval_col <- match(pval_name, colnames(result))

  # Compare first non-pvalue columns.
  expect_equal(
    result[, -pval_col],
    expected[, -pval_col],
    tolerance = tol
  )

  # Then compare p-values which are not below the threshold in the expected table.
  exp_pval_is_below_thresh <- expected[, pval_col] < pval_threshold
  expect_equal(
    result[, pval_col][!exp_pval_is_below_thresh],
    expected[, pval_col][!exp_pval_is_below_thresh],
    tolerance = tol
  )

  # Now expect that the same p-values are below the thresholds in both tables.
  res_pval_is_below_thresh <- result[, pval_col] < pval_threshold
  expect_identical(
    exp_pval_is_below_thresh,
    res_pval_is_below_thresh
  )
}
