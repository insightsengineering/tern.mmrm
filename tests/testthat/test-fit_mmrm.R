library(dplyr)

# h_get_diagnostics ----

test_that("h_get_diagnostics works as expected", {
  fit <- mmrm::mmrm(
    formula = FEV1 ~ us(AVISIT | USUBJID),
    data = mmrm_test_data
  )
  result <- h_get_diagnostics(fit)

  expected <- list(
    "REML criterion" = 3700.9,
    AIC = 3720.9,
    AICc = 3721.4,
    BIC = 3753.8
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

# fit_mmrm ----

## parallelization ----

test_that("fit_mmrm works with parallelization", {
  expect_no_error(result <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("RACE", "SEX"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "unstructured",
    parallel = TRUE
  ))
})

## optimizer ----

test_that("fit_mmrm can specify multiple optimizers to try", {
  result <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("RACE", "SEX"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "unstructured",
    optimizer = c("nlminb", "CG")
  )
  expect_true(attr(result$fit, "converged"))
  expect_identical(result$additional$optimizer, c("nlminb", "CG"))
})

## method ----

test_that("fit_mmrm can specify adjustment method", {
  result <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("RACE", "SEX"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "unstructured",
    method = "Kenward-Roger"
  )
  expect_true(attr(result$fit, "converged"))
  expect_identical(result$additional$method, "Kenward-Roger")
  expect_identical(mmrm::component(result$fit, "method"), "Kenward-Roger")
})

## character ID ----

test_that("fit_mmrm works with character ID variable", {
  dat <- mmrm_test_data
  dat$USUBJID <- as.character(dat$USUBJID) # nolint
  expect_silent(result <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("RACE", "SEX"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = dat,
    cor_struct = "unstructured"
  ))
  expected <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("RACE", "SEX"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "unstructured"
  )
  expect_identical(coef(result$fit), coef(expected$fit))
})

## unstructured numbers ----

test_that("fit_mmrm works with unstructured covariance matrix and produces same results as SAS", {
  dat <- get_version(version = "A")

  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = dat,
    cor_struct = "unstructured",
    weights_emmeans = "equal"
  )

  # Compare vs. SAS results calculated with the following statements:
  #
  # PROC MIXED DATA = ana.dat cl method=reml;
  # CLASS USUBJID ARMCD(ref='ARM B') AVISIT(ref='SCREENING') STRATA1(ref='A') BMRKR2(ref='LOW');
  # MODEL AVAL = STRATA1 BMRKR2 ARMCD AVISIT ARMCD*AVISIT / ddfm=satterthwaite solution chisq;
  # REPEATED AVISIT / subject=USUBJID type=un r rcorr;
  # LSMEANS AVISIT*ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
  # RUN;

  # REML criterion value.
  expect_equal(
    stats::deviance(mmrm_results$fit),
    3429.306,
    tolerance = 0.0001
  )

  # Fixed effects estimates.
  summary_table <- summary(mmrm_results$fit)
  fixed_effects <- as.data.frame(summary_table$coefficients[, c("Estimate", "df", "Pr(>|t|)")])

  expected_fixed_effects <- data.frame(
    Estimate = c(
      25.721040582, -0.003946099, 0.171675073, 4.664924551, 4.857505880,
      10.330361227, 15.371284868, -0.297149896, -1.097545882, 0.347873893
    ),
    df = c(
      252.8452, 173.0341, 192.3215, 142.4618, 141.4361, 154.8617, 137.4111, 136.2043, 157.6647, 128.6973
    ),
    "Pr(>|t|)" = c(
      3.645436e-41, 9.946610e-01, 4.772246e-07, 3.663454e-05, 1.229094e-08,
      8.063096e-25, 2.730408e-22, 7.931854e-01, 3.663117e-01, 8.516917e-01
    ),
    row.names = c(
      "(Intercept)", "SEXFemale", "FEV1_BL", "ARMCDTRT", "AVISITVIS2",
      "AVISITVIS3", "AVISITVIS4", "ARMCDTRT:AVISITVIS2", "ARMCDTRT:AVISITVIS3", "ARMCDTRT:AVISITVIS4"
    ),
    check.names = FALSE # Necessary to get right p-value column name.
  )

  expect_equal_result_tables(
    fixed_effects,
    expected_fixed_effects
  )

  # Now compare LS means and their contrasts.
  lsmeans_estimates <- mmrm_results$lsmeans$estimates[
    c(1, 5, 2, 6, 3, 7, 4, 8),
    c("ARMCD", "AVISIT", "estimate", "lower_cl", "upper_cl")
  ]
  expected_lsmeans_estimates <- data.frame(
    ARMCD = factor(
      c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
      labels = c("PBO", "TRT"),
    ),
    AVISIT = factor(
      c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
      labels = c("VIS1", "VIS2", "VIS3", "VIS4"),
    ),
    estimate = c(
      32.62658, 37.29150, 37.48409, 41.85186, 42.95694, 46.52432, 47.99786, 53.01066
    ),
    lower_cl = c(
      31.11091, 35.74777, 36.28937, 40.66389, 41.94015, 45.39704, 45.61267, 50.61908
    ),
    upper_cl = c(
      34.14225, 38.83523, 38.67881, 43.03983, 43.97373, 47.65160, 50.38305, 55.40224
    )
  )
  expect_equal(
    lsmeans_estimates,
    expected_lsmeans_estimates,
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  lsmeans_contrasts <-
    mmrm_results$lsmeans$contrasts[, c("ARMCD", "AVISIT", "estimate", "df", "lower_cl", "upper_cl", "p_value")]
  expected_lsmeans_contrasts <- data.frame(
    ARMCD = factor(
      c(1L, 1L, 1L, 1L),
      labels = c("TRT"),
    ),
    AVISIT = factor(
      c(1L, 2L, 3L, 4),
      labels = c("VIS1", "VIS2", "VIS3", "VIS4")
    ),
    estimate = c(4.664925, 4.367775, 3.567379, 5.012798),
    df = c(142.4618, 144.9654, 131.2891, 134.3704),
    lower_cl = c(2.501359, 2.683056, 2.051229, 1.635537),
    upper_cl = c(6.828490, 6.052493, 5.083528, 8.390059),
    p_value = c(3.663454e-05, 9.386112e-07, 7.837380e-06, 3.917510e-03)
  )
  expect_equal_result_tables(
    lsmeans_contrasts,
    expected_lsmeans_contrasts,
    pval_name = "p_value"
  )

  # Covariance matrix estimate.
  cov_estimate <- mmrm_results$cov_estimate
  expected_cov_estimate <- matrix(
    c(
      42.883581, 15.519333, 8.057128, 16.593116,
      15.519333, 26.628885, 4.627223, 10.074424,
      8.057128, 4.627223, 19.203109, 7.785749,
      16.593116, 10.074424, 7.785749, 99.811086
    ),
    nrow = 4L,
    ncol = 4L
  )
  expect_equal(
    cov_estimate,
    expected_cov_estimate,
    ignore_attr = TRUE,
    tolerance = 0.001
  )

  # Diagnostics.
  diagnostics <- mmrm_results$diagnostics
  diagnostics_values <- unlist(diagnostics)
  expected_diagnostics_values <- c(3429.306, 3449.306, 3449.733, 3482.138)
  expect_equal(
    diagnostics_values,
    expected_diagnostics_values,
    tolerance = 0.00001,
    ignore_attr = TRUE
  )
})

## missing data ----

test_that("fit_mmrm works also with missing data", {
  dat <- get_version(version = "B")
  stopifnot(identical(
    nrow(stats::na.omit(dat)),
    440L
  ))

  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = "FEV1_BL",
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = dat,
    cor_struct = "unstructured",
    weights_emmeans = "equal"
  )

  # Compare vs. SAS results calculated with the following statements:
  #
  # PROC MIXED DATA = ana.dat cl method=reml;
  # CLASS USUBJID ARMCD(ref='ARM B') AVISIT(ref='WEEK 1 DAY 8');
  # MODEL AVAL = BMRKR1 ARMCD AVISIT ARMCD*AVISIT / ddfm=satterthwaite solution chisq;
  # REPEATED AVISIT / subject=USUBJID type=un r rcorr;
  # LSMEANS AVISIT*ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
  # RUN;

  # REML criterion value.
  expect_equal(
    stats::deviance(mmrm_results$fit),
    2791.552,
    tolerance = 0.00001
  )

  # Fixed effects estimates.
  summary_table <- summary(mmrm_results$fit)
  fixed_effects <- as.data.frame(summary_table$coefficients[, c("Estimate", "df", "Pr(>|t|)")])

  expected_fixed_effects <- data.frame(
    Estimate = c(
      25.7381227, 0.1632488, 4.4265387, 4.5699130, 10.8910287, 14.7393256, 0.4212233, -1.0617322, 1.9545650
    ),
    df = c(
      231.3171, 173.0245, 120.2030, 126.5597, 132.3372, 125.3761, 124.6140, 127.8502, 111.1961
    ),
    "Pr(>|t|)" = c(
      3.446983e-40, 2.491892e-06, 1.811658e-04, 9.441030e-07, 1.087251e-22, 1.244173e-18,
      7.409659e-01, 4.202541e-01, 3.297910e-01
    ),
    row.names = c(
      "(Intercept)", "FEV1_BL", "ARMCDTRT", "AVISITVIS2", "AVISITVIS3",
      "AVISITVIS4", "ARMCDTRT:AVISITVIS2", "ARMCDTRT:AVISITVIS3", "ARMCDTRT:AVISITVIS4"
    ),
    check.names = FALSE # Necessary to get right p-value column name.
  )

  expect_equal_result_tables(
    fixed_effects,
    expected_fixed_effects
  )

  # Now compare LS means and their contrasts.
  lsmeans_estimates <- mmrm_results$lsmeans$estimates[
    c("ARMCD", "AVISIT", "estimate", "lower_cl", "upper_cl")
  ]
  expected_lsmeans_estimates <- data.frame(
    ARMCD = factor(
      c(rep(1L, 4), rep(2L, 4)),
      labels = c("PBO", "TRT"),
    ),
    AVISIT = factor(
      rep(1:4, 2),
      labels = c("VIS1", "VIS2", "VIS3", "VIS4"),
    ),
    estimate = c(
      32.26162, 36.83153, 43.15265, 47.00095, 36.68816, 41.67930, 46.51746, 53.38205
    ),
    lower_cl = c(
      30.69289, 35.59335, 41.98076, 44.46010, 35.05131, 40.42744, 45.32000, 50.83683
    ),
    upper_cl = c(
      33.83035, 38.06971, 44.32454, 49.54179, 38.32501, 42.93116, 47.71491, 55.92727
    )
  )
  expect_equal(
    lsmeans_estimates,
    expected_lsmeans_estimates,
    tolerance = 1e-3
  )

  lsmeans_contrasts <-
    mmrm_results$lsmeans$contrasts[, c("ARMCD", "AVISIT", "estimate", "df", "lower_cl", "upper_cl", "p_value")]
  expected_lsmeans_contrasts <- data.frame(
    ARMCD = factor(
      c(1L, 1L, 1L, 1L),
      labels = c("TRT"),
    ),
    AVISIT = factor(
      c(1L, 2L, 3L, 4),
      labels = c("VIS1", "VIS2", "VIS3", "VIS4")
    ),
    estimate = c(
      4.426539, 4.847762, 3.364807, 6.381104
    ),
    df = c(120.2030, 117.2153, 102.9251, 112.9055),
    lower_cl = c(
      2.158514, 3.086858, 1.689262, 2.784753
    ),
    upper_cl = c(
      6.694563, 6.608666, 5.040351, 9.977455
    ),
    p_value = c(
      1.811658e-04, 2.801930e-07, 1.272328e-04, 6.333528e-04
    )
  )
  expect_equal_result_tables(
    lsmeans_contrasts,
    expected_lsmeans_contrasts,
    pval_name = "p_value"
  )

  # Covariance matrix estimate.
  cov_estimate <- mmrm_results$cov_estimate
  expected_cov_estimate <- matrix(
    c(
      39.463808, 11.690001, 8.133941, 15.745417,
      11.690001, 23.329029, 2.735221, 8.645571,
      8.133941, 2.735221, 18.780584, 7.839711,
      15.745417, 8.645571, 7.839711, 94.703091
    ),
    nrow = 4L,
    ncol = 4L
  )
  expect_equal(
    cov_estimate,
    expected_cov_estimate,
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  # Diagnostics.
  diagnostics <- mmrm_results$diagnostics
  diagnostics_values <- unlist(diagnostics)
  expected_diagnostics_values <- c(2791.552, 2811.552, 2812.076, 2844.282)
  expect_equal(
    diagnostics_values,
    expected_diagnostics_values,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

## singular fit ----

test_that("fit_mmrm works also with rank deficient model matrix", {
  dat <- get_version(version = "A")
  dat$SEX2 <- dat$SEX # nolint

  # We get a message on the nesting structure but that is ok.
  expect_message(mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("FEV1_BL", "SEX", "SEX2"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = dat,
    cor_struct = "unstructured",
    weights_emmeans = "equal"
  ))

  mmrm_results$fit
})

## weights ----

test_that("fit_mmrm works also with weights", {
  dat <- get_version(version = "A")

  set.seed(123, kind = "Wichmann-Hill")
  dat$w <- rexp(n = nrow(dat))

  mmrm_results <- expect_silent(fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = "SEX",
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT",
      weights = "w"
    ),
    data = dat,
    cor_struct = "unstructured"
  ))

  # Confirm that weights were used in the actual fitting process.
  expect_equal(
    mmrm_results$fit$weights,
    dat$w[!is.na(mmrm_results$fit$data$FEV1)],
    ignore_attr = TRUE
  )
})

## different cov structures ----

test_that("fit_mmrm works with homogeneous toeplitz covariance matrix", {
  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "toeplitz",
    weights_emmeans = "equal"
  )
  expect_class(mmrm_results, "tern_mmrm")
})

test_that("fit_mmrm works with heterogeneous toeplitz covariance matrix", {
  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "heterogeneous toeplitz",
    weights_emmeans = "equal"
  )
  expect_class(mmrm_results, "tern_mmrm")
})

test_that("fit_mmrm works with homogeneous ante-dependence covariance matrix", {
  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "ante-dependence",
    weights_emmeans = "equal"
  )
  expect_class(mmrm_results, "tern_mmrm")
})

test_that("fit_mmrm works with heterogeneous ante-dependence covariance matrix", {
  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "heterogeneous ante-dependence",
    weights_emmeans = "equal"
  )
  expect_class(mmrm_results, "tern_mmrm")
})

test_that("fit_mmrm works with homogeneous auto-regressive covariance matrix", {
  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "auto-regressive",
    weights_emmeans = "equal"
  )
  expect_class(mmrm_results, "tern_mmrm")
})

test_that("fit_mmrm works with heterogeneous auto-regressive covariance matrix", {
  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "heterogeneous auto-regressive",
    weights_emmeans = "equal"
  )
  expect_class(mmrm_results, "tern_mmrm")
})

test_that("fit_mmrm works with homogeneous compound symmetry covariance matrix", {
  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "compound symmetry",
    weights_emmeans = "equal"
  )
  expect_class(mmrm_results, "tern_mmrm")
})

test_that("fit_mmrm works with heterogeneous compound symmetry covariance matrix", {
  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = mmrm_test_data,
    cor_struct = "heterogeneous compound symmetry",
    weights_emmeans = "equal"
  )
  expect_class(mmrm_results, "tern_mmrm")
})
