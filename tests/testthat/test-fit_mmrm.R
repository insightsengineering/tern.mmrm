library(dplyr)

# get_lme4_diagnostics ----

test_that("get_lme4_diagnostics works as expected with a random slope model", {
  fit <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  result <- get_lme4_diagnostics(fit)

  expected <- list(
    "REML criterion" = 1743.6,
    AIC = 1751.6,
    AICc = 1751.9,
    BIC = 1755.2
  )
  expect_equal(result, expected, tolerance = 0.0001)
})

# fit_mmrm ----

test_that("fit_mmrm works with parallelization", {
  dat <- lme4::sleepstudy %>%
    dplyr::mutate(
      group = factor(rep(c("A", "B"), length = nrow(lme4::sleepstudy))),
      days_grouped = cut(
        Days,
        breaks = stats::quantile(Days, probs = seq(0, 1, length = 5)),
        include.lowest = TRUE
      ),
      Subject = dplyr::case_when(
        group == "A" ~ as.character(Subject),
        TRUE ~ as.character(as.numeric(as.character(Subject)) + 50)
      )
    ) %>%
    dplyr::distinct_at(.vars = c("Subject", "days_grouped", "group"), .keep_all = TRUE)

  expect_silent(result <- fit_mmrm(
    vars = list(
      response = "Reaction",
      covariates = c(),
      id = "Subject",
      arm = "group",
      visit = "days_grouped"
    ),
    data = dat,
    cor_struct = "compound-symmetry",
    parallel = TRUE
  ))
})

# Produces different version of a ADQS subset.
get_adqs <- function(version = c("A", "B")) {
  version <- match.arg(version)
  adqs <- mmrm_test_data
  set.seed(123, kind = "Mersenne-Twister") # Because of `sample` below.
  adqs_f <- adqs %>%
    droplevels() %>%
    { # nolint
      if (version == "B") {
        dplyr::mutate(
          .,
          # Introduce extra missing response variable values.
          FEV1 = ifelse(
            sample(c(TRUE, FALSE), size = length(FEV1), replace = TRUE, prob = c(0.1, 0.9)),
            NA,
            FEV1
          ),
          # And also covariate values.
          FEV1_BL = ifelse(
            sample(c(TRUE, FALSE), size = length(FEV1_BL), replace = TRUE, prob = c(0.1, 0.9)),
            NA,
            FEV1_BL
          )
        )
      } else {
        # No further changes in version A.
        .
      }
    }

  return(adqs_f)
}

test_that("fit_mmrm works with unstructured covariance matrix and produces same results as SAS", {
  skip("does not converge at the moment, waiting for mmrm usage")

  if (compareVersion(as.character(packageVersion("lme4")), "1.1.21") <= 0) {
    skip("tests dont run with older version of lme4")
  }

  adqs_f <- get_adqs(version = "A")

  mmrm_results <- fit_mmrm(
    vars = list(
      response = "FEV1",
      covariates = c("SEX", "FEV1_BL"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = adqs_f,
    cor_struct = "unstructured",
    weights_emmeans = "equal",
    optimizer = "automatic"
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
    lme4::REMLcrit(mmrm_results$fit),
    3429.306,
    tolerance = 0.0001
  )

  # Fixed effects estimates.
  summary_table <- summary(mmrm_results$fit, ddf = "Satterthwaite")
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
  lsmeans_estimates <- mmrm_results$lsmeans$estimates[, c("ARMCD", "AVISIT", "estimate", "lower_cl", "upper_cl")]
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
    tolerance = 0.00001
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

test_that("fit_mmrm works also with missing data", {
  skip_if_too_deep(3)

  adqs_f <- get_adqs(version = "B")
  stopifnot(identical(
    nrow(stats::na.omit(adqs_f)),
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
    data = adqs_f,
    cor_struct = "unstructured",
    weights_emmeans = "equal",
    optimizer = "automatic"
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
    lme4::REMLcrit(mmrm_results$fit),
    2791.552,
    tolerance = 0.00001
  )

  # Fixed effects estimates.
  summary_table <- summary(mmrm_results$fit, ddf = "Satterthwaite")
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
  lsmeans_estimates <- mmrm_results$lsmeans$estimates[, c("ARMCD", "AVISIT", "estimate", "lower_cl", "upper_cl")]
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
    tolerance = 0.00001
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
    tolerance = 0.001
  )

  # Diagnostics.
  diagnostics <- mmrm_results$diagnostics
  diagnostics_values <- unlist(diagnostics)
  expected_diagnostics_values <- c(2791.552, 2811.552, 2812.076, 2844.282)
  expect_equal(
    diagnostics_values,
    expected_diagnostics_values,
    tolerance = 0.00001,
    ignore_attr = TRUE
  )
})

test_that("fit_mmrm works with compound symmetry covariance structure", {
  adqs_f <- get_adqs(version = "B")
  stopifnot(identical(
    nrow(stats::na.omit(adqs_f)),
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
    data = adqs_f,
    cor_struct = "compound-symmetry",
    weights_emmeans = "equal"
  )

  # Compare vs. SAS results calculated with the following statements:
  #
  # PROC MIXED DATA = ana.dat cl method=reml;
  # CLASS USUBJID ARMCD(ref='ARM B') AVISIT(ref='WEEK 1 DAY 8');
  # MODEL AVAL = BMRKR1 ARMCD AVISIT ARMCD*AVISIT / ddfm=satterthwaite solution chisq;
  # REPEATED AVISIT / subject=USUBJID type=cs r rcorr;
  # LSMEANS AVISIT*ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
  # RUN;

  # REML criterion value.
  expect_equal(
    lme4::REMLcrit(mmrm_results$fit),
    2888.673,
    tolerance = 0.0001
  )

  # Fixed effects estimates.
  summary_table <- summary(mmrm_results$fit, ddf = "Satterthwaite")
  fixed_effects <- as.data.frame(summary_table$coefficients[, c("Estimate", "df", "Pr(>|t|)")])

  expected_fixed_effects <- data.frame(
    Estimate = c(
      25.9032974, 0.1567089, 4.4368365, 4.7581709, 10.8380981, 14.7928207, 0.3221395, -0.9733533, 1.9690800
    ),
    df = c(
      245.0971, 192.7569, 429.8670, 335.5291, 353.9999, 356.4097, 332.8183, 346.6242, 336.6210
    ),
    "Pr(>|t|)" = c(
      5.338736e-33, 1.322295e-04, 3.997563e-04, 4.481823e-05, 6.025417e-18,
      1.417764e-30, 8.451677e-01, 5.689422e-01, 2.353690e-01
    ),
    row.names = c(
      "(Intercept)", "FEV1_BL", "ARMCDTRT", "AVISITVIS2", "AVISITVIS3",
      "AVISITVIS4", "ARMCDTRT:AVISITVIS2", "ARMCDTRT:AVISITVIS3", "ARMCDTRT:AVISITVIS4"
    ),
    check.names = FALSE # Necessary to get right p-value column name.
  )

  expect_equal_result_tables(
    subset(fixed_effects),
    subset(expected_fixed_effects)
  )

  # Now compare LS means and their contrasts.
  lsmeans_estimates <- mmrm_results$lsmeans$estimates[, c("ARMCD", "AVISIT", "estimate", "lower_cl", "upper_cl")]
  expected_lsmeans_estimates <- data.frame(
    ARMCD = factor(
      c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
      labels = c("PBO", "TRT"),
    ),
    AVISIT = factor(
      c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L),
      labels = c("VIS1", "VIS2", "VIS3", "VIS4"),
    ),
    estimate = c(
      32.16546, 36.92363,
      43.00356, 46.95828, 36.60229, 41.6826, 46.46704, 53.36419
    ),
    lower_cl = c(
      30.47693,
      35.20825, 41.21033, 45.2281, 34.83834, 39.94995, 44.63662, 51.6316
    ),
    upper_cl = c(
      33.85398, 38.639, 44.79678, 48.68845, 38.36625,
      43.41526, 48.29746, 55.09679
    )
  )
  expect_equal(
    lsmeans_estimates,
    expected_lsmeans_estimates,
    tolerance = 0.00001
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
      4.436837, 4.758976, 3.463483, 6.405917
    ),
    df = c(
      429.8670, 429.6275, 430.7465, 429.8073
    ),
    lower_cl = c(
      1.9929345, 2.3207047, 0.9011265, 3.9577235
    ),
    upper_cl = c(
      6.880739, 7.197247, 6.025840, 8.854110
    ),
    p_value = c(
      3.997563e-04, 1.436671e-04, 8.183930e-03, 4.118787e-07
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
      44.532058, 9.022132, 9.022132, 9.022132,
      9.022132, 44.532058, 9.022132, 9.022132,
      9.022132, 9.022132, 44.532058, 9.022132,
      9.022132, 9.022132, 9.022132, 44.532058
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
  expected_diagnostics_values <- c(2888.673, 2892.673, 2892.701, 2899.219)
  expect_equal(
    diagnostics_values,
    expected_diagnostics_values,
    tolerance = 0.00001,
    ignore_attr = TRUE
  )
})
