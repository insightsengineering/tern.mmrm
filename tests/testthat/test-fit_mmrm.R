library(dplyr)

testthat::test_that("check_mmrm_vars passes with healthy inputs and returns correct labels", {
  # No additional covariates.
  vars1 <- list(
    response = "FEV1",
    covariates = c(),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  testthat::expect_silent(result1 <- check_mmrm_vars(vars1, mmrm_test_data))
  expected1 <- list(
    response = c(FEV1 = "FEV1"),
    id = c(USUBJID = "USUBJID"),
    arm = c(ARMCD = "ARMCD"),
    visit = c(AVISIT = "AVISIT")
  )
  testthat::expect_identical(result1, expected1)

  # Additional covariates.
  vars2 <- vars1
  vars2$covariates <- c("RACE", "SEX", "FEV1_BL")
  testthat::expect_silent(result2 <- check_mmrm_vars(vars2, mmrm_test_data))
  expected2 <- c(
    expected1,
    list(
      parts = c(
        RACE = "RACE",
        SEX = "SEX",
        FEV1_BL = "FEV1_BL"
      )
    )
  )
  testthat::expect_identical(result2, expected2)

  # Without arm
  vars3 <- vars1
  vars3$arm <- NULL
  testthat::expect_silent(result3 <- check_mmrm_vars(vars3, mmrm_test_data))

  expected3 <- list(
    response = c(FEV1 = "FEV1"),
    id = c(USUBJID = "USUBJID"),
    visit = c(AVISIT = "AVISIT")
  )
  testthat::expect_identical(result3, expected3)
})

testthat::test_that("check_mmrm_vars works with interaction terms in `covariates`", {
  vars <- list(
    response = "FEV1",
    covariates = c("ARMCD*FEV1_BL", "SEX", "FEV1_BL:ARMCD"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  testthat::expect_silent(result <- check_mmrm_vars(vars, mmrm_test_data))
  expected <- list(
    response = c(FEV1 = "FEV1"),
    id = c(USUBJID = "USUBJID"),
    arm = c(ARMCD = "ARMCD"),
    visit = c(AVISIT = "AVISIT"),
    parts = c(
      ARMCD = "ARMCD",
      FEV1_BL = "FEV1_BL",
      SEX = "SEX"
    )
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("check_mmrm_vars works when there are missing values", {
  set.seed(123)
  data <- mmrm_test_data %>%
    dplyr::mutate(
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

  vars <- list(
    response = "FEV1",
    covariates = c("ARMCD*FEV1_BL", "RACE", "RACE:ARMCD"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  testthat::expect_silent(result <- check_mmrm_vars(vars, data))
  expected <- list(
    response = c(FEV1 = "FEV1"),
    id = c(USUBJID = "USUBJID"),
    arm = c(ARMCD = "ARMCD"),
    visit = c(AVISIT = "AVISIT"),
    parts = c(
      ARMCD = "ARMCD",
      FEV1_BL = "FEV1_BL",
      RACE = "RACE"
    )
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("check_mmrm_vars fails if a variable is missing", {
  full_vars <- list(
    response = "AVAL",
    id = "USUBJID",
    visit = "AVISIT"
  )

  for (var in names(full_vars)) {
    incomplete_vars <- full_vars
    incomplete_vars[[var]] <- NULL
    testthat::expect_error(check_mmrm_vars(incomplete_vars, mmrm_test_data))
  }
})

testthat::test_that("check_mmrm_vars fails if a variable is not included in `data`", {
  vars <- list(
    response = "AVAL",
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )

  for (var in names(vars)) {
    var_name <- vars[[var]]
    incomplete_data <- mmrm_test_data
    incomplete_data[[var_name]] <- NULL
    testthat::expect_error(check_mmrm_vars(vars, mmrm_test_data))
  }
})

testthat::test_that("build_mmrm_formula builds the correct formula", {
  # No additional covariates.
  vars1 <- list(
    response = "AVAL",
    covariates = c(),
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )
  cor_struct1 <- "unstructured"
  result1 <- build_mmrm_formula(vars1, cor_struct1)
  expected1 <- AVAL ~ ARM * AVISIT + (0 + AVISIT | USUBJID)
  testthat::expect_equal(result1, expected1)

  # Additional covariates.
  vars2 <- vars1
  vars2$covariates <- c("STRATA1", "BMRKR2")
  cor_struct2 <- "compound-symmetry"
  result2 <- build_mmrm_formula(vars2, cor_struct2)
  expected2 <- AVAL ~ STRATA1 + BMRKR2 + ARM * AVISIT + (1 | USUBJID)
  testthat::expect_equal(result2, expected2)

  # Without arm
  vars3 <- vars1
  vars3$arm <- NULL
  cor_struct3 <- "random-quadratic"
  result3 <- build_mmrm_formula(vars3, cor_struct3)
  expected3 <- AVAL ~ AVISIT + (stats::poly(as.numeric(AVISIT), df = 2) | USUBJID)
})

testthat::test_that("fit_lme4_single_optimizer works as expected when there are no warnings or messages", {
  # Default optimizer used.
  result1 <- fit_lme4_single_optimizer(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  testthat::expect_s4_class(result1, "lmerModLmerTest")
  testthat::expect_identical(attr(result1, "optimizer"), "nloptwrap_bobyqa")
  testthat::expect_identical(attr(result1, "messages"), character(0))

  # Non-default optimizer used.
  result2 <- fit_lme4_single_optimizer(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy,
    optimizer = "nmkbw"
  )
  testthat::expect_s4_class(result2, "lmerModLmerTest")
  testthat::expect_identical(attr(result2, "optimizer"), "nmkbw")
  testthat::expect_identical(attr(result2, "messages"), character(0))

  # Results should be equal (without attributes which capture optimizer details).
  testthat::expect_equal(result1, result2, check.attributes = FALSE)
})

# Helper function which is another implementation of the covariance matrix estimate from
# https://stackoverflow.com/questions/45650548/get-residual-variance-covariance-matrix-in-lme4
alternative_cov_estimate <- function(fit) {
  # We want to keep the same variable names etc. as in the reference above, therefore no linting here.
  # nolint start
  var.d <- Matrix::crossprod(lme4::getME(fit, "Lambdat"))
  Zt <- lme4::getME(fit, "Zt")
  vr <- stats::sigma(fit)^2
  var.b <- vr * (Matrix::t(Zt) %*% var.d %*% Zt)
  sI <- vr * Matrix::Diagonal(nrow(fit@frame))
  var.y <- var.b + sI
  return(var.y)
  # nolint end
}

testthat::test_that("get_lme4_cov_estimate works as expected with a random slope model", {
  fit <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  result <- get_lme4_cov_estimate(fit)
  expected <- as.matrix(alternative_cov_estimate(fit)[1:10, 1:10]) # We use first 10 obs.
  testthat::expect_equal(result, expected, check.attributes = FALSE)
  testthat::expect_identical(
    attributes(result),
    list(
      dim = c(10L, 10L),
      id = "308",
      n_parameters = 4L
    )
  )
})

testthat::test_that("get_lme4_cov_estimate works with a random intercept model", {
  fit <- fit_lme4(
    formula = Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy
  )
  testthat::expect_silent(result <- get_lme4_cov_estimate(fit))
})

testthat::test_that("get_lme4_cov_estimate works as expected with unbalanced data and independent of sorting", {
  # Obtain unbalanced data set.
  set.seed(123, kind = "Mersenne-Twister")
  data_unsorted <- lme4::sleepstudy %>%
    dplyr::sample_frac(0.5) # This randomly samples 50% of the rows of the data set.

  # Fit with unsorted data.
  fit_unsorted <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = data_unsorted
  )
  result_unsorted <- get_lme4_cov_estimate(fit_unsorted)
  testthat::expect_identical(
    attributes(result_unsorted),
    list(
      dim = c(10L, 10L),
      id = "372",
      n_parameters = 4L
    )
  )

  # Fit with sorted data.
  data_sorted <- data_unsorted %>%
    dplyr::arrange(Subject, Days)
  fit_sorted <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = data_sorted
  )
  result_sorted <- get_lme4_cov_estimate(fit_sorted)
  testthat::expect_identical(
    attributes(result_sorted),
    list(
      dim = c(10L, 10L),
      id = "372",
      n_parameters = 4L
    )
  )

  # Check if the reordered result from unsorted fit equals the sorted fit result.
  order_index <- data_unsorted %>%
    dplyr::filter(Subject == "372") %>%
    dplyr::pull(Days) %>%
    order()
  testthat::expect_equal(
    result_unsorted[order_index, order_index],
    result_sorted,
    check.attributes = FALSE
  )
})

testthat::test_that("get_lme4_cov_estimate works as expected with a random intercept model and unbalanced data", {
  set.seed(123, kind = "Mersenne-Twister")
  data <- lme4::sleepstudy %>%
    dplyr::sample_frac(0.5)
  fit <- fit_lme4(
    formula = Reaction ~ Days + (1 | Subject),
    data = data
  )
  result <- get_lme4_cov_estimate(fit)
  id_indices <- which(data$Subject == "372") # We get id 372 here.
  expected <- as.matrix(alternative_cov_estimate(fit)[id_indices, id_indices])
  testthat::expect_equal(result, expected, check.attributes = FALSE)
  testthat::expect_identical(
    attributes(result),
    list(
      dim = c(10L, 10L),
      id = "372",
      n_parameters = 2L
    )
  )
})

testthat::test_that("get_lme4_diagnostics works as expected with a random slope model", {
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
  testthat::expect_equal(result, expected, tol = 0.0001)
})

testthat::test_that("fit_lme4_single_optimizer correctly captures warnings and messages", {
  data <- lme4::sleepstudy
  data$days_copy <- data$Days

  testthat::expect_silent(
    result <- fit_lme4_single_optimizer(
      formula = Reaction ~ Days + (Days + days_copy | Subject),
      data = data
    )
  )
  testthat::expect_s4_class(result, "lmerModLmerTest")
  testthat::expect_identical(attr(result, "optimizer"), "nloptwrap_bobyqa")
  testthat::expect_gt(length(attr(result, "messages")), 0)
})

testthat::test_that("fit_lme4_single_optimizer fails when there is an error", {
  testthat::expect_error(
    fit_lme4_single_optimizer(
      formula = Reaction ~ Days + (Days | Subject),
      data = does_not_exist
    ),
    "bad 'data'"
  )
})

testthat::test_that("summary_all_fits works as expected", {
  single_fit <- fit_lme4_single_optimizer(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  all_fits <- list(a = single_fit, b = single_fit, c = single_fit)
  result <- summary_all_fits(all_fits)
  testthat::expect_is(result, "list")
  testthat::expect_named(result, c("messages", "fixef", "llik", "feval"))
  lapply(
    result,
    expect_named,
    expected = c("a", "b", "c") # Note that is also implicitly tests the length of the result elements.
  )
})

test_that("refit_lme4_all_optimizers fails when no optimizer succeeds", {
  skip_if_too_deep(5)

  original_fit <- fit_lme4_single_optimizer(
    formula = Reaction ~ Days + (factor(Days) | Subject),
    data = lme4::sleepstudy,
    optimizer = "nloptwrap_bobyqa"
  )
  testthat::expect_gt(length(attr(original_fit, "messages")), 0)
  testthat::expect_error(
    refit_lme4_all_optimizers(original_fit),
    "No optimizer led to a successful model fit"
  )
})

testthat::test_that("refit_lme4_all_optimizers can find a working optimizer if there is one", {
  data <- lme4::sleepstudy %>%
    dplyr::mutate(
      days_grouped = cut(
        Days,
        breaks = stats::quantile(Days, probs = seq(0, 1, length = 5)),
        include.lowest = TRUE
      )
    )
  # This optimizer fails.
  failed_fit <- fit_lme4_single_optimizer(
    formula = Reaction ~ days_grouped + (days_grouped | Subject),
    data = data,
    optimizer = "nloptwrap_bobyqa"
  )
  testthat::expect_gt(length(attr(failed_fit, "messages")), 0)
  # But this one works.
  successful_fit <- fit_lme4_single_optimizer(
    formula = Reaction ~ days_grouped + (days_grouped | Subject),
    data = data,
    optimizer = "nloptwrap_neldermead"
  )
  testthat::expect_length(attr(successful_fit, "messages"), 0L)
  # So we expect that we can find the working one (or at least one working one).
  final_fit <- refit_lme4_all_optimizers(failed_fit)
  testthat::expect_length(attr(final_fit, "messages"), 0L)
  testthat::expect_equal(successful_fit, final_fit, check.attributes = FALSE)
})


testthat::test_that("refit_lme4_all_optimizers works with parallelization", {
  skip_if_too_deep(5)

  original_fit <- fit_lme4_single_optimizer(
    formula = Reaction ~ Days + (factor(Days) | Subject),
    data = lme4::sleepstudy,
    optimizer = "nloptwrap_bobyqa"
  )
  testthat::expect_gt(length(attr(original_fit, "messages")), 0)
  # Note that here we get the wrong error message somehow in devtools::check.
  # Therefore we don't compare the message text.
  testthat::expect_error(
    refit_lme4_all_optimizers(original_fit, n_cores = 4L)
  )
})

testthat::test_that("fit_lme4 works with healthy inputs", {
  result <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  testthat::expect_s4_class(result, "lmerModLmerTest")
})

testthat::test_that("fit_lme4 fails when there are convergence issues with all optimizers", {
  data <- lme4::sleepstudy
  data$days_copy <- data$Days

  testthat::expect_error(
    fit_lme4(
      formula = Reaction ~ Days + (Days + days_copy | Subject),
      data = data,
      n_cores = 2L
    ),
    msg = "No optimizer led to a successful model fit"
  )
})

testthat::test_that("fit_lme4 fails when there are convergence issues with a specific optimizer", {
  data <- lme4::sleepstudy
  data$days_copy <- data$Days

  testthat::expect_error(
    fit_lme4(
      formula = Reaction ~ Days + (Days + days_copy | Subject),
      data = data,
      optimizer = "bobyqa"
    ),
    msg = "Chosen optimizer 'bobyqa' led to problems during model fit"
  )
})

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
    optimizer = "automatic" # fails to converge with "bobyqa"
  )
  testthat::expect_silent(result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = 0.95,
    weights = "proportional"
  ))
  testthat::expect_is(result, "list")
  testthat::expect_is(result$estimates, "data.frame")
  testthat::expect_is(result$contrasts, "data.frame")
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


testthat::test_that("fit_mmrm works with parallelization", {
  dat <- lme4::sleepstudy %>%
    dplyr::mutate(
      group = factor(rep(c("A", "B"), length = nrow(lme4::sleepstudy))),
      days_grouped = cut(
        Days,
        breaks = stats::quantile(Days, probs = seq(0, 1, length = 5)),
        include.lowest = TRUE
      ),
      Subject = case_when(
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

# Helper function to compare result and expected tables with proper handling of p-value column.
expect_equal_result_tables <- function(result,
                                       expected,
                                       tol = 0.001,
                                       pval_name = "Pr(>|t|)",
                                       pval_threshold = 0.0001) {
  pval_col <- match(pval_name, colnames(result))

  # Compare first non-pvalue columns.
  testthat::expect_equal(
    result[, -pval_col],
    expected[, -pval_col],
    tol = tol
  )

  # Then compare p-values which are not below the threshold in the expected table.
  exp_pval_is_below_thresh <- expected[, pval_col] < pval_threshold
  testthat::expect_equal(
    result[, pval_col][!exp_pval_is_below_thresh],
    expected[, pval_col][!exp_pval_is_below_thresh],
    tol = tol
  )

  # Now expect that the same p-values are below the thresholds in both tables.
  res_pval_is_below_thresh <- result[, pval_col] < pval_threshold
  testthat::expect_identical(
    exp_pval_is_below_thresh,
    res_pval_is_below_thresh
  )
}

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

testthat::test_that("fit_mmrm works with unstructured covariance matrix and produces same results as SAS", {
  skip_if_too_deep(5)

  if (compareVersion(as.character(packageVersion("lme4")), "1.1.21") <= 0) {
    testthat::skip("tests dont run with older version of lme4")
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
    optimizer = "nloptwrap_neldermead" # To speed up this test.
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
  testthat::expect_equal(
    lme4::REMLcrit(mmrm_results$fit),
    3429.306,
    tol = 0.0001
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
  testthat::expect_equal(
    lsmeans_estimates,
    expected_lsmeans_estimates,
    tol = 0.00001
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
      42.883581, 15.519333,  8.057128, 16.593116,
      15.519333, 26.628885,  4.627223, 10.074424,
      8.057128,  4.627223, 19.203109,  7.785749,
      16.593116, 10.074424,  7.785749, 99.811086
    ),
    nrow = 4L,
    ncol = 4L
  )
  testthat::expect_equal(
    cov_estimate,
    expected_cov_estimate,
    check.attributes = FALSE,
    tol = 0.001
  )

  # Diagnostics.
  diagnostics <- mmrm_results$diagnostics
  diagnostics_values <- unlist(diagnostics)
  expected_diagnostics_values <- c(3429.306, 3449.306, 3449.733, 3482.138)
  testthat::expect_equal(
    diagnostics_values,
    expected_diagnostics_values,
    tol = 0.00001,
    check.attributes = FALSE
  )
})

testthat::test_that("fit_mmrm works also with missing data", {
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
  testthat::expect_equal(
    lme4::REMLcrit(mmrm_results$fit),
    2791.552,
    tol = 0.00001
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
      c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
      labels = c("PBO", "TRT"),
    ),
    AVISIT = factor(
      c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
      labels = c("VIS1", "VIS2", "VIS3", "VIS4"),
    ),
    estimate = c(
      32.26162, 36.68816, 36.83153, 41.67930, 43.15265, 46.51746, 47.00095, 53.38205
    ),
    lower_cl = c(
      30.69289, 35.05131, 35.59335, 40.42744, 41.98076, 45.32000, 44.46010, 50.83683
    ),
    upper_cl = c(
      33.83035, 38.32501, 38.06971, 42.93116, 44.32454, 47.71491, 49.54179, 55.92727
    )
  )
  testthat::expect_equal(
    lsmeans_estimates,
    expected_lsmeans_estimates,
    tol = 0.00001
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
  testthat::expect_equal(
    cov_estimate,
    expected_cov_estimate,
    check.attributes = FALSE,
    tol = 0.001
  )

  # Diagnostics.
  diagnostics <- mmrm_results$diagnostics
  diagnostics_values <- unlist(diagnostics)
  expected_diagnostics_values <- c(2791.552, 2811.552, 2812.076, 2844.282)
  testthat::expect_equal(
    diagnostics_values,
    expected_diagnostics_values,
    tol = 0.00001,
    check.attributes = FALSE
  )
})

testthat::test_that("fit_mmrm works with compound symmetry covariance structure", {
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
  testthat::expect_equal(
    lme4::REMLcrit(mmrm_results$fit),
    2888.673,
    tol = 0.0001
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
      c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
      labels = c("PBO", "TRT"),
    ),
    AVISIT = factor(
      c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
      labels = c("VIS1", "VIS2", "VIS3", "VIS4"),
    ),
    estimate = c(
      32.16546, 36.60229, 36.92363, 41.68260, 43.00356, 46.46704, 46.95828, 53.36419
    ),
    lower_cl = c(
      30.47693, 34.83834, 35.20825, 39.94995, 41.21033, 44.63662, 45.22810, 51.63160
    ),
    upper_cl = c(
      33.85398, 38.36625, 38.63900, 43.41526, 44.79678, 48.29746, 48.68845, 55.09679
    )
  )
  testthat::expect_equal(
    lsmeans_estimates,
    expected_lsmeans_estimates,
    tol = 0.00001
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
  testthat::expect_equal(
    cov_estimate,
    expected_cov_estimate,
    check.attributes = FALSE,
    tol = 0.001
  )

  # Diagnostics.
  diagnostics <- mmrm_results$diagnostics
  diagnostics_values <- unlist(diagnostics)
  expected_diagnostics_values <- c(2888.673, 2892.673, 2892.701, 2899.219)
  testthat::expect_equal(
    diagnostics_values,
    expected_diagnostics_values,
    tol = 0.00001,
    check.attributes = FALSE
  )
})
