fit_mmrm_object <- fit_mmrm(
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

fit_mmrm_no_arms_object <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    id = "USUBJID",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "unstructured"
)

# g_mmrm_diagnostic ----

test_that("g_mmrm_diagnostic works well with defaults", {
  expect_silent(result <- g_mmrm_diagnostic(fit_mmrm_object))

  skip_if_not_installed("vdiffr")
  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_diagnostic with defaults", result)
})

test_that("g_mmrm_diagnostic works well for Q-Q residuals plot", {
  expect_silent(result <- g_mmrm_diagnostic(fit_mmrm_object, type = "q-q-residual"))

  skip_if_not_installed("vdiffr")
  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_diagnostic q-q-residual defaults", result)
})

test_that("g_mmrm_diagnostic works well for Q-Q residuals plot with z threshold", {
  expect_silent(result <- g_mmrm_diagnostic(fit_mmrm_object, type = "q-q-residual", z_threshold = 2))

  skip_if_not_installed("vdiffr")
  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_diagnostic q-q-residual with z threshold", result)
})

test_that("g_mmrm_diagnostic works well for Q-Q residuals plot with weights", {
  dat <- get_version(version = "A")

  set.seed(123, kind = "Wichmann-Hill")
  dat$w <- rexp(n = nrow(dat))

  mmrm_results <- fit_mmrm(
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
  )
  expect_silent(result <- g_mmrm_diagnostic(mmrm_results, type = "q-q-residual"))

  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_diagnostic q-q-residual with weighted MMRM", result)
})

# g_mmrm_lsmeans ----

test_that("g_mmrm_lsmeans works well with default arguments", {
  expect_silent(
    expect_message(
      result <- g_mmrm_lsmeans(fit_mmrm_object),
      "Coordinate system already present"
    )
  )

  skip_if_not_installed("vdiffr")
  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with defaults", result)
})

test_that("g_mmrm_lsmeans can select estimates only", {
  expect_silent(result <- g_mmrm_lsmeans(fit_mmrm_object, select = "estimates"))

  skip_if_not_installed("vdiffr")
  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with estimates", result)
})

test_that("g_mmrm_lsmeans can select contrasts only", {
  expect_silent(
    expect_message(
      result <- g_mmrm_lsmeans(fit_mmrm_object, select = "contrasts"),
      "Coordinate system already present"
    )
  )

  skip_if_not_installed("vdiffr")
  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with contrasts", result)
})

test_that("g_mmrm_lsmeans works well with constant baseline added", {
  expect_silent(
    expect_message(
      result <- g_mmrm_lsmeans(fit_mmrm_object, constant_baseline = c(XYZBSL = 0)),
      "Coordinate system already present"
    )
  )

  skip_if_not_installed("vdiffr")
  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with constant baseline", result)
})

test_that("g_mmrm_lsmeans works well with lines added", {
  expect_silent(
    expect_message(
      result <- g_mmrm_lsmeans(fit_mmrm_object, show_lines = TRUE),
      "Coordinate system already present"
    )
  )

  skip_if_not_installed("vdiffr")
  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with lines", result)
})

test_that("g_mmrm_lsmeans works well with multiple customizations", {
  expect_silent(
    result <- g_mmrm_lsmeans(
      fit_mmrm_object,
      titles = c(estimates = "LS means", contrasts = "LS mean contrasts"),
      xlab = "visit",
      ylab = "estimates",
      width = 0.3,
      show_pval = FALSE,
      show_lines = TRUE,
      constant_baseline = c(BLA = 2)
    )
  )

  skip_if_not_installed("vdiffr")
  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with multiple customizations", result)
})

test_that("g_mmrm_lsmeans works well with constant baseline and no arms", {
  expect_silent(result <- g_mmrm_lsmeans(fit_mmrm_no_arms_object, constant_baseline = c(XYZBSL = 10)))

  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with constant baseline and no arms", result)
})

test_that("g_mmrm_lsmeans plots stats table for estimates as expected", {
  expect_silent(
    result <- g_mmrm_lsmeans(
      fit_mmrm_object,
      select = "estimates",
      table_stats = c("n", "estimate", "ci", "se")
    )
  )

  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with estimates stats table", result)
})

test_that("g_mmrm_lsmeans plots estimates stats table with custom settings", {
  expect_silent(
    result <- g_mmrm_lsmeans(
      fit_mmrm_object,
      select = "estimates",
      table_stats = c("n", "estimate", "ci", "se"),
      table_formats = c(
        n = "xx.xx",
        estimate = "xx.xxxx",
        se = "xx.",
        ci = "(xx.xxxx, xx.xxxx)"
      ),
      table_labels = c(
        n = "N",
        estimate = "mean",
        se = "SE",
        ci = "CI"
      ),
      table_font_size = 2,
      table_rel_height = 1
    )
  )

  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with customized estimates stats table", result)
})

test_that("g_mmrm_lsmeans plots estimates stats table also without arms", {
  expect_silent(
    result <- g_mmrm_lsmeans(
      fit_mmrm_no_arms_object,
      select = "estimates",
      table_stats = c("n", "se")
    )
  )

  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans without arms and with table", result)
})

test_that("g_mmrm_lsmeans plots estimates stats table also with constant baseline", {
  expect_silent(
    result <- g_mmrm_lsmeans(
      fit_mmrm_object,
      select = "estimates",
      table_stats = c("n", "se"),
      constant_baseline = c(BSL = 1),
      n_baseline = c(TRT = 101L, PBO = 100L)
    )
  )

  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with table and constant baseline", result)
})

test_that("g_mmrm_lsmeans plots estimates stats table also with constant baseline and without arms", {
  expect_silent(
    result <- g_mmrm_lsmeans(
      fit_mmrm_no_arms_object,
      select = "estimates",
      table_stats = c("n", "se"),
      constant_baseline = c(BSL = 1),
      n_baseline = 150L
    )
  )

  skip_on_ci()
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with table, baseline, no arms", result)
})
