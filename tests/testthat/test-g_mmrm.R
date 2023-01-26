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
  result <- g_mmrm_diagnostic(fit_mmrm_object)
  vdiffr::expect_doppelganger("g_mmrm_diagnostic with defaults", result)
})

test_that("g_mmrm_diagnostic works well for Q-Q residuals plot", {
  result <- g_mmrm_diagnostic(fit_mmrm_object, type = "q-q-residual")
  vdiffr::expect_doppelganger("g_mmrm_diagnostic q-q-residual defaults", result)
})

test_that("g_mmrm_diagnostic works well for Q-Q residuals plot with z threshold", {
  result <- g_mmrm_diagnostic(fit_mmrm_object, type = "q-q-residual", z_threshold = 2)
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

  result <- g_mmrm_diagnostic(mmrm_results, type = "q-q-residual")
  vdiffr::expect_doppelganger("g_mmrm_diagnostic q-q-residual with weighted MMRM", result)
})

# g_mmrm_lsmeans ----

test_that("g_mmrm_lsmeans works well with default arguments", {
  result <- g_mmrm_lsmeans(fit_mmrm_object)
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with defaults", result)
})

test_that("g_mmrm_lsmeans can select estimates only", {
  result <- g_mmrm_lsmeans(fit_mmrm_object, select = "estimates")
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with estimates", result)
})

test_that("g_mmrm_lsmeans can select contrasts only", {
  result <- g_mmrm_lsmeans(fit_mmrm_object, select = "contrasts")
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with contrasts", result)
})

test_that("g_mmrm_lsmeans works well with constant baseline added", {
  result <- g_mmrm_lsmeans(fit_mmrm_object, constant_baseline = c(XYZBSL = 0))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with constant baseline", result)
})

test_that("g_mmrm_lsmeans works well with lines added", {
  result <- g_mmrm_lsmeans(fit_mmrm_object, show_lines = TRUE)
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with lines", result)
})

test_that("g_mmrm_lsmeans works well with multiple customizations", {
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
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with multiple customizations", result)
})

test_that("g_mmrm_lsmeans works well with constant baseline and no arms", {
  result <- g_mmrm_lsmeans(fit_mmrm_no_arms_object, constant_baseline = c(XYZBSL = 10))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with constant baseline and no arms", result)
})

test_that("g_mmrm_lsmeans plots stats table for estimates as expected", {
  result <- g_mmrm_lsmeans(
    fit_mmrm_object,
    select = "estimates",
    table_stats = c("n", "estimate", "ci", "se")
  )
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with estimates stats table", result)
})

test_that("g_mmrm_lsmeans plots estimates stats table with custom settings", {
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
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with customized estimates stats table", result)
})

test_that("g_mmrm_lsmeans plots estimates stats table also without arms", {
  result <- g_mmrm_lsmeans(
    fit_mmrm_no_arms_object,
    select = "estimates",
    table_stats = c("n", "se")
  )
  vdiffr::expect_doppelganger("g_mmrm_lsmeans without arms and with table", result)
})

test_that("g_mmrm_lsmeans plots estimates stats table also with constant baseline", {
  result <- g_mmrm_lsmeans(
    fit_mmrm_object,
    select = "estimates",
    table_stats = c("n", "se"),
    constant_baseline = c(BSL = 1),
    n_baseline = c(TRT = 101L, PBO = 100L)
  )
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with table and constant baseline", result)
})

test_that("g_mmrm_lsmeans plots estimates stats table also with constant baseline and without arms", {
  result <- g_mmrm_lsmeans(
    fit_mmrm_no_arms_object,
    select = "estimates",
    table_stats = c("n", "se"),
    constant_baseline = c(BSL = 1),
    n_baseline = 150L
  )
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with table, baseline, no arms", result)
})
