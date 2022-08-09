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

# g_mmrm_lsmeans ----

test_that("g_mmrm_lsmeans works well with default arguments", {
  result <- expect_silent(g_mmrm_lsmeans(fit_mmrm_object))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with defaults", result)
})

test_that("g_mmrm_lsmeans can select estimates only", {
  result <- expect_silent(g_mmrm_lsmeans(fit_mmrm_object, select = "estimates"))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with estimates", result)
})

test_that("g_mmrm_lsmeans can select contrasts only", {
  result <- expect_silent(g_mmrm_lsmeans(fit_mmrm_object, select = "contrasts"))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with contrasts", result)
})

test_that("g_mmrm_lsmeans works well with constant baseline added", {
  result <- expect_silent(g_mmrm_lsmeans(fit_mmrm_object, constant_baseline = c(XYZBSL = 0)))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with constant baseline", result)
})

test_that("g_mmrm_lsmeans works well with lines added", {
  result <- expect_silent(g_mmrm_lsmeans(fit_mmrm_object, show_lines = TRUE))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with lines", result)
})

test_that("g_mmrm_lsmeans works well with multiple customizations", {
  result <- expect_silent(g_mmrm_lsmeans(
    fit_mmrm_object,
    titles = c(estimates = "LS means", contrasts = "LS mean contrasts"),
    xlab = "visit",
    ylab = "estimates",
    width = 0.3,
    show_pval = FALSE,
    show_lines = TRUE,
    constant_baseline = c(BLA = 2)
  ))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with multiple customizations", result)
})

test_that("g_mmrm_lsmeans works well with constant baseline and no arms", {
  result <- expect_silent(g_mmrm_lsmeans(fit_mmrm_no_arms_object, constant_baseline = c(XYZBSL = 10)))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with constant baseline and no arms", result)
})

test_that("g_mmrm_lsmeans plots estimates table as expected", {
  result <- expect_silent(g_mmrm_lsmeans(
    fit_mmrm_object,
    select = "estimates",
    estimates_table = c("n", "estimate", "ci", "se")
  ))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with estimates table", result)
})

test_that("g_mmrm_lsmeans plots estimates table with custom settings", {
  result <- expect_silent(g_mmrm_lsmeans(
    fit_mmrm_object,
    select = "estimates",
    estimates_table = c("n", "estimate", "ci", "se"),
    table_format = c(
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
  ))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans with customized estimates table", result)
})

test_that("g_mmrm_lsmeans plots estimates table also without arms", {
  result <- expect_silent(g_mmrm_lsmeans(
    fit_mmrm_no_arms_object,
    select = "estimates",
    estimates_table = c("n", "se")
  ))
  vdiffr::expect_doppelganger("g_mmrm_lsmeans without arms and with estimates table", result)
})
