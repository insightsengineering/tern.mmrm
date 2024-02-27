dat <- mmrm_test_data
dat$EXTRA <- factor(c(rep("A", 10), rep("B", nrow(dat) - 10))) # nolint
formatters::var_labels(dat) <- c(
  "Subject ID",
  "Visit No.",
  "Treatment",
  "Race",
  "Gender",
  "FEV1 at Baseline",
  "FEV1 measurement",
  "Extra variable"
)
mmrm_results <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = dat,
  cor_struct = "unstructured",
  averages_emmeans = list("VIS1+2" = c("VIS1", "VIS2"))
)

# h_mmrm_subgroup_df ----

test_that("h_mmrm_subgroup_df works as expected for overall group", {
  result <- h_mmrm_subgroup_df(
    lsmeans = mmrm_results$lsmeans,
    overall_fit = mmrm_results,
    is_in_subset = rep(TRUE, nrow(mmrm_test_data)),
    visit = "VIS3",
    treatment_arm = "TRT",
    subgroup = "All",
    var = "ALL",
    label = "All Patients"
  )
  expected <- list(
    estimates = data.frame(
      arm = factor(c("PBO", "TRT")),
      n = c(71L, 58L),
      lsmean = c(43.3, 46.4),
      subgroup = c("All", "All"),
      var = c("ALL", "ALL"),
      var_label = c("All Patients", "All Patients"),
      row_type = c("content", "content")
    ),
    contrasts = data.frame(
      n_tot = 129L,
      diff = 3.1,
      lcl = 1.7,
      ucl = 4.4,
      pval = 0.00001,
      conf_level = 0.95,
      subgroup = "All",
      var = "ALL",
      var_label = "All Patients",
      row_type = "content"
    )
  )
  expect_equal(result, expected, tolerance = 1e-1)
})

test_that("h_mmrm_subgroup_df works as expected for subgroup when fit did not converge", {
  result <- h_mmrm_subgroup_df(
    lsmeans = NULL,
    overall_fit = mmrm_results,
    is_in_subset = c(FALSE, rep(TRUE, nrow(mmrm_test_data) - 1)),
    visit = "VIS2",
    treatment_arm = "TRT",
    subgroup = "Sub",
    var = "FOO",
    label = "Foo subgroup"
  )
  expected <- list(
    estimates = data.frame(
      arm = factor(c("PBO", "TRT")),
      n = c(69L, 71L),
      lsmean = as.numeric(c(NA, NA)),
      subgroup = c("Sub", "Sub"),
      var = c("FOO", "FOO"),
      var_label = c("Foo subgroup", "Foo subgroup"),
      row_type = c("analysis", "analysis")
    ),
    contrasts = data.frame(
      n_tot = 128L,
      diff = as.numeric(NA),
      lcl = as.numeric(NA),
      ucl = as.numeric(NA),
      pval = as.numeric(NA),
      conf_level = 0.95,
      subgroup = "Sub",
      var = "FOO",
      var_label = "Foo subgroup",
      row_type = "analysis"
    )
  )
  expect_equal(result, expected, tolerance = 1e-1)
})

# extract_mmrm_subgroups ----

test_that("extract_mmrm_subgroups works as expected", {
  result <- extract_mmrm_subgroups(
    fit = mmrm_results,
    visit = "VIS3",
    subgroups = c("RACE", "SEX")
  )
  expect_snapshot_value(result, tolerance = 1e-2, style = "serialize")
})

test_that("extract_mmrm_subgroups works as expected without subgroup variables", {
  result <- extract_mmrm_subgroups(
    fit = mmrm_results,
    visit = "VIS2"
  )
  expect_snapshot_value(result, tolerance = 1e-2, style = "serialize")
})

test_that("extract_mmrm_subgroups works as expected with groups_lists", {
  result <- extract_mmrm_subgroups(
    fit = mmrm_results,
    visit = "VIS2",
    subgroups = "RACE",
    groups_lists = list(RACE = list(
      "A" = c("Asian", "Black or African American"),
      "B" = c("Black or African American", "White")
    ))
  )
  expect_snapshot_value(result, tolerance = 1e-2, style = "serialize")
})

test_that("extract_mmrm_subgroups works when model does not work for some subgroups", {
  expect_message(result <- extract_mmrm_subgroups(
    fit = mmrm_results,
    visit = "VIS1+2",
    subgroups = c("SEX", "EXTRA")
  ))
  expect_snapshot_value(result, tolerance = 1e-2, style = "serialize")
})

# a_mmrm_subgroups ----

test_that("a_mmrm_subgroups works as expected", {
  result <- a_mmrm_subgroups(list(n = "xx."))
  expect_list(result, len = 1L)
  expect_function(result$n, args = c("df", "labelstr"))
})

# tabulate_mmrm_subgroups ----

test_that("tabulate_mmrm_subgroups works as expected", {
  df <- extract_mmrm_subgroups(
    fit = mmrm_results,
    visit = "VIS3",
    subgroups = c("RACE", "SEX")
  )

  tab <- basic_table() %>%
    tabulate_mmrm_subgroups(df)
  tab_matrix <- to_string_matrix(tab, with_spaces = FALSE)
  expect_snapshot_value(tab_matrix, style = "serialize")

  forest <- g_forest(tab, logx = FALSE, xlim = c(-10, 10), x_at = c(-10, -5, 0, 5, 10), vline = 0)

  skip_on_ci()
  vdiffr::expect_doppelganger("MMRM forest plot", function() grid::grid.draw(forest))
})

test_that("tabulate_mmrm_subgroups with custom settings works as expected", {
  df <- extract_mmrm_subgroups(
    fit = mmrm_results,
    visit = "VIS1+2",
    subgroups = "SEX"
  )

  tab <- basic_table() %>%
    tabulate_mmrm_subgroups(
      df,
      vars = c("diff", "ci", "n_tot", "pval"),
      .formats = list(
        n_tot = "xx",
        diff = "xx.",
        ci = "(xx., xx.)",
        pval = "xx.xx"
      ),
      .labels = list(
        n_tot = "Total",
        diff = "Difference",
        ci = "CI",
        pval = "p"
      )
    )
  tab_matrix <- to_string_matrix(tab, with_spaces = FALSE)
  expect_snapshot_value(tab_matrix, style = "serialize")

  forest <- g_forest(tab, logx = FALSE, xlim = c(-10, 10), x_at = c(-10, -5, 0, 5, 10), vline = 0)

  skip_on_ci()
  vdiffr::expect_doppelganger("MMRM forest plot with customizations", function() grid::grid.draw(forest))
})
