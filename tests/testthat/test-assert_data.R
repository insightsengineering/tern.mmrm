# h_assert_one_rec_pt_visit ----

test_that("h_assert_one_rec_pt_visit passes as expected if data is ok", {
  vars <- list(visit = "AVISIT", id = "MYID")
  data <- data.frame(
    MYID = c(1, 1, 2, 2, 3, 3),
    AVISIT = c(1, 2, 1, 2, 1, 2)
  )
  expect_silent(h_assert_one_rec_pt_visit(vars, data))
})

test_that("h_assert_one_rec_pt_visit fails as expected if data is wrong", {
  vars <- list(visit = "AVISIT", id = "MYID")

  # Patient 1 at visit 2 twice observed.
  data <- data.frame(
    MYID = c(1, 1, 1, 2, 2, 3, 3),
    AVISIT = c(1, 2, 2, 1, 2, 1, 2)
  )
  expect_error(
    h_assert_one_rec_pt_visit(vars, data),
    "There are 1 subjects with more than one record per visit: MYID = 1 with AVISIT = 2",
    fixed = TRUE
  )

  # Patient 1 at visit 2, Patient 3 at visit 1 twice observed.
  data <- data.frame(
    MYID = c(3, 1, 1, 1, 2, 2, 3, 3),
    AVISIT = c(1, 1, 2, 2, 1, 2, 1, 2)
  )
  expect_error(
    h_assert_one_rec_pt_visit(vars, data),
    paste(
      "There are 2 subjects with more than one record per visit:",
      "MYID = 1 with AVISIT = 2, MYID = 3 with AVISIT = 1"
    ),
    fixed = TRUE
  )
})

test_that("h_assert_one_rec_pt_visit works with just one patient and one visit", {
  vars <- list(visit = "AVISIT", id = "MYID")
  data <- data.frame(
    MYID = 1,
    AVISIT = 5
  )
  expect_silent(h_assert_one_rec_pt_visit(vars, data))
})

# h_assert_rsp_var ----

test_that("h_assert_rsp_var works as expected", {
  vars <- list(response = "bla")

  data <- data.frame(bla = numeric(1))
  expect_silent(h_assert_rsp_var(vars, data))

  data2 <- data.frame(foo = numeric(1))
  expect_error(h_assert_rsp_var(vars, data2))

  data3 <- data.frame(bla = factor(1))
  expect_error(h_assert_rsp_var(vars, data3))
})

# h_assert_visit_var ----

test_that("h_assert_visit_var works as expected", {
  vars <- list(visit = "vis")

  data <- data.frame(vis = factor(1))
  expect_silent(h_assert_visit_var(vars, data))

  data2 <- data.frame(boo = factor(1))
  expect_error(h_assert_visit_var(vars, data2))

  data3 <- data.frame(vis = numeric(1))
  expect_error(h_assert_visit_var(vars, data3))
})

# h_assert_id_var ----

test_that("h_assert_id_var works as expected", {
  vars <- list(id = "MYID")

  data <- data.frame(MYID = factor(1))
  expect_silent(h_assert_id_var(vars, data))

  data2 <- data.frame(MYID = "1")
  expect_silent(h_assert_id_var(vars, data2))

  data3 <- data.frame(boo = factor(1))
  expect_error(h_assert_id_var(vars, data3))

  data4 <- data.frame(MYID = numeric(1))
  expect_error(h_assert_visit_var(vars, data4))
})

# h_assert_weights_var ----

test_that("h_assert_weights_var works as expected", {
  vars <- list(weights = "bla")

  data <- data.frame(bla = 0.001)
  expect_silent(h_assert_weights_var(vars, data))

  data2 <- data.frame(foo = 0.001)
  expect_error(h_assert_weights_var(vars, data2), "Must be of type 'numeric'")

  data3 <- data.frame(bla = -0.001)
  expect_error(h_assert_weights_var(vars, data3), "Element 1 is not >=")

  data4 <- data.frame(bla = NA_real_)
  expect_error(h_assert_weights_var(vars, data4), "Contains missing values")
})

# h_assert_data ----

test_that("h_assert_data passes as expected", {
  vars <- list(visit = "AVISIT", id = "MYID", response = "RSP")
  data <- data.frame(
    MYID = as.character(c(1, 1, 2, 2, 3, 3)),
    AVISIT = factor(c(1, 2, 1, 2, 1, 2)),
    RSP = c(25.3245, 234.34, 5.1, 35.2, 24.24, 346.32)
  )
  expect_silent(h_assert_data(vars, data))
})

test_that("h_assert_data does not look at rows with incomplete regressors for checking duplicates", {
  vars <- list(visit = "AVISIT", id = "MYID", response = "RSP", covariates = "BLA")
  data <- data.frame(
    MYID = factor(c(1, 1, 2, 2, 3, 3)),
    AVISIT = factor(c(1, 2, 1, 2, 1, 1)), # Duplicate visit 1 for id 3.
    BLA = c(1, 1, 1, 1, 1, NA), # But regressor is missing there.
    RSP = c(25.3245, 234.34, 5.1, 35.2, 24.24, 346.32)
  )
  expect_silent(h_assert_data(vars, data))
})

test_that("h_assert_data fails when less than 5 rows in complete data set without arm", {
  vars <- list(visit = "AVISIT", id = "MYID", response = "RSP", covariates = "BLA")
  # Only 4 rows with complete data.
  data <- data.frame(
    MYID = factor(c(1, 1, 2, 2, 3, 3)),
    AVISIT = factor(c(1, 2, 1, 2, 1, 1)),
    BLA = c(1, 1, 1, 1, 1, NA),
    RSP = c(25.3245, 234.34, 5.1, NA, 24.24, 346.32)
  )
  expect_error(
    h_assert_data(vars, data),
    "Must have at least 5 rows, but has 4 rows"
  )
})

test_that("h_assert_data fails when less than 5 rows in complete data set per arm", {
  vars <- list(visit = "AVISIT", id = "MYID", response = "RSP", arm = "TRT")
  # Only 4 rows with complete data for TRT 1.
  data <- data.frame(
    MYID = factor(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)),
    AVISIT = factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)),
    TRT = factor(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)),
    RSP = c(25.3245, 234.34, 5.1, NA, 24.24, 346.32, 1.2, 1.3, 1.4, 1.5)
  )
  expect_error(h_assert_data(vars, data))
})

test_that("h_assert_data works with interaction terms in `covariates`", {
  vars <- list(
    response = "FEV1",
    covariates = c("ARMCD*FEV1_BL", "SEX", "FEV1_BL:ARMCD"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  )
  expect_silent(h_assert_data(vars, mmrm_test_data))
})

test_that("h_assert_data works when there are missing values", {
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
  expect_silent(h_assert_data(vars, data))
})

test_that("h_assert_data fails if a variable is missing", {
  full_vars <- list(
    response = "AVAL",
    id = "USUBJID",
    visit = "AVISIT"
  )

  for (var in names(full_vars)) {
    incomplete_vars <- full_vars
    incomplete_vars[[var]] <- NULL
    expect_error(h_assert_data(incomplete_vars, mmrm_test_data))
  }
})

test_that("h_assert_data fails if a variable is not included in `data`", {
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
    expect_error(h_assert_data(vars, mmrm_test_data))
  }
})
