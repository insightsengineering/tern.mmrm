# h_is_specified ----

test_that("h_is_specified works as expected", {
  vars <- list(
    response = "AVAL",
    id = "MYID",
    arm = "ARM",
    visit = "AVISIT",
    covariates = c("SEX", "RACE"),
    bla = c(),
    foo = character()
  )
  expect_true(h_is_specified("arm", vars))
  expect_true(h_is_specified("covariates", vars))
  expect_false(h_is_specified("bla", vars))
  expect_false(h_is_specified("foo", vars))
})

# h_is_specified_and_in_data ----

test_that("h_is_specified_and_in_data works as expected", {
  data <- data.frame(
    MYID = c(1, 1, 2, 2, 3, 3),
    ARM = factor(c(1, 2, 3, 1, 2, 3)),
    AVAL = c(2, 4, 6, 8, 10, 12),
    AVISIT = c("W1", "W2", "W3", "W1", "W2", "W3"),
    SEX = c("Female", "Female", "Female", "Male", "Male", "Male"),
    RACE = c("Asian", "Asian", "Asian", "White", "Asian", "White")
  )
  vars <- list(response = "AVAL", id = "MYID", arm = "ARM", visit = "AVISIT", covariates = c("SEX", "RACE"))
  result <- h_is_specified_and_in_data("arm", vars, data)
  expect_true(result)
})

# h_check_and_get_label ----

test_that("h_check_and_get_label works as expected", {
  data <- data.frame(
    MYID = c(1, 1, 2, 2, 3, 3),
    ARM = factor(c(1, 2, 3, 1, 2, 3)),
    AVAL = structure(c(2, 4, 6, 8, 10, 12), label = "value"),
    AVISIT = c("W1", "W2", "W3", "W1", "W2", "W3"),
    SEX = c("Female", "Female", "Female", "Male", "Male", "Male"),
    RACE = c("Asian", "Asian", "Asian", "White", "Asian", "White")
  )
  vars <- list(response = "AVAL", id = "MYID", arm = "ARM", visit = "AVISIT", covariates = c("SEX", "RACE"))
  result <- h_check_and_get_label("response", vars, data)
  expected <- c(AVAL = "value")
  expect_identical(result, expected)
})

# h_labels ----

test_that("h_labels works as expected", {
  data <- data.frame(
    MYID = structure(c(1, 1, 2, 2, 3, 3), label = "lab1"),
    ARM = structure(factor(c(1, 2, 3, 1, 2, 3)), label = "lab2"),
    AVAL = structure(c(2, 4, 6, 8, 10, 12), label = "value"),
    AVISIT = structure(c("W1", "W2", "W3", "W1", "W2", "W3"), label = "lab3"),
    SEX = structure(c("Female", "Female", "Female", "Male", "Male", "Male"), label = "lab4"),
    RACE = structure(c("Asian", "Asian", "Asian", "White", "Asian", "White"), label = "lab5")
  )
  vars <- list(
    response = "AVAL",
    id = "MYID",
    arm = "ARM",
    visit = "AVISIT",
    covariates = c("SEX", "RACE")
  )
  result <- h_labels(vars, data)
  expected <- list(
    response = c(AVAL = "value"),
    id = c(MYID = "lab1"),
    visit = c(AVISIT = "lab3"),
    arm = c(ARM = "lab2"),
    parts = c(SEX = "lab4", RACE = "lab5")
  )
  expect_identical(result, expected)
})

test_that("h_labels works if covariates is empty vector", {
  data <- data.frame(
    MYID = structure(c(1, 1, 2, 2, 3, 3), label = "lab1"),
    ARM = structure(factor(c(1, 2, 3, 1, 2, 3)), label = "lab2"),
    AVAL = structure(c(2, 4, 6, 8, 10, 12), label = "value"),
    AVISIT = structure(c("W1", "W2", "W3", "W1", "W2", "W3"), label = "lab3"),
    SEX = structure(c("Female", "Female", "Female", "Male", "Male", "Male"), label = "lab4"),
    RACE = structure(c("Asian", "Asian", "Asian", "White", "Asian", "White"), label = "lab5")
  )
  vars <- list(
    response = "AVAL",
    id = "MYID",
    arm = "ARM",
    visit = "AVISIT",
    covariates = character()
  )
  result <- expect_silent(h_labels(vars, data))
  expected <- list(
    response = c(AVAL = "value"),
    id = c(MYID = "lab1"),
    visit = c(AVISIT = "lab3"),
    arm = c(ARM = "lab2")
  )
  expect_identical(result, expected)
})

test_that("h_labels works as expected also with weights", {
  data <- data.frame(
    MYID = structure(c(1, 1, 2, 2, 3, 3), label = "lab1"),
    ARM = structure(factor(c(1, 2, 3, 1, 2, 3)), label = "lab2"),
    AVAL = structure(c(2, 4, 6, 8, 10, 12), label = "value"),
    WEIGHTS = structure(c(1, 2, 3, 4, 5, 6), label = "weighting"),
    AVISIT = structure(c("W1", "W2", "W3", "W1", "W2", "W3"), label = "lab3"),
    SEX = structure(c("Female", "Female", "Female", "Male", "Male", "Male"), label = "lab4"),
    RACE = structure(c("Asian", "Asian", "Asian", "White", "Asian", "White"), label = "lab5")
  )
  vars <- list(
    response = "AVAL",
    id = "MYID",
    arm = "ARM",
    visit = "AVISIT",
    covariates = c("SEX", "RACE"),
    weights = "WEIGHTS"
  )
  result <- h_labels(vars, data)
  expected <- list(
    response = c(AVAL = "value"),
    id = c(MYID = "lab1"),
    visit = c(AVISIT = "lab3"),
    arm = c(ARM = "lab2"),
    parts = c(SEX = "lab4", RACE = "lab5"),
    weights = c(WEIGHTS = "weighting")
  )
  expect_identical(result, expected)
})
