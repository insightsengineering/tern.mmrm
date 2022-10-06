#' Adding Labels To Variables For Model
#'
#' @param vars (`list`)\cr variables to use.
#' @param data (`data.frame`)\cr data to use.
#' @param x (`character`)\cr an element in vars.
#'
#' @name labels
#' @keywords internal
NULL

#' @describeIn labels checks if element in `vars` is not `NULL` and not empty.
h_is_specified <- function(x, vars) {
  !is.null(vars[[x]]) && (length(vars[[x]]) > 0)
}

#' @describeIn labels checks if element in vars is not NULL and exists in dataset.
h_is_specified_and_in_data <- function(x, vars, data) {
  h_is_specified(x, vars) && all(vars[[x]] %in% names(data))
}

#' @describeIn labels gets label for each element in vars.
h_check_and_get_label <- function(x, vars, data) {
  assert_true(h_is_specified_and_in_data(x, vars, data))
  res <- NULL
  for (v in vars[[x]]) {
    label <- attr(data[[v]], "label")
    string <- ifelse(!is.null(label), label, v)
    res <- c(res, stats::setNames(string, v))
  }
  res
}

#' @describeIn labels returns the list of variables with labels.
h_labels <- function(vars,
                     data) {
  assert_list(vars)
  assert_data_frame(data)
  labels <- list()
  labels$response <- h_check_and_get_label("response", vars, data)
  labels$id <- h_check_and_get_label("id", vars, data)
  labels$visit <- h_check_and_get_label("visit", vars, data)
  if (h_is_specified("arm", vars)) {
    labels$arm <- h_check_and_get_label("arm", vars, data)
  }
  if (h_is_specified("covariates", vars)) {
    vars$parts <- h_get_covariate_parts(vars$covariates)
    labels$parts <- h_check_and_get_label("parts", vars, data)
  }
  if (h_is_specified("weights", vars)) {
    labels$weights <- h_check_and_get_label("weights", vars, data)
  }
  return(labels)
}
