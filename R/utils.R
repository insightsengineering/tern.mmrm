#' Extraction of Covariate Parts from Character Vector
#'
#' @param covariates (`character`)\cr specification in the usual way, see examples.
#'
#' @return Character vector of the covariates involved in `covariates` specification.
#' @keywords internal
h_get_covariate_parts <- function(covariates) {
  assert_character(covariates, null.ok = TRUE)
  if (is.null(covariates)) {
    NULL
  } else {
    unique(unlist(strsplit(covariates, split = "\\*|:")))
  }
}
