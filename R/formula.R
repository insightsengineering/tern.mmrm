#' Building Model Formula
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This builds the model formula which is used inside [fit_mmrm()] and provided
#' to [mmrm::mmrm()] internally. It can be instructive to look at the resulting
#' formula directly sometimes.
#'
#' @param vars (`list`)\cr variables to use in the model.
#' @param cor_struct (`string`)\cr specify the covariance structure to use.
#' @return Formula to use in [mmrm::mmrm()].
#' @export
#'
#' @examples
#' vars <- list(
#'   response = "AVAL", covariates = c("RACE", "SEX"),
#'   id = "USUBJID", arm = "ARMCD", visit = "AVISIT"
#' )
#' build_formula(vars, "auto-regressive")
#' build_formula(vars)
build_formula <- function(vars,
                          cor_struct = c(
                            "unstructured",
                            "toeplitz",
                            "heterogeneous toeplitz",
                            "ante-dependence",
                            "heterogeneous ante-dependence",
                            "auto-regressive",
                            "heterogeneous auto-regressive",
                            "compound symmetry",
                            "heterogeneous compound symmetry"
                          )) {
  assert_list(vars)
  cor_struct <- match.arg(cor_struct)
  covariates_part <- paste(
    vars$covariates,
    collapse = " + "
  )
  arm_visit_part <- if (is.null(vars$arm)) {
    vars$visit
  } else {
    paste0(
      vars$arm,
      "*",
      vars$visit
    )
  }
  random_effects_fun <- switch(cor_struct,
    "unstructured" = "us",
    "toeplitz" = "toep",
    "heterogeneous toeplitz" = "toeph",
    "ante-dependence" = "ad",
    "heterogeneous ante-dependence" = "adh",
    "auto-regressive" = "ar1",
    "heterogeneous auto-regressive" = "ar1h",
    "compound symmetry" = "cs",
    "heterogeneous compound symmetry" = "csh"
  )
  random_effects_part <- paste0(
    random_effects_fun, "(", vars$visit, " | ", vars$id, ")"
  )
  rhs_formula <- paste(
    arm_visit_part,
    "+",
    random_effects_part
  )
  if (covariates_part != "") {
    rhs_formula <- paste(
      covariates_part,
      "+",
      rhs_formula
    )
  }
  stats::as.formula(paste(
    vars$response,
    "~",
    rhs_formula
  ))
}
