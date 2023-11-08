#' Linear Mixed Model Diagnostics
#'
#' Compute the model diagnostic statistics for a linear mixed model fit.
#'
#' @param fit (`mmrm`)\cr object fit with [mmrm::mmrm()].
#'
#' @return A list with the `REML` criterion, the `AIC`, `AICc` and `BIC`.
#'
#' @keywords internal
h_get_diagnostics <- function(fit) {
  assert_class(fit, "mmrm")
  assert_true(fit$reml)

  result <- list(
    "REML criterion" = stats::deviance(fit),
    AIC = stats::AIC(fit),
    AICc = stats::AIC(fit, corrected = TRUE),
    BIC = stats::BIC(fit)
  )
  return(result)
}

#' `MMRM` Analysis
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Does the `MMRM` analysis. Multiple other functions can be called on the result to produce
#' tables and graphs.
#'
#' @param vars (named `list` of `string` or `character`)\cr specifying the variables in the `MMRM`.
#'   The following elements need to be included as character vectors and match corresponding columns
#'   in `data`:
#'
#'   - `response`: the response variable.
#'   - `covariates`: the additional covariate terms (might also include interactions).
#'   - `id`: the subject ID variable.
#'   - `arm`: the treatment group variable (factor).
#'   - `visit`: the visit variable (factor).
#'   - `weights`: optional weights variable (if `NULL` or omitted then no weights will be used).
#'
#'   Note that the main effects and interaction of `arm` and `visit` are by default
#'   included in the model.
#' @param data (`data.frame`)\cr with all the variables specified in
#'   `vars`. Records with missing values in any independent variables
#'   will be excluded.
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#' @param cor_struct (`string`)\cr specifying the covariance structure, defaults to
#'   `"unstructured"`. See the details.
#' @param averages_emmeans (`list`)\cr optional named list of visit levels which should be averaged
#'   and reported along side the single visits.
#' @param weights_emmeans (`string`)\cr argument from [emmeans::emmeans()], `"proportional"` by default.
#' @param parallel (`flag`)\cr controls whether the optimizer search can use available free cores on the
#'   machine (not default).
#' @param ... additional arguments for [mmrm::mmrm()], in particular `reml` and options listed in
#'   [mmrm::mmrm_control()].
#'
#' @details Multiple different degree of freedom adjustments are available via the `method` argument
#'   for [mmrm::mmrm()]. In addition, covariance matrix adjustments are available via `vcov`.
#'   Please see [mmrm::mmrm_control()] for details and additional useful options.
#'
#'   For the covariance structure (`cor_struct`), the user can choose among the following options.
#'
#'   - `unstructured`: Unstructured covariance matrix. This is the most flexible choice and default.
#'        If there are `T` visits, then `T * (T+1) / 2` variance parameters are used.
#'   - `toeplitz`: Homogeneous Toeplitz covariance matrix, which uses `T` variance parameters.
#'   - `heterogeneous toeplitz`: Heterogeneous Toeplitz covariance matrix,
#'        which uses `2 * T - 1` variance parameters.
#'   - `ante-dependence`: Homogeneous Ante-Dependence covariance matrix, which uses `T` variance parameters.
#'   - `heterogeneous ante-dependence`: Heterogeneous Ante-Dependence covariance matrix,
#'        which uses `2 * T - 1` variance parameters.
#'   - `auto-regressive`: Homogeneous Auto-Regressive (order 1) covariance matrix,
#'        which uses 2 variance parameters.
#'   - `heterogeneous auto-regressive`: Heterogeneous Auto-Regressive (order 1) covariance matrix,
#'        which uses `T + 1` variance parameters.
#'   - `compound symmetry`: Homogeneous Compound Symmetry covariance matrix, which uses 2
#'        variance parameters.
#'   - `heterogeneous compound symmetry`: Heterogeneous Compound Symmetry covariance matrix, which uses
#'        `T + 1` variance parameters.
#'
#' @return A `tern_mmrm` object which is a list with MMRM results:
#'
#'   - `fit`: The `mmrm` object which was fitted to the data. Note that via `mmrm::component(fit, "optimizer")`
#'       the finally used optimization algorithm can be obtained, which can be useful for refitting the model
#'       later on.
#'   - `cov_estimate`: The matrix with the covariance matrix estimate.
#'   - `diagnostics`: A list with model diagnostic statistics (REML criterion, AIC, corrected AIC, BIC).
#'   - `lsmeans`: This is a list with data frames `estimates` and `contrasts`.
#'        The attributes `averages` and `weights` save the settings used
#'        (`averages_emmeans` and `weights_emmeans`).
#'   - `vars`: The variable list.
#'   - `labels`: Corresponding list with variable labels extracted from `data`.
#'   - `cor_struct`: input.
#'   - `parallel`: input.
#'   - `ref_level`: The reference level for the arm variable, which is always the first level.
#'   - `treatment_levels`: The treatment levels for the arm variable.
#'   - `conf_level`: The confidence level which was used to construct the `lsmeans` confidence intervals.
#'   - `additional`: List with any additional inputs passed via `...`
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(rtables)
#'
#' mmrm_results <- fit_mmrm(
#'   vars = list(
#'     response = "FEV1",
#'     covariates = c("RACE", "SEX"),
#'     id = "USUBJID",
#'     arm = "ARMCD",
#'     visit = "AVISIT"
#'   ),
#'   data = mmrm_test_data,
#'   cor_struct = "unstructured",
#'   weights_emmeans = "equal",
#'   averages_emmeans = list(
#'     "VIS1+2" = c("VIS1", "VIS2")
#'   )
#' )
#'
fit_mmrm <- function(vars = list( # nolint start
                       response = "AVAL",
                       covariates = c(),
                       id = "USUBJID",
                       arm = "ARM",
                       visit = "AVISIT"
                     ), # nolint end
                     data,
                     conf_level = 0.95,
                     cor_struct = "unstructured",
                     weights_emmeans = "proportional",
                     averages_emmeans = list(),
                     parallel = FALSE,
                     ...) {
  h_assert_data(vars, data)
  labels <- h_labels(vars, data)
  formula <- build_formula(vars, cor_struct)
  weights <- if (!is.null(vars$weights)) data[[vars$weights]] else NULL

  fit <- mmrm::mmrm(
    formula = formula,
    data = data,
    weights = weights,
    reml = TRUE,
    n_cores = ifelse(parallel, parallelly::availableCores(omit = 1L), 1L),
    ...
  )

  lsmeans <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    averages = averages_emmeans,
    weights = weights_emmeans
  )

  results <- list(
    fit = fit,
    cov_estimate = mmrm::VarCorr(fit),
    diagnostics = h_get_diagnostics(fit),
    lsmeans = lsmeans,
    vars = vars,
    labels = labels,
    cor_struct = cor_struct,
    parallel = parallel,
    ref_level = if (is.null(vars$arm)) NULL else levels(data[[vars$arm]])[1],
    treatment_levels = if (is.null(vars$arm)) NULL else levels(data[[vars$arm]])[-1],
    conf_level = conf_level,
    additional = list(...)
  )
  class(results) <- "tern_mmrm"
  return(results)
}
