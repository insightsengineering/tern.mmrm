#' Linear Mixed Model Diagnostics
#'
#' Compute the model diagnostic statistics for a linear mixed model fit.
#'
#' @param fit (`mmrm`)\cr object fit with [mmrm::mmrm()].
#'
#' @return A list with the REML criterion, the AIC, AICc and BIC.
#'
#' @keywords internal
#'
get_diagnostics <- function(fit) {
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
#' Does the `MMRM` analysis. Multiple other functions can be called on the result to produce
#' tables and graphs.
#'
#' @param vars (named `list` of `string` or `character`)\cr specifying the variables in the `MMRM`.
#'   The following elements need to be included as character vectors and match corresponding columns
#'   in \code{data}:
#'   \describe{
#'   \item{response}{the response variable}
#'   \item{covariates}{the additional covariate terms (might also include interactions)}
#'   \item{id}{the subject ID variable}
#'   \item{arm}{the treatment group variable (factor)}
#'   \item{visit}{the visit variable (factor)}
#'   }
#'   Note that the main effects and interaction of `arm` and `visit` are by default included in the model.
#' @param data a \code{data.frame} with all the variables specified in
#'   \code{vars}. Records with missing values in any independent variables
#'   will be excluded.
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#' @param cor_struct a string specifying the correlation structure, defaults to
#'   \code{"unstructured"}. See the details.
#' @param averages_emmeans (`list`)\cr optional named list of visit levels which should be averaged
#'   and reported along side the single visits.
#' @param weights_emmeans argument from [emmeans::emmeans()], "proportional" by default.
#' @param optimizer a string specifying the optimization algorithm which should be used. By default, "automatic"
#'   will (if necessary) try all possible optimization algorithms and choose the best result. If another algorithm
#'   is chosen and does not give a valid result, an error will occur.
#' @param parallel flag that controls whether "automatic" optimizer search can use available free cores on the
#'   machine (not default).
#'
#' @details Only Satterthwaite adjusted degrees of freedom (d.f.) are supported, because they
#'   match the results obtained in SAS (confirmed for unstructured and compound symmetry correlation structures).
#'
#'   For the correlation structure (\code{cor_struct}), the user can choose among the following options, sorted
#'   in descending number of variance parameters:
#'   \describe{
#'   \item{unstructured}{Unstructured covariance matrix. This is the most flexible choice and default.
#'      If there are \code{T} visits, then \code{T * (T+1) / 2} variance parameters are used.
#'      Note: the current actual implementation uses one more variance parameter, which does not have any
#'      effect of the results. Therefore we report here the actually relevant number of parameters.}
#'   \item{random-quadratic}{Random quadratic spline for the random effects of the time variable.
#'      7 variance parameters are used.}
#'   \item{random-slope}{Random slope for the random effects of the time variable. 4 variance parameters are used.}
#'   \item{compound-symmetry}{Constant correlation between visits. 2 variance parameters are used.}
#'   }
#'
#'   For the \code{optimizer}, the user can choose among the following alternatives to the recommended "automatic":
#'   \describe{
#'   \item{nloptwrap_neldermead}{\code{NLopt} version of the Nelder-Mead algorithm (via package \code{nloptr})}
#'   \item{nloptwrap_bobyqa}{\code{NLopt} version of the BOBYQA algorithm (via package \code{nloptr})}
#'   \item{bobyqa}{BOBYQA algorithm (via package \code{minqa})}
#'   \item{nlminbwrap}{nlminb algorithm (wrapper for \code{\link[stats]{nlminb})}}
#'   \item{neldermead}{\code{lme4} version of the Nelder-Mead algorithm with box constraints (via package \code{lme4})}
#'   \item{nmkbw}{Nelder-Mead algorithm (via package \code{dfoptim})}
#'   \item{optimx_lbfgsb}{L-BFGS-B algorithm (via package \code{optimx})}
#'   }
#'
#' @return A \code{tern_mmrm} object which is a list with MMRM results:
#' \describe{
#'   \item{fit}{The \code{mmrm} object which was fitted to the data. Note that the attribute \code{optimizer}
#'     contains the finally used optimization algorithm, which can be useful for refitting the model later on.}
#'   \item{cov_estimate}{The matrix with the covariance matrix estimate.}
#'   \item{diagnostics}{A list with model diagnostic statistics (REML criterion, AIC, corrected AIC, BIC).}
#'   \item{lsmeans}{This is a list with data frames \code{estimates} and \code{contrasts}.}
#'   \item{vars}{The variable list.}
#'   \item{labels}{Corresponding list with variable labels extracted from \code{data}.}
#'   \item{ref_level}{The reference level for the arm variable, which is always the first level.}
#'   \item{conf_level}{The confidence level which was used to construct the confidence intervals.}
#' }
#'
#' @note
#' The ordering of the input data sets can lead to slightly different numerical results or
#' different convergence behavior. This is a known observation with the used package
#' \code{lme4}. However, once convergence is achieved, the results are reliable up to
#' numerical precision.
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
fit_mmrm <- function(vars = list(
                       response = "AVAL",
                       covariates = c(),
                       id = "USUBJID",
                       arm = "ARM",
                       visit = "AVISIT"
                     ),
                     data,
                     conf_level = 0.95,
                     cor_struct = "unstructured",
                     weights_emmeans = "proportional",
                     averages_emmeans = list(),
                     optimizer = "automatic",
                     parallel = FALSE) {
  assert_data(vars, data)
  labels <- h_labels(vars, data)
  formula <- h_build_formula(vars, cor_struct)

  fit <- mmrm::mmrm(
    formula = formula,
    data = data,
    reml = TRUE,
    optimizer = optimizer,
    n_cores = ifelse(parallel, mmrm::h_free_cores(), 1L)
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
    diagnostics = get_diagnostics(fit),
    lsmeans = lsmeans,
    vars = vars,
    labels = labels,
    ref_level = if (is.null(vars$arm)) NULL else levels(data[[vars$arm]])[1],
    conf_level = conf_level
  )
  class(results) <- "tern_mmrm"
  return(results)
}
