#' `tern.mmrm` Package
#'
#' `tern.mmrm` is an analysis package to create tables, listings and graphs to analyze clinical trials data.
#'
#' @keywords internal
"_PACKAGE"

#' @import checkmate
#' @import ggplot2
#' @import rtables
#' @importFrom magrittr %>%
#' @importFrom mmrm df_1d
#' @importFrom rlang .data :=
#' @importFrom tern f_conf_level
NULL

#' Example dataset for `tern.mmrm` package.
#'
#' Measurements of FEV1 (forced expired volume in one second) is a measure of how quickly the lungs can be emptied.
#' Low levels of FEV1 may indicate chronic obstructive pulmonary disease (COPD).
#' @format A `tibble` with 800 rows and 7 variables:
#' \describe{
#'   \item{`USUBJID`}{price, in US dollars}
#'   \item{`AVISIT`}{visit number}
#'   \item{`ARMCD`}{treatment, `TRT` or `PBO`}
#'   \item{`RACE`}{3-category race}
#'   \item{`SEX`}{sex}
#'   \item{`FEV1_BL`}{FEV1 at baseline (%)}
#'   \item{`FEV1`}{FEV1 at study visits}
#' }
"mmrm_test_data"
