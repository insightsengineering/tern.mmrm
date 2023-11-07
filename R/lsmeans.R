# lsmeans_helpers ----

#' Helpers for Processing Least Square Means
#'
#' @param fit result of [mmrm::mmrm()].
#' @inheritParams fit_mmrm
#' @param averages (`list`)\cr optional named list of visit levels which should be averaged
#'   and reported along side the single visits.
#' @param weights (`string`)\cr argument from [emmeans::emmeans()], "proportional" by default.
#' @param specs (`list`)\cr list of least square means specifications, with
#'   elements `coefs` (coefficient list) and `grid` (corresponding `data.frame`).
#' @param emmeans_res (`list`)\cr initial `emmeans` result from [h_get_emmeans_res()].
#'
#' @name lsmeans_helpers
#' @keywords internal
NULL

#' @describeIn lsmeans_helpers returns a list with
#'   `object` (`emmGrid` object containing `emmeans` results) and `grid`
#'   (`data.frame` containing the potential arm and the visit variables
#'   together with the sample size `n` for each combination).
#' @importFrom stats model.frame
h_get_emmeans_res <- function(fit, vars, weights) {
  assert_class(fit, "mmrm")
  data_complete <- model.frame(fit)
  assert_data_frame(data_complete)
  assert_list(vars)

  emmeans_object <- emmeans::emmeans(
    fit,
    data = data_complete,
    specs = c(vars$visit, vars$arm),
    weights = weights
  )

  # Save grid with renamed number of subjects column.
  visit_arm_grid <- emmeans_object@grid
  wgt_index <- match(".wgt.", names(visit_arm_grid))
  names(visit_arm_grid)[wgt_index] <- "n"
  visit_arm_grid$n <- as.integer(visit_arm_grid$n)

  list(
    object = emmeans_object,
    grid = visit_arm_grid
  )
}

#' @describeIn lsmeans_helpers constructs average of visits specifications.
h_get_average_visit_specs <- function(emmeans_res,
                                      vars,
                                      averages,
                                      fit) {
  visit_grid <- emmeans_res$grid[[vars$visit]]
  model_frame <- stats::model.frame(fit)
  averages_list <- list()
  visit_vec <- n_vec <- c()
  if (!is.null(vars$arm)) {
    arm_grid <- emmeans_res$grid[[vars$arm]]
    arm_vec <- c()
  }
  for (i in seq_along(averages)) {
    average_label <- names(averages)[i]
    visits_average <- averages[[i]]
    assert_subset(visits_average, choices = levels(visit_grid))
    which_visits_in_average <- visit_grid %in% visits_average
    average_coefs <- as.integer(which_visits_in_average) / length(visits_average)
    zero_coefs <- numeric(length = length(average_coefs))

    if (is.null(vars$arm)) {
      averages_list[[average_label]] <- average_coefs
      visit_vec <- c(visit_vec, average_label)
      is_in_subset <- (model_frame[[vars$visit]] %in% visits_average)
      this_n <- length(unique(model_frame[is_in_subset, vars$id]))
      n_vec <- c(n_vec, this_n)
    } else {
      for (this_arm in levels(arm_grid)) {
        this_coefs <- zero_coefs
        arm_average_label <- paste(this_arm, average_label, sep = ".")
        which_arm <- arm_grid == this_arm
        this_coefs[which_arm] <- average_coefs[which_arm]
        averages_list[[arm_average_label]] <- this_coefs
        arm_vec <- c(arm_vec, this_arm)
        visit_vec <- c(visit_vec, average_label)
        is_in_subset <- (model_frame[[vars$arm]] == this_arm) &
          (model_frame[[vars$visit]] %in% visits_average)
        this_n <- length(unique(model_frame[is_in_subset, vars$id]))
        n_vec <- c(n_vec, this_n)
      }
    }
  }
  if (is.null(vars$arm)) {
    averages_grid <- data.frame(visit = visit_vec, n = n_vec)
    names(averages_grid) <- c(vars$visit, "n")
  } else {
    averages_grid <- data.frame(arm = arm_vec, visit = visit_vec, n = n_vec)
    names(averages_grid) <- c(vars$arm, vars$visit, "n")
  }
  list(
    coefs = averages_list,
    grid = averages_grid
  )
}

#' @describeIn lsmeans_helpers estimates least square means as a `data.frame`
#'   given specifications.
#' @param tests (`flag`)\cr whether to add test results to the estimates.
h_get_spec_visit_estimates <- function(emmeans_res,
                                       specs,
                                       conf_level,
                                       tests = FALSE) {
  assert_list(emmeans_res)
  assert_list(specs)
  assert_number(conf_level)
  assert_flag(tests)

  conts <- emmeans::contrast(
    emmeans_res$object,
    specs$coefs
  )
  cis <- stats::confint(conts, level = conf_level)
  res <- cbind(
    specs$grid,
    data.frame(
      estimate = cis$estimate,
      se = cis$SE,
      df = cis$df,
      lower_cl = cis$lower.CL,
      upper_cl = cis$upper.CL
    )
  )
  if (tests) {
    conts_df <- as.data.frame(conts)
    res$t_stat <- conts_df$t.ratio
    res$p_value <- conts_df$p.value
  }
  res
}

#' @describeIn lsmeans_helpers estimates least square means for single visits.
h_get_single_visit_estimates <- function(emmeans_res,
                                         conf_level) {
  assert_list(emmeans_res)
  assert_number(conf_level)

  cis <- stats::confint(emmeans_res$object, level = conf_level)
  cbind(
    emmeans_res$grid[, setdiff(names(emmeans_res$grid), "n"), drop = FALSE],
    data.frame(
      estimate = cis$emmean,
      se = cis$SE,
      df = cis$df,
      lower_cl = cis$lower.CL,
      upper_cl = cis$upper.CL
    ),
    emmeans_res$grid[, "n", drop = FALSE]
  )
}



#' @describeIn lsmeans_helpers constructs `data.frame` with
#'   relative reduction vs. reference arm based on single visit estimates.
#' @param estimates (`data.frame`)\cr single visit least square mean estimates.
h_get_relative_reduc_df <- function(estimates,
                                    vars) {
  assert_data_frame(estimates)
  assert_list(vars)

  ref_arm_level <- estimates[[vars$arm]][1L]
  ref_estimates <- estimates[
    estimates[[vars$arm]] == ref_arm_level,
    c(vars$visit, "estimate")
  ]
  names(ref_estimates)[2L] <- "ref"
  result <- merge(
    estimates[estimates[[vars$arm]] != ref_arm_level, ],
    ref_estimates,
    by = vars$visit,
    sort = FALSE
  )
  result$relative_reduc <- (result$ref - result$estimate) / result$ref
  result[, c(vars$visit, vars$arm, "relative_reduc")]
}

#' @describeIn lsmeans_helpers constructs single visit contrast specifications.
h_single_visit_contrast_specs <- function(emmeans_res,
                                          vars) {
  assert_list(emmeans_res)
  assert_list(vars)

  emmeans_res$grid$index <- seq_len(nrow(emmeans_res$grid))

  grid_by_visit <- split(emmeans_res$grid, emmeans_res$grid[[vars$visit]])

  arm_levels <- emmeans_res$object@levels[[vars$arm]]
  ref_arm_level <- arm_levels[1L]
  zeros_coefs <- numeric(nrow(emmeans_res$grid))
  overall_list <- list()
  arm_vec <- visit_vec <- c()
  for (j in seq_along(grid_by_visit)) {
    this_grid <- grid_by_visit[[j]]
    ref_index <- which(this_grid[[vars$arm]] == ref_arm_level)
    this_visit <- names(grid_by_visit)[j]
    this_ref_coefs <- zeros_coefs
    this_ref_coefs[this_grid$index[ref_index]] <- -1
    this_list <- list()
    for (i in seq_len(nrow(this_grid))[-ref_index]) {
      this_coefs <- this_ref_coefs
      this_coefs[this_grid$index[i]] <- 1
      this_arm <- as.character(this_grid[[vars$arm]][i])
      arm_vec <- c(arm_vec, this_arm)
      visit_vec <- c(visit_vec, this_visit)
      this_label <- paste(this_arm, this_visit, sep = ".")
      this_list[[this_label]] <- this_coefs
    }
    overall_list <- c(overall_list, this_list)
  }

  grid <- data.frame(arm = arm_vec, visit = visit_vec)
  names(grid) <- c(vars$arm, vars$visit)
  list(
    coefs = overall_list,
    grid = grid
  )
}

#' @describeIn lsmeans_helpers constructs average visits contrast specifications,
#'   given the `specs` for single visit contrasts and the averages required.
h_average_visit_contrast_specs <- function(specs,
                                           averages) {
  arm_visit_grid <- specs$grid
  arm_visit_grid$index <- seq_len(nrow(arm_visit_grid))
  grid_by_arm <- split(
    arm_visit_grid,
    arm_visit_grid[, 1L]
  )
  overall_list <- list()
  arm_vec <- visit_vec <- c()
  for (j in seq_along(grid_by_arm)) {
    this_arm <- names(grid_by_arm)[j]
    this_grid <- grid_by_arm[[j]]
    for (i in seq_along(averages)) {
      average_label <- names(averages)[i]
      visits_average <- averages[[i]]
      which_visits_in_average <- this_grid[, 2L] %in% visits_average
      averaged_indices <- this_grid$index[which_visits_in_average]
      this_comb <- paste(this_arm, average_label, sep = ".")
      averaged_coefs <- colMeans(do.call(rbind, specs$coefs[averaged_indices]))
      overall_list[[this_comb]] <- averaged_coefs
      arm_vec <- c(arm_vec, this_arm)
      visit_vec <- c(visit_vec, average_label)
    }
  }

  grid <- data.frame(arm = arm_vec, visit = visit_vec)
  names(grid) <- names(arm_visit_grid[, c(1, 2)])

  list(
    coefs = overall_list,
    grid = grid
  )
}

# get_mmrm_lsmeans ----

#' Extract Least Square Means from `MMRM`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Extracts the least square means from an `MMRM` fit.
#'
#' @param fit (`mmrm`)\cr result of [mmrm::mmrm()].
#' @inheritParams fit_mmrm
#' @param averages (`list`)\cr named list of visit levels which should be averaged
#'   and reported along side the single visits.
#' @param weights (`string`)\cr type of weights to be used for the least square means,
#'   see [emmeans::emmeans()] for details.
#' @return A list with data frames `estimates` and `contrasts`.
#'   The attributes `averages` and `weights` save the settings used.
#'
#' @export
get_mmrm_lsmeans <- function(fit,
                             vars,
                             conf_level,
                             weights,
                             averages = list()) {
  assert_class(fit, "mmrm")
  assert_list(averages, types = "character")
  emmeans_res <- h_get_emmeans_res(fit, vars, weights)

  # Get least square means estimates for single visits, and possibly averaged visits.
  estimates <- h_get_single_visit_estimates(emmeans_res, conf_level)
  if (length(averages)) {
    average_specs <- h_get_average_visit_specs(emmeans_res, vars, averages, fit)
    average_estimates <- h_get_spec_visit_estimates(emmeans_res, average_specs, conf_level)
    estimates <- rbind(estimates, average_estimates)
  }
  has_arm_var <- !is.null(vars$arm)
  if (!has_arm_var) {
    return(list(estimates = estimates))
  }
  # Continue with contrasts when we have an arm variable.
  contrast_specs <- h_single_visit_contrast_specs(emmeans_res, vars)
  contrast_estimates <- h_get_spec_visit_estimates(emmeans_res, contrast_specs, conf_level, tests = TRUE)
  if (length(averages)) {
    average_contrast_specs <- h_average_visit_contrast_specs(contrast_specs, averages)
    average_contrasts <- h_get_spec_visit_estimates(emmeans_res, average_contrast_specs, conf_level, tests = TRUE)
    contrast_estimates <- rbind(contrast_estimates, average_contrasts)
  }

  relative_reduc_df <- h_get_relative_reduc_df(estimates, vars)
  contrast_estimates <- merge(
    contrast_estimates,
    relative_reduc_df,
    by = c(vars$arm, vars$visit),
    sort = FALSE
  )
  contrast_estimates[[vars$arm]] <- factor(contrast_estimates[[vars$arm]])
  contrast_estimates[[vars$visit]] <- factor(contrast_estimates[[vars$visit]])
  structure(
    list(
      estimates = estimates,
      contrasts = contrast_estimates
    ),
    averages = averages,
    weights = weights
  )
}
