#' Helper Evaluating `emmeans` Results
#'
#' @param fit
#' @param data_complete
#' @param vars
#' @param weights
#'
#' @return List with:
#'   - `object`: `emmGrid` object containing `emmeans` results.
#'   - `grid`: `data.frame` containing the potential arm and the visit variables
#'               together with the sample size `n` for each combination.
#' @export
h_get_emmeans_res <- function(fit, data_complete, vars, weights) {
  emmeans_object <- emmeans::emmeans(
    fit,
    data = data_complete,
    specs = c(vars$visit, vars$arm),
    mode = "satterthwaite",
    weights = weights,
    # The below option is needed to enable analysis of more than 3000 observations.
    lmerTest.limit = nrow(data_complete)
  )

  visit_arm_grid <- emmeans_object@grid[-match(".wgt.", names(emmeans_object@grid))]
  data_by_visit_arm <- split(data_complete, f = as.formula(paste("~", vars$arm, "+", vars$visit)))
  n_by_visit_arm <- sapply(data_by_visit_arm, nrow)
  visit_arm_grid$n <- n_by_visit_arm

  list(
    object = emmeans_object,
    grid = visit_arm_grid
  )
}


#' Helper Constructing Average of Visits Specifications
#'
#' @param emmeans_res
#' @param vars
#' @param averages
#'
#' @return List with `coefs` list and `grid` data frame.
#' @export
h_get_average_visit_specs <- function(emmeans_res,
                                      vars,
                                      averages) {
  visit_grid <- emmeans_res$grid[[vars$visit]]
  arm_grid <- emmeans_res$grid[[vars$arm]]
  averages_list <- list()
  arm_vec <- visit_vec <- n_vec <- c()
  for (i in seq_along(averages)) {
    average_label <- names(averages)[i]
    visits_average <- averages[[i]]
    which_visits_in_average <- visit_grid %in% visits_average
    average_coefs <- as.integer(which_visits_in_average) / length(visits_average)
    zero_coefs <- numeric(length = length(average_coefs))
    for (this_arm in levels(arm_grid)) {
      arm_average_label <- paste(this_arm, average_label)
      this_coefs <- zero_coefs
      which_arm <- arm_grid == this_arm
      this_coefs[which_arm] <- average_coefs[which_arm]
      averages_list[[arm_average_label]] <- this_coefs
      arm_vec <- c(arm_vec, this_arm)
      visit_vec <- c(visit_vec, average_label)
      n_vec <- c(n_vec, min(emmeans_res$grid$n[which_visits_in_average & which_arm]))
    }
  }
  averages_grid <- data.frame(arm = arm_vec, visit = visit_vec, n = n_vec)
  names(averages_grid) <- c(vars$arm, vars$visit, "n")

  list(
    coefs = averages_list,
    grid = averages_grid
  )
}


#' Helper Estimating General Combinations Least Square Means
#'
#' @param emmeans_res
#' @param average_specs
#' @param conf_level
#'
#' @return A `data.frame` with estimates.
#' @export
h_get_spec_visit_estimates <- function(emmeans_res, specs, conf_level, tests = FALSE) {
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

#' Helper Calculating Single Visit Least Square Means Results
#'
#' @param emmeans_res
#' @param conf_level
#'
#' @return The `data.frame` with the estimates.
#' @export
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



#' Helper Constructing Data Frame with Relative Reduction vs. Reference Arm
#'
#' @param estimates
#' @param vars
#'
#' @return A `data.frame` with visit, arm and `relative_reduc`, the relative
#'   reduction.
#' @export
h_get_relative_reduc_df <- function(estimates,
                                    vars) {
  assert_data_frame(estimates)
  assert_list(vars)

  ref_arm_level <- estimates[[vars$arm]][1L]
  ref_estimates <- estimates[estimates[[vars$arm]] == ref_arm_level, c(vars$visit, "estimate")]
  names(ref_estimates)[2L] <- "ref"
  tmp <- merge(
    estimates[estimates[[vars$arm]] != ref_arm_level, ],
    ref_estimates,
    by = vars$visit,
    sort = FALSE
  )
  tmp$relative_reduc <- (tmp$ref - tmp$estimate) / tmp$ref
  tmp[, c(vars$visit, vars$arm, "relative_reduc")]
}

#' Helper Constructing Single Visit Contrast Specification List
#'
#' @param emmeans_res
#' @param vars
#'
#' @return A list with `coefs` and `grid`, similar as from
#'   [h_get_average_visit_specs()].
#'
#' @export
h_single_visit_contrast_specs <- function(emmeans_res, vars) {
  emmeans_res$grid$index <- seq_len(nrow(emmeans_res$grid))

  grid_by_visit <- split(emmeans_res$grid, emmeans_res$grid$AVISIT)

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


h_average_visit_contrast_specs <- function(contrast_specs,
                                           averages) {
  arm_visit_grid <- contrast_specs$grid
  arm_visit_grid$index <- seq_len(nrow(arm_visit_grid))
  grid_by_arm <- split(
    arm_visit_grid,
    arm_visit_grid[, 1]
  )
  overall_list <- list()
  arm_vec <- visit_vec <- c()
  for (j in seq_along(grid_by_arm)) {
    this_arm <- names(grid_by_arm)[j]
    this_grid <- grid_by_arm[[j]]
    for (i in seq_along(averages)) {
      average_label <- names(averages)[i]
      visits_average <- averages[[i]]
      which_visits_in_average <- arm_visit_grid[, 2] %in% visits_average
      averaged_indices <- this_grid$index[which_visits_in_average]
      this_comb <- paste(this_arm, average_label, sep = ".")
      averaged_coefs <- colMeans(do.call(rbind, contrast_specs$coefs[averaged_indices]))
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

#' Extract Least Square Means from `MMRM`
#'
#' Helper function to extract the least square means from an `MMRM` fit.
#'
#' @param fit result of [fit_lme4()].
#' @inheritParams fit_mmrm
#' @param averages (`list`)\cr named list of visit levels which should be averaged
#'   and reported along side the single visits.
#' @param weights (`string`)\cr type of weights to be used for the least square means,
#'   see [emmeans::emmeans()] for details.
#'
#' @keywords internal
#'
get_mmrm_lsmeans <- function(fit,
                             vars,
                             conf_level,
                             averages,
                             weights) {
  data_complete <- fit@frame
  assert_list(averages, types = "character")
  emmeans_res <- h_get_emmeans_res(fit, data_complete, vars, weights)

  # Get least square means estimates for single visits, and possibly averaged visits.
  estimates <- h_get_single_visit_estimates(emmeans_res, conf_level)
  if (length(averages)) {
    average_specs <- h_get_average_visit_specs(emmeans_res, vars, averages)
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
  list(
    estimates = estimates,
    contrasts = contrast_estimates
  )
}
