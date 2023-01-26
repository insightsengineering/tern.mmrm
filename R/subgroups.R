#' Helper for Extraction and Formatting of MMRM Subgroups
#'
#' This is an internal helper to extract the correct LS mean estimates and
#' contrast for a specific visit and treatment arm relative to the reference
#' arm for one MMRM subgroup.
#'
#' @param lsmeans (named `list` or `NULL`)\cr LS mean estimates from [fit_mmrm()]
#'   on this subgroup.
#' @param overall_fit (`tern_mmrm`)\cr result of [fit_mmrm()] on overall data.
#' @param is_in_subset (`logical`)\cr specifying which row from the overall data
#'   should be used for this subset.
#' @param visit (`string`)\cr which visit to extract.
#' @param treatment_arm (`string`)\cr which treatment arm to extract.
#' @param subgroup (`string`)\cr for labeling in the resulting `data.frame`.
#' @param var (`string`)\cr specifies which variable was used to derive the subset,
#'   if `ALL` then this means the overall data was used.
#' @param label (`string`)\cr variable label.
#'
#' @return List with `estimates` (with 2 rows) and `contrasts` (with 1 row) in the
#'   format needed in [extract_mmrm_subgroups()].
#' @keywords internal
h_mmrm_subgroup_df <- function(lsmeans,
                               overall_fit,
                               is_in_subset,
                               visit,
                               treatment_arm,
                               subgroup,
                               var,
                               label) {
  assert_list(lsmeans, len = 2L, types = "data.frame", null.ok = TRUE)
  ok <- !is.null(lsmeans)
  if (ok) assert_names(names(lsmeans), identical.to = c("estimates", "contrasts"))
  assert_class(overall_fit, "tern_mmrm")
  assert_logical(is_in_subset, len = nrow(overall_fit$fit$data), any.missing = FALSE)
  assert_string(visit)
  assert_string(treatment_arm)
  assert_string(subgroup)
  assert_string(var)
  assert_string(label)

  v <- overall_fit$vars
  ref_arm <- overall_fit$ref_level

  if (ok) {
    estimates <- lsmeans$estimates
    est_selected <- estimates[[v$visit]] == visit &
      estimates[[v$arm]] %in% c(ref_arm, treatment_arm)
    estimates <- estimates[est_selected, ]

    contrasts <- lsmeans$contrasts
    cont_selected <- contrasts[[v$visit]] == visit &
      contrasts[[v$arm]] == treatment_arm
    contrasts <- contrasts[cont_selected, ]
  } else {
    subset_model_frame <- stats::model.frame(
      overall_fit$fit$formula_parts$full_formula,
      data = overall_fit$fit$data[is_in_subset, , drop = FALSE]
    )
    averages <- attr(overall_fit$lsmeans, "averages")
    visit_selected <- if (visit %in% names(averages)) {
      subset_model_frame[[v$visit]] %in% averages[[visit]]
    } else {
      subset_model_frame[[v$visit]] == visit
    }
    visit_model_frame <- subset_model_frame[visit_selected, ]
    ref_model_frame <- visit_model_frame[visit_model_frame[[v$arm]] == ref_arm, ]
    n_ref <- length(unique(ref_model_frame[[v$id]]))
    trt_model_frame <- visit_model_frame[visit_model_frame[[v$arm]] == treatment_arm, ]
    n_trt <- length(unique(trt_model_frame[[v$id]]))
  }

  is_all <- var == "ALL"
  row_type <- if (is_all) "content" else "analysis"
  list(
    estimates = data.frame(
      arm = factor(c(ref_arm, treatment_arm), levels = c(ref_arm, treatment_arm)),
      n = if (ok) estimates$n else c(n_ref, n_trt),
      lsmean = if (ok) estimates$estimate else NA_real_,
      subgroup = subgroup,
      var = var,
      var_label = label,
      row_type = row_type
    ),
    contrasts = data.frame(
      n_tot = if (ok) sum(estimates$n) else (n_ref + n_trt),
      diff = if (ok) contrasts$estimate else NA_real_,
      lcl = if (ok) contrasts$lower_cl else NA_real_,
      ucl = if (ok) contrasts$upper_cl else NA_real_,
      pval = if (ok) contrasts$p_value else NA_real_,
      conf_level = overall_fit$conf_level,
      subgroup = subgroup,
      var = var,
      var_label = label,
      row_type = row_type
    )
  )
}

#' Extraction of MMRM Subgroup Results based on Population Model Definition
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This prepares LS mean estimates and contrasts for a specific visit and treatment
#' arm relative to the reference arm, along with a list of subgroup variables and corresponding
#' (grouped) factor levels.
#'
#' @param fit (`tern_mmrm`)\cr model fit on the total population.
#' @param visit (`string`)\cr single visit or name of averages of visits (referring
#'   to the averages specified when creating the `fit`).
#' @param subgroups (`character` or `NULL`)\cr names of subgroup variables to use in the
#'   forest plot, these need to be factors.
#' @param groups_lists (named `list` of `list`)\cr optionally contains for each
#'   `subgroups` variable a list, which specifies groups of factor levels, see
#'   details.
#' @param treatment_arm (`string`)\cr single treatment arm to compare with the reference
#'   arm.
#' @param label_all (`string`)\cr label for the total population analysis.
#'
#' @details The `groups_lists` argument is handy when you don't want to have
#'   subgroups identical to the original levels of the factor variable. This might
#'   be the case when you want to merge levels into a single subgroup, define
#'   overlapping subgroups or omit levels completely. Then you insert an element into
#'   `groups_lists` with the name of the `subgroups` variable and containing
#'   as a named list the subgroup definitions. See the example below.
#'
#' @note If the original model `vars` include `covariates` which are used here in
#'   `subgroups` then these are dropped from `covariates` before the corresponding
#'   model is fitted.
#'
#' @return A list with two elements:
#'
#'   - `estimates`: `data.frame` with columns `arm`, `n`, `lsmean`, `subgroup`,
#'        `var`, `var_label`, `row_type`, containing the LS means results for
#'        the overall population and the specified subgroups.
#'   - `contrasts`: `data.frame` with columns `n_tot`, `diff`, `lcl`, `ucl`,
#'        `pval`, `subgroup`, `var`, `var_label`, `row_type`. Note
#'        that this has half the number of rows as `estimates`.
#'
#' @export
#' @examples
#' mmrm_results <- fit_mmrm(
#'   vars = list(
#'     response = "FEV1",
#'     covariates = "RACE",
#'     id = "USUBJID",
#'     arm = "ARMCD",
#'     visit = "AVISIT"
#'   ),
#'   data = mmrm_test_data,
#'   cor_struct = "compound symmetry",
#'   weights_emmeans = "equal",
#'   averages_emmeans = list(
#'     "VIS1+2" = c("VIS1", "VIS2")
#'   )
#' )
#'
#' extract_mmrm_subgroups(
#'   fit = mmrm_results,
#'   visit = "VIS3",
#'   subgroups = c("RACE", "SEX"),
#'   groups_lists = list(
#'     RACE = list(
#'       A = c("Asian", "White"),
#'       B = c("Black or African American", "White")
#'     )
#'   )
#' )
extract_mmrm_subgroups <- function(fit,
                                   visit,
                                   subgroups = NULL,
                                   groups_lists = list(),
                                   treatment_arm = fit$treatment_levels[1L],
                                   label_all = "All Patients") {
  assert_class(fit, "tern_mmrm")
  assert_string(visit)
  assert_character(subgroups, null.ok = TRUE)
  assert_list(groups_lists)
  assert_string(treatment_arm)
  assert_string(label_all)

  # Get overall result (all patients) - we have it already.
  result <- h_mmrm_subgroup_df(
    lsmeans = fit$lsmeans,
    overall_fit = fit,
    is_in_subset = rep(TRUE, nrow(fit$fit$data)),
    visit = visit,
    treatment_arm = treatment_arm,
    subgroup = label_all,
    var = "ALL",
    label = label_all
  )
  if (is.null(subgroups)) {
    return(result)
  }

  # Now go over all subgroup variables and groups within them to calculate
  # subgroup results with separate MMRM fits.
  subgroup_labels <- formatters::var_labels(fit$fit$data[subgroups], fill = TRUE)
  subgroup_results <- lapply(subgroups, function(this_var) {
    assert_factor(fit$fit$data[[this_var]])

    this_groups_list <- if (!is.null(groups_lists[[this_var]])) {
      groups_lists[[this_var]]
    } else {
      tmp <- levels(fit$fit$data[[this_var]])
      stats::setNames(as.list(tmp), tmp)
    }

    # Remove this_var from covariates (if present).
    this_vars <- fit$vars
    this_vars$covariates <- setdiff(this_vars$covariates, this_var)

    this_var_results <- list()
    for (this_group in names(this_groups_list)) {
      # Create the data subset.
      this_levels <- this_groups_list[[this_group]]
      is_in_subset <- fit$fit$data[[this_var]] %in% this_levels
      this_data <- fit$fit$data[is_in_subset, , drop = FALSE]

      # Fit the corresponding model.
      args_fit_mmrm <- c(
        list(
          vars = this_vars,
          data = this_data,
          cor_struct = fit$cor_struct,
          parallel = fit$parallel,
          weights_emmeans = attr(fit$lsmeans, "weights"),
          averages_emmeans = attr(fit$lsmeans, "averages")
        ),
        fit$additional
      )
      this_fit <- tryCatch(
        expr = {
          do.call(fit_mmrm, args_fit_mmrm)
        },
        error = function(e) {
          message(paste(e, "\n"))
          return(NULL)
        }
      )
      this_var_results[[this_group]] <- h_mmrm_subgroup_df(
        lsmeans = this_fit$lsmeans,
        overall_fit = fit,
        is_in_subset = is_in_subset,
        visit = visit,
        treatment_arm = treatment_arm,
        subgroup = this_group,
        var = this_var,
        label = unname(subgroup_labels[this_var])
      )
    }
    this_var_results
  })
  results <- c(
    list(overall = result),
    unlist(subgroup_results, recursive = FALSE)
  )
  list(
    estimates = do.call(rbind, c(lapply(results, "[[", "estimates"), list(make.row.names = FALSE))),
    contrasts = do.call(rbind, c(lapply(results, "[[", "contrasts"), list(make.row.names = FALSE)))
  )
}

#' Formatted Analysis Function for MMRM Subgroups
#'
#' @param .formats (named `list`)\cr containing the formats for the statistics
#'   to use in the MMRM subgroups table.
#'
#' @return List of formatted analysis functions to be used in
#'   [tabulate_mmrm_subgroups()].
#' @keywords internal
a_mmrm_subgroups <- function(.formats) {
  assert_list(.formats)
  assert_subset(
    names(.formats),
    c("n", "n_tot", "lsmean", "diff", "ci", "pval")
  )
  Map(
    function(stat, fmt) {
      if (stat == "ci") {
        function(df, labelstr = "", ...) {
          in_rows(
            .list = tern::combine_vectors(df$lcl, df$ucl),
            .labels = as.character(df$subgroup),
            .formats = fmt
          )
        }
      } else {
        function(df, labelstr = "", ...) {
          in_rows(
            .list = as.list(df[[stat]]),
            .labels = as.character(df$subgroup),
            .formats = fmt
          )
        }
      }
    },
    stat = names(.formats),
    fmt = .formats
  )
}

#' Tabulation of MMRM Subgroups Results
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function tabulates the results from [extract_mmrm_subgroups()].
#'
#' @param lyt (`layout`)\cr input layout where analyses will be added to.
#' @param df (`list`)\cr of data frames containing all analysis variables, is the
#'   result from [extract_mmrm_subgroups()].
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `n_tot` (total number of patients per group),
#'  `n` (number of patients per treatment arm and group),
#'  `lsmean` (least square mean point estimate),
#'  `diff` (difference of least square mean estimates between treatment and reference arm),
#'  `ci` (confidence interval of difference) and
#'  `pval` (p value of the `diff`, not adjusted for multiple comparisons).
#'  Note, the statistics `n_tot`, `diff` and `ci` are required.
#' @param .formats (named `list`)\cr containing the formats for the statistics.
#' @param .labels (named `list`)\cr containing the labels for the statistics.
#'
#' @return The `rtables` object.
#' @export
#'
#' @examples
#' mmrm_results <- fit_mmrm(
#'   vars = list(
#'     response = "FEV1",
#'     covariates = "RACE",
#'     id = "USUBJID",
#'     arm = "ARMCD",
#'     visit = "AVISIT"
#'   ),
#'   data = mmrm_test_data,
#'   cor_struct = "compound symmetry",
#'   weights_emmeans = "equal",
#'   averages_emmeans = list(
#'     "VIS1+2" = c("VIS1", "VIS2")
#'   )
#' )
#'
#' df <- extract_mmrm_subgroups(
#'   fit = mmrm_results,
#'   visit = "VIS3",
#'   subgroups = c("RACE", "SEX")
#' )
#'
#' ## Table with default columns.
#' basic_table() %>%
#'   tabulate_mmrm_subgroups(df)
#'
#' ## Table with selected columns.
#' tab <- basic_table() %>%
#'   tabulate_mmrm_subgroups(
#'     df = df,
#'     vars = c("n_tot", "diff", "ci", "pval")
#'   )
#' tab
#'
#' ## Forest plot can be produced based on this very easily.
#' g_forest(tab, logx = FALSE, xlim = c(-10, 10), x_at = c(-10, -5, 0, 5, 10), vline = 0)
tabulate_mmrm_subgroups <- function(lyt,
                                    df,
                                    vars = c("n_tot", "n", "lsmean", "diff", "ci"),
                                    .formats = list(
                                      n = "xx",
                                      n_tot = "xx",
                                      lsmean = "xx.x",
                                      diff = "xx.x",
                                      ci = "(xx.x, xx.x)",
                                      pval = "x.xxxx | (<0.0001)"
                                    ),
                                    .labels = list(
                                      n = "n",
                                      n_tot = "Total n",
                                      lsmean = "Mean",
                                      diff = "Mean Difference",
                                      ci = paste0(round(100 * df$contrasts$conf_level[1]), "% CI"),
                                      pval = "p-value"
                                    )) {
  afun_lst <- a_mmrm_subgroups(.formats = .formats)
  var_labels <- unlist(.labels)
  assert_subset(x = c("n_tot", "diff", "ci"), choices = vars)

  colvars <- vars
  # The `lcl` variable is just a placeholder available in the analysis data,
  # it is not actually used in the tabulation.
  # Variables used in the tabulation are `lcl` and `ucl`
  colvars[colvars == "ci"] <- "lcl"
  colvars <- list(
    vars = colvars,
    labels = var_labels[vars]
  )
  is_estimates <- names(colvars$labels) %in% c("n", "lsmean")
  colvars_estimates <- list(
    vars = colvars$vars[is_estimates],
    labels = colvars$labels[is_estimates]
  )
  is_contrasts <- names(colvars$labels) %in% c("n_tot", "diff", "ci", "pval")
  colvars_contrasts <- list(
    vars = colvars$vars[is_contrasts],
    labels = colvars$labels[is_contrasts]
  )

  # Columns from table_estimates are optional.
  if (length(colvars_estimates$vars) > 0) {
    lyt_estimates <- split_cols_by(lyt = lyt, var = "arm")
    lyt_estimates <- split_rows_by(
      lyt = lyt_estimates,
      var = "row_type",
      split_fun = keep_split_levels("content"),
      nested = FALSE
    )
    lyt_estimates <- summarize_row_groups(
      lyt = lyt_estimates,
      var = "var_label",
      cfun = afun_lst[names(colvars_estimates$labels)]
    )
    lyt_estimates <- split_cols_by_multivar(
      lyt = lyt_estimates,
      vars = colvars_estimates$vars,
      varlabels = colvars_estimates$labels
    )

    if ("analysis" %in% df$estimates$row_type) {
      lyt_estimates <- split_rows_by(
        lyt = lyt_estimates,
        var = "row_type",
        split_fun = keep_split_levels("analysis"),
        nested = FALSE,
        child_labels = "hidden"
      )
      lyt_estimates <- split_rows_by(lyt = lyt_estimates, var = "var_label", nested = TRUE)
      lyt_estimates <- analyze_colvars(
        lyt = lyt_estimates,
        afun = afun_lst[names(colvars_estimates$labels)],
        inclNAs = TRUE
      )
    }

    table_estimates <- build_table(lyt_estimates, df = df$estimates)
  } else {
    table_estimates <- NULL
  }

  # Columns "n_tot", "diff", "ci" in table_contrasts are required.
  lyt_contrasts <- split_rows_by(
    lyt = lyt,
    var = "row_type",
    split_fun = keep_split_levels("content"),
    nested = FALSE
  )
  lyt_contrasts <- summarize_row_groups(
    lyt = lyt_contrasts,
    var = "var_label",
    cfun = afun_lst[names(colvars_contrasts$labels)]
  )
  lyt_contrasts <- split_cols_by_multivar(
    lyt = lyt_contrasts,
    vars = colvars_contrasts$vars,
    varlabels = colvars_contrasts$labels
  ) %>%
    append_topleft("Baseline Risk Factors")

  if ("analysis" %in% df$contrasts$row_type) {
    lyt_contrasts <- split_rows_by(
      lyt = lyt_contrasts,
      var = "row_type",
      split_fun = keep_split_levels("analysis"),
      nested = FALSE,
      child_labels = "hidden"
    )
    lyt_contrasts <- split_rows_by(lyt = lyt_contrasts, var = "var_label", nested = TRUE)
    lyt_contrasts <- analyze_colvars(
      lyt = lyt_contrasts,
      afun = afun_lst[names(colvars_contrasts$labels)],
      inclNAs = TRUE
    )
  }
  table_contrasts <- build_table(lyt_contrasts, df = df$contrasts)

  n_tot_id <- grep("n_tot", colvars_contrasts$vars, fixed = TRUE)
  if (is.null(table_estimates)) {
    result <- table_contrasts
    diff_id <- match("diff", colvars_contrasts$vars)
    ci_id <- match("lcl", colvars_contrasts$vars)
  } else {
    # Reorder the table.
    result <- cbind_rtables(table_contrasts[, n_tot_id], table_estimates, table_contrasts[, -n_tot_id])
    # And then calculate column indices accordingly.
    diff_id <- 1L + ncol(table_estimates) + match("diff", colvars_contrasts$vars[-n_tot_id])
    ci_id <- 1L + ncol(table_estimates) + match("lcl", colvars_contrasts$vars[-n_tot_id])
    n_tot_id <- 1L
  }

  structure(
    result,
    forest_header = paste0(levels(df$estimates$arm), "\nHigher"),
    col_x = diff_id,
    col_ci = ci_id,
    col_symbol_size = n_tot_id
  )
}
