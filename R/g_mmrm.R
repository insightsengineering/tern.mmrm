#' Diagnostic Plots for `MMRM`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function produces diagnostic plots.
#'
#' @param object (`tern_mmrm`)\cr model result produced by [fit_mmrm()].
#' @param type (`string`)\cr specifying the type of diagnostic plot to be produced:
#'
#'   - `fit-residual`: A fitted vs residuals plot, grouped by visits.
#'       This allows to see if there is remaining
#'       structure in the residuals that might be captured by adding additional
#'       covariates to the model.
#'   - `q-q-residual`: A Q-Q plot for the residuals (i.e. sorted standardized residuals
#'       vs. normal quantiles), grouped by visits. Observations with an absolute
#'       standardized residual above `z_threshold` will be labeled.
#'
#' @param z_threshold (`numeric`)\cr optional number indicating the normal quantile threshold
#'   for the Q-Q plot.
#'
#' @return A `ggplot2` plot.
#'
#' @details Here we use marginal fitted values and residuals. That is, we estimate fitted values,
#'   and the difference of those fitted values vs. the observed data are
#'   the residuals.
#'
#' @export
#'
#' @seealso [g_mmrm_lsmeans()] for plotting the least-squares means and contrasts.
#'
#' @examples
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
#'   weights_emmeans = "equal"
#' )
#' g_mmrm_diagnostic(mmrm_results)
#' g_mmrm_diagnostic(mmrm_results, type = "q-q-residual")
g_mmrm_diagnostic <- function(object,
                              type = c("fit-residual", "q-q-residual"),
                              z_threshold = NULL) {
  assert_class(object, "tern_mmrm")
  type <- match.arg(type)
  stopifnot(is.null(z_threshold) || (is.numeric(z_threshold) && z_threshold > 0))

  model <- object$fit
  vars <- object$vars
  amended_data <- stats::model.frame(model)
  amended_data$.fitted <- stats::fitted(model)
  amended_data$.resids <- stats::residuals(model, type = "response")

  result <- if (type == "fit-residual") {
    amended_data_smooth <- suppressWarnings(tryCatch(
      expr = {
        # nolint
        tern::get_smooths(amended_data, x = ".fitted", y = ".resids", groups = vars$visit, level = 0.95)
      },
      error = function(msg) {
        message(
          paste(
            "Warning: Data not amenable to the Locally Weighted Scatterplot Smoothing.",
            "\nSmooth line will not be displayed in the fit-residual plot."
          )
        )
      }
    ))
    tmp <- ggplot2::ggplot(amended_data, ggplot2::aes(x = .data$.fitted, y = .data$.resids)) +
      ggplot2::geom_point(colour = "blue", alpha = 0.3) +
      ggplot2::facet_grid(stats::as.formula(paste(". ~", vars$visit)), scales = "free_x") +
      ggplot2::geom_hline(yintercept = 0)
    if (!is.null(amended_data_smooth)) {
      tmp <- tmp + ggplot2::geom_line(
        data = amended_data_smooth,
        ggplot2::aes(x = .data$x, y = .data$y, group = !!sym(vars$visit)),
        color = "red",
        linewidth = 1.4
      ) +
        ggplot2::geom_ribbon(
          data = amended_data_smooth,
          ggplot2::aes(
            x = .data$x,
            y = NULL,
            ymin = .data$ylow,
            ymax = .data$yhigh,
            group = !!sym(vars$visit)
          ),
          alpha = 0.4,
          color = "light grey"
        )
    }
    tmp <- tmp +
      ggplot2::xlab("Fitted values") +
      ggplot2::ylab("Residuals")
  } else if (type == "q-q-residual") {
    amended_data$.scaled_resids <- stats::residuals(model, type = "pearson")

    # For each visit, calculate x and y coordinates of the specific Q-Q-plot.
    plot_data <- split(amended_data, amended_data[[vars$visit]]) %>%
      lapply(function(data) {
        res <- data.frame(
          x = stats::qnorm(stats::ppoints(data$.scaled_resids)),
          y = sort(data$.scaled_resids)
        )
        res[[vars$visit]] <- data[[vars$visit]] # Note that these are all the same.
        res
      }) %>%
      do.call(what = rbind)
    tmp <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_point(colour = "blue", alpha = 0.3) +
      ggplot2::xlab("Standard normal quantiles") +
      ggplot2::ylab("Standardized residuals") +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::facet_grid(stats::as.formula(paste(". ~", vars$visit)))
    if (!is.null(z_threshold)) {
      label_data <- plot_data
      label_data$label <- ifelse(
        abs(plot_data$y) > z_threshold,
        as.character(amended_data[[vars$id]]),
        ""
      )
      tmp <- tmp +
        ggplot2::geom_text(
          ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
          data = label_data,
          hjust = "inward",
          size = 2
        ) +
        ggplot2::coord_cartesian(clip = "off")
    }
    tmp
  }
  return(result)
}

#' Plot LS means for MMRM
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function summarizes adjusted `lsmeans` and standard error, as well as conducts
#' comparisons between groups' adjusted `lsmeans`, where the first level of the group
#' is the reference level.
#'
#' @param object (`tern_mmrm`)\cr model result produced by [fit_mmrm()].
#' @param select (`character`)\cr to select one or both of "estimates" and "contrasts" plots.
#' Note "contrasts" option is not applicable to model summaries excluding an arm variable.
#' @param titles (`character`)\cr with elements `estimates` and `contrasts` containing the plot titles.
#' @param xlab (`string`)\cr the x axis label.
#' @param ylab (`string`)\cr the y axis label.
#' @param xlimits (`numeric`)\cr x axis limits.
#' @param ylimits (`numeric`)\cr y axis limits.
#' @param width (`numeric`)\cr width of the error bars.
#' @param show_pval (`flag`)\cr should the p-values for the differences of
#'   LS means vs. control be displayed (not default)?
#' @param show_lines (`flag`)\cr should the visit estimates be connected by lines (not default)?
#' @param constant_baseline (named `number` or `NULL`)\cr optional constant baseline for the
#'   LS mean estimates. If specified then needs to be a named `number`, and the name will be used to
#'   label the corresponding baseline visit. The differences of LS means will always be 0 at this
#'   baseline visit.
#' @param n_baseline (`int` or named `integer`)\cr optional number of patients at baseline.
#'   Since this can be visible in the optional table below the estimates plot, and we cannot
#'   infer this from `object` (since that is then only fit without baseline data), we need
#'   to allow the user to specify this. If `number` then assumed constant across potential
#'   treatment arms, otherwise one number per treatment arm can be provided.
#' @param table_stats (`character`)\cr names of the statistics that will be displayed in the
#'   table below the estimates plot. Note that the table will only be added when selecting only the
#'   "estimates" plot. Available statistics are `n`, `estimate`, `se`, and `ci`.
#' @param table_formats (named `character`)\cr format patterns for descriptive statistics
#'   used in the (optional) estimates table appended to the estimates plot.
#' @param table_labels (named `character`)\cr
#'   labels for the statistics in the (optional) estimates table.
#' @param table_font_size (`number`)\cr
#'   controls the font size of values in the (optional) estimates table.
#' @param table_rel_height (`number`)\cr
#'   controls the relative height of the (optional) estimates table compared to the estimates plot.
#'
#' @return A `ggplot2` plot.
#'
#' @details If variable labels are available in the original data set, then these are used. Otherwise
#'   the variable names themselves are used for annotating the plot.
#'
#'   The contrast plot is not going to be returned if treatment is not
#'   considered in the `tern_mmrm` object input,
#'   no matter if `select` argument contains the `contrasts` value.
#'
#' @export
#'
#' @examples
#' library(dplyr)
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
#'   weights_emmeans = "equal"
#' )
#' g_mmrm_lsmeans(mmrm_results, constant_baseline = c(BSL = 0))
#' g_mmrm_lsmeans(
#'   mmrm_results,
#'   select = "estimates",
#'   show_lines = TRUE,
#'   xlab = "Visit"
#' )
#' g_mmrm_lsmeans(
#'   mmrm_results,
#'   select = "contrasts",
#'   titles = c(contrasts = "Contrasts of FKSI-FWB means"),
#'   show_pval = TRUE,
#'   show_lines = TRUE,
#'   width = 0.8
#' )
#'
#' mmrm_test_data2 <- mmrm_test_data %>%
#'   filter(ARMCD == "TRT")
#'
#' mmrm_results_no_arm <- fit_mmrm(
#'   vars = list(
#'     response = "FEV1",
#'     covariates = c("RACE", "SEX"),
#'     id = "USUBJID",
#'     visit = "AVISIT"
#'   ),
#'   data = mmrm_test_data2,
#'   cor_struct = "unstructured",
#'   weights_emmeans = "equal"
#' )
#'
#' g_mmrm_lsmeans(mmrm_results_no_arm, select = "estimates")
#' g_mmrm_lsmeans(
#'   mmrm_results_no_arm,
#'   select = c("estimates", "contrasts"),
#'   titles = c(
#'     estimates = "Adjusted mean of FKSI-FWB",
#'     contrasts = "it will not be created"
#'   ),
#'   show_pval = TRUE,
#'   width = 0.8
#' )
#'
#' g_mmrm_lsmeans(
#'   mmrm_results_no_arm,
#'   select = "estimates",
#'   titles = c(estimates = "Adjusted mean of FKSI-FWB"),
#'   show_pval = TRUE,
#'   width = 0.8,
#'   show_lines = TRUE
#' )
#'
#' g_mmrm_lsmeans(
#'   mmrm_results,
#'   select = "estimates",
#'   titles = c(estimates = "Adjusted mean of FKSI-FWB"),
#'   table_stats = c("n", "ci")
#' )
g_mmrm_lsmeans <-
  function(object,
           select = c("estimates", "contrasts"),
           titles = c(
             estimates = paste("Adjusted mean of", object$labels$response, "by treatment at visits"),
             contrasts = paste0(
               "Differences of ", object$labels$response, " adjusted means vs. control ('", object$ref_level, "')"
             )
           ),
           xlab = object$labels$visit,
           ylab = paste0("Estimates with ", round(object$conf_level * 100), "% CIs"),
           xlimits = NULL,
           ylimits = NULL,
           width = 0.6,
           show_pval = TRUE,
           show_lines = FALSE,
           constant_baseline = NULL,
           n_baseline = NA_integer_,
           table_stats = character(),
           table_formats = c(
             n = "xx.",
             estimate = "xx.x",
             se = "xx.x",
             ci = "(xx.xx, xx.xx)"
           ),
           table_labels = c(
             n = "n",
             estimate = "LS mean",
             se = "Std. Error",
             ci = paste0(round(object$conf_level * 100), "% CI")
           ),
           table_font_size = 3,
           table_rel_height = 0.5) {
    assert_class(object, "tern_mmrm")
    select <- match.arg(select, several.ok = TRUE)
    if (is.null(object$vars$arm)) {
      select <- "estimates"
      arms <- FALSE
      if (identical(names(titles), "contrasts")) {
        names(titles) <- "estimates"
      }
      titles <- titles
    } else {
      arms <- TRUE
    }
    assert_character(titles)
    assert_subset(select, names(titles))
    assert_string(xlab)
    assert_string(ylab)
    assert_number(width)
    assert_flag(show_pval)
    if (isFALSE(arms)) {
      stopifnot(sum(duplicated(object$lsmeans$estimates$AVISIT)) == 0L)
    }
    assert_flag(show_lines)
    assert_number(constant_baseline, null.ok = TRUE)
    assert_numeric(n_baseline)
    incl_const_baseline <- !is.null(constant_baseline)

    # Get relevant subsets of the estimates and contrasts data frames.
    v <- object$vars
    if (arms) {
      estimates <- object$lsmeans$estimates[, c(v$arm, v$visit, "estimate", "se", "n", "lower_cl", "upper_cl")]
      contrasts <- object$lsmeans$contrasts[, c(v$arm, v$visit, "estimate", "se", "lower_cl", "upper_cl", "p_value")]
      contrasts[[v$arm]] <- factor(contrasts[[v$arm]], levels = levels(estimates[[v$arm]]))
      contrasts$n <- NA
    } else {
      estimates <- object$lsmeans$estimates[, c(v$visit, "estimate", "se", "n", "lower_cl", "upper_cl")]
    }

    # Optionally add constant baseline estimates and 0 contrasts.
    if (incl_const_baseline) {
      baseline_visit <- names(constant_baseline)
      assert_string(baseline_visit, min.chars = 1L)
      assert_false(baseline_visit %in% levels(estimates[[v$visit]]))
      new_visit_levels <- c(baseline_visit, levels(estimates[[v$visit]]))

      baseline_est_row <- data.frame(
        visit = baseline_visit,
        estimate = constant_baseline,
        se = 0,
        n = n_baseline[1],
        lower_cl = constant_baseline,
        upper_cl = constant_baseline,
        row.names = NULL
      )
      names(baseline_est_row)[1] <- v$visit

      if (!arms) {
        assert_scalar(n_baseline, na.ok = TRUE)
        baseline_estimates <- baseline_est_row
      } else {
        baseline_cont_row <- data.frame(
          visit = baseline_visit,
          estimate = 0,
          se = 0,
          n = NA,
          lower_cl = 0,
          upper_cl = 0,
          p_value = 1,
          row.names = NULL
        )
        names(baseline_cont_row)[1] <- v$visit

        arm_lvls <- levels(estimates[[v$arm]])
        arm_levels_factor <- factor(arm_lvls, levels = arm_lvls)
        baseline_estimates <- cbind(
          arm_levels_factor,
          baseline_est_row[rep(1L, length(arm_levels_factor)), ]
        )
        n_baseline_vec <- if (length(n_baseline) > 1) {
          assert_names(names(n_baseline), permutation.of = arm_lvls)
          n_baseline[arm_lvls]
        } else {
          rep(n_baseline, length.out = length(arm_lvls))
        }
        baseline_estimates$n <- n_baseline_vec
        baseline_contrasts <- cbind(
          arm_levels_factor[-1L], # without the reference.
          baseline_cont_row[rep(1L, length(arm_levels_factor) - 1), ]
        )
        names(baseline_estimates)[1] <- names(baseline_contrasts)[1] <- v$arm

        contrasts <- rbind(baseline_contrasts, contrasts)
        contrasts[[v$visit]] <- factor(contrasts[[v$visit]], levels = new_visit_levels)
      }
      estimates <- rbind(baseline_estimates, estimates)
      estimates[[v$visit]] <- factor(estimates[[v$visit]], levels = new_visit_levels)
    }
    estimates$p_value <- NA

    plot_data <- if (identical(select, "estimates") || isFALSE(arms)) {
      cbind(estimates, type = "estimates")
    } else if (identical(select, "contrasts")) {
      cbind(contrasts, type = "contrasts")
    } else {
      contrasts <- contrasts[, colnames(estimates)]
      rbind(
        cbind(estimates, type = "estimates"),
        cbind(contrasts, type = "contrasts")
      )
    }

    pd <- ggplot2::position_dodge2(width, preserve = "total", padding = .2)

    aes_choice <- if (arms) {
      ggplot2::aes(
        x = !!sym(v$visit),
        y = .data$estimate,
        colour = !!sym(v$arm),
        group = !!sym(v$arm),
        ymin = .data$lower_cl,
        ymax = .data$upper_cl
      )
    } else {
      ggplot2::aes(
        x = !!sym(v$visit),
        y = .data$estimate,
        ymin = .data$lower_cl,
        ymax = .data$upper_cl
      )
    }

    result <- ggplot2::ggplot(plot_data, aes_choice) +
      ggplot2::geom_errorbar(width = width, position = pd) +
      ggplot2::geom_point(position = pd) +
      ggplot2::scale_color_discrete(
        name = if (arms) object$labels$arm else NULL,
        drop = FALSE # To ensure same colors for only contrasts plot.
      ) +
      ggplot2::ylab(ylab) +
      ggplot2::xlab(xlab) +
      ggplot2::facet_wrap(
        ~type,
        nrow = length(select),
        scales = "free_y", # Since estimates and contrasts need to have different y scales.
        labeller = ggplot2::as_labeller(titles)
      ) +
      ggplot2::coord_cartesian(ylim = ylimits, xlim = xlimits)
    if (show_lines) {
      result <- result +
        ggplot2::geom_line(position = pd)
    }
    if ("contrasts" %in% select) {
      result <- result +
        ggplot2::geom_hline(
          data = data.frame(type = "contrasts", height = 0),
          ggplot2::aes(yintercept = .data$height),
          colour = "black"
        )
      if (show_pval) {
        pval_data <- plot_data[plot_data$type == "contrasts", ]
        pval_data$y_pval <- ifelse(
          as.numeric(pval_data[[v$arm]]) %% 2,
          pval_data$upper_cl,
          pval_data$lower_cl
        )
        pval_data$vjust <- ifelse(
          as.numeric(pval_data[[v$arm]]) %% 2,
          -0.1,
          +1.1
        )
        pval_data$y_total <- pval_data$y_pval + pval_data$vjust
        pval_data$label <- ifelse(
          pval_data$p_value < 0.0001,
          "<0.0001",
          sprintf("%.4f", pval_data$p_value)
        )
        result <- result +
          ggplot2::geom_text(
            data = pval_data,
            mapping = ggplot2::aes(y = .data$y_pval, vjust = .data$vjust, label = .data$label),
            position = pd,
            show.legend = FALSE
          ) +
          ggplot2::coord_cartesian(clip = "off")
      }
    } else if (length(table_stats) > 0) {
      table_stats <- match.arg(
        table_stats,
        choices = c("n", "estimate", "se", "ci"),
        several.ok = TRUE
      )
      est_stats_tab <- NULL
      strata_vars <- c(v$arm, v$visit)
      for (i in seq_len(nrow(estimates))) {
        x_list <- as.list(estimates[i, ])
        x_list$ci <- c(x_list$lower_cl, x_list$upper_cl)
        this_row <- cbind(
          estimates[i, strata_vars, drop = FALSE],
          tern::h_format_row(
            x = x_list[table_stats],
            format = table_formats,
            labels = table_labels
          )
        )
        est_stats_tab <- rbind(est_stats_tab, this_row)
      }

      stats_lev <- rev(setdiff(colnames(est_stats_tab), strata_vars))

      est_stats_tab <- est_stats_tab %>%
        tidyr::pivot_longer(
          cols = -dplyr::all_of(strata_vars),
          names_to = "stat",
          values_to = "value",
          names_ptypes = list(stat = factor(levels = stats_lev))
        )

      tbl <- ggplot2::ggplot(
        est_stats_tab,
        ggplot2::aes(
          x = !!sym(v$visit),
          y = .data$stat,
          label = .data$value
        )
      ) +
        ggplot2::geom_text(size = table_font_size) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(hjust = 0),
          strip.text.x = ggplot2::element_text(margin = ggplot2::margin(1.5, 0, 1.5, 0, "pt")),
          strip.background = ggplot2::element_rect(fill = "grey95", color = NA),
          legend.position = "none"
        )

      # Facet by treatment arm, if available.
      if (arms) {
        tbl <- tbl + ggplot2::facet_wrap(
          facets = v$arm,
          ncol = 1
        )
      }

      # Align estimates plot and estimates table.
      result <- result +
        ggplot2::theme(
          legend.position = "bottom"
        )
      result <- cowplot::plot_grid(
        result,
        tbl,
        ncol = 1,
        align = "v",
        rel_heights = c(1, table_rel_height)
      )
    }
    return(result)
  }
