#' Helpers for Covariance Plot
#'
#' Get the inputted symmetric matrices row and column labels as numeric time points.
#'
#' @param vcov_matrix (`matrix`)\cr symmetric covariance matrix with identical
#'   row and column names.
#' @param string (`string`)\cr string in the names of `vcov_matrix` that precedes
#'   the time point value, see details.
#' @return This function returns a list with of two sets of numbers:
#'   `row_time` and `col_time`, identifying the timepoints of the upper triangular
#'   part of `vcov_matrix`.
#'
#' @details The default `string` value is `NULL`, which assumes that the names
#'   of the input matrix don't have any character string other than time point
#'   value. If a `string` is specified, this value should appear in the names
#'   of `vcov_matrix`.
#'
#' @keywords internal
h_get_timepoint_vars <- function(vcov_matrix,
                                 string = NULL) {
  assert_matrix(vcov_matrix)
  assert_string(string, null.ok = TRUE)

  diag <- upper.tri(vcov_matrix, diag = TRUE)
  vect <- vcov_matrix[diag]
  rnames <- rownames(vcov_matrix)
  cnames <- colnames(vcov_matrix)
  assert_true(identical(rnames, cnames))
  nmat <- outer(rnames, cnames, paste, sep = ":")
  names(vect) <- nmat[diag]
  split_names <- unlist(strsplit(names(vect), ":"))
  row_label <- split_names[seq(1, length(vect) * 2, by = 2)]
  col_label <- split_names[seq(2, length(vect) * 2, by = 2)]

  if (is.null(string)) {
    row_time <- suppressWarnings(as.numeric(row_label))
    col_time <- suppressWarnings(as.numeric(col_label))
    if (any(is.na(row_time)) || any(is.na(col_time))) {
      stop(
        "You have not used the `string` argument when needed\n",
        "or the names of `vcov_matrix` don't contain the time values"
      )
    }
  } else {
    if (!any(grepl(string, row_label)) && !any(grepl(string, col_label))) {
      stop("The `string` you entered is not part of the names of `vcov_matrix`")
    } else {
      row_time <- as.numeric(gsub(string, "", row_label))
      col_time <- as.numeric(gsub(string, "", col_label))
    }
  }
  list(
    row_time = row_time,
    col_time = col_time
  )
}
#' @description Plot of covariance or correlation structures as a function of lag or time. The covariance structure
#' needs to be vectorized and lag or time distances computed
#' @param  vcov_dataframe (`data frame`}\cr name of the data frame created by h_vectorization.
#' @param {x_var} (`string`)\cr can be "lag" or "time_point_distribution" for lag and time distance respectively. The default value is "lag".
#' @param legend_position (`string`)\cr denoting where the legend should be shown. Option are c("topright","topleft","bottomright","bottomleft"). Default #' value #' is "topright"
#' @examples
#' g_covariance(vcov_dataframe = h_vectorization(vcov_matrix = cholest.cov), x_var = "time_point_distribution")
g_covariance <- function(vcov_dataframe, x_var = c("lag", "time_point_distribution"), ylab = "",
                         xlab = NULL, col = tre_col, pch = NULL, lty = NULL, cex = 2,
                         legend_position = c("topright", "topleft", "bottomright", "bottomleft"), ...) {
  tre_col <- c(
    "#0080ff", "#ff00ff", "darkgreen", "#ff0000", "orange",
    "#00ff00", "brown"
  )
  x_var <- match.arg(x_var)
  if (x_var == "lag" & is.null(xlab)) {
    xlab <- "Lag"
  }
  if (x_var == "time_point_distribution" & is.null(xlab)) {
    xlab <- "Distance (time units) btw measurements"
  }
  n_col <- length(unique(vcov_dataframe$lag))
  ntp <- 1:max(vcov_dataframe$rank_row)
  if (length(col) < n_col) col <- rep(col, n_col)
  if (is.null(pch)) pch <- ntp
  if (is.null(lty)) lty <- ntp
  legend_position <- match.arg(legend_position)
  vcov_dataframe$rank_row <- as.factor(vcov_dataframe$rank_row)
  ggplot(vcov_dataframe, aes(x = .data[[x_var]], y = .data$Vect, colour = rank_row, group = rank_row)) +
    geom_point() +
    geom_line() +
    labs(colour = "From time:", x = xlab, y = ylab)
}
