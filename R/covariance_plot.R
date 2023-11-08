#' Time points helper for Covariance Plot
#'
#' Get the inputted symmetric matrices row and column labels as numeric time points.
#'
#' @param vcov_matrix (`matrix`)\cr symmetric covariance matrix with identical
#'   row and column names.
#' @param time_prefix (`string`)\cr string in the names of `vcov_matrix` that
#'   precedes the time point value.
#' @return This function returns a list with of two sets of numbers:
#'   `row_time` and `col_time`, identifying the timepoints of the upper triangular
#'   part of `vcov_matrix`.
#'
#' @keywords internal
h_get_timepoint_vars <- function(vcov_matrix,
                                 time_prefix = NULL) {
  assert_matrix(vcov_matrix)
  assert_string(time_prefix, null.ok = TRUE)

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

  if (is.null(time_prefix)) {
    row_time <- suppressWarnings(as.numeric(row_label))
    col_time <- suppressWarnings(as.numeric(col_label))
    if (any(is.na(row_time)) || any(is.na(col_time))) {
      stop(
        "You have not used the `string` argument when needed\n",
        "or the names of `vcov_matrix` don't contain the time values"
      )
    }
  } else {
    if (!any(grepl(time_prefix, row_label)) && !any(grepl(time_prefix, col_label))) {
      stop("The `string` you entered is not part of the names of `vcov_matrix`")
    } else {
      row_time <- as.numeric(gsub(time_prefix, "", row_label))
      col_time <- as.numeric(gsub(time_prefix, "", col_label))
    }
  }
  list(
    row_time = row_time,
    col_time = col_time
  )
}

#' Vectorization helper for Covariance Plot
#'
#' This function vectorizes the upper-diagonal elements of a symmetric matrix
#' (e.g. the covariance matrix) and obtains the lag and time distance between
#' pairs of observations if the time values are part of the names or are part of
#' the matrix column and row names.
#'
#' @inheritParams h_get_timepoint_vars
#'
#' @return A `data.frame` with the upper-diagonal elements of `vcov_matrix`.
#'   In the context of repeated measures, this matrix contains the association between pairs
#'   of measurements taken at different time points. It contains the following columns:
#'
#' - `Vect`: the upper-diagonal elements of `vcov_matrix`.
#' - `time_diff`: the difference between column and row times.
#' - `lag`: the lag, defined as the difference between column and row ranks.
#' - `rank_row`: the row rank.
#' - `rank_col`: the column rank.
#'
#' @keywords internal
h_vectorization <- function(vcov_matrix, time_prefix = NULL) {
  assert_matrix(vcov_matrix)
  assert_true(isSymmetric(vcov_matrix))
  diag <- upper.tri(vcov_matrix, diag = TRUE)
  vect <- vcov_matrix[diag]
  timepoints <- h_get_timepoint_vars(vcov_matrix, time_prefix)
  time_diff <- timepoints$col_time - timepoints$row_time
  rank_row <- as.numeric(as.factor(timepoints$row_time))
  rank_col <- as.numeric(as.factor(timepoints$col_time))
  lag <- rank_col - rank_row
  if (any(is.na(lag)) || any(is.na(time_diff))) {
    warning(paste(
      "Verify you have used the string argument correctly or",
      "that the columns and rows of your input matrix are named as expected"
    ))
  }
  data.frame(Vect = vect, time_diff, lag, rank_row, rank_col)
}

#' Visualization of Covariance Matrix
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Plot of covariance (or correlation) matrix as a function of lag or time.
#' The covariance structure is vectorized internally and lag or time distances
#' are computed and can be used for visualization.
#'
#' @inheritParams h_get_timepoint_vars
#' @param x_var (`string`)\cr can be `lag` or `time_diff` for lag
#'   or time difference, respectively.
#' @param xlab (`string` or `NULL`)\cr x-axis label, if `NULL` then automatically
#'   determined from `x_var`.
#' @param ylab (`string`)\cr y-axis label.
#' @return The `ggplot` object.
#'
#' @details The default `time_prefix` value is `NULL`, which assumes that the names
#'   of the input matrix don't have any character string other than time point
#'   value. If a `time_prefix` is specified, this string should appear in front of
#'   all the names in `vcov_matrix`.
#'
#' @export
#' @examples
#' vcov_matrix <- matrix(
#'   c(49, 12, 12, 23),
#'   nrow = 2, ncol = 2,
#'   dimnames = list(
#'     c(1, 2),
#'     c(1, 2)
#'   )
#' )
#' g_covariance(vcov_matrix, x_var = "time_diff")
g_covariance <- function(vcov_matrix,
                         time_prefix = NULL,
                         x_var = c("lag", "time_diff"),
                         xlab = NULL,
                         ylab = "") {
  x_var <- match.arg(x_var)
  if (is.null(xlab)) {
    xlab <- if (x_var == "lag") "Lag" else "Distance (time units) btw measurements"
  }
  vcov_dataframe <- h_vectorization(vcov_matrix, time_prefix)
  vcov_dataframe$rank_row <- as.factor(vcov_dataframe$rank_row)
  ggplot(vcov_dataframe, aes(x = .data[[x_var]], y = .data$Vect, colour = .data$rank_row, group = .data$rank_row)) +
    geom_point() +
    geom_line() +
    labs(colour = "From time:", x = xlab, y = ylab)
}
