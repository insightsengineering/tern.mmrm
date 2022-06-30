#' Helpers for Covariance Plot
#'
#' @description get the inputted symmetric matrices row and column labels as numeric time points
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
#' @export
#'
#' @examples
#' vcov_matrix <- matrix(
#'   c(49, 24, 12, 23, 24, 35, 11, 20, 12, 11, 24, 14, 23, 20, 14, 107),
#'   nrow = 4, ncol = 4,
#'   dimnames = list(
#'     c("VIS1", "VIS2", "VIS3", "VIS4"),
#'     c("VIS1", "VIS2", "VIS3", "VIS4")
#'   )
#' )
#' h_get_timepoint_vars(vcov_matrix, string = "VIS")
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
#' @description This function vectorizes the upper-diagonal elements of a symmetric matrix (e.g. the covariance matrix)
#' and obtains the lag and time distance between pairs of observations if the time values are part of
#' the names or are part of the matrix column/row names.
#'
#' @param vcov_matrix (`matrix`)\cr name of the input symmetric matrix.
#' @return This function returns a data frame with the upper-diagonal elements or a covariance or correlation
#' matrix. In the context of repeated measures, this matrix contains the association between pairs
#' of measurements taken at different time points.
#'
#' @examples
#' vcov_matrix <- matrix(
#'   c(49, 24, 12, 23, 24, 35, 11, 20, 12, 11, 24, 14, 23, 20, 14, 107),
#'   nrow = 4, ncol = 4,
#'   dimnames = list(
#'     c("VIS1", "VIS2", "VIS3", "VIS4"),
#'     c("VIS1", "VIS2", "VIS3", "VIS4")
#'   )
#' )
#' h_vectorization(vcov_matrix, string = "VIS")
h_vectorization <- function(vcov_matrix, string = NULL) {
  assert_matrix(vcov_matrix)
  assert_true(isSymmetric(vcov_matrix))
  diag <- upper.tri(vcov_matrix, diag = T)
  vect <- vcov_matrix[diag]
  timepoints <- h_get_timepoint_vars(vcov_matrix, string)
  time_point_distribution <- timepoints$row_time - timepoints$row_time
  rank_row <- as.numeric(as.factor(timepoints$row_time))
  rank_col <- as.numeric(as.factor(timepoints$row_time))
  lag <- rank_col - rank_row
  if (any(is.na(lag)) | any(is.na(time_point_distribution))) {
    warning("Verify you have used the string argument correctly or
  that the columns and rows of your input matrix are named as expected")
  }
  vect <- data.frame(Vect = vect, time_point_distribution, lag, rank_row, rank_col)
  return(vect)
}
