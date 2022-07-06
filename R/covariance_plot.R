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
