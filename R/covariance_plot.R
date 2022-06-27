#' Helpers for Covariance Plot
#'
#' @description get the inputted symmetric matrices row and column labels as numeric time points
#' @param vcov_matrix (`matrix`)\cr name of the input symmetric matrix.
#' @param string (`string`)\cr string in the column/row names of vcov_matrix that precedes
#' the time point value. The default value is NULL,
#' which assumes that the   column/row names of the input matrix
#' don't have any character
#' string other than time point value. If string is specified, this value should appear in the column/row names
#' of the input matrix separated from the time point value by a period.
#' @return This function returns a list with of two sets of numbers:
#' the inputted symmetric matrices row labels and column label time points.
#' @export
#'
#' @examples
#' h_get_timepoint_vars(vcov_matrix = cholest.cov)
h_get_timepoint_vars <- function(vcov_matrix, string = NULL) {
  assert_string(string, null.ok = TRUE)
  resp_name <- paste(string, sep = "")
  diag <- upper.tri(vcov_matrix, diag = T)
  vect <- vcov_matrix[diag]
  rnames <- rownames(vcov_matrix)
  cnames <- colnames(vcov_matrix)
  nmat <- outer(rnames, cnames, paste, sep = ":")
  names(vect) <- nmat[diag]
  split_names <- unlist(strsplit(names(vect), ":"))
  vcov_matrix_row_label <- split_names[seq(1, length(vect) * 2, by = 2)]
  vcov_matrix_col_label <- split_names[seq(2, length(vect) * 2, by = 2)]
  if (is.null(string)) {
    if (any(is.na(as.numeric(vcov_matrix_row_label)))) {
      stop("You have not used the string argument when needed \n
           or the col/row names of your input matrix don't have the time values")
    } else {
      numeric_vcov_matrix_row_label <- as.numeric(vcov_matrix_row_label) # tp.1
      numeric_vcov_matrix_col_label <- as.numeric(vcov_matrix_col_label) # tp.2
    }
  } else {
    if (!any(grepl(string, cnames))) {
      stop("The string you entered is not part of the column/row names of vcov_matrix")
    } else {
      numeric_vcov_matrix_row_label <- as.numeric(gsub(resp_name, "", vcov_matrix_row_label))
      numeric_vcov_matrix_col_label <- as.numeric(gsub(resp_name, "", vcov_matrix_col_label))
    }
  }
  return(list(
    numeric_vcov_matrix_row_label = numeric_vcov_matrix_row_label,
    numeric_vcov_matrix_col_label = numeric_vcov_matrix_col_label
  ))
}
