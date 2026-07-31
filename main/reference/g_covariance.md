# Visualization of Covariance Matrix

**\[experimental\]**

Plot of covariance (or correlation) matrix as a function of lag or time.
The covariance structure is vectorized internally and lag or time
distances are computed and can be used for visualization.

## Usage

``` r
g_covariance(
  vcov_matrix,
  time_prefix = NULL,
  x_var = c("lag", "time_diff"),
  xlab = NULL,
  ylab = ""
)
```

## Arguments

- vcov_matrix:

  (`matrix`)\
  symmetric covariance matrix with identical row and column names.

- time_prefix:

  (`string`)\
  string in the names of `vcov_matrix` that precedes the time point
  value.

- x_var:

  (`string`)\
  can be `lag` or `time_diff` for lag or time difference, respectively.

- xlab:

  (`string` or `NULL`)\
  x-axis label, if `NULL` then automatically determined from `x_var`.

- ylab:

  (`string`)\
  y-axis label.

## Value

The `ggplot` object.

## Details

The default `time_prefix` value is `NULL`, which assumes that the names
of the input matrix don't have any character string other than time
point value. If a `time_prefix` is specified, this string should appear
in front of all the names in `vcov_matrix`.

## Examples

``` r
vcov_matrix <- matrix(
  c(49, 12, 12, 23),
  nrow = 2, ncol = 2,
  dimnames = list(
    c(1, 2),
    c(1, 2)
  )
)
g_covariance(vcov_matrix, x_var = "time_diff")
```
