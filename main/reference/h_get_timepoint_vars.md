# Time points helper for Covariance Plot

Get the inputted symmetric matrices row and column labels as numeric
time points.

## Usage

``` r
h_get_timepoint_vars(vcov_matrix, time_prefix = NULL)
```

## Arguments

- vcov_matrix:

  (`matrix`)\
  symmetric covariance matrix with identical row and column names.

- time_prefix:

  (`string`)\
  string in the names of `vcov_matrix` that precedes the time point
  value.

## Value

This function returns a list with of two sets of numbers: `row_time` and
`col_time`, identifying the timepoints of the upper triangular part of
`vcov_matrix`.
