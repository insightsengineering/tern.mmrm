# Vectorization helper for Covariance Plot

This function vectorizes the upper-diagonal elements of a symmetric
matrix (e.g. the covariance matrix) and obtains the lag and time
distance between pairs of observations if the time values are part of
the names or are part of the matrix column and row names.

## Usage

``` r
h_vectorization(vcov_matrix, time_prefix = NULL)
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

A `data.frame` with the upper-diagonal elements of `vcov_matrix`. In the
context of repeated measures, this matrix contains the association
between pairs of measurements taken at different time points. It
contains the following columns:

- `Vect`: the upper-diagonal elements of `vcov_matrix`.

- `time_diff`: the difference between column and row times.

- `lag`: the lag, defined as the difference between column and row
  ranks.

- `rank_row`: the row rank.

- `rank_col`: the column rank.
