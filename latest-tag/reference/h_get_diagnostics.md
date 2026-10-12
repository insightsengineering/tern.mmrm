# Linear Mixed Model Diagnostics

Compute the model diagnostic statistics for a linear mixed model fit.

## Usage

``` r
h_get_diagnostics(fit)
```

## Arguments

- fit:

  (`mmrm`)\
  object fit with
  [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

## Value

A list with the `REML` criterion, the `AIC`, `AICc` and `BIC`.
