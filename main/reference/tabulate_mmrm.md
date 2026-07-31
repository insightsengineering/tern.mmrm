# Tabulation of `MMRM` Results

**\[stable\]**

These functions can be used to produce tables from a fitted `MMRM`
produced with
[`fit_mmrm()`](https://insightsengineering.github.io/tern.mmrm/reference/fit_mmrm.md).

## Usage

``` r
# S3 method for class 'tern_mmrm'
as.rtable(x, type = c("fixed", "cov", "diagnostic"), ...)

h_mmrm_fixed(x, format = "xx.xxxx")

h_mmrm_cov(x, format = "xx.xxxx")

h_mmrm_diagnostic(x, format = "xx.xxxx")

# S3 method for class 'tern_mmrm'
tidy(x, ...)

s_mmrm_lsmeans(df, .in_ref_col, show_relative = c("reduction", "increase"))

a_mmrm_lsmeans(df, .in_ref_col, show_relative = c("reduction", "increase"))

s_mmrm_lsmeans_single(df)

a_mmrm_lsmeans_single(df)

summarize_lsmeans(
  lyt,
  arms = TRUE,
  ...,
  table_names = "lsmeans_summary",
  .stats = NULL,
  .formats = NULL,
  .indent_mods = NULL,
  .labels = NULL
)
```

## Arguments

- x:

  (`tern_mmrm`)\
  the original result from
  [`fit_mmrm()`](https://insightsengineering.github.io/tern.mmrm/reference/fit_mmrm.md).

- type:

  (`string`)\
  type of table which should be returned.

- ...:

  additional argument `format` for controlling the numeric format.

- format:

  (`string`)\
  format for the numbers in the table.

- df:

  (`data frame`)\
  data set containing all analysis variables.

- .in_ref_col:

  (`logical`)\
  `TRUE` when working with the reference level, `FALSE` otherwise.

- show_relative:

  should the "reduction" (`control - treatment`, default) or the
  "increase" (`treatment - control`) be shown for the relative change
  from baseline?

- lyt:

  (`layout`)\
  input layout where analyses will be added to.

- arms:

  (`flag`)\
  should treatment variable be considered when using `summarize_lsmeans`
  layout generating function.

- table_names:

  (`character`)\
  this can be customized in case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

- .stats:

  (`character`)\
  statistics to select for the table.

- .formats:

  (named `character` or `list`)\
  formats for the statistics.

- .indent_mods:

  (named `integer`)\
  indent modifiers for the labels.

- .labels:

  (named `character`)\
  labels for the statistics (without indent).

## Value

`as.rtable.tern_mmrm()` returns the fixed effects, covariance estimate
or diagnostic statistics tables.

## Functions

- `as.rtable(tern_mmrm)`: Produce simple `MMRM` tables via the generic
  [`as.rtable()`](https://rdrr.io/pkg/tern/man/as.rtable.html).

- `h_mmrm_fixed()`: Helper function to produce fixed effects table.

- `h_mmrm_cov()`: Helper function to produce a covariance matrix table.

- `h_mmrm_diagnostic()`: Helper function to produce a diagnostic
  statistics table.

- `tidy(tern_mmrm)`: Helper method (for
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)) to
  prepare a `data.frame` from an `tern_mmrm` object containing the
  least-squares means and contrasts.

- `s_mmrm_lsmeans()`: Statistics function which is extracting estimates
  from a tidied least-squares means data frame.

- `a_mmrm_lsmeans()`: Formatted Analysis function which can be further
  customized by calling
  [`rtables::make_afun()`](https://rdrr.io/pkg/rtables/man/make_afun.html)
  on it. It is used as `afun` in
  [`rtables::analyze()`](https://rdrr.io/pkg/rtables/man/analyze.html).

- `s_mmrm_lsmeans_single()`: Statistics function which is extracting
  estimates from a tidied least-squares means data frame when `ARM` is
  not considered in the model.

- `a_mmrm_lsmeans_single()`: Formatted Analysis function (when `ARM` is
  not considered in the model) which can be further customized by
  calling
  [`rtables::make_afun()`](https://rdrr.io/pkg/rtables/man/make_afun.html)
  on it. It is used as `afun` in
  [`rtables::analyze()`](https://rdrr.io/pkg/rtables/man/analyze.html).

- `summarize_lsmeans()`: Analyze function for tabulating least-squares
  means estimates from tidied `mmrm` results.

## Examples

``` r
result <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "unstructured",
  weights_emmeans = "equal"
)
as.rtable(result, type = "cov", format = "xx.x")
#>        VIS1   VIS2   VIS3   VIS4
#> ————————————————————————————————
#> VIS1   40.6   14.4   5.0    13.4
#> VIS2   14.4   26.6   2.8    7.5 
#> VIS3   5.0    2.8    14.9   0.9 
#> VIS4   13.4   7.5    0.9    95.6

result_no_arm <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    id = "USUBJID",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "unstructured",
  weights_emmeans = "equal"
)
as.rtable(result_no_arm, type = "cov", format = "xx.x")
#>        VIS1   VIS2   VIS3   VIS4
#> ————————————————————————————————
#> VIS1   43.3   17.8   7.3    18.1
#> VIS2   17.8   30.4   6.0    10.7
#> VIS3   7.3    6.0    17.0   5.0 
#> VIS4   18.1   10.7   5.0    98.9
df <- broom::tidy(result)
df_no_arm <- broom::tidy(result_no_arm)
s_mmrm_lsmeans(df[8, ], .in_ref_col = FALSE)
#> $n
#> [1] 67
#> 
#> $adj_mean_se
#> [1] 52.78422  1.18776
#> 
#> $adj_mean_ci
#> [1] 50.43481 55.13362
#> attr(,"label")
#> [1] "95% CI"
#> 
#> $diff_mean_se
#> [1] 4.398457 1.680545
#> 
#> $diff_mean_ci
#> [1] 1.074493 7.722422
#> attr(,"label")
#> [1] "95% CI"
#> 
#> $change
#> [1] -0.09090396
#> attr(,"label")
#> [1] "Relative Reduction (%)"
#> 
#> $p_value
#> [1] 0.009886854
#> 
s_mmrm_lsmeans_single(df_no_arm[4, ])
#> $n
#> [1] 134
#> 
#> $adj_mean_se
#> [1] 50.6510864  0.8508787
#> 
#> $adj_mean_ci
#> [1] 48.96834 52.33383
#> attr(,"label")
#> [1] "95% CI"
#> 
library(dplyr)

dat_adsl <- mmrm_test_data %>%
  select(USUBJID, ARMCD) %>%
  unique()
basic_table() %>%
  split_cols_by("ARMCD", ref_group = result$ref_level) %>%
  add_colcounts() %>%
  split_rows_by("AVISIT") %>%
  summarize_lsmeans(
    .stats = c("n", "adj_mean_se", "adj_mean_ci", "diff_mean_se", "diff_mean_ci"),
    .labels = c(adj_mean_se = "Adj. LS Mean (Std. Error)"),
    .formats = c(adj_mean_se = sprintf_format("%.1f (%.2f)"))
  ) %>%
  build_table(
    df = broom::tidy(result),
    alt_counts_df = dat_adsl
  )
#>                                             PBO                TRT       
#>                                           (N=105)             (N=95)     
#> —————————————————————————————————————————————————————————————————————————
#> VIS1                                                                     
#>   n                                          68                 66       
#>   Adj. LS Mean (Std. Error)             33.3 (0.76)        37.1 (0.76)   
#>   95% CI                              (31.839, 34.825)   (35.599, 38.613)
#>   Difference in Adjusted Means (SE)                       3.774 (1.074)  
#>   95% CI                                                  (1.651, 5.897) 
#> VIS2                                                                     
#>   n                                          69                 71       
#>   Adj. LS Mean (Std. Error)             38.2 (0.61)        41.9 (0.60)   
#>   95% CI                              (36.963, 39.380)   (40.713, 43.094)
#>   Difference in Adjusted Means (SE)                       3.732 (0.859)  
#>   95% CI                                                  (2.035, 5.430) 
#> VIS3                                                                     
#>   n                                          71                 58       
#>   Adj. LS Mean (Std. Error)             43.7 (0.46)        46.8 (0.51)   
#>   95% CI                              (42.760, 44.588)   (45.748, 47.761)
#>   Difference in Adjusted Means (SE)                       3.081 (0.690)  
#>   95% CI                                                  (1.716, 4.445) 
#> VIS4                                                                     
#>   n                                          67                 67       
#>   Adj. LS Mean (Std. Error)             48.4 (1.19)        52.8 (1.19)   
#>   95% CI                              (46.035, 50.737)   (50.435, 55.134)
#>   Difference in Adjusted Means (SE)                       4.398 (1.681)  
#>   95% CI                                                  (1.074, 7.722) 

basic_table() %>%
  split_rows_by("AVISIT") %>%
  summarize_lsmeans(arms = FALSE) %>%
  build_table(
    df = broom::tidy(result_no_arm),
    alt_counts_df = dat_adsl
  )
#>                            all obs     
#> ———————————————————————————————————————
#> VIS1                                   
#>   n                          134       
#>   Adjusted Mean (SE)    35.191 (0.550) 
#>     95% CI             (34.105, 36.277)
#> VIS2                                   
#>   n                          140       
#>   Adjusted Mean (SE)    40.039 (0.455) 
#>     95% CI             (39.140, 40.938)
#> VIS3                                   
#>   n                          129       
#>   Adjusted Mean (SE)    45.029 (0.361) 
#>     95% CI             (44.314, 45.744)
#> VIS4                                   
#>   n                          134       
#>   Adjusted Mean (SE)    50.651 (0.851) 
#>     95% CI             (48.968, 52.334)
```
