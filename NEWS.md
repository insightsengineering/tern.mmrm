# tern.mmrm 0.2.0

### Breaking changes

* Complete refactoring of the computations, which now use the new `mmrm`
  package instead of `lme4` and `lmerTest`. This increases convergence
  and speed substantially. Different covariance structures and optimizers are
  therefore now available compared to before.

### New features

* Added function `g_covariance()` which visualizes a covariance matrix, which
  can be helpful for choosing or visualizing the covariance structure in the MMRM.
* Added option `average_emmeans` to `fit_mmrm()` which allows to estimate
  least square means for averages of visits.

### Enhancements

* Added option `accept_singular` to `fit_mmrm()` which allows to estimate
  rank-deficient models (similar as `lm()` and `gls()` do) by omitting the columns
  of singular coefficients from the design matrix.
* Added options `show_lines` and `xlab` to `g_mmrm_lsmeans()`
  which offer addition of lines connecting the estimates, as well as a custom
  x-axis label.
* Added options `table_stats`, `table_formats`, `table_labels`, `table_font_size`,
  `table_rel_height` to `g_mmrm_lsmeans` which offer the addition of LS means
  estimates statistics table below the LS means estimates plot, and control the
  appearance details of it.
* Added options `constant_baseline` and `n_baseline` to `g_mmrm_lsmeans()`
  which allow to plot a constant baseline value and to specify the
  corresponding number of patients (visible in the optional table) for the LS means
  plots.

# tern.mmrm 0.1.1

### Miscellaneous

* Removed dependency on `purrr`, `tibble`, `scda` and `scda.2022`
* Changed tests and example apps to use `mmrm_test_data` as sample data.
* Updated the license.
* Updated the package authors.

# tern.mmrm 0.1.0

* Starting this separate package by moving out MMRM functionality from `tern`.
