# tern.mmrm 0.3.2

### Miscellaneous

Added `xlimits` and `ylimits` arguments to the `g_mmrm_lsmeans` function.

# tern.mmrm 0.3.1

### Miscellaneous

* Replace usage of `scda` and `scda.2022` in vignettes with `random.cdisc.data`.
* Change package maintainer to Joe Zhu.

# tern.mmrm 0.3.0

Adapt to release 0.3 of the `mmrm` package.

### New features

* Any additional arguments can be passed now via `...` to `mmrm::mmrm` when
  calling `fit_mmrm`. In particular, the `method` argument allows to choose
  Kenward-Roger adjustment of degrees of freedom and coefficients covariance
  matrix.

### Breaking changes

* The optimizer interface is different now: For choosing automatically the right
  optimizer, just omit the `optimizer` argument when calling `fit_mmrm`.
  
### Miscellaneous

* `parallelly` is now used internally to handle the determination of available cores.

# tern.mmrm 0.2.1

### Breaking changes

* Completed refactoring of the computations to use the new `mmrm` package instead
  of `lme4` and `lmerTest`. This greatly increases convergence and speed. Different
  covariance structures and optimizers are now available compared to before.

### New features

* Added function `g_covariance()` to visualize a covariance matrix, which
  can be helpful for choosing or visualizing the covariance structure in the MMRM.
* Added option `averages_emmeans` to `fit_mmrm()` which allows estimation of
  least square means for averages of visits.
* Added weights option for fitting MMRMs.

### Enhancements

* Added option `accept_singular` to `fit_mmrm()` which allows estimation of
  rank-deficient models (like `lm()` and `gls()`) by omitting the columns
  of singular coefficients from the design matrix.
* Added options `show_lines` and `xlab` to `g_mmrm_lsmeans()` which allow the
  addition of lines connecting the estimates, as well as a custom x-axis label.
* Added options `table_stats`, `table_formats`, `table_labels`, `table_font_size`,
  and `table_rel_height` to `g_mmrm_lsmeans()` which allow the addition of and
  configure the appearance of an LS means estimates statistics table below the LS
  means estimates plot.
* Added options `constant_baseline` and `n_baseline` to `g_mmrm_lsmeans()` which
  allow plotting of a constant baseline value and specifying the corresponding
  number of patients (visible in the optional table) for the LS means plots.

# tern.mmrm 0.1.1

### Miscellaneous

* Removed dependency on `purrr`, `tibble`, `scda` and `scda.2022`
* Changed tests and example apps to use `mmrm_test_data` as sample data.
* Updated the license.
* Updated the package authors.

# tern.mmrm 0.1.0

* Starting this separate package by moving out MMRM functionality from `tern`.
