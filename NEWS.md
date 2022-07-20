# tern.mmrm 0.1.9.9014

* Complete refactoring of the computations, which now use the new `mmrm`
  package instead of `lme4` and `lmerTest`. This increases convergence
  and speed a lot. Different covariance structures and optimizers are therefore
  now available compared to before.
* Added option `average_emmeans` to `fit_mmrm()` which allows to estimate
  least square means for averages of visits.
* Added options `show_lines`, `xlab` and `constant_baseline` to `g_mmrm_lsmeans()`
  which offer addition of lines connecting the estimates, x axis label configuration
  and optional constant baseline estimates for the LS means plots.

# tern.mmrm 0.1.1

* Removed dependency on `purrr`, `tibble`, `scda` and `scda.2022`
* Changed tests and example apps to use `mmrm_test_data` as sample data.
* Updated the `Satterthwaite DF Example` vignette.
* Updated the license.
* Updated the package authors.

# tern.mmrm 0.1.0

* Starting this separate package by moving out MMRM functionality from `tern`.
