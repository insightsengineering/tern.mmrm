# tern.mmrm

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## Overview

`tern.mmrm` provides an interface for mixed model repeated measures (MMRM) within the
[`tern`](https://insightsengineering.github.io/tern) framework
to produce commonly used tables (using [`rtables`](https://roche.github.io/rtables)) and graphs.
It builds on the R-package [`mmrm`](https://openpharma.github.io/mmrm/) for the actual MMRM calculations.

## When to use this package

If you would like to use the [`tern`](https://insightsengineering.github.io/tern) framework for
tabulation and graphs, then this package is ideal for your MMRM fits.
However if you use another reporting framework then it will be better to directly use
[`mmrm`](https://openpharma.github.io/mmrm/) and perform the tabulation and plots differently.

## Main Features

* Fitting of MMRM models to continuous longitudinal data collected over several time points
  (called visits) and optionally treatment arms.
* Tabulation and plots of least square means per visit and treatment arm.
* Tabulation of model diagnostics (e.g. BIC).
* Diagnostic graphs (e.g. Q-Q plot of residuals).
* Tabulation and plots of the covariance matrix estimate.
* Subgroup specific refitting of the MMRM model and resulting forest plot.

## Installation

### GitHub

It is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/tern.mmrm@*release")
```

### `NEST` distribution

A stable release of all `NEST` packages is also available [here](https://github.com/insightsengineering/depository#readme).

## Getting started

You can get started by trying out the example:

```r
library(tern.mmrm)
fit <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "unstructured",
  weights_emmeans = "equal",
  averages_emmeans = list(
    "VIS1+2" = c("VIS1", "VIS2")
  )
)
```

This specifies an MMRM with the `FEV1` outcome and the `RACE` and `SEX` covariates
for subjects identified by `USUBJID` and treatment arm `ARMCD` observed over time points
identified by `AVISIT` in the `mmrm_test_data` data set. An unstructured covariance
matrix is assumed. Note that only restricted maximum likelihood (REML) can be used.
The least square means assume equal weights for factor combinations, and in addition
to the single visit specific results an average across visits `VIS1` and `VIS2` will
be computed.

## Details

For a more detailed introduction to all of the features of this package, look at the introduction vignette:

```{r intro-vignette, eval = FALSE}
vignette("introduction")
```

## Stargazers

### Current

[![Stargazers repo roster for @insightsengineering/tern.mmrm](https://reporoster.com/stars/insightsengineering/tern.mmrm)](https://github.com/insightsengineering/tern.mmrm/stargazers)
[![Forkers repo roster for @insightsengineering/tern.mmrm](https://reporoster.com/forks/insightsengineering/tern.mmrm)](https://github.com/insightsengineering/tern.mmrm/network/members)

### Over time

[![Stargazers over time](https://starchart.cc/insightsengineering/tern.mmrm.svg)](https://starchart.cc/insightsengineering/tern.mmrm)
