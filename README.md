# tern.mmrm

## Overview

`tern.mmrm` provides an interface for mixed model repeated measures (MMRM) within `tern` framework. Please refer to
documentation of `fit_mmrm()` to see a minimal example.

## Background

For details on why we and how we implemented the MMRM in R please see the following
slidedeck:
[Implementing Mixed Models with Repeated Measures (MMRM) in R](https://drive.google.com/file/d/1sOZUAFOc004H4jO8vuUc_4HyYHEgu45b/view)

For details on the methodology please see [Chapter 6](https://docs.roche.com/doc/statistics_clinical_trials/latest/mixed-effect-models-with-repeated-measures-mmrm.html) in the NEST Statistics book.

## Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/tern.mmrm@*release", dependencies = FALSE)
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

In order to run many of the examples you will also need to install the [`scda`](https://github.com/insightsengineering/scda) package.
