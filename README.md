# tern.mmrm

## Overview

`tern.mmrm` provides an interface for mixed model repeated measures (MMRM) within the `tern` framework. Please refer to
documentation of `fit_mmrm()` to see a minimal example.

## Background

For details on why we and how we implemented the MMRM in R please see the following
slides:
[Implementing Mixed Models with Repeated Measures (MMRM) in R](https://drive.google.com/file/d/1sOZUAFOc004H4jO8vuUc_4HyYHEgu45b/view)

## Installation

It is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/tern.mmrm@*release", dependencies = FALSE)
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

See package vignettes `browseVignettes(package = "tern.mmrm")` for usage of this package.
