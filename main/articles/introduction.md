# Introduction to tern.mmrm

## Introduction

Mixed effect Models with Repeated Measures (MMRM) are often used as the
primary analysis of continuous longitudinal endpoints in clinical
trials. In this setting, an MMRM is a specific linear mixed effects
model that includes as fixed effects the variables: treatment arm,
categorical visit, treatment by visit interaction, and other covariates
for adjustment (e.g. age, gender). The covariance structure of the
residuals can take on different forms. Often, an unstructured
(i.e. saturated parametrization) covariance matrix is assumed which can
be represented by random effects in the mixed model.

All of this has been implemented in proprietary software such as `SAS`,
whose `PROC MIXED` routine is generally considered the gold standard for
mixed models. However, this does not allow the use of interactive web
applications to explore the clinical study data in a flexible way.
Therefore, we wanted to implement MMRM in R in a way which could easily
be embedded into a `shiny` application. See the [`teal.modules.clinical`
package](https://insightsengineering.github.io/teal.modules.clinical/)
for more details about using this code inside a `shiny` application.

## Descriptive Statistics

Descriptive statistics for a relevant analysis population (e.g. those
patients with at least one post baseline visit) can be obtained by the
functions described in Section @ref(baseline-tables).

## Methodology

### Statistical model

Under the linear mixed model (`LMM`) framework, an outcome vector \${\bf
Y}\$ is modeled as \$\$\bf Y = X \boldsymbol \beta + Z b + {\boldsymbol
\varepsilon}\$\$ where \$\bf X\$ is the matrix for fixed effects, \$\bf
Z\$ is the matrix for random effects,
$`\mathbf b \sim N(0, \mathbf D(\boldsymbol \theta))`$, and
$`{\boldsymbol \varepsilon} \sim N(0, \mathbf R(\boldsymbol \theta))`$.
Letting $`\mathbf V = \mathbf Z \mathbf D \mathbf Z^T + \mathbf R`$, the
marginal and conditional models are then given by \$\mathbf Y \sim N(\bf
X \boldsymbol \beta, V)\$ and \$\mathbf Y \| \mathbf b \sim N(\bf X
\boldsymbol \beta + Z b, R)\$, respectively.

An MMRM is a special case of a `LMM` such that \$\bf Y\$ is a collection
of measurements made on a set of individuals over time, i.e. \$\mathbf Y
= ({\bf Y}\_1^T, {\bf Y}\_2^T, ...)^T\$ where
$`\mathbf Y_i = \mathbf X_i \boldsymbol \beta + \mathbf Z_i \mathbf b_i + {\boldsymbol \varepsilon}_i`$.
In our context of clinical trials, individuals are patients identified
by their unique subject `id`, measurements are of treatment `response`,
and fixed effects include treatment `arm`, categorical `visit`,
treatment by visit interaction, and potentially other `covariates`
(e.g. age, gender).

### Estimation

The parameters $`\boldsymbol \beta`$, \$\bf b\$, and
$`\boldsymbol \theta`$ can be estimated by maximizing the penalized and
restricted maximum likelihoods. The average treatment response at each
visit is often of particular interest. However, simply comparing
predicted marginal responses $`E(Y_{ij})`$ averaged across treatment x
visit groups will not account for potential imbalance in covariates.
(Imbalance may occur even in randomized trials due to patients dying or
dropping out over time.) For a more fair comparison, **least-squares
(LS) mean**:

1.  establishes a reference grid where each cell represents a unique
    combination of the factor (covariate) levels,
2.  calculates the predicted marginal response for each cell, and then
3.  takes a weighted average of the predicted marginal responses.

The following simple example illustrates the concept of LS means.
Suppose we have a longitudinal clinical trial where three factors are
considered: treatment (A and B), visit (1, 2, …), and gender (M and F).
The marginal predicted response (sample size) for each reference cell is
as follows.

|     |    A1    |   B1    |  …  |
|:----|:--------:|:-------:|:---:|
| M   | 100 (20) | 90 (35) |     |
| F   | 50 (30)  | 40 (15) |     |

The average predicted responses for arms A and B at Visit 1 are
$`(100 \times 20 + 50 \times 30) / 50 = 70`$ and
$`(90 \times 35 + 40 \times 15) / 50 = 75`$, respectively. The overall
mean is higher in arm B than in arm A even though the mean is lower
within each gender category. This seeming contradiction is caused by the
imbalance in the data. LS mean calculates the weighted average across
cells of the same treatment and visit. In this example, this is
equivalent to taking the weighted average over each column. One may
assign *equal* weights to each cell,
i.e. $`0.5 \times 100 + 0.5 \times 50 = 75`$ and
$`0.5 \times 90 + 0.5 \times 40 = 65`$. Alternatively, one may assign
weights *proportional* to the observed frequencies of the factor
combinations, i.e. $`0.55 \times 100 + 0.45 \times 50 = 77.5`$ and
$`0.55 \times 90 + 0.45 \times 40 = 67.5`$. In both cases, the LS mean
of response is lower in arm B than in arm A.

LS means are calculated in `tern.mmrm` via the R package `emmeans`.
Users have the option to weigh marginal predicted responses with either
`equal` weights or `proportional` weights. Note that for proportional
weights, the weights are calculated at each visit by taking into account
the observed frequencies of factor combinations at that time. Therefore,
even though covariate imbalance may vary over time, LS mean provides an
adjusted analysis of treatment response at all visits.

### Inference

Performing inference on estimated parameters (e.g. calculating p-values)
is less straightforward for MMRM. This is because the exact null
distributions for parameter effects are unknown. `SAS` addresses this
issue by utilizing Satterthwaite’s method to approximate the adjusted
degrees of freedom for $`F`$ and $`t`$ tests. `lme4` and `lmerTest` have
also implemented Satterthwaite’s method. Unfortunately we found that
these are not robust in their convergence behavior. Compared to `lme4`,
the R package `nlme` can consider more flexible covariance structures.
However, we have chosen not to use this package because it does not
provide exact Satterthwaite adjusted degrees of freedom and the
available approximate degrees of freedom can differ substantially.
Therefore we built the new package `mmrm`. With `mmrm`, `tern.mmrm` is
able to replicate outputs from `SAS`.

### Covariance structure

Users of `tern.mmrm` have currently the following options for the
covariance structure $`\mathbf V_i`$:

- Unstructured:
  ``` math
  V_{ij} = \theta_{ij}
  ```
- Homogeneous AR(1):
  ``` math
  (\mathbf V_i)_{jk} = \sigma^2 \rho^{|j-k|}
  ```
- Heterogeneous AR(1):
  ``` math
  (\mathbf V_i)_{jk} = \sigma_j \sigma_k \rho^{|j-k|}
  ```
- Heterogeneous Toeplitz:
  ``` math
  (\mathbf V_i)_{jk}=\sigma_j \sigma_k \theta_{|j-k|}
  ```
- Heterogeneous Ante-Dependence:
  ``` math
  (\mathbf V_i)_{jk} = \sigma_j \sigma_k \prod_{i=j}^{k}\rho_i
  ```

## Model fitting

In this section, we illustrate how to fit a MMRM with `tern.mmrm` and
how to fit a MMRM manually in R. We then compare with `SAS` to show that
the results match.

### Setup

Our example dataset consists of several variables: subject ID
(`USUBJID`), visit number (`AVISIT`), treatment (`ARMCD` = `TRT` or
`PBO`), 3-category race, sex, `FEV1` at baseline (%), and `FEV1` at
study visits (%). `FEV1` (forced expired volume in one second) is a
measure of how quickly the lungs can be emptied. Low levels of `FEV1`
may indicate chronic obstructive pulmonary disease (`COPD`). The
scientific question at hand is whether treatment leads to an increase in
`FEV1` over time after adjusting for baseline covariates.

``` r

library(tern.mmrm)
#> Loading required package: tern
#> Loading required package: rtables
#> Loading required package: formatters
#> 
#> Attaching package: 'formatters'
#> The following object is masked from 'package:base':
#> 
#>     %||%
#> Loading required package: magrittr
#> 
#> Attaching package: 'rtables'
#> The following object is masked from 'package:utils':
#> 
#>     str
#> Registered S3 method overwritten by 'tern':
#>   method   from 
#>   tidy.glm broom
data(mmrm_test_data)
head(mmrm_test_data)
#> # A tibble: 6 × 7
#>   USUBJID AVISIT ARMCD RACE                      SEX    FEV1_BL  FEV1
#>   <fct>   <fct>  <fct> <fct>                     <fct>    <dbl> <dbl>
#> 1 PT1     VIS1   TRT   Black or African American Female    25.3  NA  
#> 2 PT1     VIS2   TRT   Black or African American Female    25.3  40.0
#> 3 PT1     VIS3   TRT   Black or African American Female    25.3  NA  
#> 4 PT1     VIS4   TRT   Black or African American Female    25.3  20.5
#> 5 PT2     VIS1   PBO   Asian                     Male      45.0  NA  
#> 6 PT2     VIS2   PBO   Asian                     Male      45.0  31.5
```

### Model fitting via `tern.mmrm`

Fitting MMRM is easy in `tern.mmrm`. By default, the model fitting
function `fit_mmrm` assumes unstructured correlation and proportional
weights when calculating LS means.

``` r

mmrm_results <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX", "FEV1_BL"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "unstructured",
  weights_emmeans = "proportional"
)
```

The resulting object consists among other things the estimated random
and fixed effects (`fit`), the estimated LS means for a particular
treatment at a particular visit (`lsmeans`), and the difference in LS
means at a particular visit (`lsmeans$contrasts`). Based on the output,
there is evidence that supports treatment leading to an increase in
`FEV1` over placebo at all study visits.

``` r

mmrm_results$lsmeans$contrasts
#>   ARMCD AVISIT estimate        se       df lower_cl upper_cl   t_stat
#> 1   TRT   VIS1 3.983290 1.0454036 142.3210 1.916765 6.049815 3.810289
#> 2   TRT   VIS2 3.930758 0.8135131 142.2576 2.322622 5.538895 4.831831
#> 3   TRT   VIS3 2.983718 0.6656674 129.6093 1.666737 4.300699 4.482295
#> 4   TRT   VIS4 4.404001 1.6604869 132.8789 1.119595 7.688407 2.652235
#>        p_value relative_reduc
#> 1 2.058960e-04    -0.12101832
#> 2 3.460313e-06    -0.10424997
#> 3 1.606280e-05    -0.06893864
#> 4 8.969722e-03    -0.09154581
```

Other correlation structures and weighting schemes can be considered.

### Model fitting manually in R

One can reproduce these results manually in R. For example, to fit the
unstructured model:

``` r

library(mmrm)
#> 
#> Attaching package: 'mmrm'
#> The following object is masked from 'package:tern.mmrm':
#> 
#>     fit_mmrm
library(emmeans)
#> Welcome to emmeans.
#> Caution: You lose important information if you filter this package's results.
#> See '? untidy'

fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = mmrm_test_data
)
emmeans::emmeans(fit, pairwise ~ ARMCD | AVISIT, weights = "proportional")
#> $emmeans
#> AVISIT = VIS1:
#>  ARMCD emmean    SE  df lower.CL upper.CL
#>  PBO     32.9 0.732 141     31.5     34.4
#>  TRT     36.9 0.743 141     35.4     38.4
#> 
#> AVISIT = VIS2:
#>  ARMCD emmean    SE  df lower.CL upper.CL
#>  PBO     37.7 0.576 142     36.6     38.8
#>  TRT     41.6 0.570 140     40.5     42.8
#> 
#> AVISIT = VIS3:
#>  ARMCD emmean    SE  df lower.CL upper.CL
#>  PBO     43.3 0.441 127     42.4     44.2
#>  TRT     46.3 0.494 130     45.3     47.2
#> 
#> AVISIT = VIS4:
#>  ARMCD emmean    SE  df lower.CL upper.CL
#>  PBO     48.1 1.170 133     45.8     50.4
#>  TRT     52.5 1.170 133     50.2     54.8
#> 
#> Results are averaged over the levels of: RACE, SEX 
#> Confidence level used: 0.95 
#> 
#> $contrasts
#> AVISIT = VIS1:
#>  contrast  estimate    SE  df t.ratio p.value
#>  PBO - TRT    -3.98 1.050 142  -3.810  0.0002
#> 
#> AVISIT = VIS2:
#>  contrast  estimate    SE  df t.ratio p.value
#>  PBO - TRT    -3.93 0.814 142  -4.832 <0.0001
#> 
#> AVISIT = VIS3:
#>  contrast  estimate    SE  df t.ratio p.value
#>  PBO - TRT    -2.98 0.666 130  -4.482 <0.0001
#> 
#> AVISIT = VIS4:
#>  contrast  estimate    SE  df t.ratio p.value
#>  PBO - TRT    -4.40 1.660 133  -2.652  0.0090
#> 
#> Results are averaged over the levels of: RACE, SEX
```

Compared to manual implementation, `tern.mmrm` users can avoid
specifying the `formula` manually. Note also that `tern.mmrm` has a
default optimizer which is in fact the same as the one shown here. If
the default fails to converge, it automatically attempts to use multiple
other optimizers, delivering results should there be a consensus.

### Model fitting via `SAS`

For the unstructured model, the equivalent `SAS` code is given by

``` sas
proc import datafile="location_of_data.csv"
     out=dat
     dbms=csv
     replace;
     getnames=yes;
run;

proc mixed data= dat;
       class ARMCD(ref="PBO") AVISIT(ref="VIS1") USUBJID SEX(ref="Male") RACE(ref="Asian");
       Model FEV1 = ARMCD  RACE  SEX  FEV1_BL  AVISIT  ARMCD*AVISIT    / solution ddfm=satterthwaite;
       repeated AVISIT / type=UN subject=USUBJID;
       lsmeans ARMCD*AVISIT/pdiff obsmargins cl;
run;
```

## Diagnostics

### Model fitting

A key part in the analysis of data is model selection, where often the
desire is to select a parsimonious model that adequately fits the data.
We provide four of the same information criteria as found in `SAS`
`PROC MIXED`.

``` r

mmrm_results$diagnostics
#> $`REML criterion`
#> [1] 3361.379
#> 
#> $AIC
#> [1] 3381.379
#> 
#> $AICc
#> [1] 3381.807
#> 
#> $BIC
#> [1] 3414.211
```

Each criterion is specifically calculated as follows:

1.  `REML` criterion: $`-2 l`$ where $`l`$ is the `REML` log-likelihood.
    This criterion has no penalty for complexity, so generally decreases
    as model becomes more complex.
2.  `AIC`: $`-2l + 2d`$ where $`d`$ is the number of covariance
    parameters.
3.  `AICc`: $`-2l + 2dn^*/(n^*-d-1)`$ where $`n^*`$ is the number of
    observations minus the number of fixed effects, or $`d + 2`$ if it
    is smaller than that. `AICc` is a finite-sample corrected version of
    `AIC`.
4.  `BIC`: $`-2l + d \log n`$ where $`n`$ is the number of subjects as
    opposed to the number of observations.

An objective approach to model selection is to minimize the `AIC`,
`AICc`, or `BIC`. For more details on these information criteria
(including references), see [SAS
Documentation](https://documentation.sas.com/?cdcId=statcdc&cdcVersion=14.2&docsetId=statug&docsetTarget=statug_mixed_syntax01.htm&locale=en#statug.mixed.procstmt_ic).

### Residual analysis

Another key part in the analysis of data is verifying model assumptions.
`g_mmrm_diagnostic(fit, type = ...)` provides several handy residual
plots, each of which is diagnostic for a different aspect of MMRMs.

- Marginal fitted values ($`\mathbf X_i \widehat {\boldsymbol \beta}`$)
  vs. Marginal residuals
  ($`\mathbf Y_i - \mathbf X_i \widehat{\boldsymbol \beta}`$)
  - Argument: `type = "fit-residual"`
  - Diagnostic for: homoskedasticity of marginal residuals
- QQ plot of standardized marginal residuals
  ($`\widehat{\mathbf V}_i^{-1/2} (\mathbf Y_i - \mathbf X_i \widehat{\boldsymbol \beta})`$)
  - Argument: `type = q-q-residual`
  - Diagnostic for: normality of marginal residuals

## Outlook

We are actively looking to expand the capabilities of `tern.mmrm`. In
particular we are working on the foundational package `mmrm` to include
more features, such as treatment group specific covariance matrices and
additional covariance structures.
