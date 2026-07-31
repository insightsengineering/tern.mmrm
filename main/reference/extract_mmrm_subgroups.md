# Extraction of MMRM Subgroup Results based on Population Model Definition

**\[experimental\]**

This prepares LS mean estimates and contrasts for a specific visit and
treatment arm relative to the reference arm, along with a list of
subgroup variables and corresponding (grouped) factor levels.

## Usage

``` r
extract_mmrm_subgroups(
  fit,
  visit,
  subgroups = NULL,
  groups_lists = list(),
  treatment_arm = fit$treatment_levels[1L],
  label_all = "All Patients"
)
```

## Arguments

- fit:

  (`tern_mmrm`)\
  model fit on the total population.

- visit:

  (`string`)\
  single visit or name of averages of visits (referring to the averages
  specified when creating the `fit`).

- subgroups:

  (`character` or `NULL`)\
  names of subgroup variables to use in the forest plot, these need to
  be factors.

- groups_lists:

  (named `list` of `list`)\
  optionally contains for each `subgroups` variable a list, which
  specifies groups of factor levels, see details.

- treatment_arm:

  (`string`)\
  single treatment arm to compare with the reference arm.

- label_all:

  (`string`)\
  label for the total population analysis.

## Value

A list with two elements:

- `estimates`: `data.frame` with columns `arm`, `n`, `lsmean`,
  `subgroup`, `var`, `var_label`, `row_type`, containing the LS means
  results for the overall population and the specified subgroups.

- `contrasts`: `data.frame` with columns `n_tot`, `diff`, `lcl`, `ucl`,
  `pval`, `subgroup`, `var`, `var_label`, `row_type`. Note that this has
  half the number of rows as `estimates`.

## Details

The `groups_lists` argument is handy when you don't want to have
subgroups identical to the original levels of the factor variable. This
might be the case when you want to merge levels into a single subgroup,
define overlapping subgroups or omit levels completely. Then you insert
an element into `groups_lists` with the name of the `subgroups` variable
and containing as a named list the subgroup definitions. See the example
below.

## Note

If the original model `vars` include `covariates` which are used here in
`subgroups` then these are dropped from `covariates` before the
corresponding model is fitted.

## Examples

``` r
mmrm_results <- fit_mmrm(
  vars = list(
    response = "FEV1",
    covariates = "RACE",
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = mmrm_test_data,
  cor_struct = "compound symmetry",
  weights_emmeans = "equal",
  averages_emmeans = list(
    "VIS1+2" = c("VIS1", "VIS2")
  )
)

extract_mmrm_subgroups(
  fit = mmrm_results,
  visit = "VIS3",
  subgroups = c("RACE", "SEX"),
  groups_lists = list(
    RACE = list(
      A = c("Asian", "White"),
      B = c("Black or African American", "White")
    )
  )
)
#> $estimates
#>    arm  n   lsmean     subgroup  var    var_label row_type
#> 1  PBO 71 43.65406 All Patients  ALL All Patients  content
#> 2  TRT 58 46.49354 All Patients  ALL All Patients  content
#> 3  PBO 42 43.21069            A RACE         RACE analysis
#> 4  TRT 45 46.71975            A RACE         RACE analysis
#> 5  PBO 44 43.91592            B RACE         RACE analysis
#> 6  TRT 34 48.26623            B RACE         RACE analysis
#> 7  PBO 35 42.96082         Male  SEX          SEX analysis
#> 8  TRT 23 46.69734         Male  SEX          SEX analysis
#> 9  PBO 36 44.20545       Female  SEX          SEX analysis
#> 10 TRT 35 46.37633       Female  SEX          SEX analysis
#> 
#> $contrasts
#>   n_tot     diff        lcl      ucl        pval conf_level     subgroup  var
#> 1   129 2.839483  0.5272786 5.151687 0.016186862       0.95 All Patients  ALL
#> 2    87 3.509058  0.5556445 6.462472 0.020028685       0.95            A RACE
#> 3    78 4.350310  1.2151947 7.485425 0.006681564       0.95            B RACE
#> 4    58 3.736518  0.2280853 7.244951 0.036955391       0.95         Male  SEX
#> 5    71 2.170876 -0.9742637 5.316016 0.175312117       0.95       Female  SEX
#>      var_label row_type
#> 1 All Patients  content
#> 2         RACE analysis
#> 3         RACE analysis
#> 4          SEX analysis
#> 5          SEX analysis
#> 
```
