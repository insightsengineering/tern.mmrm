# Helper for Extraction and Formatting of MMRM Subgroups

This is an internal helper to extract the correct LS mean estimates and
contrast for a specific visit and treatment arm relative to the
reference arm for one MMRM subgroup.

## Usage

``` r
h_mmrm_subgroup_df(
  lsmeans,
  overall_fit,
  is_in_subset,
  visit,
  treatment_arm,
  subgroup,
  var,
  label
)
```

## Arguments

- lsmeans:

  (named `list` or `NULL`)\
  LS mean estimates from
  [`fit_mmrm()`](https://insightsengineering.github.io/tern.mmrm/reference/fit_mmrm.md)
  on this subgroup.

- overall_fit:

  (`tern_mmrm`)\
  result of
  [`fit_mmrm()`](https://insightsengineering.github.io/tern.mmrm/reference/fit_mmrm.md)
  on overall data.

- is_in_subset:

  (`logical`)\
  specifying which row from the overall data should be used for this
  subset.

- visit:

  (`string`)\
  which visit to extract.

- treatment_arm:

  (`string`)\
  which treatment arm to extract.

- subgroup:

  (`string`)\
  for labeling in the resulting `data.frame`.

- var:

  (`string`)\
  specifies which variable was used to derive the subset, if `ALL` then
  this means the overall data was used.

- label:

  (`string`)\
  variable label.

## Value

List with `estimates` (with 2 rows) and `contrasts` (with 1 row) in the
format needed in
[`extract_mmrm_subgroups()`](https://insightsengineering.github.io/tern.mmrm/reference/extract_mmrm_subgroups.md).
