# Assertions for Datasets

We provide assertion functions to check the input dataset.

## Usage

``` r
h_assert_one_rec_pt_visit(vars, data)

h_assert_rsp_var(vars, data)

h_assert_visit_var(vars, data)

h_assert_id_var(vars, data)

h_assert_weights_var(vars, data)

h_assert_data(vars, data)
```

## Arguments

- vars:

  (`list`)\
  variables to use.

- data:

  (`data.frame`)\
  data to use.

## Value

Nothing, only fails with an error if assertion does not hold.

## Functions

- `h_assert_one_rec_pt_visit()`: asserts single record per patient and
  visit.

- `h_assert_rsp_var()`: assert numeric response variable.

- `h_assert_visit_var()`: assert factor visit variable.

- `h_assert_id_var()`: assert subject ID variable.

- `h_assert_weights_var()`: assert numeric weights variable.

- `h_assert_data()`: high-level assertion function to check the dataset.
