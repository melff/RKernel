# Widgets to show tabular data

This is a widget that can be used to potentially large tabular data
objects, such as data frames. It is also a potential backend for the
[`View`](https://melff.github.io/RKernel/reference/View.md) function.

## Usage

``` r
virtable_widget(
  x,
  pagesize = getOption("rkernel_view_size", c(10, 10)),
  continuous_update = TRUE
)

fmt_tab_section(x, i, j)

# Default S3 method
fmt_tab_section(x, i, j)

# S3 method for class 'tbl_df'
fmt_tab_section(x, i, j)
```

## Arguments

- x:

  A tabular object, e.g. a data frame

- pagesize:

  Number of rows and columns that are shown by the widget.

- continuous_update:

  Logical, whether sliders should lead continuous updates.

- i:

  Integer values, indexing rows

- j:

  Integer values, indexing cols

## Functions

- `fmt_tab_section()`: Format a section of a tabular object to be used
  by `virtable_widget`.

- `fmt_tab_section(default)`: Default method

- `fmt_tab_section(tbl_df)`: Tibble method
