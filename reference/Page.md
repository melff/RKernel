# Display an object using the Jupyter notebook pager

This function allows to display an *R* object in the pager of a Jupyter
notebook. Note that acts like
[`display()`](https://melff.github.io/RKernel/reference/display.md) when
Jupyter Lab is used.

## Usage

``` r
Page(x, ...)

# Default S3 method
Page(x, start = 1, ...)
```

## Arguments

- x:

  An object to be displayed in the Notebook pager

- ...:

  Other arguments, ignored or passed to specific methods.

- start:

  Integer, where to start the output.

## Methods (by class)

- `Page(default)`: S3 default method – calls `display_data` and marks it
  as pager payload
