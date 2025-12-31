# Invoke a Data Viewer

This is a re-implementation of
[`View`](https://rdrr.io/r/utils/View.html) that works within Jupyter
notebooks by leveraging the [Jupyter
widgets](https://ipywidgets.readthedocs.io) infrastructure or by using
the [DataTables](https://datatables.net/) Javascript library. The latter
is the case if the system option "View.backend" is set to "dataTable" or
if this option is not set. Otherwise a 'virtable_widget' is used.

## Usage

``` r
View(x, title = deparse(substitute(x)), ...)

# Default S3 method
View(x, title = deparse(substitute(x)), ...)

# S3 method for class 'data.frame'
View(x, title = deparse(substitute(x)), ...)

# S3 method for class 'data.set'
View(x, title = deparse(substitute(x)), ...)

# S3 method for class 'importer'
View(x, title = deparse(substitute(x)), ...)
```

## Arguments

- x:

  An R object which can be coerced to a data frame with non-zero numbers
  of rows and columns.

- title:

  A string used as title. Currently unused.

- ...:

  Other arguments, ignored.
