# Controls for Interactive Widgets

A set of functions that can be used to create interactive widgets and to
interact with widgets.

## Usage

``` r
mkWidget(x, ...)

# S3 method for class 'integer'
mkWidget(x, description = NULL, ...)

# S3 method for class 'numeric'
mkWidget(x, description = NULL, ...)

# S3 method for class 'logical'
mkWidget(x, description = NULL, ...)

# S3 method for class 'character'
mkWidget(x, description = NULL, ...)

# S3 method for class 'Fixed'
mkWidget(x, ...)

# S3 method for class 'ValueWidget'
mkWidget(x, ...)
```

## Arguments

- x:

  an object

- ...:

  Other arguments, passed to more specific methods or ignored

- description:

  NULL or a character string that contains a description.

## Details

The function `mkWidget` is a generic function that creates a widget that
allows to manipulate the arguments of a function that is called in an
interactive widget. This generic function is called by the function
[`mkWidgets`](https://melff.github.io/RKernel/reference/interaction.md).
The function `Fixed` marks a value as fixed, so that `mkWidget` returns
it as is.
