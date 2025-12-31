# R6 Objects That Are Not Locked

This function calls the R6 class constructor in such a way that new
members can be added to the created objects.

## Usage

``` r
R6Class_(..., lock_objects = FALSE)
```

## Arguments

- ...:

  Arguments passed to the superclass constructor

- lock_objects:

  A logical value, indicates whether objects should be locked. See
  [`R6Class`](https://r6.r-lib.org/reference/R6Class.html).
