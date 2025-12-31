# Get the id of an object display

This function returns the id of an object created by
[`display_data()`](https://melff.github.io/RKernel/reference/display_data.md)
or `update_display_data()`.

## Usage

``` r
display_id(x)

# S3 method for class 'display_data'
display_id(x)

# S3 method for class 'update_display_data'
display_id(x)
```

## Arguments

- x:

  An object of class "display_data" or "update_display_data"

## Value

a character string with the id.

## Methods (by class)

- `display_id(display_data)`: S3 method for "display_data" objects

- `display_id(update_display_data)`: S3 method for "update_display_data"
  objects
