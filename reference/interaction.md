# Interactions Using Widgets

A variety of functions to create interactive function calls

## Usage

``` r
interactive_output(
  FUN,
  controls,
  out,
  button = NULL,
  continuous_update = TRUE,
  autorun = TRUE,
  clear = FALSE,
  mime_type = "text/plain"
)

mkWidgets(...)

Interactive(
  FUN,
  ...,
  continuous_update = TRUE,
  append_output = FALSE,
  use_display = TRUE
)

interact(
  FUN,
  ...,
  continuous_update = TRUE,
  append_output = FALSE,
  use_display = TRUE
)
```

## Arguments

- FUN:

  A function to called with arguments manipulated using interactive
  widgets.

- controls:

  A list of controlling widgets, usually created with the function
  `mkwidgets`

- out:

  An output widget, i.e. a widget in class "OutputWidget"

- button:

  An (optional) button widget; when clicked, the function `FUN` is
  called.

- continuous_update:

  A logical value, if `TRUE` the function `FUN` is called whenever one
  of the controlling widgets changes a value

- autorun:

  Logical, whether the function `FUN` will be automatically called when
  any of the controlling widget values is changed or only when `button`
  is clicked.

- clear:

  Logical, whether `out` is cleared before each call of `FUN`.

- mime_type:

  A character string that specifies the mime type as which the return
  value of `FUN` is displayed.

- ...:

  Named arguments, transformed into widgets using the generic function
  [`mkWidget`](https://melff.github.io/RKernel/reference/mkWidget.md).

- append_output:

  Logical, whether existing output should be appended to or overwritten.

- use_display:

  Logical, whether the display mechanism is used internally for output
  streams.
