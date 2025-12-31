# Create an HTML iframe tag that refers to some (usually HTML) code

Create an HTML iframe tag that refers to some (usually HTML) code

## Usage

``` r
url2iframe(
  url,
  resize = FALSE,
  width = "100%",
  aspect_ratio = "16 / 10",
  height = character(0),
  class = "rkernel-iframe",
  style = "border-style:none",
  ...
)
```

## Arguments

- url:

  The URL of the page to be shown in the iframe (can also be a data URI)

- resize:

  Logical; should the iframe be resizeable?

- width:

  The intended width of the iframe, a string or a number

- aspect_ratio:

  The intended aspect ratio of the iframe, a string

- height:

  The intended height, a string. Overrides the aspect ratio if given.

- class:

  The DOM class attribute the iframe, a string

- style:

  The CSS style attribte of the iframe, a string

- ...:

  Other arguments, ignored.
