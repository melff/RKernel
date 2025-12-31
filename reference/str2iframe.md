# Create an HTML iframe tag that refers to some (usually HTML) code

Create an HTML iframe tag that refers to some (usually HTML) code

## Usage

``` r
str2iframe(
  code,
  resize = FALSE,
  width = "100%",
  aspect_ratio = "16 / 10",
  height = character(0),
  class = "rkernel-iframe",
  style = "border-style:none",
  use_srcdoc = FALSE,
  ...
)
```

## Arguments

- code:

  The code to be shown in the iframe

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

- use_srcdoc:

  A logical value, whether the 'srcdoc' attribute should be used to set
  the iframe content, rather than the 'src' attribute.

- ...:

  Other arguments, ignored.
