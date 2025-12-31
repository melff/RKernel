# Register a handler for magics

Similar to ' There are pre-defined magics for LaTeX math, CSS,
Javascrippt, HTML, and iframes.

## Usage

``` r
register_magic_handler(magic, handler)
```

## Arguments

- magic:

  A character string that selects a handler

- handler:

  A function that takes at least the argument 'code' and more '...'
  arguments. The latter are constructed from the arguments of the
  percentage magic.
