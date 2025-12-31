# Environment Browser Widgets

This function returns a widget that allows to browse within an
environment

## Usage

``` r
envBrowser(
  pos = -1,
  name = NULL,
  envir,
  parent = NULL,
  all.names = FALSE,
  pattern = NULL,
  mode = "any"
)
```

## Arguments

- pos:

  integer indicating the [`search`](https://rdrr.io/r/base/search.html)
  position, or -1 for the current environment.

- name:

  optional name indicating a position in the search path, see
  [`ls`](https://rdrr.io/r/base/ls.html).

- envir:

  environment to use, see [`ls`](https://rdrr.io/r/base/ls.html).

- parent:

  an optional parent environment.

- all.names:

  logical; if true names starting with '.' are not omitted, see
  [`ls`](https://rdrr.io/r/base/ls.html).

- pattern:

  an optional pattern of names to restrict browsing to, see
  [`ls`](https://rdrr.io/r/base/ls.html).

- mode:

  an optional string indicating the mode of objects to which browsing is
  restricted, see [`ls`](https://rdrr.io/r/base/ls.html).
