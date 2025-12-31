# A HTML version of 'ls.str()'

This function is now just an alias for
[`envBrowser`](https://melff.github.io/RKernel/reference/envBrowser.md).

## Usage

``` r
ls_str(
  pos = -1,
  name = NULL,
  envir,
  all.names = FALSE,
  pattern = NULL,
  mode = "any"
)
```

## Arguments

- pos:

  integer indicating [`search`](https://rdrr.io/r/base/search.html) path
  position, or -1 for the current environment.

- name:

  an optional name indicating search path position, see
  [`ls`](https://rdrr.io/r/base/ls.html).

- envir:

  the environment to look into

- all.names:

  logical value, if FALSE objects with names that start with a dot are
  ignored

- pattern:

  a character string, the pattern of the names of the objects to show

- mode:

  a character string, the mode of the objects to be shown

## See also

[`ls.str`](https://rdrr.io/r/utils/ls_str.html)
