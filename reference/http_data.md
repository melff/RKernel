# Handle requests with slug "data".

These request can be used to get the value of an R object, usually a
data frame.

## Usage

``` r
http_data(path, query, ...)
```

## Arguments

- path:

  The url, excluding the hostname and port, but including the slug
  "data" and the name of an R object.

- query:

  The query string translated into a character vector. The value of the
  "format" query parameter should be one of "deparse", "str", "raw",
  "cat", "json", or "print". The query parameters "rows" and "cols" can
  be used to select rows and columns.

- ...:

  Any other arguments, ignored.

## Examples

``` r
if (FALSE) { # \dontrun{
  browseURL(paste0(httpd_url(),"/data/iris?rows=1-5&format=json"))
} # }
```
