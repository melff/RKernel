# Handle requests with slug "eval".

These request can be used to have R evaluate an expression

## Usage

``` r
http_eval(path, query, ...)
```

## Arguments

- path:

  The url, excluding the hostname and port.

- query:

  The query string translated into a character vector. The query
  argument named "expr" will be parsed and evaluated in a temporary
  environment. The result of evaluated expression will be shown in a
  format specified by the "format" query parameter. The value of the
  "format" query parameter should be one of "deparse", "str", "raw",
  "cat", "json", or "print".

- ...:

  Any other arguments, ignored.

## Examples

``` r
if (FALSE) { # \dontrun{
 browseURL(paste0(httpd_url(),"/eval?expr=Sys.time()&format=json"))
} # } 
```
