# Handle requests with slug "echo".

A GET request from URL \`http://localhost:XYZ/echo\` will just return
the request.

## Usage

``` r
http_echo(path, query, ...)
```

## Arguments

- path:

  The url, excluding the hostname and port. Will be "/echo".

- query:

  The query string translated into a character vector.

- ...:

  Any other arguments, ignored.
