# Add a handler for HTTP requests with slug given by \`slug\`.

Add a handler for HTTP requests with slug given by \`slug\`.

## Usage

``` r
add_http_handler(slug, handler)
```

## Arguments

- slug:

  The first part (slug) of the URL to be handled

- handler:

  A function that handles GET requests with URLs that have \`slug\` as
  path component
