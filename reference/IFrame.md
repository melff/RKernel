# Include HTML content using in an iframe

Display the contents of a webpage or other HTML content by including
output using an
\[iframe\](https://html.spec.whatwg.org/multipage/iframe-embed-object.html).

## Usage

``` r
IFrame(url, width = "100%", height = "70ex", class = NULL, srcdoc = FALSE)
```

## Arguments

- url:

  A character string, the URL of the content to be included

- width:

  A character string that specifies the width of the iframe

- height:

  A character string that specifies the width of the iframe

- class:

  An optional character string with DOM classes to be assigned to the
  iframe.

- srcdoc:

  Logical, whether to use a 'src' (FALSE, the default) or 'srcdoc'
  attribute.
