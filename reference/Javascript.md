# Send Javascript to the frontend

Send Javascript code in a character string or a text file to the
frontend.

## Usage

``` r
Javascript(text, file, as_tag = FALSE)
```

## Arguments

- text:

  A character string with Javascript code

- file:

  Path of a file with Javascript code

- as_tag:

  Logical, whether to return a '\<script\>' tag

## Value

An S3 object of class "display_data" with mime data of type
"application/javascript"
