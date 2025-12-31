# Send a GET request to an URL and read the response.

Send a GET request to an URL and read the response.

## Usage

``` r
http_get(x)
```

## Arguments

- x:

  A character string with an URL.

## Examples

``` r
# \donttest{
 http_get(paste0(httpd_url(),"/data/iris?rows=1-5&format=json"))
#> [1] "[{\"Sepal.Length\":5.1,\"Sepal.Width\":3.5,\"Petal.Length\":1.4,\"Petal.Width\":0.2,\"Species\":\"setosa\"},{\"Sepal.Length\":4.9,\"Sepal.Width\":3,\"Petal.Length\":1.4,\"Petal.Width\":0.2,\"Species\":\"setosa\"},{\"Sepal.Length\":4.7,\"Sepal.Width\":3.2,\"Petal.Length\":1.3,\"Petal.Width\":0.2,\"Species\":\"setosa\"},{\"Sepal.Length\":4.6,\"Sepal.Width\":3.1,\"Petal.Length\":1.5,\"Petal.Width\":0.2,\"Species\":\"setosa\"},{\"Sepal.Length\":5,\"Sepal.Width\":3.6,\"Petal.Length\":1.4,\"Petal.Width\":0.2,\"Species\":\"setosa\"}]"
# }
```
