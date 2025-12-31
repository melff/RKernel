# A Genereric Converter to JSON

The function `to_json` is a generic and idempotent interface to
[`toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

## Usage

``` r
to_json(x, auto_unbox = TRUE, ...)

# Default S3 method
to_json(x, auto_unbox = TRUE, ...)

# S3 method for class 'list'
to_json(x, auto_unbox = TRUE, ...)

# S3 method for class 'json'
to_json(x, ...)

# S3 method for class 'Trait'
to_json(x, auto_unbox = TRUE, ...)

# S3 method for class 'Vector'
to_json(x, ...)

# S3 method for class 'List'
to_json(x, auto_unbox = TRUE, ...)

# S3 method for class 'Widget'
to_json(x, ...)

# S3 method for class 'Bytes'
to_json(x, ...)
```

## Arguments

- x:

  An object to be converted as JSON

- auto_unbox:

  A logical value, whether one-element a JSON list should be changed
  into JSON scalar.

- ...:

  Other arguments, passed on to
  [`toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

## Methods (by class)

- `to_json(default)`: Default S3 method

- `to_json(list)`: S3 method for lists.

- `to_json(json)`: S3 method for JSON character strings. Returns its
  argument as is, making `to_json` idempotent.

- `to_json(Trait)`: S3 method for 'TraitClass' objects, i.e. traitlets.

- `to_json(Vector)`: S3 method for 'VectorClass' objects

- `to_json(List)`: S3 method for 'ListClass' objects

- `to_json(Widget)`: S3 method for 'WidgetClass' objects, i.e. jupyter
  widgets

- `to_json(Bytes)`: S3 method for 'BytesClass' objects
