# A Unicode String Vector Trait

A Unicode String Vector Trait

## Usage

``` r
# S3 method for class 'Unicode'
as.character(x, ...)
```

## Arguments

- x:

  A Unicode traitlet

- ...:

  Other arguments, ignored.

## Functions

- `as.character(Unicode)`: Coerce a unicode string trait to a character
  vector

## Super class

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
`Unicode`

## Public fields

- `coerce`:

  Logical value, whether values should be coerced to character strings.

- `length`:

  Length of the unicode character vector

- `value`:

  The value of the unicode character vector

- `optional`:

  A logical value, whether value can be empty.

## Methods

### Public methods

- [`Unicode$validator()`](#method-Unicode-validator)

- [`Unicode$new()`](#method-Unicode-initialize)

- [`Unicode$clone()`](#method-Unicode-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### `Unicode$validator()`

A validator function

#### Usage

    Unicode$validator(value)

#### Arguments

- `value`:

  The value to be assigned

------------------------------------------------------------------------

### `Unicode$new()`

Initialize an object

#### Usage

    Unicode$new(
      initial = character(0),
      coerce = TRUE,
      optional = length(initial) == 0,
      length = 1L
    )

#### Arguments

- `initial`:

  An initial value

- `coerce`:

  Logical value, whether values should be coerced to character stringes

- `optional`:

  Logical value, whether the value may be empty

- `length`:

  Integer, the intended length of the unicode vector

------------------------------------------------------------------------

### `Unicode$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Unicode$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
