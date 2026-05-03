# Floating Point Number Traitlets

A class and a constructor function to create floating point vector
trait(let)s.

## Usage

``` r
Float(...)

# S3 method for class 'Float'
as.integer(x, ...)

# S3 method for class 'Float'
as.numeric(x, ...)
```

## Arguments

- ...:

  Other arguments.

- x:

  A floating point traitlet.

## Super class

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
`Float`

## Public fields

- `value`:

  A numeric vector.

- `optional`:

  Logical value, whether a length-zero value is allowed.

- `coerce`:

  Logical value, whether assignments to the value field should be
  coerced to the appropriate type.

- `length`:

  Integer number, the length the value should have.

## Methods

### Public methods

- [`Float$validator()`](#method-Float-validator)

- [`Float$new()`](#method-Float-initialize)

- [`Float$clone()`](#method-Float-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### `Float$validator()`

Check the value assigned to the traitlet.

#### Usage

    Float$validator(value)

#### Arguments

- `value`:

  The value assigned to the traitlet.

------------------------------------------------------------------------

### `Float$new()`

Initialize the traitlet.

#### Usage

    Float$new(
      initial = numeric(0),
      coerce = TRUE,
      optional = length(initial) == 0,
      length = 1L
    )

#### Arguments

- `initial`:

  A numeric vector, the initial value for the traitlet.

- `coerce`:

  coerce Logical value, whether assignments to the value field should be
  coerced to the appropriate type.

- `optional`:

  Logical value, whether a length-zero value is allowed.

- `length`:

  Integer number, the length the value should have.

------------------------------------------------------------------------

### `Float$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Float$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
