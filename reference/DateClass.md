# Date Traitlets

A class and constructor of date traitlets.

## Usage

``` r
Date(...)

# S3 method for class 'DateClass'
as.Date(x, ...)
```

## Arguments

- ...:

  Other arguments.

- x:

  A date traitlet.

## Super class

[`RKernel::Trait`](https://melff.github.io/RKernel/reference/Traitlets.md)
-\> `DateClass`

## Public fields

- `value`:

  A date.

- `coerce`:

  Logical value, whether assignments to the value field should be
  coerced to the appropriate type.

## Methods

### Public methods

- [`DateClass$validator()`](#method-DateClass-validator)

- [`DateClass$new()`](#method-DateClass-new)

- [`DateClass$clone()`](#method-DateClass-clone)

Inherited methods

- [`RKernel::Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`RKernel::Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### Method `validator()`

Check the value assigned to the traitlet.

#### Usage

    DateClass$validator(value)

#### Arguments

- `value`:

  The value assigned to the traitlet.

------------------------------------------------------------------------

### Method `new()`

Initialize the traitlet.

#### Usage

    DateClass$new(
      initial = as.Date(integer(0)),
      year = integer(0),
      month = integer(0),
      day = integer(0),
      coerce = TRUE
    )

#### Arguments

- `initial`:

  An optional Date object or date string

- `year`:

  An optional integer

- `month`:

  An optional integer

- `day`:

  An optional integer

- `coerce`:

  An optional logical value

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DateClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
