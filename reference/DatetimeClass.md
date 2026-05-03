# Datetime Traitlets

A class and constructor of datetime traitlets.

## Usage

``` r
Datetime(...)

Time(...)
```

## Arguments

- ...:

  Arguments that are passed to the initialize method of 'TimeClass'

## Super class

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
`DatetimeClass`

## Public fields

- `value`:

  A date.

- `coerce`:

  Logical value, whether assignments to the value field should be
  coerced to the appropriate type.

## Methods

### Public methods

- [`DatetimeClass$validator()`](#method-DatetimeClass-validator)

- [`DatetimeClass$new()`](#method-DatetimeClass-initialize)

- [`DatetimeClass$clone()`](#method-DatetimeClass-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### `DatetimeClass$validator()`

Check the value assigned to the traitlet.

#### Usage

    DatetimeClass$validator(value)

#### Arguments

- `value`:

  The value assigned to the traitlet.

------------------------------------------------------------------------

### `DatetimeClass$new()`

Initialize the traitlet.

#### Usage

    DatetimeClass$new(initial = as.POSIXct(integer(0)), coerce = TRUE)

#### Arguments

- `initial`:

  An optional POSIXct object or an object coercive into such an object

- `coerce`:

  An optional logical value

------------------------------------------------------------------------

### `DatetimeClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatetimeClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
