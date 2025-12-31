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

[`RKernel::Trait`](https://melff.github.io/RKernel/reference/Traitlets.md)
-\> `DatetimeClass`

## Public fields

- `value`:

  A date.

- `coerce`:

  Logical value, whether assignments to the value field should be
  coerced to the appropriate type.

## Methods

### Public methods

- [`DatetimeClass$validator()`](#method-DatetimeClass-validator)

- [`DatetimeClass$new()`](#method-DatetimeClass-new)

- [`DatetimeClass$clone()`](#method-DatetimeClass-clone)

Inherited methods

- [`RKernel::Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`RKernel::Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### Method `validator()`

Check the value assigned to the traitlet.

#### Usage

    DatetimeClass$validator(value)

#### Arguments

- `value`:

  The value assigned to the traitlet.

------------------------------------------------------------------------

### Method `new()`

Initialize the traitlet.

#### Usage

    DatetimeClass$new(initial = as.POSIXct(integer(0)), coerce = TRUE)

#### Arguments

- `initial`:

  An optional POSIXct object or an object coercive into such an object

- `coerce`:

  An optional logical value

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatetimeClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
