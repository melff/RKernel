# Time Traitlets

A class and constructor of time traitlets.

## Super class

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
`TimeClass`

## Public fields

- `value`:

  A date.

- `coerce`:

  Logical value, whether assignments to the value field should be
  coerced to the appropriate type.

## Methods

### Public methods

- [`TimeClass$validator()`](#method-TimeClass-validator)

- [`TimeClass$new()`](#method-TimeClass-initialize)

- [`TimeClass$clone()`](#method-TimeClass-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### `TimeClass$validator()`

Check the value assigned to the traitlet.

#### Usage

    TimeClass$validator(value)

#### Arguments

- `value`:

  The value assigned to the traitlet.

------------------------------------------------------------------------

### `TimeClass$new()`

Initialize the traitlet.

#### Usage

    TimeClass$new(initial = as.POSIXct(integer(0)), coerce = TRUE)

#### Arguments

- `initial`:

  An optional POSIXct object or an object coercive into such an object

- `coerce`:

  An optional logical value

------------------------------------------------------------------------

### `TimeClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TimeClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
