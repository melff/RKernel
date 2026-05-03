# Raw Bytes Traitlets

A class and a constructor function to create (raw) bytes trait(let)s.

## Usage

``` r
Bytes(...)
```

## Arguments

- ...:

  Arguments that are passed to the initialize method of 'BytesClass'

## Super class

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
`Bytes`

## Public fields

- `value`:

  A raw bytes vector

- `optional`:

  Logical, whether an initializing logical value must be provided

- `coerce`:

  Logical, whether 'as.raw()' is implicitely used when a value is
  assigned to the trait

## Methods

### Public methods

- [`Bytes$validator()`](#method-Bytes-validator)

- [`Bytes$new()`](#method-Bytes-initialize)

- [`Bytes$clone()`](#method-Bytes-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### `Bytes$validator()`

A validator method

#### Usage

    Bytes$validator(value)

#### Arguments

- `value`:

  A value to be checked for validity

------------------------------------------------------------------------

### `Bytes$new()`

The initializing method

#### Usage

    Bytes$new(initial = raw(0), coerce = TRUE, optional = TRUE)

#### Arguments

- `initial`:

  A value with which the traitlet is initialized

- `coerce`:

  Logical, used to initialize the 'coerce' field

- `optional`:

  Logical, used to initialize the 'optional' field

------------------------------------------------------------------------

### `Bytes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Bytes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
