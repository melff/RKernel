# Generic Vector Traitles

A class and a constructor function to create generic vector trait(let)s.

## Usage

``` r
Vector(...)
```

## Arguments

- ...:

  Arguments that are passed to the initialize method of 'VectorClass'

## Super class

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
`Vector`

## Public fields

- `value`:

  A list

- `class`:

  A character string, the common class of the vector elements

## Methods

### Public methods

- [`Vector$validator()`](#method-Vector-validator)

- [`Vector$new()`](#method-Vector-initialize)

- [`Vector$clone()`](#method-Vector-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### `Vector$validator()`

A function that checks the validity of an assigned value, i.e. whether
the assigned value is a list with all elements of the same class

#### Usage

    Vector$validator(value)

#### Arguments

- `value`:

  A value to be assigned as the traitlet value

------------------------------------------------------------------------

### `Vector$new()`

#### Usage

    Vector$new(class = NULL, ...)

#### Arguments

- `class`:

  String, optional common class

- `...`:

  Arguments passed to the superclass initializer

------------------------------------------------------------------------

### `Vector$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Vector$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
