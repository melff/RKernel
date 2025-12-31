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

[`RKernel::Trait`](https://melff.github.io/RKernel/reference/Traitlets.md)
-\> `Vector`

## Public fields

- `value`:

  A list

- `class`:

  A character string, the common class of the vector elements

## Methods

### Public methods

- [`VectorClass$validator()`](#method-Vector-validator)

- [`VectorClass$new()`](#method-Vector-new)

- [`VectorClass$clone()`](#method-Vector-clone)

Inherited methods

- [`RKernel::Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`RKernel::Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### Method `validator()`

A function that checks the validity of an assigned value, i.e. whether
the assigned value is a list with all elements of the same class

#### Usage

    VectorClass$validator(value)

#### Arguments

- `value`:

  A value to be assigned as the traitlet value

------------------------------------------------------------------------

### Method `new()`

#### Usage

    VectorClass$new(class = NULL, ...)

#### Arguments

- `class`:

  String, optional common class

- `...`:

  Arguments passed to the superclass initializer

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VectorClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
