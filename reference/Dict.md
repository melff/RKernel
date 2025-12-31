# Dictionary Traitlets

A class and a constructor of dictionary trait(let)s. These are lists
with unique element names.

## Usage

``` r
Dict(...)
```

## Arguments

- ...:

  Arguments that are passed to the initialize method of 'DictClass'

## Super classes

[`RKernel::Trait`](https://melff.github.io/RKernel/reference/Traitlets.md)
-\> [`RKernel::List`](https://melff.github.io/RKernel/reference/List.md)
-\> `Dict`

## Methods

### Public methods

- [`DictClass$validator()`](#method-Dict-validator)

- [`DictClass$clone()`](#method-Dict-clone)

Inherited methods

- [`RKernel::Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`RKernel::Trait$initialize()`](https://melff.github.io/RKernel/reference/Trait.html#method-initialize)
- [`RKernel::Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### Method `validator()`

A function that checks the validity of an assigned value, i.e. whether
the assigned value is a list with unique names

#### Usage

    DictClass$validator(value)

#### Arguments

- `value`:

  A value to be assigned as the traitlet value

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DictClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
