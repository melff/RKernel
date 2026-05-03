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

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
[`List`](https://melff.github.io/RKernel/reference/List.md) -\> `Dict`

## Methods

### Public methods

- [`Dict$validator()`](#method-Dict-validator)

- [`Dict$clone()`](#method-Dict-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$initialize()`](https://melff.github.io/RKernel/reference/Trait.html#method-initialize)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### `Dict$validator()`

A function that checks the validity of an assigned value, i.e. whether
the assigned value is a list with unique names

#### Usage

    Dict$validator(value)

#### Arguments

- `value`:

  A value to be assigned as the traitlet value

------------------------------------------------------------------------

### `Dict$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Dict$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
