# List Traitlets

A class and a constructor function to create list trait(let)s.

## Usage

``` r
List(...)
```

## Arguments

- ...:

  Arguments that are passed to the initialize method of 'ListClass'

## Super class

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
`List`

## Public fields

- `value`:

  A list

## Methods

### Public methods

- [`List$validator()`](#method-List-validator)

- [`List$clone()`](#method-List-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$initialize()`](https://melff.github.io/RKernel/reference/Trait.html#method-initialize)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### `List$validator()`

A function that checks the validity of an assigned value, i.e. whether
the assigned value is a list

#### Usage

    List$validator(value)

#### Arguments

- `value`:

  A value to be assigned as the traitlet value

------------------------------------------------------------------------

### `List$clone()`

The objects of this class are cloneable with this method.

#### Usage

    List$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
