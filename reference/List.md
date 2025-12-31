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

[`RKernel::Trait`](https://melff.github.io/RKernel/reference/Traitlets.md)
-\> `List`

## Public fields

- `value`:

  A list

## Methods

### Public methods

- [`ListClass$validator()`](#method-List-validator)

- [`ListClass$clone()`](#method-List-clone)

Inherited methods

- [`RKernel::Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`RKernel::Trait$initialize()`](https://melff.github.io/RKernel/reference/Trait.html#method-initialize)
- [`RKernel::Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### Method `validator()`

A function that checks the validity of an assigned value, i.e. whether
the assigned value is a list

#### Usage

    ListClass$validator(value)

#### Arguments

- `value`:

  A value to be assigned as the traitlet value

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ListClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
